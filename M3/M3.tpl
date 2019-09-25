// =========================================================================================================================================== 
//
//                         		           Modifiable Multistock Model (M3)                      
//
//                                	           	        v6.2                                 
//
//                                   		           21th Sept 2019                          
//                                                                     
//                           		            ICCAT GBYP, Tom Carruthers UBC 
//
// ===========================================================================================================================================
//
//
// -- About M3 --
//
// M3 is a spatial, multi-stock, multi-fleet, seasonal, hybrid-SRA-statistical catch-at-length stock assessment model. M3 was designed to be 
// used as an operating model that could be fitted to various data to predict spatial stock structure of a multi-stock fishery (specifically 
// Atlantic bluefin tuna). In this regard M3 includes several simplifications over a conventional age-structured stock assessment in order to
// reduce calculations and ensure that the estimation problem is well defined. 
//
// M3 is currently in a developmental (beta) stage. Any comments, bugs or otherwise can be forwarded to t.carruthers@fisheries.ubc.ca. 
//
// M3 was compiled using ADMB 11.5 (64 bit) under Windows 10 using mingw64 
//
//
// -- Acknowledgements --
//
// Model structure and assumptions follow the recommendations of the ICCAT MSE technical team and BFT working group.
//
//    ------------------------------------------------------------------------------------------ 
//       ---------------------------- ICCAT MSE TECHNICAL GROUP ----------------------------
//                   Doug Butterworth, Haritz Arrizabalaga, John Walter, Ana Gordoa, 
//                 Gary Melvin, Shuya Nakatsuka, Ai Kimoto, Alex Hanke, Tom Carruthers, 
//                    Nick Duprey
//          
//                                    Previous collaborators: 
//                 Joe Powers, David Die, Toshi Kitakado, Campbell Davies, Laurie Kell,
//         Antonio Di Natale, Paul de Bruyn, Yukio Takeuchi, Sylvain Bonhommeau, Clay Porch 
//    ------------------------------------------------------------------------------------------
//
//
// M3 uses Steve Martell's stats.cxx library (publically available as part of the iSCAM stock assessment package, https://github.com/smartell/iSCAM)
//
//
// -- Some conventions ------
//
// 1) Matrices are arranged in order of population, year, season
// 2) Timesteps include subyears and are indexed to year and subyear by yind and sind, respectively
// 3) Stock separation is not explicit in the data except for stock of origin, some PSAT (not PSAT2) and some conventional tags
// 4) Movement by subyear is the movement into an area in that subyear: models runs in the order: F(y-1) M(y-1), mov(y), F(y), M(y), mov(y+1),...
// 5) Recapture probabilities are 100% assigned to the release area in first time step after which they are affected by movement e.g. for 
//    a 5 area model (0,1,0,0,0) (0.05,0.7,0.1,0.1,0.5),...
// 6) First subyear of first year is for initialisation and there is no recorded fishing
// 7) Rows in movement matrix with no recorded movements are assigned 1/nareas probability to avoid the vanishing fish problem (which 
//    would occur if all areas were assigned zero probability)
//
//-- Change v6.2 ------------
//
// * Fishery independent indices can now be related to spawning stock biomass by quarter and region
// * Fishery independent indices can now be related to vulnerable numbers of fish for both stocks (Iobs mode 5)
// * Priors on regional seasonality now possible (SpatPr)
//
//-- Change v6.1 ------------
//
// * Per datum weighting (all data)
// * Per datum CV for indices 
// * Length ranges for CPUE indices
// * Truncated fleet selectivities
// * Fleet mirroring for indices
//
//-- Changes v5.2 -----------
//
// * Ilencat
//
//-- Changes v5.1 -----------
//
// * bounded selectivity
//
//-- Changes v5.0 -----------
//
// * The annual deviations from master indices: FDYt
// * Selectivity mirroring
// * Size-specific indices, same q - ie two indices can be for the same fleet with one q but correspond with varying size classes (e.g. US RR 145cm +)
//
// -- To do minor ----------- 
//
// * infleclim is no longer used in the code below - update model writing functions etc
// * Investigate speed improvements from assigning data predictions to vector and using vectorized likelihoods
//
// -- To do major -----------
//
// * Dynamic age-length key based on linear interpolation between discrete growth groups
// * If required, convert PSAT2 likelihood to movm configuration
// 



// ==========================================================================================================================================


DATA_SECTION
	
	// -- Model Dimensions --
	init_int nHy;                          // Number of historical years
	init_int ny;                           // Number of years
	init_int ns;                           // Number of sub-year time steps
	init_int np;                           // Number of stocks
	init_int na;                           // Maximum age
	init_int nr;                           // Number of areas
	init_int nf;	                       // Number of fleets			
	init_int nl;                           // Number of length classes 
	init_int nRPT;                         // Maximimum number of repapture probability time-steps (seasons)
	init_matrix RPTind(1,ns,1,nRPT);       // The correct subyear index for timesteps after initial subyear e.g. for release in s=2: 2,3,4,1,2,3...
	init_vector sdur(1,ns);                // Subyear duration e.g. 0.3,0.1,0.2,0.4
	init_int nydist;                       // Number of years over which initial stock distribution is calculated (prior to spool up)
	init_vector ml(1,nl);                  // Mean length in each length class (used for calculating selectivity only) 
		
	// -- Growth ------------
	init_4darray ALK(1,np,1,ny,1,na,1,nl); // Age-Length Key by stock
	init_vector lwa(1,np);                 // Length-weight conversion factor a w=al^b
		
	init_vector  lwb(1,np);                // Length-weight conversion factor b w=al^b
	init_3darray len_age(1,ny,1,na,1,np);  // Length at age (pass through for independently calculating biomass from .rep file)
	init_3darray wt_age(1,ny,1,na,1,np);   // Weight at age (pass through for independently calculating biomass from .rep file)
		
	// -- Maturity ----------
	init_matrix Fec(1,np,1,na);            // SSB by age by stock (previously a fecundity calculation)
	
	// -- Stock recruit steepness 
	init_int nSR;                          // number of SR estimated 
	init_vector SRminyr(1,nSR);            // starting model year for each SR
	init_vector SRmaxyr(1,nSR);            // ending model year for each SR
	init_ivector nRDs(1,4);                // Up to four blocks of recruitment deviations, how long (yrs) are each?
	init_matrix RDts(1,np,1,ny);           // Up to four blocks of recruitment deviations, how long (yrs) are each?
	init_matrix RDno(1,np,1,ny);           // Up to four blocks of recruitment deviations, how long (yrs) are each?
	init_vector SRp(1,nSR);                // stock for each SR
	init_vector SRpar(1,nSR);              // Steepness of the stock-recruitment curve
	init_vector SSBpR(1,np);               // Spawning stock biomass per recruit
	
	// -- Spawning ----------
	init_vector spawns(1,np);              // The subyear in which the stock spawns
	init_matrix canspawn(1,np,1,nr);       // The areas in which spawning occurs (1) or not (0) for each stock
	
	// -- Natural Mortality rate --
	init_matrix Ma(1,np,1,na);             // Instantaneous natural mortality rate at age by stock
		
	// -- Fishery data ------
	init_int nCobs;                        // Number of catch observations by year, subyear, area, fleet 
	init_matrix Cobs(1,nCobs,1,7);         // Catch observations
	
	init_int nCPUEq;                       // Number of estimated catchabilities for CPUE indices
	init_int nCPUEobs;                     // Number of rows of CPUE data 
	init_matrix CPUEobs(1,nCPUEobs,1,9);   // A table of CPUE index observations by year, subyear, area, fleet, index No. (nE), cv, wt
	//init_vector CPUEwt(1,nCPUEq);        // Individual fitting weights by CPUE index
		
	init_int nE;                           // Number of effort series used / partial F's
	init_int nEobs;                        // Number of effort observations by year, subyear, area, fleet, index No. (nE) 
	init_matrix Eobs(1,nEobs,1,6);         // Observed effort observations
	
	init_int nCLobs;                       // Number of Catch-at-length observations
	init_matrix CLobs(1,nCLobs,1,7);       // Catch-at-length observations by year, subyear, area, fleet, length category, N
	
	init_4darray HCobs(1,nHy,1,ns,1,na,1,nr); // Historical catch observations Hy, subyear, area, fleet
	
	init_3darray RAI(1,nr,1,ns,1,ny);      // The real relative abundance index from the simulation / cpue index (this is pass-through data so that output fit can be summarized without other files)
	
	// -- Fishery independent indices --
	init_int nI;                           // Number of fishery independent indices
	init_int nIobs;                        // Number of relative abundance index observations by timestep, area 
	init_matrix Iobs(1,nIobs,1,11);        // Observed relative index observations year, subyear, area, stock, index number (nI), type (1) biomass (2) SSB, index, fleet (if type=4), cv, wt
	//init_vector Iwt(1,nI);               // Individual fitting weights by fishery independent index
	
	// -- PSAT tags ----------
	init_int nPSAT;                        // Number of PSAT recapture events (known SOO)
	init_matrix PSAT(1,nPSAT,1,8);         // PSAT release-recapture events from stock, age class, subyear, timestep, from-area, to-area, N, Frac
	init_int nPSAT2;                       // Number of PSAT recapture events (not known SOO)
	init_matrix PSAT2(1,nPSAT2,1,8);       // PSAT2 release-recapture events from age class, year, subyear, timestep, from-area, to-area, prob p=1, p=2...
	
	// -- Conventional tags --
	init_int nTag;                         // Number of conventional tag observations
	init_matrix Tag(1,nTag,1,10);          // Tag release-recpature from Year/subyear/area/age to Year/subyear/area/fleet/age, N 
		
	// -- Stock of origin ----
	init_int nSOOobs;                      // Number of stock of origin observations by stock, age, year, subyear, area, fleet, N
	init_matrix SOOobs(1,nSOOobs,1,9);     // Stock of origin data p a y s r N type (1 = otolith mc, 2 = genetics)
	
	
	// -- Selectivity controls --
	init_int nsel;                         // Number of estimated selectivities
	init_ivector seltype(1,nsel);          // 2: logistic, 3: Thompson dome-shaped
	init_vector selind(1,nf);              // Which selectivity is assigned to each fleet
	init_vector ratiolim(1,2);             // Limits on logistic slope parameter relative to inflection point
	init_vector infleclim(1,2);            // Limits on modal selectivity 
	init_vector LB(1,nf);                  // Lower bound on selectivity (selectivities are zero below this)
	init_vector UB(1,nf);                  // Upper bound on selectivity (selectivities are zero above this)
	
		
	// -- Movement estimation --
	init_int nMP;                          // Number of movement parameters estimated
	init_int nma;                          // Number of movement age-classes
	init_matrix ma(1,np,1,na);             // Movement age classes by age for each stock
	init_matrix macat(1,nma,1,2);          // Age class bounds 
	init_int nmovind;                      // Number of non-residency paramters estimated              
	init_matrix movind(1,nmovind,1,5);     // Index of estimable movements (stock, age class, subyear, area from, area to)
	init_int nmov1;                        // Number of unestimated zero (logspace) movements
	init_matrix mov1(1,nmov1,1,5);         // Index of first non-estimated movement parameter (fixed to zero)
	init_int movtype;                      // 1: gravity (nr), 2: Markov ((nr-1) x nr), 3: fractional (nr-1, no viscosity)
	init_int nMovExc;                      // Number of movement exclusions
	init_matrix MovExc(1,nMovExc,1,6);     // Movement exclusions: 1: stock, 2: age class, 3: season, 4: from area, 5: to area
	
	// -- Indexing ---
	init_int nIlencat;
	init_matrix Ilencat(1,nIlencat,1,2);   // the upper and lower bounds of length categories for the fleet vulnerability schedules
		
	// -- Observation errors --
	//init_vector CobsCV(1,nf);            // Catch observation error   
	//init_vector CPUEobsCV(1,nCPUEq);     // CPUE observation error
	//init_vector IobsCV(1,nI);            // Fishery independent index obs error (only if complexF = 1)
	init_number CLCV_num;                  // Numerator of the Maunder normal composition likelihood function
	
	// -- Priors --
	init_number RDCV;                      // Recruitment deviation penalty	
	init_int nSSBprior;                    // n SSBprior
	init_matrix SSBprior(1,nSSBprior,1,3); // SSB prior
	init_number SSBCV;                     // SSB CV
	init_int nDepprior;                    // n Depprior
	init_matrix Depprior(1,nDepprior,1,3); // Depletion prior
	init_number DepCV;                     // Depletion CV
	init_matrix BSfrac(1,np,1,ns);         // The predicted biomass of East Stock in West Area (1st row), West Stock in East area (2nd row)
		
	init_number FCV;                       // F prior 
	init_number movCV;                     // Movement prior 
	init_number selCV;                     // Selectivity prior 
	init_number R0diffCV;                  // Prior on R0 difference between early and late R0s (where applicable)
	init_number BSfracCV; 		       // BSfrac ratio 
	init_int nSpatPrq;                     // Number of spatial indices
	init_int nSpatPr;                      // Number of Spatial Prior observations by year, subyear, area, Ino, Index,  CV, wt, lb age, ub age 
	init_matrix SpatPr(1,nSpatPr,1,9);     // Spatial Prior
	
	
	// -- Likelihood weights --
	init_int nLHw;                         // Number of likelihood weights
	init_vector LHw(1,nLHw);               // Likelihood weights (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel)
		
	// -- Initial values --
	init_vector muR_ini(1,np);                        // Mean historical recruitment
	init_matrix sel_ini(1,nf,1,nl);                   // Selectivity 
	init_matrix selpars_ini(1,nf,1,3);                // Selectivity parameters
	init_vector lnF_ini(1,nCobs);                     // Effort (complexF=0) or log fishing mortality rate (complexF=1) 
	init_matrix lnRD_ini(1,np,1,ny);                  // Recruitment deviations
	init_5darray mov_ini(1,np,1,ns,1,na,1,nr,1,nr);   // Movement parameters
	init_vector lnqCPUE_ini(1,nCPUEq);                // q estimates for CPUE fleets
	init_vector lnqI_ini(1,nI);                       // q estimates for Fish. Ind. Indices
	init_vector D_ini(1,np);                          // Simulated depletion
        
	// -- Misc --
	init_int complexRD;                    // 0: estimate five year blocks of recruitment 1: estimate all annual recruitment deviations
	init_int complexF;                     // 0: Estimate a q by fleet using C/index as a covariate of effort (nf params e.g. 5) 1: Estimate an F for every Cobs (nCobs params e.g. 1200)
	init_int nF;                           // nCobs if complexF = 1, or 1 if complexF=0
	init_number MICV;                      // CV - extra Fmod variability in F in annual deviations from Master Index
	init_int Phase1;                       // Phases for estimation
	init_int Phase2;                       // Phases for estimation
	init_int Phase3;                       // Phases for estimation
	init_int Phase4;                       // Phases for estimation
	init_int debug;                        // 1 = run with initial values
	init_int verbose;                      // 1 = print handy text
	init_number datacheck;                 // Validates length of dat file read, 991199
	
	// -- Integer definitions --
	int mi; 			       // Movement index used only in the gravity model to keep track of where the viscosity parameters are
	int lc;                                // length class for SOO calculations
	int nodemax;                           // Temporary integer for use in defining nodes (MCMC)
	int j;                                 // Index location for allocating variables to node (MCMC)
	int nRD;                               // total recruitment deviation parameters estimated
	
	
PARAMETER_SECTION
		
	//LOCAL_CALCS
	//	cout<<SpatPr<<endl;
	//END_CALCS
	
	// -- Estimated parameters --
	init_bounded_vector lnR0(1,nSR,12,14.5,Phase1);                     // Unfished recruitment (period 1, period 2)
	init_bounded_matrix selpar(1,nsel,1,seltype,-5.,5.,Phase2);         // Selectivity parameters
	init_bounded_matrix movest_p1_a1(1,ns,1,nr-2,-8.,8.,Phase3);        // Movement deviations age class 1
	init_bounded_matrix movest_p2_a1(1,ns,1,nr-2,-8.,8.,Phase3);
	init_bounded_matrix movest_p1_a2(1,ns,1,nr-2,-8.,8.,Phase1);        // Movement age class 2
	init_bounded_matrix movest_p2_a2(1,ns,1,nr-2,-8.,8.,Phase1);
	init_bounded_matrix movest_p1_a3(1,ns,1,nr-2,-8.,8.,Phase3);        // Movement deviations age class 3
	init_bounded_matrix movest_p2_a3(1,ns,1,nr-2,-8.,8.,Phase3);
	init_bounded_vector lnqE(1,nE,-10.,1.,Phase1);                      // q estimates for E fleets
       	init_bounded_dev_vector Fmod(1,ns*nr,-10.,10.,Phase2);              // F modifier by season and area
	init_bounded_matrix visc(1,np,1,ns,-3.,3,Phase2);                   // max viscosity of 2
	
	LOCAL_CALCS
	  //cout<<"here 0"<<endl;
	  int nRD1=nRDs(1);      // Note here that there are up to four recruitment deviation vectors
	  int nRD2=nRDs(2);
	  int nRD3=nRDs(3);
	  int nRD4=nRDs(4);
	  nRD=sum(nRDs);
	  	   
	END_CALCS
	
	init_bounded_dev_vector lnRD1(1,nRD1,-1.5,1.5,Phase1); // Up to four time series of recruitment deviations
	init_bounded_dev_vector lnRD2(1,nRD2,-1.5,1.5,Phase1); // Up to four time series of recruitment deviations
	init_bounded_dev_vector lnRD3(1,nRD3,-1.5,1.5,Phase1); // Up to four time series of recruitment deviations
	init_bounded_dev_vector lnRD4(1,nRD4,-1.5,1.5,Phase1); // Up to four time series of recruitment deviations
	
	
	// A long list of dev vectors to address seasonal spatial stock trends
	init_bounded_dev_vector FDY_1_1(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_1_2(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_1_3(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_1_4(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_2_1(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_2_2(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_2_3(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_2_4(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_3_1(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_3_2(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_3_3(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_3_4(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_4_1(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_4_2(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_4_3(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_4_4(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_5_1(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_5_2(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_5_3(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_5_4(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_6_1(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_6_2(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_6_3(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_6_4(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_7_1(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_7_2(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_7_3(1,ny,-5.,5.,Phase3);
	init_bounded_dev_vector FDY_7_4(1,ny,-5.,5.,Phase3);
		
	
	LOCAL_CALCS
	  nodemax = nSR + sum(seltype) + (np*ns*nma*(nr-2)) + nE + nI + nCPUEq + ns*nr + nRD + ns*2 + nr*ns*ny;
	END_CALCS
	
	matrix lnRDmat(1,4,1,nRDs);
	matrix RDmat(1,4,1,nRDs);
	
	vector nodes(1,nodemax);                    // Parameter values stored in nodes vector for mcmc output
	
	// -- Objective function values --
	objective_function_value objG;              // Global objective function value
        number objC;                                // Catch observations
        number objCPUE;                             // Standardized cpue indices 
        number objI;                                // Fishery independent indices
        number objCL;                               // Length composition
        number objSOOm;                             // Stock of origin microchemistry
        number objSOOg;                             // Stock of origin genetics
        number objSOO;                              // All SOO data combined
        number objRD;                               // Recruitment deviations
        number objmov;                              // Priors on movement
        number objsel;                              // Priors on selectivity
        number objPSAT;                             // PSAT tags with certain stock of origin
        number objPSAT2;                            // PSAT tags with uncertain stock of origin
        number objSRA;                              // The penalty for F exceeding Fmax 
        number objSSB;                              // The prior on SSB
        number objDep;                              // The prior on depletion
        number objFmod;                             // The Fmod prior
        number objR0;                               // R0 ratio prior for two-step recruitment estimation
        number objBSfrac;                           // Prior on mixing 
        number objFDY;
        number objSP;                               // Spatial prior
          
        // -- Derived estimates (qs) --
        vector qI(1,nI);                            // q estimates for Fish. Ind. indices
        vector qCPUE(1,nCPUEq);                     // Catchability of fishery CPUE indices CPUE=qVB
        vector qSP(1,nSpatPrq);                     // Spatial prior q's
        
        // -- Transitions --
        5darray N(1,np,1,ny,1,ns,1,na,1,nr);        // Stock numbers
        matrix Rec_wd(1,np,1,ny);                   // Recruitment with deviation
        matrix Rec_mu(1,np,1,ny);                   // Recruitment mean (without deviation)
        matrix SSB_mu(1,np,1,ny);                   // SSB for calculating recruitment
        matrix NLA(1,na,1,nl);                      // Stock numbers by age and length
        matrix surv(1,np,1,na);                     // Survival
        3darray SSB(1,np,1,ny,1,ns);                // Spawning stock biomass
        3darray hSSB(1,np,1,nHy,1,ns);              // Spawning stock biomass
        3darray SSBi(1,np,1,ny,1,ns);               // Spawning stock biomass index
        matrix SSBdist(1,np,1,nr);                  // A temporary vector storing the spatial distribution of spawning biomass
        matrix spawnr(1,np,1,nr);                   // The fraction of spawning in each area
        4darray VB(1,ny,1,ns,1,nr,1,nf);            // Vulnerable biomass
        4darray VBi(1,ny,1,ns,1,nr,1,nf);           // Vulnerable biomass index
        4darray VBind(1,ny,1,ns,1,nr,1,nf);         // 5 categories of vulnerability (1) 66-115 (2) 115-144 (3) less than 145 (4) greater than 177 (5) greater than 195
        5darray VL(1,ny,1,ns,1,nr,1,nf,1,nl);       // Vulnerable at length recorded
        5darray VBL(1,ny,1,ns,1,nr,1,nf,1,nl);      // Vulnerable at length recorded
        3darray B(1,ny,1,ns,1,nr);                  // Biomass
        4darray BL(1,ny,1,ns,1,nr,1,nl);            // Biomass at length     
        vector SSB0(1,np);                          // Unfished spawning stock biomass
        vector D(1,np);                             // Stock depletion SSB now / SSB0
        vector Dt(1,np);                            // Stock depletion SSB now / first assessment year
        vector SSBnow(1,np);                        // SSB now
        matrix SSB_EW(1,np,1,ny);		    // Temporary East- West division 5-10, 1-4 areas
        vector movest(1,np*ns*nma*nr);              // Transcoding of estimated movement
            
        LOCAL_CALCS
      	  if(debug)cout<<"here2"<<endl;
      	  // exit(1);
	END_CALCS
      
        matrix Rec(1,np,1,ny);                      // Recruitment deviations (fractions)
	matrix R0(1,np,1,ny);                        // Unfished recruitment by stock and time period
	matrix steep(1,np,1,ny);
	matrix lnR02(1,np,1,2);                        // Unfished recruitment by stock and time period
	5darray CTL(1,np,1,ny,1,ns,1,nr,1,nl);      // Catch at length
	5darray CTA(1,np,1,ny,1,ns,1,nr,1,na);      // Catch at age
        matrix Btrend(1,np,1,ny);                   // Total biomass trend
        matrix meanF(1,np,1,ny);                    // Average F-at-age 
        
        LOCAL_CALCS
	   if(debug)cout<<"here3"<<endl;
	END_CALCS
        
        
        // -- Exploitation rates --
        vector F(1,nF);                                // Estimated fishing mortality rate
        5darray FAT(1,np,1,ny,1,ns,1,nr,1,na);         // Population level F at Age Total
        4darray hFAT(1,nHy,1,ns,1,na,1,nr);            // Historical F at Age (don't need disagg by pop as SRA using catch at age obs)
        5darray hZ(1,np,1,nHy,1,ns,1,na,1,nr);         // Historical Z at Age (don't need disagg by pop as SRA using catch at age obs)
       
        5darray FL(1,ny,1,ns,1,nr,1,nf,1,nl);          // Fishing mortality rate at length
        4darray FT(1,ny,1,ns,1,nr,1,nl);               // Total fishing mortality rate at length all fleets
        5darray Z(1,np,1,ny,1,ns,1,na,1,nr);           // Total mortality rate at age
        vector qE(1,nE);                               // Catchability F=qE
        vector Ipred(1,nIobs);                         // Predicted fishery independent index
        vector Ipred_vec(1,nIobs);                     // Paired vector of CPUE index predictions
        matrix Ipredtot(1,2,1,nI);                     // Record of observed and predicted indices for normalization (drop leading q) 
        vector CPUEpred(1,nCPUEobs);
        vector CPUEpred_vec(1,nCPUEobs);               // Paired vector of fishery independent index predictions        
        matrix CPUEpredtot(1,2,1,nCPUEq);              // Record of observed and predicted indices for normalization (drop leading q) 
        vector SPpred(1,nSpatPr);
	vector SPpred_vec(1,nSpatPr);                  // Paired vector of fishery independent index predictions        
	matrix SPpredtot(1,2,1,nSpatPrq);              // Record of observed and predicted indices for normalization (drop leading q) 
               
        
        LOCAL_CALCS
	  if(debug)cout<<"here4"<<endl;
	END_CALCS
                
        // -- Selectivity --
        matrix msel(1,nsel,1,nl);                      // Master (estimated) length selectivities
        matrix sel(1,nf,1,nl);                         // Fleet selectivities assigned from master
        vector spar(1,3);                              // Vector of transformed selectivity parameters
        
        // -- Growth -- 
        matrix wl(1,np,1,nl);                          // Weight at length group
                
        // -- Movement --
        5darray movcalc(1,np,1,ns,1,nma,1,nr,1,nr);    // Movement calculation matrix
        5darray movm(1,np,1,ns,1,nma,1,nr,1,nr);       // Master movement matrix by movement age class
        5darray mov(1,np,1,ns,1,na,1,nr,1,nr);         // Movement matrix by age
        
        // -- Composition likelihood function
        vector CLprop(1,nCLobs);                       // Calculated proportions
        vector CLsd(1,nf);                             // Conditional MLE std dev 
        vector CLn(1,nf); 
        vector CLvar(1,nf); 
        
        // -- Tagging --
        6darray RecapP(1,np,1,ns,1,nma,1,nRPT,1,nr,1,nr);    // Recapture probabilities
        
        LOCAL_CALCS
	   if(debug)cout<<"here5"<<endl;
	END_CALCS
        
        
        // -- Master index invariant arrays
        3darray FDYt(1,nr,1,ns,1,ny);                   // Estimated trend deviations from the Master index
               
        // -- Temporary arrays --
        3darray stemp(1,np,1,ns,1,nr);                 // Temporary season index
        matrix BSfrac_pred(1,np,1,ns);                 // Predicted fraction of E/W stock in W/E area
        vector sind(1,nRPT);                           // Temporary season index for calculation of recapture probabilities
        vector NLtemp(1,nl);                           // Stores a temporary vector of numbers at length (efficiency)
        number Ntemp;                                  // A temporarly record of stock numbers (SRA)
        vector tempR(1,nr);
        
        LOCAL_CALCS
	  if(debug)cout<<"here6"<<endl;
	END_CALCS
        
        // -- Observations --
        5darray CWpred(1,np,1,ny,1,ns,1,nr,1,nf);      // Catches by fleet (weight) and stock
        4darray CWtotpred(1,ny,1,ns,1,nr,1,nf);        // Total catches by weight
        6darray CLpred(1,np,1,ny,1,ns,1,nr,1,nf,1,nl); // Catches by fleet (weight) and stock
        5darray CLtotpred(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet
        5darray CLtotfrac(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet fraction
       	vector SOOpred(1,nSOOobs);   // Calculated predicted fraction of each stock of origin (there aren't that many stock of origin observations so it involves less computation to do this as needed)
	vector PSATpred(1,nPSAT);
	vector PSAT2pred(1,nPSAT2);
	
       	LOCAL_CALCS
	  if(debug)cout<<"here7"<<endl;
	END_CALCS
       	
       	
       	sdreport_number temp;                          // Requirement of mceval for producing posterior estimates of model parameters/variables                   


PROCEDURE_SECTION
	
	if(debug)cout<<"datacheck: "<<datacheck<<endl; // Were the data the correct length?
	
	calcSurvival();                  // Calculate survival
				
	if(debug)cout<<"Survival calculated"<<endl; 
	
	calcMovement();                  // Calculate movement
               
        if(debug)cout<<"Movement calculated"<<endl;  
         
        calcSelectivities();             // Calculate selectivities
	
	if(debug)cout<<"Selectivities calculated"<<endl; 
	
	assignPars();                    // Assigns estimates of R0, F, iRD, RD, qCPUE, qI
        
        if(debug)cout<<"Pars assigned"<<endl; 
        
        calcFDY();                       // Assign dev vecs to FDYt array
        
        if(debug)cout<<"Master index invariant calcs completed"<<endl; 
	
        if(debug==1)	assignInits();   // Overwrite R0, sel, F, iRD, RD, mov, qCPUE, qI to simulated values
	
	calcF();                         // Calculate fishing mortality rate at age / length
		
	if(debug)cout<<"Fs calculated"<<endl; 
	
	initModel();                     // Initialize the model (numbers / catches in first year)
	
	if(debug)cout<<"Model initialized"<<endl; 
	 
	//calcDynALK();                  // Dynamically calculate inverse age-length key based on predicted fishing mortality rate
		
	calcTransitions();               // Move/kill/reproduce fish over model years
	
	if(debug)cout<<"Transitions calculated"<<endl; 
	
	calcRecaptureProb();             // Calcualte recapture probabilities
	
	if(debug)cout<<"Recapture probabilities calculated"<<endl; 
		
	calcObjective();	         // Calculate objective function
	
	if(debug)cout<<"Objective function calculated"<<endl; 
		
	if(verbose)simsam();             // Print out simulated values versus estimated values for each function evaluation
		
	if(debug==1) exit(1);            // Exit prior to first function evaluation if in debugging mode
	
        popnodes();                      // populate a vector of parameters for mcmc output
        
        if(mceval_phase()){
          ofstream nodesout("nodes.cha", ios::app);
          nodesout<<nodes<<"\n";
        }
        

FUNCTION assignPars
  {
	// -- Assign estimated parameters --
	// Time period 1
	for(int sr=1; sr<=nSR; sr++){
	  
	  int pp=SRp(sr);
	  int styr=SRminyr(sr);
	  int edyr=SRmaxyr(sr);
	  if(pp==1)R0(pp)(styr,edyr)=mfexp(lnR0(sr)+1.); // 
	  if(pp==2)R0(pp)(styr,edyr)=mfexp(lnR0(sr));
	  steep(pp)(styr,edyr)=SRpar(sr);
		  
	}
	
	qE=mfexp(lnqE);                     // Assign catchability of partial F series' 
		
	lnRDmat(1)=lnRD1;
	lnRDmat(2)=lnRD2;
	lnRDmat(3)=lnRD3;
	lnRDmat(4)=lnRD4;
	
	for(int sr=1; sr<=nSR; sr++){
	
	  Ntemp=sum(pow(lnRDmat(sr),2))/(nRDs(sr)-1);	// Variance of rec devs
	  RDmat(sr)=mfexp(lnRDmat(sr)-Ntemp/2);    // log normal bias correction
	
	}
			
	for(int pp=1;pp<=np;pp++){        // Loop over stocks
	   
	  for(int yy=1;yy<=ny;yy++){      // Loop over years
	    
	    int rts = RDts(pp,yy);         // Find the correct reference block for this year
	    int rno = RDno(pp,yy);
	    Rec(pp,yy)=RDmat(rts,rno);
	    	      
	  }
	  	   
	}                                 // End of stocks
		
	for(int pp=1;pp<=np;pp++){          // Loop over stocks
	
	  wl(pp)=lwa(pp)*pow(ml,lwb(pp));   // Calcualte weight at length
	
	}                                   // End of stocks
	
	if(debug)cout<<"--- Finished assignPars ---"<<endl;
  }



FUNCTION assignInits
  {
	// -- Assign initial (starting) values --
	//R0=R0_ini;                        // Assign unfished recruitment
	//iRD=mfexp(ilnRD_ini);             // Assign initial recruitment deviations  
	//muR=muR_ini;                        // Assing mean historical recruitment
	//RD=mfexp(lnRD_ini);                 // Assign recruitment deviations
	//if(complexF)F=mfexp(lnF_ini);       // Assign fishing mortality rates
	//sel=sel_ini;                        // Assign selectivitiese
	//mov=mov_ini;                        // Assign movement
	//qCPUE=mfexp(lnqCPUE_ini);           // Assign CPUE index catchability
	//qI=mfexp(lnqI_ini);                 // Assign fishery independent index catchability
	
	if(debug)cout<<"--- Finished assignInits ---"<<endl;
  }


FUNCTION calcFDY
  {
     FDYt.initialize();      // Master index deviations
     
     FDYt(1)(1)=mfexp(FDY_1_1);
     FDYt(1)(2)=mfexp(FDY_1_2);
     FDYt(1)(3)=mfexp(FDY_1_3);
     FDYt(1)(4)=mfexp(FDY_1_4);
     
     FDYt(2)(1)=mfexp(FDY_2_1);
     FDYt(2)(2)=mfexp(FDY_2_2);
     FDYt(2)(3)=mfexp(FDY_2_3);
     FDYt(2)(4)=mfexp(FDY_2_4);
     
     FDYt(3)(1)=mfexp(FDY_3_1);
     FDYt(3)(2)=mfexp(FDY_3_2);
     FDYt(3)(3)=mfexp(FDY_3_3);
     FDYt(3)(4)=mfexp(FDY_3_4);
     
     FDYt(4)(1)=mfexp(FDY_4_1);
     FDYt(4)(2)=mfexp(FDY_4_2);
     FDYt(4)(3)=mfexp(FDY_4_3);
     FDYt(4)(4)=mfexp(FDY_4_4);
     
     FDYt(5)(1)=mfexp(FDY_5_1);
     FDYt(5)(2)=mfexp(FDY_5_2);
     FDYt(5)(3)=mfexp(FDY_5_3);
     FDYt(5)(4)=mfexp(FDY_5_4);
     
     FDYt(6)(1)=mfexp(FDY_6_1);
     FDYt(6)(2)=mfexp(FDY_6_2);
     FDYt(6)(3)=mfexp(FDY_6_3);
     FDYt(6)(4)=mfexp(FDY_6_4);
     
     FDYt(7)(1)=mfexp(FDY_7_1);
     FDYt(7)(2)=mfexp(FDY_7_2);
     FDYt(7)(3)=mfexp(FDY_7_3);
     FDYt(7)(4)=mfexp(FDY_7_4);     

  }
  

FUNCTION calcSurvival
  {
	// -- Calculate survival --
	for(int pp=1;pp<=np;pp++){                   // Loop over stocks      
	 
	  surv(pp,1)=1.;                             // Survival to age 1 is 100%
	  
	  for(int aa=1;aa<=na;aa++){             // Loop over age classes
	    
	    surv(pp,aa)=exp(-sum(Ma(pp)(1,aa-1)));   // Calculate survivial
	  
	  }                                          // End of age classes
		  
	}                                            // End of stocks
	
	if(debug)cout<<"--- Finished calcSurvival ---"<<endl;
  }

 

FUNCTION calcMovement
  {
  		
	//  -- Constrained, stock specific, fractional movement modelling -----------
	
	for(int ss=1;ss<=ns;ss++){               // Loop over subyears
	
	  for(int fr =1;fr<=nr;fr++){  
	    
	    for(int ac=1;ac<=nma;ac++){
	      movcalc(1,ss,ac,fr,nr)=0.;   // Med mov is determined
	      movcalc(2,ss,ac,fr,1)=0.;    // GOM mov is determined
            }
            
	    for(int tr =1;tr<=(nr-2);tr++){ 
	      // Custom age-class specific movement, estimated as deviations from age class 2
	      movcalc(1,ss,2,fr,tr+1)=movest_p1_a2(ss,tr);
	      movcalc(2,ss,2,fr,tr+1)=movest_p2_a2(ss,tr);
	   
	      movcalc(1,ss,1,fr,tr+1)=movcalc(1,ss,2,fr,tr+1)+movest_p1_a1(ss,tr); // deviation from age class 2
	      movcalc(2,ss,1,fr,tr+1)=movcalc(2,ss,2,fr,tr+1)+movest_p2_a1(ss,tr); // deviation from age class 2
	      
	      movcalc(1,ss,3,fr,tr+1)=movcalc(1,ss,2,fr,tr+1)+movest_p1_a3(ss,tr); // deviation from age class 2
	      movcalc(2,ss,3,fr,tr+1)=movcalc(2,ss,2,fr,tr+1)+movest_p2_a3(ss,tr); // deviation from age class 2
		   
	   } // to area
	   
	   movcalc(1,ss,2,fr,fr)+=mfexp(visc(1,ss));
	   movcalc(2,ss,2,fr,fr)+=mfexp(visc(2,ss));
	   movcalc(1,ss,1,fr,fr)+=mfexp(visc(1,ss));
	   movcalc(2,ss,1,fr,fr)+=mfexp(visc(2,ss));
	   movcalc(1,ss,3,fr,fr)+=mfexp(visc(1,ss));
	   movcalc(2,ss,3,fr,fr)+=mfexp(visc(2,ss));
		   
	 }  // from area
       } // subyear
	
	for(int i=1;i<=nMovExc;i++){
		       
	 int pp=MovExc(i,1);   // Stock
	 int ac=MovExc(i,2);   // Age Class
	 int ss=MovExc(i,3);   // Season
	 int fr=MovExc(i,4);   // From Area
	 int tr=MovExc(i,5);   // To Area
	 //Ntemp=(1-MovExc(i,6))/(1-movm(pp,ss,ac,fr,tr)); //how much to inflate the remaining movements 1-wt
	 //movm(pp)(ss)(ac)(fr)*=Ntemp; // uprate all 
	 movcalc(pp,ss,ac,fr,tr)= MovExc(i,6);   // now downweight tr to wt 

       }
 	
 	
       // -- Logit transformation ----------------------------------------------
	
       movm=exp(movcalc);                         // Make positive
		
       j=1; 
       for(int pp=1;pp<=np;pp++){                 // Stock       

	  for(int ss=1;ss<=ns;ss++){               // Subyear

	    for(int aa=1;aa<=nma;aa++){            // Movement age class

	      for(int fr = 1; fr<=nr;fr++){        // From area
                Ntemp=sum(movm(pp)(ss)(aa)(fr));
		movm(pp)(ss)(aa)(fr)/=Ntemp;  // Normalize to sum to 1 (inv logit)
	      }                                    // End of area

	      for(int tr = 1; tr<=nr;tr++){ 

		movest(j)=movm(pp,ss,aa,1,tr);
		j+=1;

	      }
	    }					   // End of movement age class   	

	  }                                        // End of subyear
	        
        }           
  	  	
	// -- Transcode this to an age structured movement matrix -----------
	
	for(int pp=1;pp<=np;pp++){                 // Stock       
		  
          for(int ss=1;ss<=ns;ss++){               // Subyear
		    
	    for(int aa=1;aa<=na;aa++){             // Age 
		    
	      int aam=ma(pp,aa);                   // Retrieve relevant movement age class
	      mov(pp)(ss)(aa)=movm(pp)(ss)(aam);   // Assign movement age class to age
	            
	    }                                      // End of age
	             
	  }					   // End of subyear	
	          
	}                                          // End of stock

	if(debug)cout<<"--- Finished calcMovement ---"<<endl;
  	
  }
	


FUNCTION calcSelectivities
  {
	
	// Selectivity calculations =======================================================
	
	// -- Master selectivities (can be mirrored across fleets) --
	
	for(int ss=1;ss<=nsel;ss++){ // Loop over estimated selectivities
		  
	  switch(seltype(ss)){       // Cases match number of estimated parameters for simplicity
		    
	    case 2: // Logistic selectivity
	     
	      spar(2)=LB(ss)+(UB(ss)-LB(ss))*(0.05+0.85*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));        // Inflection point (2) as a fraction of largest length I(0.1|0.8)
	      spar(1)=(UB(ss)-LB(ss))*(0.005+0.11*(mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)))));    // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
	      
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	      
	        msel(ss,ll)=1/(1+mfexp((spar(2)-ml(ll))/spar(1)));                         // Logistic selectivity function
	        if(ml(ll)<LB(ss))msel(ss,ll)=0.;
	        if(ml(ll)>UB(ss))msel(ss,ll)=0.;
	        
	      } 
	      	      
	      msel(ss)/=max(msel(ss)); // Need to normalize at least one index to max 1 or face counfounding with q
	      // End of length classes
	    
	    break;                                                                         // End of logistic selectivity
		   
            case 3: // Double normal selectivity
	      
	      spar(1)=LB(ss)+(UB(ss)-LB(ss))*(0.05+0.95*mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)))); // Length at max selectivity bounded between 5 and 95 percent of length range
	      spar(2)=2.*spar(1)*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2)));       // Lower sd (divided by 4 just to start at a reasonable guess)
	      spar(3)=(UB(ss)-LB(ss))*mfexp(selpar(ss,3));         // Upper sd
	     
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	        
	        if(ml(ll)<spar(1)){
	            
	          msel(ss,ll)=pow(2.0,-(ml(ll)-spar(1))/spar(2)*(ml(ll)-spar(1))/spar(2)); 
	        
	        }else{
	        
	          msel(ss,ll)=pow(2.0,-(ml(ll)-spar(1))/spar(3)*(ml(ll)-spar(1))/spar(3));
	        
	        }
	        
	        if(ml(ll)<LB(ss))msel(ss,ll)=0.;
		if(ml(ll)>UB(ss))msel(ss,ll)=0.;
	    	      
	      }									           // End of length classes
	      	      
	      msel(ss)/=max(msel(ss));                                                     // Need to normalize to max 1 or face counfounding with q
	          
	    break;
	   		      
	  }
	          
	}
		
	// -- Fleet specific selectivities --
	  
	for(int ff=1;ff<=nf;ff++){   
	  
	  int si=selind(ff);                    // Find correct master index   
	  sel(ff)=msel(si);                     // Map master selectivities onto fleet-specific selectivities
	
	}
		
	if(debug)cout<<"--- Finished calcSelectivities ---"<<endl;
	
  }	



FUNCTION calcF
  {
  	double tiny=1E-10;	
  	
  	FL.initialize();      // Fishing mortality rate at length = 0
  	FT.initialize();      // Total fishing mortality rate = 0
  	FAT.initialize();     // Total fishing mortality rate at age = 0
  	Z.initialize();       // Total mortality = 0
  	meanF.initialize();   // Track average F = 0
  	  	
  	for(int i=1;i<=nEobs;i++){
			
	  int yy=Eobs(i,1);    // Year
          int ss=Eobs(i,2);    // Subyear
          int rr=Eobs(i,3);    // Region
	  int ff=Eobs(i,4);    // Fleet
	  int ll=(ss-1)*nr+rr; // position in Fmod
	     
	  FL(yy)(ss)(rr)(ff)= sel(ff)*Eobs(i,6)*qE(ff)*mfexp(Fmod(ll))*FDYt(rr,ss,yy);  // Calculate fishing mortality rate at length 
	    
	}
		
        for(int yy=1; yy<=ny;yy++){                    // Loop over years
	  
	  for(int ss=1;ss<=ns;ss++){                   // Loop over seasons
	    
	    for(int rr =1;rr<=nr;rr++){                // Loop over areas
	      
	      for(int ll=1;ll<=nl;ll++){               // Loop over length bins
	        
	        FT(yy,ss,rr,ll)=tiny;                  // Avoids division by zero problems
	        
	        for(int ff=1;ff<=nf;ff++){             // Loop over fleets
	          
	          FT(yy,ss,rr,ll)+=FL(yy,ss,rr,ff,ll); // Summation of F-at-length
	        
	        }                                      // End of fleets
	      
	      }                                        // End of length classes
	    
	    }                                          // End of areas
	  
	  }                                            // End of subyears
	
	}                                              // End of years
         
        // -- Calculate F at age --
	
	for(int pp=1;pp<=np;pp++){                     // Loop over stocks
		
	  for(int yy=1;yy<=ny;yy++){                   // Loop over years
	          
	    for(int ss=1;ss<=ns;ss++){                 // Loop over subyears
	      
	      for(int rr=1;rr<=nr;rr++){               // Loop over areas
	      
	        FAT(pp)(yy)(ss)(rr)=FT(yy)(ss)(rr)*trans(ALK(pp)(yy))+tiny; // Calculate fishing mortality rate at age, F(a) = sigma(l) P(l|a)*F(l)
	        meanF(pp,yy)+=sum(FAT(pp)(yy)(ss)(rr))/(na*nr); // Calculate mean fishing mortality rate at age (debugging)
	        
	        for(int aa=1;aa<=na;aa++){             // Loop over ages
	        
	          Z(pp)(yy)(ss)(aa)(rr)=FAT(pp)(yy)(ss)(rr)(aa)+(Ma(pp,aa)*sdur(ss)); // Calculate total mortality rate
		        
	        }                                      // End of ages
	      
	      }                                        // End of areas
	      
	    }                                          // End of subyears
		    
          }                                            // End of years
		  
	}                                              // End of stocks
             
        if(debug)cout<<"--- Finished calcF ---"<<endl;
  
  }



FUNCTION initModel
  {
	double tiny=1E-10;             // Define a small number for ensuring that log(0) doesn't happen

	N.initialize();                // Stock numbers = 0
	B.initialize();                // Stock biomass = 0
	BL.initialize();               // Biomass at length = 0
	hZ.initialize();               // Historical Z = 0
	SSB.initialize();              // Spawning stock biomass = 0
	hSSB.initialize();             // historical Spawning stock biomass = 0
	SSBdist.initialize();          // Spawning distribution = 0
	SSB0.initialize();             // Unfished spawning stock biomass = 0
	SSB_EW.initialize();           // SSB by area initialized at 0
	CWtotpred.initialize();        // Total catch (weight) = 0
	CWpred.initialize();           // Catch (weight) = 0
	//CNpred.initialize();         // Catch (numbers) = 0
	CLpred.initialize();           // Catch (length class) = 0
	CLtotpred.initialize();        // Total catch (length class) = 0
	CLtotfrac.initialize();        // Total catch fractions (length class) = 0
	CTA.initialize();              // Temporary catch at age = 0
	VB.initialize();               // Vulnerable biomass = 0
	VBi.initialize();              // Vulnerable biomass index = 0
	VL.initialize();               // Vulnerable length
	VBL.initialize();              // Vulnerable biomass at length
	D.initialize();                // Spawning Stock depletion = 0
	Btrend.initialize();           // Trend in biomass = 0
	objSRA.initialize();           // Penalty for historical F's over 0.9 = 0
	
	for(int pp=1;pp<=np;pp++){
	
	  SSB0(pp)=R0(pp,1)*SSBpR(pp);
	  
	}
	
	
	for(int pp=1;pp<=np;pp++){                              // Loop over stocks
	
	  // -- Initial guess at stock distribution -------------------------------------------
	  
	  stemp(pp)(ns)=1./nr;                                  // Distribute a fish evenly over areas                     
	  
	  for(int ii=1;ii<=20;ii++){                        // Loop over nydist initial spatial distribution (stabilizes within 3 usually)
	    
	    for(int ss=1;ss<=ns;ss++){                          // Loop over subyears
	        
	      if(ss==1){                                        // Take bits of fish from previous years final subyear
	      
	        stemp(pp)(1)=stemp(pp)(ns)*mov(pp)(ss)(na);     // Move them assuming mature movement
	      
	      }                                                 // End of 'if first subyear'	      
	      else{                                             // Take bits of fish from this years' previous subyear
	        
	        stemp(pp)(ss)=stemp(pp)(ss-1)*mov(pp)(ss)(na);  // Move them assuming mature movement
	      
	      }                                                 // End of 'if not first subyear'
	       //cout<<rowsum(stemp(pp))<<endl;
	    }                                                   // End of subyears
	  
	  }                                                     // End of years
	  
	  
 	  // -- Initial guess 
	  
	  for(int rr=1;rr<=nr;rr++){                            // Loop over areas
	    
	    for(int ss=1;ss<=ns;ss++){ 
	    
	      for(int aa=1;aa<=na;aa++){
	        
	        N(pp,1,ss,aa,rr)=R0(pp,1)*surv(pp,aa)*stemp(pp,ss,rr);    // Stock numbers are spatial distribution multiplied by Survival and R0
	        hSSB(pp,1,ss)+=N(pp,1,ss,aa,rr)*Fec(pp,aa);           // Historical spawning stock biomass
	        
	      }                                                       // end of ages
	      
	      N(pp,1,ss,na,rr)+=R0(pp,1)*surv(pp,na)*stemp(pp,ss,rr)*mfexp(-Ma(pp,na))/(1-mfexp(-Ma(pp,na))); // Indefinite intergral for plus group
	      hSSB(pp,1,ss)+=(N(pp,1,ss,na,rr)*Fec(pp,na))*mfexp(-Ma(pp,na))/(1-mfexp(-Ma(pp,na)));
	      
	    }                                                       // End of seasons
	  
	  }                                                         // End of areas 
	  
	}                                                           // End of stocks
	
	
	// -- Age and movement implications of spool-up
	
	for(int yy=2;yy<=nHy;yy++){         // Loop over historical years
	  
	  for(int ss=1;ss<=ns;ss++){         // Loop over seasons
	     
	    if(ss==1){                       // First subyear isn't a spawning season  ------------------------------------------------
		       
	      for(int aa=1;aa<=na;aa++){     // Loop over age classes
		        
		for(int rr=1;rr<=nr;rr++){// Loop over areas
		
		  Ntemp=tiny;                    // Keep count, tiny is to prevent zero divided by zero in hFAT calculation
		
		  for(int pp=1;pp<=np;pp++){
		
		    Ntemp+=N(pp,1,ns,aa,rr)*mfexp(-Ma(pp,aa)*sdur(ss)/2.);   // Sum over populations
		
		  }
			  
		  dvariable pen=0.;
		  
		  hFAT(yy-1,ns,aa,rr)=-log(posfun(1-(HCobs(yy-1,ns,aa,rr)/(Ntemp+tiny)),1-0.9,pen));            // F estimate based on half of M
		  
		  objSRA+=pen;
		  
		   
		}
		 for(int pp=1;pp<=np;pp++){   // Loop over stocks	  
		  
		  for(int rr=1;rr<=nr;rr++){
		  
		    hZ(pp,yy-1,ns,aa,rr)=hFAT(yy-1,ns,aa,rr)+Ma(pp,aa)*sdur(ns);
		    
		  }
		  		 
		  N(pp)(1)(1)(aa)=elem_prod(mfexp(-hZ(pp)(yy-1)(ns)(aa)),N(pp)(1)(ns)(aa))*mov(pp)(1)(aa); // Mortality then move
				
		  for(int rr=1;rr<=nr;rr++){ // Loop over areas
		          
		    hSSB(pp,yy,1)+=N(pp,1,1,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
		          
		  }                          // End of areas
			      
	         
	        }                            // End of stocks
		        
              }                              // End of ages
	   
	    }else{                           // Could be a spawning season ------------------------------
		        	      
	      if(ss==spawns(1)){             // !!! right now assumes same spawning season among stocks !!! If a spawning season...-------------------------------------------------------------------
		
		for(int pp=1;pp<=np;pp++){
		
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
			  	        
	            SSBdist(pp,rr)=0.;         // Reset SSB distribution counter
			  	        
		  }                            // End of areas
		          
		  for(int aa=1;aa<=na;aa++){   // Loop over age classes
		          
		    for(int rr=1;rr<=nr;rr++){ // Loop over areas
		    
		      SSBdist(pp,rr)+=N(pp,1,ss,aa,rr)*Fec(pp,aa);  // The distribution of SSB among areas (last year)
		
		    }                          // End of areas
		   	    
	          }                            // End of age classes
		         
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
		          
		    spawnr(pp,rr)=(SSBdist(pp,rr)*canspawn(pp,rr))/sum(elem_prod(canspawn(pp),SSBdist(pp)));  // Calculate spawning fraction of SSB by area
		         
		  }
		  
		}  
		
		
		for(int aa=1;aa<=na;aa++){     // Loop over age classes
				        
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
		  
		    Ntemp=tiny;                    // Keep count
				
		    for(int pp=1;pp<=np;pp++){
				
		      Ntemp+=N(pp,1,ss-1,aa,rr)*mfexp(-Ma(pp,aa)*sdur(ss)/2.);   // Sum over populations
				
		    }
		     
		    dvariable pen=0.;		
		    hFAT(yy,ss-1,aa,rr)=(-log(posfun(1-(HCobs(yy,ss-1,aa,rr)/(Ntemp+tiny)),1-0.99,pen)));            // F estimate based on half of M
		    objSRA+=pen;
			     
		  }
		  
		  for(int pp=1;pp<=np;pp++){   // Loop over stocks	  
			
		    for(int rr=1;rr<=nr;rr++){
					  
		      hZ(pp,yy,ss-1,aa,rr)=hFAT(yy,ss-1,aa,rr)+Ma(pp,aa)*sdur(ss);
					    
		    }
						
		    N(pp)(1)(ss)(aa)=elem_prod(mfexp(-hZ(pp)(yy)(ss-1)(aa)),N(pp)(1)(ss-1)(aa))*mov(pp)(ss)(aa); // Mortality then move
		  }
		
		}
		
		for(int pp=1;pp<=np;pp++){   // Loop over stocks	
		  
		  N(pp)(1)(ss)(na)+=N(pp)(1)(ss)(na-1);          // Plus group
		  
		  for(int aa=(na-1);aa>=2;aa-=1){                // Loop down age classes from plusgroup(ns)-1
			   	        
	            N(pp)(1)(ss)(aa)=N(pp)(1)(ss)(aa-1);         // Age fish
		  	        
		    for(int rr=1;rr<=nr;rr++){ // Loop over areas
		    			      
		      hSSB(pp,yy,ss)+=N(pp,1,ss,aa,rr)*Fec(pp,aa);//*canspawn(pp,rr));    // SSB is summed (+=) over age classes and areas (sum())
		      
		    }                         // End of areas
		    
		  }                           // End of ages
		  
		  SSB_mu(pp,1)=sum(SSBdist(pp));
		  Rec_mu(pp,1)=(0.8*R0(pp,1)*steep(pp,1)*sum(SSBdist(pp)))/(0.2*SSBpR(pp)*R0(pp,1)*(1-steep(pp,1))+(steep(pp,1)-0.2) * sum(SSBdist(pp)));
		  Rec_wd(pp,1)=Rec_mu(pp,1);//*Rec(pp,1);
		  N(pp)(1)(ss)(1)=spawnr(pp)*Rec_wd(pp,1); // Recruitment 
	       		 
		}                             // End of stocks  
		 	   
              }else{                          	// Not a spawning season ----------------------------------------------------------------
		  
		for(int aa=1;aa<=na;aa++){      // Loop over age classes
		  				        
		  for(int rr=1;rr<=nr;rr++){    // Loop over areas
		  
		    Ntemp=tiny;                   // Keep count
		  				
		    for(int pp=1;pp<=np;pp++){
		  				
		      Ntemp+=N(pp,1,ss-1,aa,rr)*mfexp(-Ma(pp,aa)*sdur(ss)/2.);   // Sum over populations
		  				
		    }
		    
		    dvariable pen=0.;		
		    hFAT(yy,ss-1,aa,rr)=(-log(posfun(1-(HCobs(yy,ss-1,aa,rr)/(Ntemp+tiny)),1-0.9,pen)))+tiny;            // F estimate based on half of M
		    objSRA+=pen;
		        
		  }
						
		  for(int pp=1;pp<=np;pp++){   // Loop over stocks	  
		  				
		    for(int rr=1;rr<=nr;rr++){
		    					  
		      hZ(pp,yy,ss-1,aa,rr)=hFAT(yy,ss-1,aa,rr)+Ma(pp,aa)*sdur(ss);
		    					    
		    }
		    		    
		    N(pp)(1)(ss)(aa)=elem_prod(mfexp(-hZ(pp)(yy)(ss-1)(aa)),N(pp)(1)(ss-1)(aa))*mov(pp)(ss)(aa); // Mortality then move
			  	    
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
			    
		      hSSB(pp,yy,ss)+=N(pp,1,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
			    
		    }	                      // End of areas        
		
		  }                           // End of populations
		
		}                             // End of ages
		          
              }                               // End of 'not a spawning season'
		     
            }                                 // End of 'could be a spawning season?'
		
	  }                                   // End of subyears
	  
        }                                     // End of historical year
         
               
	// -- Initial year catch calculations ----------------------------------------------------------
	
	for(int pp=1;pp<=np;pp++){						 // Loop over stocks
	
	  for(int rr=1;rr<=nr;rr++){                                             // Loop over areas
	    
	    for(int ss=1;ss<=ns;ss++){                                           // Loop over seasons
	      
              CTA(pp)(1)(ss)=trans(elem_prod(
	      	      	      elem_prod(N(pp)(1)(ss),1-mfexp(-Z(pp)(1)(ss))),
	      	      	      elem_div(trans(FAT(pp)(1)(ss)),Z(pp)(1)(ss))));    
	   
              CTL(pp)(1)(ss)(rr)=CTA(pp)(1)(ss)(rr)*ALK(pp)(1);                  // Catch at length is catch at age * inverse age length key
              	      
	      for(int aa=1;aa<=na;aa++){                      // Loop over age class
	      
	        NLA(aa)=N(pp)(1)(ss)(aa)(rr)*ALK(pp)(1)(aa);  // Temporarily store numbers at length
	      
	      }                                               // End of age class
	      
	      NLtemp=colsum(NLA);                             // Numbers at length (sum over age classes)
	      BL(1)(ss)(rr)+=elem_prod(NLtemp,wl(pp));
	      B(1,ss,rr)+=sum(elem_prod(NLtemp,wl(pp)));      // Biomass summed over stocks	
	      Btrend(pp)(1)+=B(1,ss,rr)/ns;                   // Record first year of biomass trend (debugging)
	      
	      for(int ff=1;ff<=nf;ff++){                      // Loop over fleet types
						  
		VB(1,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));   // Vulnerable biomass summed over stocks
		VBL(1)(ss)(rr)(ff)+=elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff));    // Vulnerable biomass at length
		VL(1)(ss)(rr)(ff)+=elem_prod(NLtemp,sel(ff));  
	      }                                               // End of fleets
	  	
	      for(int ff=1;ff<=nf;ff++){                      // Loop over fleets
	  	
	  	for(int ll=1;ll<=nl;ll++){                    // Loop over length classes
	          
	          CLpred(pp)(1)(ss)(rr)(ff)(ll)=CTL(pp)(1)(ss)(rr)(ll)*(FL(1)(ss)(rr)(ff)(ll)/FT(1)(ss)(rr)(ll));  // Catch at length by stock                                    // Length composition catches
	      	  CLtotpred(1)(ss)(rr)(ff)(ll)+=CLpred(pp)(1)(ss)(rr)(ff)(ll);                                     // Total predicted length composition of catches
	        
	        }                                             // End of length classes
	        
	        CWpred(pp,1,ss,rr,ff)=sum(elem_prod(CLpred(pp)(1)(ss)(rr)(ff),wl(pp)));                  // Total catch weight by fleet and stock
	        CWtotpred(1,ss,rr,ff)+=CWpred(pp,1,ss,rr,ff);                                            // Total predicted catch weight by fleet
	       	CLtotfrac(1)(ss)(rr)(ff)=CLtotpred(1)(ss)(rr)(ff)/(sum(CLtotpred(1)(ss)(rr)(ff))+tiny);  // Fraction of catch at length class
	      
	      }                                                                                          // End of fleets
	  
            }                                                 // End of subyears          
          
          }                                                   // End of areas
          
          
          for(int ss=1;ss<=ns;ss++){                          // Loop over subyears
            
            SSB(pp,1,ss)=hSSB(pp,nHy,ss);                     // Transcode historical SSB to current SSB
          
          }                                                   // End of subyears
             
        }                                                     // End of stocks
		
	if(debug)cout<<"--- Finished initModel ---"<<endl;
	
  }


	
FUNCTION calcTransitions
  {
        // Order of calculations Catch(y-1), M(y-1), move(y), Catch(y), M(y), mov(y+1)... 
        
        double tiny=1E-10;                    // Create a small constant to avoid the log(0) error.     
          
	for(int pp=1;pp<=np;pp++){            // Loop over stocks
	  
	  for(int yy=2;yy<=ny;yy++){          // Loop over years
	   
	    for(int ss=1;ss<=ns;ss++){        // Loop over seasons
	        
	      if(ss==1){                      // First subyear isn't a spawning season  ----------------------------------------------------------
	        
	        for(int aa=1;aa<=na;aa++){    // Loop over age classes
	        
	          N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy-1)(ns)(aa)),N(pp)(yy-1)(ns)(aa))*mov(pp)(ss)(aa); // Mortality then move
	          
	          for(int rr=1;rr<=nr;rr++){  // Loop over areas
	          
	            SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
	            
	            if(rr<4){
		      SSB_EW(2,yy)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*(1./ns);
		    }else{
		      SSB_EW(1,yy)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*(1./ns);
		    }
	            
	          }                           // End of areas
	        
	        }                             // End of ages
	          
	      }else{                          // Could be a spawning season ---------------------------------------------------------------------
	        	      
	        if(ss==spawns(pp)){           // If a spawning season...-------------------------------------------------------------------
	          
	          for(int rr=1;rr<=nr;rr++){  // Loop over areas
		  	        
		    SSBdist(pp,rr)=0.;        // Reset SSB distribution counter
		  	        
	          }                           // End of areas
	          
	          for(int aa=1;aa<=na;aa++){  // Loop over age classes
	          
	            N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss)(aa); // Mortality then move
	            
		    for(int rr=1;rr<=nr;rr++){ // Loop over areas
		      
		      SSBdist(pp,rr)+=N(pp,yy-1,ss,aa,rr)*Fec(pp,aa); // Calculate distribution of SSB over areas
		    	            
	            }                          // End of areas
		    
		  }                            // End of age classes
	         
	          for(int rr=1;rr<=nr;rr++){   // Loop over areas
	          
	            spawnr(pp,rr)=(SSBdist(pp,rr)*canspawn(pp,rr))/sum(elem_prod(SSBdist(pp),canspawn(pp))); // Calculate spawning fraction
	          
	          }                            // End of areas
	            
	          N(pp)(yy)(ss)(na)+=N(pp)(yy)(ss)(na-1);  // Plus group calculation
	          
	          for(int aa=(na-1);aa>=2;aa-=1){          // Loop down age classes
		   	        
		    N(pp)(yy)(ss)(aa)=N(pp)(yy)(ss)(aa-1); // Age fish
		  	        
	          }                                        // End of age classes
	          
	          SSB_mu(pp,yy)=sum(SSBdist(pp));
	          Rec_mu(pp,yy) = (0.8*R0(pp,yy)*steep(pp,yy)*sum(SSBdist(pp)))/(0.2*SSBpR(pp)*R0(pp,yy)*(1-steep(pp,yy))+(steep(pp,yy)-0.2) * sum(SSBdist(pp)));
                  Rec_wd(pp,yy)=Rec(pp,yy)*Rec_mu(pp,yy); 
                  N(pp)(yy)(ss)(1)=spawnr(pp)*Rec_wd(pp,yy); // Recruitment (SSB lag 1 year)
	       	 	   	 
	       	  for(int aa=1;aa<=na;aa++){  // Loop over age classes
		                
		     for(int rr=1;rr<=nr;rr++){ // Loop over areas
		  		      
		  	SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
		   	
			if(rr<4){
		          SSB_EW(2,yy)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*(1./ns);
		        }else{
		          SSB_EW(1,yy)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*(1./ns);
		        }
			
		  	
		     }                          // End of areas
		  		    
		  }                            // End of age classes
	                   
	        }else{                          // Not a spawning season ----------------------------------------------------------------
	        
	          for(int aa=1;aa<=na;aa++){    // Loop over age classes
		    
		    N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss)(aa); // M then move	        
		    
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
		    
		      SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
		      if(rr<4){
		        SSB_EW(2,yy)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*(1./ns);
		      }else{
		        SSB_EW(1,yy)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*(1./ns);
		      }
 
		    }	                        // End of areas 
		  
		  }                             // End of ages
	          
	        }                               // Not a spawning season
	     
	      }                                 // Could be a spawning season?
	     
	      CTA(pp)(yy)(ss)=trans(elem_prod(
	        elem_prod(N(pp)(yy)(ss),1-mfexp(-Z(pp)(yy)(ss))),
	      	elem_div(trans(FAT(pp)(yy)(ss)),Z(pp)(yy)(ss)))); // Calculate catch at age
	    	       	
	      for(int rr=1;rr<=nr;rr++){                              // Loop over areas
		
		CTL(pp)(yy)(ss)(rr)=CTA(pp)(yy)(ss)(rr)*ALK(pp)(yy);  // Calculate catch at length
		
		for(int aa=1;aa<=na;aa++){                            // Loop over age classes
			      
		  NLA(aa)=N(pp)(yy)(ss)(aa)(rr)*ALK(pp)(yy)(aa);      // Temporarily store numbers at length
		  	      
		}                                                     // End of age classes
			      
	        NLtemp=colsum(NLA);                                   // Calculate numbers by length class (sum over ages)
	        BL(yy)(ss)(rr)+=elem_prod(NLtemp,wl(pp));             // Biomass at length summed over stocks
		B(yy,ss,rr)+=sum(elem_prod(NLtemp,wl(pp)));           // Biomass summed over stocks				  
	        Btrend(pp)(yy)+=B(yy,ss,rr)/ns;                       // Record biomass trend (debugging)
	     
		for(int ff=1;ff<=nf;ff++){                            // Loop over fleets
		   
		  VB(yy,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));    // Vulnerable biomass summed over stocks
		  VL(yy)(ss)(rr)(ff)+=elem_prod(NLtemp,sel(ff)); 
		  VBL(yy)(ss)(rr)(ff)+=elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff));    // Vulnerable biomass at length 
		  
		}                                                     // End of fleets
				
		for(int ff=1;ff<=nf;ff++){                            // Loop over fleets
		  
		  for(int ll=1;ll<=nl;ll++){                          // Loop over length classes
		  	          
		    CLpred(pp)(yy)(ss)(rr)(ff)(ll)=CTL(pp)(yy)(ss)(rr)(ll)*(FL(yy)(ss)(rr)(ff)(ll)/FT(yy)(ss)(rr)(ll));  // Catch at length by stock                                    // Length composition catches
		    CLtotpred(yy)(ss)(rr)(ff)(ll)+=CLpred(pp)(yy)(ss)(rr)(ff)(ll);                                       // Total predicted length composition of catches
		  	        
		  }                                                                                                      // End of length classes
	         
	       	  CLtotfrac(yy)(ss)(rr)(ff)=CLtotpred(yy)(ss)(rr)(ff)/(sum(CLtotpred(yy)(ss)(rr)(ff))+tiny);             // Calculate catch fractions by length class
	      	  CWpred(pp,yy,ss,rr,ff)=sum(elem_prod(CLpred(pp)(yy)(ss)(rr)(ff),wl(pp)));                              // Total catch weight by fleet by stock
	      	  CWtotpred(yy,ss,rr,ff)+=CWpred(pp,yy,ss,rr,ff);                                                        // Total (over ages and stocks) predicted catches by fleet
	      	      	
		}                                                    // End of fleets
				    
	      }                                                      // End of areas
	     
	    }                                                        // End of subyear
	   
	  }                                                          // End of year
	
	  SSBi(pp)=SSB(pp)/(sum(SSB(pp))/(ny*ns));                   // Calculate SSB index normalized to 1
	  Btrend(pp)/=Btrend(pp,1);                                  // Normalize Btrend to depletion
	  
	  for(int pp=1;pp<=np;pp++){  // Loop over stocks
	  		  		      
	     Dt(pp)=SSB(pp,ny,ns)/SSB(pp,1,1); // SSB depletion over time series
	     D(pp)=SSB(pp,ny,ns)/SSB0(pp);     // SSB depletion from unfished	
	     SSBnow(pp)=SSB(pp,ny,ns);         // SSB now  
          }        
	  
	}                                                            // End of stock
		
	if(debug)cout<<"--- Finished calcTransitions ---"<<endl;
	
  }



FUNCTION calcRecaptureProb
  {
  	
  	/*for(int pp=1;pp<=np;pp++){                // Stocks
  	  
  	  for(int ss=1; ss<=ns;ss++){             // Subyears
  	     
  	    //int s2 = RPTind(ss,2);                // Retrieve the correct subyear for timestep tt and release subyear ss
  	    
  	    for(int aa=1; aa<=nma; aa++){         // loop over movement age classes
  	      
  	      for(int rr=1;rr<=nr;rr++){          // Regions
  	      
  	        RecapP(pp)(ss)(aa)(1)(rr)=0.;     // Set the area vector to all zeros
  	        RecapP(pp,ss,aa,1,rr,rr)=1.;      // Recapture probability is 100% for same area in the same timestep
  	      
  	        //for(int tt=2;tt<=nRPT;tt++){    // Timesteps (incremental subyears)
  	        RecapP(pp)(ss)(aa)(2)(rr)=RecapP(pp)(ss)(aa)(1)(rr)*mov(pp)(s2)(aa);   // Recalculate recapture probability in next timestep given movement
  	     
  	        
  	        //} // timestep (nRPt)
  	      }
  	      
  	    }                                    // End of areas
  	  
  	  }                                      // End of subyears
  	
  	}                                        // End of stocks
    
        if(debug)cout<<"--- Finished calcRecaptureProb ---"<<endl;
    */
  }

  

FUNCTION calcObjective
  {
	objG.initialize();                      // Global 
	objC.initialize();                      // Catch data
	objCPUE.initialize();                   // Standardized cpue indices 
	objI.initialize();                      // Fishery independent indices
	objCL.initialize();                     // Length composition
	objSOOm.initialize();                    // Stock of origin
	objSOOg.initialize();                    // Stock of origin
	objSOO.initialize();                    // Stock of origin
	objRD.initialize();                     // Recruitment deviations
	objmov.initialize();                    // Priors on movement
	objsel.initialize();			// Priors on selectivity
	objPSAT.initialize();                   // PSAT tags certain stock of origin
	objPSAT2.initialize();                  // PSAT tags w uncertain stock of origin
	objSSB.initialize();                    // SSB prior
	objDep.initialize();                    // Dep prior
	objFmod.initialize();                   // Fmod prior
	objR0.initialize();			// R0 ratio prior for cases where there is two-stage recruitment estimation
	objBSfrac.initialize();                 // Proportion of E/W stock biomass in W/E area
	objFDY.initialize();                    // Penalty on extra Fmod annual variability in F=qE (master index)
	objSP.initialize();                     // Prior on Spatial Priors
	
	CPUEpredtot.initialize();               // Record of fishery CPUE indices for normalization (dropping the leading qs)
	CPUEpred_vec.initialize();              // Paired CPUE index predictions 
	Ipredtot.initialize();                  // Record of fishery independent indices for normalization (dropping the leading qs)
	Ipred_vec.initialize();                 // Paired fishery independent index predictions
	SPpredtot.initialize();                 // Record of spatial seasonal prior totals for normalization
	SPpred_vec.initialize();                // Paired spatial seasonal prior predictions               
	
	Ipred.initialize();                     // Predicted fishery-independent index
	CLprop.initialize();                    // Catch at length composition predicted proportions
	CLsd.initialize();                      // Conditional MLE estimate of the sd 
	CLn.initialize();
	CLvar.initialize();
	
	dvariable LHtemp;                       // Temporary store of the calculated likelihood values
        double tiny=1E-10;                      // Create a small constant to avoid log(0) error
	
	// -- Catch observations --
	
	for(int i=1;i<=nCobs;i++){              // Loop over catch observations
	
	  int yy=Cobs(i,1); // Year
	  int ss=Cobs(i,2); // Subyear
	  int rr=Cobs(i,3); // Region
  	  int ff=Cobs(i,4); // Fleet
  		  
  	  if(last_phase()){
  	     LHtemp=dnorm(log(CWtotpred(yy,ss,rr,ff)+tiny),log(Cobs(i,5)+tiny),Cobs(i,6)); // Log-normal LHF
  	  }else{
  	     LHtemp=dnorm(log(CWtotpred(yy,ss,rr,ff)+tiny),log(Cobs(i,5)+tiny),(Cobs(i,6)*2.)); // Log-normal LHF - relaxed until last phase
  	  }
  	  objC+=LHtemp*Cobs(i,7)*LHw(1);                                                           // Weighted likelihood contribution
  	  objG+=LHtemp*Cobs(i,7)*LHw(1);  
  	  
  	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" Cobs="<<Cobs(i,5)<<" Cpred="<<CWtotpred(yy,ss,rr,ff)<<endl;
	}
	
	if(debug)cout<<"---  * Finished Catch LHF ---"<<endl;
	
	// -- CPUE observations --
	
	dvariable CPUEtemp;
		
	if(nCPUEobs>0){                   // The CPUE indices are contributing to the likelihood function
	  
	  for(int ff=1;ff<=nf;ff++){
	    
	    CPUEtemp=0.;                      // Reset the counter
	    
	    for(int yy=1;yy<=ny;yy++){
	      
	      for(int ss=1;ss<=ns;ss++){
	        
	        for(int rr=1;rr<=nr;rr++){
	        
	          CPUEtemp+=VB(yy,ss,rr,ff);  // Sum vulnerable biomass over whole time series
	       	
	       	}
	  	
	      }
	      
	    }
	   
	    CPUEtemp/=ny*ns*nr; // Get mean 
	   
	    for(int yy=1;yy<=ny;yy++){
	    	      
	      for(int ss=1;ss<=ns;ss++){	    	        
	        
	        for(int rr=1;rr<=nr;rr++){
	        
	           VBi(yy,ss,rr,ff) = VB(yy,ss,rr,ff)/CPUEtemp; // normalise VB into an index with mean 1
	        
	        }
	        
	      }
	     
	    }
	    
	  } // End of fleets
		
	  
	  for(int i=1;i<=nCPUEobs;i++){ // Loop over catch rate indices
		
	    int yy=CPUEobs(i,1);    // Year
	    int ss=CPUEobs(i,2);    // Subyear
	    int rr=CPUEobs(i,3);    // Region
	    int ii=CPUEobs(i,4);    // index No ID
	    int ff=CPUEobs(i,5);    // fleet index ID
	    int lt=CPUEobs(i,7);    // if related to a length group > 0	    
	    //cout<<yy<<"-"<<ss<<"-"<<rr<<"-"<<ii<<"-"<<ff<<"-"<<CPUEobs(i,5)<<endl;
	    
	    LHtemp=0.; 
	    
	    if(lt<1){ // vulnerable biomass
	    	
	      CPUEpred(i)=VBi(yy,ss,rr,ff);                                       // Calculate vulnerable biomass
	           	  	    
	    }else{// 
	      
	      CPUEpred(i)=sum(VBL(yy)(ss)(rr)(ff)(Ilencat(lt,1),Ilencat(lt,2))); // the prediced index
	             
	    }
	    
	    CPUEpredtot(1,ii)+=CPUEpred(i);
	    CPUEpredtot(2,ii)+=CPUEobs(i,6);
	    	  
	 }
	 
	 
	 for(int ii=1; ii<=nCPUEq;ii++){
	 
	    qCPUE(ii)=CPUEpredtot(2,ii)/CPUEpredtot(1,ii); // q is sum of un-normalized observed indices / un-normalized predicted biomass, q=I/B
	 
	 }
	  
	 for(int i=1;i<=nCPUEobs;i++){ // Loop over catch rate indices
		
	    int yy=CPUEobs(i,1);    // Year
	    int ss=CPUEobs(i,2);    // Subyear
	    int rr=CPUEobs(i,3);    // Region
	    int ii=CPUEobs(i,4);    // index No ID
	    int ff=CPUEobs(i,5);    // fleet index ID
	    int lt=CPUEobs(i,7);    // if related to a length group > 0	     
	    
	    CPUEpred_vec(i)=CPUEpred(i)*qCPUE(ii);
	    LHtemp=dnorm(log(CPUEpred_vec(i)+tiny),log(CPUEobs(i,6)+tiny),CPUEobs(i,8)); //,CPUEobsCV(ii));        // Log-normal LHF
	    objCPUE+=LHtemp*CPUEobs(i,9)*LHw(2);                                                   // Weighted likelihood contribution
	    objG+=LHtemp*CPUEobs(i,9)*LHw(2); // Weighted likelihood contribution
	    		  
	  }
	  
	  if(debug)cout<<"---  * Finished CPUE LHF ---"<<endl;
	   
	}
	
	// -- Fishery independent indices --
	
	dvariable Itemp;              // Create a dummy variable cor calculating a normalilized (mean 1) index 
	
	for(int i=1; i<=nIobs;i++){   // Loop over fishery - independent indices PRECALCULATION for normalization
	
	  int yy=Iobs(i,1);   // Year
	  int ss=Iobs(i,2);   // Subyear
	  int rr=Iobs(i,3);   // Region
	  int pp=Iobs(i,4);   // stock
	  int ii=Iobs(i,5);   // q index (often stock for SSB types)
	  int tt=Iobs(i,6);   // Type
	  int ff=Iobs(i,9);   // Selectivity mirror
	  int lt=Iobs(i,11);  // length category
		  
	  switch(tt){
	  		    
	    case 1:  // Biomass
	  	      
	      Ipred(i)=B(yy,ss,rr);
	      
	      break;
	  		   
	    case 2:  // SSB
	       
	      Ipred(i)=0.;
	      for(int aa=1;aa<=na;aa++){
	        Ipred(i)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); //SSB(pp,yy,ss);   // Predicted index 
	      }  
	      
	      break;
	     
	    case 3:  // Biomass first two stocks
	     
	      Ipred(i)=B(yy,1,rr)+B(yy,2,rr);  // Predicted index 
	     
	      break;
	      
	    case 4:  // Biomass first two stocks
	     
	      Ipred(i)= VBi(yy,ss,rr,ff); // CPUEpred(i)=sum(VL(yy)(ss)(rr)(ff));//(Ilencat(lt,1),Ilencat(lt,2)));B(yy,1,rr)+B(yy,2,rr);  // Predicted index 
	     
	      break;  
	      
	    case 5:  // Biomass first two stocks
	      	     
	      Ipred(i)= sum(VL(yy)(ss)(rr)(ff)(Ilencat(lt,1),Ilencat(lt,2))); // CPUEpred(i)=sum(VL(yy)(ss)(rr)(ff));//(Ilencat(lt,1),Ilencat(lt,2)));B(yy,1,rr)+B(yy,2,rr);  // Predicted index 

	      break;  
	  		      
	  }
	  
	  Ipredtot(1,ii)+=Ipred(i);
	  Ipredtot(2,ii)+=Iobs(i,7);
	  
	}
	
	for(int ii=1; ii<=nI;ii++){
	  qI(ii)=Ipredtot(2,ii)/Ipredtot(1,ii); // q is sum of un-normalized observed indices / un-normalized predicted biomass, q=I/B
	}
	
	for(int i=1; i<=nIobs;i++){   // Loop over fishery - independent indices PRECALCULATION for normalization
		
	  int yy=Iobs(i,1);   // Year
	  int ss=Iobs(i,2);   // Subyear
	  int rr=Iobs(i,3);   // Region
	  int pp=Iobs(i,4);   // stock
	  int ii=Iobs(i,5);   // q index (often stock for SSB types)
	  int tt=Iobs(i,6);   // Type
	  
	  Ipred_vec(i)=Ipred(i)*qI(ii); // Normalized log index with mean 0
	  LHtemp=dnorm(log(Ipred_vec(i)+tiny),log(Iobs(i,7)+tiny),Iobs(i,8)); // Log-normal LHF
	  objI+=LHtemp*Iobs(i,10)*LHw(3);                                          // Weighted likelihood contribution
	  objG+=LHtemp*Iobs(i,10)*LHw(3);                                          // Weighted likelihood contribution
	  
	}
	
	if(debug)cout<<"---  * Finished FI index LHF ---"<<endl;
	
	// -- Length composition data --  
	
	for(int i=1;i<=nCLobs;i++){  // Loop over catch at length observations
		
	  int yy=CLobs(i,1);   // Year
          int ss=CLobs(i,2);   // Subyear
	  int rr=CLobs(i,3);   // Region
	  int ff=CLobs(i,4);   // Fleet type
	  int ll=CLobs(i,5);   // Length class
	  CLprop(i)=VL(yy,ss,rr,ff,ll)/sum(VL(yy)(ss)(rr)(ff));
	  //CLn(ff)+=1;
	  //CLvar(ff)+=pow(sqrt(CLprop(i)+tiny)-sqrt(CLobs(i,6)+tiny),2.);
	  
	}
	
	//for(int ff=1;ff<=nf;ff++){
	
	//  CLsd(ff)=sqrt(CLvar(ff)/CLn(ff)); // Some comp likelihoods need the sd estimate
	  
	//}
		
	for(int i=1;i<=nCLobs;i++){  // Loop over catch at length observations
	
	  int yy=CLobs(i,1);   // Year
	  int ss=CLobs(i,2);   // Subyear
	  int rr=CLobs(i,3);   // Region
	  int ff=CLobs(i,4);   // Fleet type
	  int ll=CLobs(i,5);   // Length class
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" l="<<ll<<" CLobs="<<CLobs(i,6)<<" CLpred="<<CLtotpred(yy,ss,rr,ff,ll)<<endl;
	  //LHtemp=dnorm(log(CLtotpred(yy)(ss)(rr)(ff)(ll)),log(CLobs(i,6)),0.2);
	  LHtemp=dnorm(log(CLprop(i)+tiny),log(CLobs(i,6)),sqrt(CLCV_num/CLobs(i,6))); // was 0.05
	  objCL+=LHtemp*LHw(4)*CLobs(i,7);                                     // Weighted likelihood contribution
	  objG+=LHtemp*LHw(4)*CLobs(i,7);                                      // Weighted likelihood contribution
	
	}
	
	if(debug)cout<<"---  * Finished length composition LHF ---"<<endl;
	
	// -- Stock of origin data -- 
	
	dvariable SOOtot;    // For storing the sum of SOO numbers
	dvariable SOOeast;
	dvariable SOOfrac;
	
	SOOtot.initialize(); // SSOtot = 0
	SOOeast.initialize(); // 
	SOOfrac.initialize(); // 
	
	for(int i=1;i<=nSOOobs;i++){ // Loop over stock of origin observations
	  
	  int aa=SOOobs(i,1);   // Age class
	  int yy=SOOobs(i,2);   // Year
	  int ss=SOOobs(i,3);   // Subyear
	  int rr=SOOobs(i,4);   // Region
	  int tp=SOOobs(i,8);   //type 1: microchemistry, 2: genetics
	  	  
	  if(aa==1) lc =3; // get length class 1 for age group 1
	  if(aa==2) lc =6; // get length class 6 for age group 2
	  if(aa==3) lc= 10; // get length class 10 for age group 3
	  
	  SOOtot=0.;            // Reset sum
	  SOOeast=0.;
	  	  
	  SOOeast=sum(CTA(1)(yy)(ss)(rr)(macat(aa,1),macat(aa,2)));
          SOOtot=sum(CTA(1)(yy)(ss)(rr)(macat(aa,1),macat(aa,2)))+sum(CTA(2)(yy)(ss)(rr)(macat(aa,1),macat(aa,2)));
	  	  
	  SOOfrac=SOOeast/SOOtot;
	  SOOpred(i)=log((SOOfrac/(1.0001-SOOfrac))+tiny); // Calculate predicted fraction (robustified to be positive)
	  LHtemp=dnorm(SOOpred(i),SOOobs(i,6),SOOobs(i,7)); //SOOobs(i,7));
	  
	  //cout<<SOOpred(i)<<" "<<SOOobs(i,6)<<" "<<SOOeast<<" "<<SOOtot<<" "<<CLpred(1)(yy)(ss)(rr)(1)(lc)<<" "<<CLpred(2)(yy)(ss)(rr)(1)(lc)<<endl;
	  
	  objSOO+=LHtemp*LHw(5)*SOOobs(i,9);                     // Weighted likelihood contribution
	  if(tp==1)objSOOm+=LHtemp*LHw(5)*SOOobs(i,9); 
	  if(tp==2)objSOOg+=LHtemp*LHw(5)*SOOobs(i,9); 
	  objG+=LHtemp*LHw(5)*SOOobs(i,9);                       // Weighted likelihood contribution
	  	
	}
		
	//cout<<SOOpred<<endl;
	//exit(1);
	
	if(debug)cout<<"---  * Finished SOO LHF ---"<<endl;
	
	// -- PSAT tagging --
	
	for(int i=1;i<=nPSAT;i++){ // Tags with certain stock of origin
	
	  int pp=PSAT(i,1);   // Population 
	  int aa=PSAT(i,2);   // Movement age class
	  int ss=PSAT(i,3);   // Subyear
	  int tt=PSAT(i,4);   // Time at liberty (subyears)
	  int rr=PSAT(i,5);   // Region from
	  int r2=PSAT(i,6);   // Region to
	  	  	  
	  if(ss==1){
	   LHtemp=(-PSAT(i,7)*log(movm(pp,ns,aa,rr,r2)+tiny)); // Multinomial LHF
	   PSATpred(i)=movm(pp,ns,aa,rr,r2);
	  }else{
	   LHtemp=(-PSAT(i,7)*log(movm(pp,ss-1,aa,rr,r2)+tiny)); // Multinomial LHF
	   PSATpred(i)=movm(pp,ss-1,aa,rr,r2);
	  }
	  
	  //LHtemp=(-PSAT(i,7)*log(RecapP(pp,ss,aa,tt,rr,r2)+tiny)); // Multinomial LHF
	  //PSATpred(i)=RecapP(pp,ss,aa,tt,rr,r2);
	  //LHtemp=dnorm(log(RecapP(pp,ss,aa,tt,rr,r2)+tiny),log(PSAT(i,8)),sqrt(0.04/(PSAT(i,7)*PSAT(i,8))));
	  
	  objPSAT+=LHtemp*LHw(6);                               // Weighted likelihood contribution
	  objG+=LHtemp*LHw(6);                                  // weighted likelihood contribution
	}
	
	/*
	for(int i=1;i<=nPSAT2;i++){ // Tags with uncertain stock of origin
		
	  int aa=PSAT2(i,1);   // Population 
	  int yy=PSAT2(i,2);   // Movement age class
	  int ss=PSAT2(i,3);   // Subyear
	  int tt=PSAT2(i,4);   // Time at liberty (subyears)
	  int rr=PSAT2(i,5);   // Region from
	  int r2=PSAT2(i,6);   // Region to

          if(aa==1){
             Ntemp=(N(1)(yy)(ss)(4)(rr)*RecapP(1,ss,aa,tt,rr,r2)+N(2)(yy)(ss)(4)(rr)*RecapP(2,ss,aa,tt,rr,r2)) / (sum(RecapP(1)(ss)(aa)(tt)(rr)*N(1)(yy)(ss)(4)(rr)) + sum(RecapP(2)(ss)(aa)(tt)(rr)*N(2)(yy)(ss)(4)(rr)));
          }else if(aa==2){
             Ntemp=(N(1)(yy)(ss)(6)(rr)*RecapP(1,ss,aa,tt,rr,r2)+N(2)(yy)(ss)(6)(rr)*RecapP(2,ss,aa,tt,rr,r2)) / (sum(RecapP(1)(ss)(aa)(tt)(rr)*N(1)(yy)(ss)(6)(rr)) + sum(RecapP(2)(ss)(aa)(tt)(rr)*N(2)(yy)(ss)(6)(rr)));
          }else{
             Ntemp=(N(1)(yy)(ss)(9)(rr)*RecapP(1,ss,aa,tt,rr,r2)+N(2)(yy)(ss)(9)(rr)*RecapP(2,ss,aa,tt,rr,r2)) / (sum(RecapP(1)(ss)(aa)(tt)(rr)*N(1)(yy)(ss)(9)(rr)) + sum(RecapP(2)(ss)(aa)(tt)(rr)*N(2)(yy)(ss)(9)(rr)));
          }
	  LHtemp=(-PSAT2(i,7)*log(Ntemp+tiny)); // Multinomial LHF
	  PSAT2pred(i)=Ntemp;
	  //LHtemp = dnorm(log(Ntemp+tiny),log(PSAT2(i,8)+tiny),sqrt(0.05/(PSAT2(i,7)*PSAT2(i,8))));
	  objPSAT2+=LHtemp*LHw(7);                               // Weighted likelihood contribution
	  objG+=LHtemp*LHw(7);                                  // weighted likelihood contribution
	}
	*/
	objPSAT2=0.;
	
	if(debug)cout<<"---  * Finished PSAT known SOO LHF ---"<<endl;
			
	for(int i=1;i<=nSSBprior;i++){ 
	  
	  int pp=SSBprior(i,1);
	  int yy=SSBprior(i,2);
	  
	  if(last_phase()){
	    LHtemp=dnorm(log(SSB_EW(pp,yy)/1000.+tiny),log(SSBprior(i,3)),SSBCV);
	  }else{
	    LHtemp=dnorm(log(SSB_EW(pp,yy)/1000.+tiny),log(SSBprior(i,3)),SSBCV*4.);
	  }
	  
	  objSSB+=LHtemp*LHw(12);
	  objG+=LHtemp*LHw(12);
	  
	}
	
	for(int i=1;i<=nDepprior;i++){ 
	  int pp=Depprior(i,1);
	  int yy=Depprior(i,2);
	  
	  if(last_phase()){
	    LHtemp=dnorm(log(SSB_EW(pp,yy)/SSB_EW(pp,2)+tiny),log(Depprior(i,3)),DepCV);
	  }else{
	    LHtemp=dnorm(log(SSB_EW(pp,yy)/SSB_EW(pp,2)+tiny),log(Depprior(i,3)),DepCV*4);
	  }
	  
	  objDep+=LHtemp*LHw(13);
	  objG+=LHtemp*LHw(13);
	}	
		
	// -- Recruitment deviations --
	
	temp=(ny+na)/nRD;
	
	for(int sr=1; sr<=nSR; sr++){
	
	  int nRDs_s=nRDs(sr);
	  
	  for(int rno=1; rno<=nRDs_s; rno++){
	  
	    LHtemp=dnorm(lnRDmat(sr,rno),0,RDCV);   // Recruitment deviations
	    objRD+=LHtemp*LHw(8);                // Weighted likelihood contribution
	    objG+=LHtemp*LHw(8);                 // Weighted likelihood contribution
	    
	  }
        }
        
	// -- Movement parameters ---
	
	LHtemp=0.0;
	for(int ss=1;ss<=ns;ss++){               // Loop over subyears
	
	  for(int tr =1;tr<=nr-2;tr++){ 
	    // Custom age-class specific movement, estimated as deviations from age class 2
	    LHtemp+=dnorm(movest_p1_a2(ss,tr),0.0,movCV);
	    LHtemp+=dnorm(movest_p2_a2(ss,tr),0.0,movCV);
	    
	    LHtemp+=dnorm(movest_p1_a1(ss,tr),0.0,movCV);
	    LHtemp+=dnorm(movest_p2_a1(ss,tr),0.0,movCV);
	    
	    LHtemp+=dnorm(movest_p1_a3(ss,tr),0.0,movCV);
	    LHtemp+=dnorm(movest_p2_a3(ss,tr),0.0,movCV);
	    
	 } // to area
       } // subyear
			 
       objmov+=LHtemp*LHw(9);               // Weighted likelihood contribution
       objG+=LHtemp*LHw(9);                 // Weighted likelihood contribution 
		
	
       // -- Spatial priors -----------
       //exit(1);
       
       for(int i=1; i<=nSpatPr;i++){   // Loop over spatial priors 
       	
       	  int yy=SpatPr(i,1);   // Year
       	  int ss=SpatPr(i,2);   // Subyear
       	  int rr=SpatPr(i,3);   // Region
       	  int ii=SpatPr(i,4);   // q index 
       	  int lf=SpatPr(i,8);   // length from
       	  int lt=SpatPr(i,9);   // length to
       	      	     
       	  SPpred(i)= sum(BL(yy)(ss)(rr)(lf,lt)); // CPUEpred(i)=sum(VL(yy)(ss)(rr)(ff));//(Ilencat(lt,1),Ilencat(lt,2)));B(yy,1,rr)+B(yy,2,rr);  // Predicted index 
       	  SPpredtot(1,ii)+=SPpred(i);
       	  SPpredtot(2,ii)+=SpatPr(i,5);
       	  
       }
       	
       for(int ii=1; ii<=nSpatPrq;ii++){
       	  qSP(ii)=SPpredtot(2,ii)/SPpredtot(1,ii); // q is sum of un-normalized observed indices / un-normalized predicted biomass, q=I/B
       }
       	
       for(int i=1; i<=nSpatPr;i++){   // Loop over fishery - independent indices PRECALCULATION for normalization
       		
       	   int yy=SpatPr(i,1);       // Year
	   int ss=SpatPr(i,2);       // Subyear
	   int rr=SpatPr(i,3);       // Region
	   int ii=SpatPr(i,4);       // q index 
	   
       	   SPpred_vec(i)=SPpred(i)*qSP(ii);                                         // Normalized log index with mean 0
       	   LHtemp=dnorm(log(SPpred_vec(i)+tiny),log(SpatPr(i,5)+tiny),SpatPr(i,6)); // Log-normal LHF
       	   objSP+=LHtemp*SpatPr(i,7)*LHw(18);                                       // Weighted likelihood contribution
       	   objG+= LHtemp*SpatPr(i,7)*LHw(18);                                       // Weighted likelihood contribution
       	  
       }
		
       // -- Selectivity parameters ---
	
       for(int i=1;i<=nsel;i++){ 
	  
	  objsel+=dnorm(selpar(i),0,selCV)*LHw(10);  
	  objG+=dnorm(selpar(i),0,selCV)*LHw(10);   // Prior on selectivity to add numerical stability
	
       }
	
       if(nSR>3){
          LHtemp=lnR0(1)-lnR0(2);
          objR0+=dnorm(LHtemp,0,R0diffCV)*LHw(15); 
          objG+=dnorm(LHtemp,0,R0diffCV)*LHw(15);
          
          LHtemp=lnR0(3)-lnR0(4);
          objR0+=dnorm(LHtemp,0,R0diffCV)*LHw(15); 
          objG+=dnorm(LHtemp,0,R0diffCV)*LHw(15);
       }
	
       //if(last_phase()){
	
       objG+=objSRA*LHw(11);                      // Add the posfun penalty for SRA harvest rates over 90%
	
     	LHtemp=0.;	
	// F modifier prior
	for(int ll=1;ll<=ns*nr;ll++){
	  
	  LHtemp+=dnorm(Fmod(ll),0.,FCV);
	  
	}
	objFmod=LHtemp*LHw(14);
	objG+=LHtemp*LHw(14);
	
	
	// BSfrac prior
		
	for(int ss=1; ss<=ns;ss++){
	  BSfrac_pred(1,ss)=sum(stemp(1)(ss)(1,3))/sum(stemp(1)(ss)); // East stock (1) in western areas (1-3)
	  BSfrac_pred(2,ss)=sum(stemp(2)(ss)(4,nr))/sum(stemp(2)(ss)); // West stock (2) in eastern areas (4-7)
	  objBSfrac+=dnorm(log(BSfrac_pred(1,ss)+tiny),log(BSfrac(1,ss)+tiny),BSfracCV)*LHw(16);
	  objBSfrac+=dnorm(log(BSfrac_pred(2,ss)+tiny),log(BSfrac(2,ss)+tiny),BSfracCV)*LHw(16);
	}  
		
	objG+=objBSfrac;
	
	// Master index invariant calculations
	LHtemp=0.;	  
	for(int ss=1;ss<=ns;ss++){                   // Loop over seasons

	  for(int rr =1;rr<=nr;rr++){  

	    LHtemp+=dnorm(log(FDYt(rr)(ss)),0.,MICV);

	  }

	}

	objFDY=LHtemp*LHw(17);
	objG+=LHtemp*LHw(17);
	
	
	// End of OBJ calcs
	
	if(debug)cout<<"---  * Finished rec dev penalty ---"<<endl;
	if(debug)cout<<"--- Finished calcObjective ---"<<endl;
	
	if(verbose)cout<<"Catch LHF "<<objC<<endl;            // Report catch likelihood component
	if(verbose)cout<<"CPUE LHF "<<objCPUE<<endl;          // Report CPUE likelihood component
	if(verbose)cout<<"FI index LHF "<<objI<<endl;         // Report FI index likelihood component
	if(verbose)cout<<"Length comp LHF "<<objCL<<endl;     // Report catch at length likelihood component
        if(verbose)cout<<"SOO LHF "<<objSOO<<endl;            // Report stock of origin likelihood component
	if(verbose)cout<<"PSAT LHF "<<objPSAT<<endl;          // Report PSAT likelihood component
	if(verbose)cout<<"PSAT uSOO LHF "<<objPSAT2<<endl;    // Report PSAT2 likelihood component
	if(verbose)cout<<"Rec dev LHF "<<objRD<<endl;         // Report Rec dev likelihood component
	if(verbose)cout<<"mov prior "<<objmov<<endl;          // Report Rec dev likelihood component
	if(verbose)cout<<"selectivity prior "<<objsel<<endl;  // Report Rec dev likelihood component
	if(verbose)cout<<"SRA penalty "<<objSRA*LHw(11)<<endl;// Report penalty for excessive F in SRA
	if(verbose)cout<<"SSB prior "<<objSSB<<endl;          // Report penalty for current SSB prior
	if(verbose)cout<<"Dep prior "<<objDep<<endl;          // Report penalty for unfished SSB ratio (East / West)
	if(verbose)cout<<"Fmod prior "<<objFmod<<endl;        // Report penalty for Fmod prior
	if(verbose)cout<<"R0 ratio prior "<<objR0<<endl;      // Report penalty for R0 ratio prior
	if(verbose)cout<<"BS frac prior "<<objBSfrac<<endl;   // Report penalty for BSfrac mixing 
	if(verbose)cout<<"FDY MIinv prior "<<objFDY<<endl;    // Report penalty for extra Fmod variability in F=qE (master index)
	if(verbose)cout<<"Spatial Priors "<<objSP<<endl;      // Report penalty for SpatPr prior
	if(verbose)cout<<"Global objective "<<objG<<endl;     // Report Global objective function
	//if(verbose)cout<<"Global objective x 10000 "<<objG*10000<<endl;     // Report Global objective function
	
  }


FUNCTION simsam
  {
        // If working with simulated data do some printing
        cout<<"log R0 = " <<lnR0<<endl;                    // Estimated R0 
        cout<<"selpar = "<<selpar<<endl;                     // Estimated R0
        for(int ff=1;ff<=nf;ff++){
          //cout<<"sel sim f1= "<<sel_ini(1)<<endl;            // Simulated selectivity fleet 1
          cout<<"sel sam "<<ff<<" "<<sel(ff)<<endl;            // Estimated selectivity fleet 1
        }
        cout<<"RecDevs= "<<Rec<<endl;
       	cout<<"Visc= "<<visc<<endl;
       	cout<<"movest= "<<movest<<endl;                         // Estimated movement parameters
	
	cout<<"East   GOM         WATL      GSL      SATL      NATL      EATL      MED      -     GOM         WATL      GSL      SATL      NATL      EATL      MED     -     GOM         WATL      GSL      SATL      NATL      EATL      MED"<<endl;
	//cout<<"mov sam p1 s1 = "<<round(value(mov(1)(1)(1)(1)),2)<<endl;
	cout<<"s1 = "<<mov(1)(1)(1)(1)<<" - "<<mov(1)(1)(7)(1)<<" - "<<mov(1)(1)(12)(1) <<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"s2 = "<<mov(1)(2)(1)(1)<<" - "<<mov(1)(2)(7)(1)<<" - "<<mov(1)(2)(12)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"s3 = "<<mov(1)(3)(1)(1)<<" - "<<mov(1)(3)(7)(1)<<" - "<<mov(1)(3)(12)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"s4 = "<<mov(1)(4)(1)(1)<<" - "<<mov(1)(4)(7)(1)<<" - "<<mov(1)(4)(12)(1)<<endl; 
		
	cout<<"West   GOM         WATL      GSL      SATL      NATL      EATL      MED      -     GOM         WATL      GSL      SATL      NATL      EATL      MED     -     GOM         WATL      GSL      SATL      NATL      EATL      MED"<<endl;
	cout<<"s1 = "<<mov(2)(1)(1)(1)<<" - "<<mov(2)(1)(7)(1)<<" - "<<mov(2)(1)(12)(1) <<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"s2 = "<<mov(2)(2)(1)(1)<<" - "<<mov(2)(2)(7)(1)<<" - "<<mov(2)(2)(12)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"s3 = "<<mov(2)(3)(1)(1)<<" - "<<mov(2)(3)(7)(1)<<" - "<<mov(2)(3)(12)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"s4 = "<<mov(2)(4)(1)(1)<<" - "<<mov(2)(4)(7)(1)<<" - "<<mov(2)(4)(12)(1)<<endl; 
	
	cout<<"Fmod = "<<Fmod<<endl;
	cout<<"FDY = "<<endl;
	cout<<FDYt<<endl;
	
	cout<<"qCPUE sam= "<<qCPUE<<endl;                      // Estimated catchabilities
	cout<<"qE sam= "<<qE<<endl;                             // Simulated FI index catchability
	cout<<"lnqE sam= "<<lnqE<<endl; 
	cout<<"qI sam= "<<qI<<endl;                          // Estimated FI index catchability
	cout<<"D sam= "<<D<<endl;			     // Estimated depletion SSB/SSB0
	cout<<"SSBnow= "<<SSBnow<<endl;                      // Current SSB by stock
	cout<<"SPpred= "<<SPpred_vec<<endl;
	cout<<"------------------------------------"<<endl; 
  }



FUNCTION popnodes
  {
	// -- Populate nodes for mcmc output --
        //ORDER: nSR + sum(seltype) + (np*ns*nma*(nr-2)) + nE + nI + nCPUEq + ns*nr + nRD;	
	nodes(1,nSR)=lnR0;
	
	j=nSR;
	
	for(int ff=1; ff<=nsel;ff++){
	  for(int sp=1; sp<=seltype(ff);sp++){
	    j+=1;
	    nodes(j)=selpar(ff,sp);
	  }   
	}
			
	for(int ss=1; ss<=ns;ss++){
	  for(int rr=1; rr<=(nr-2);rr++){
	
	    j+=1;
	    nodes(j)=movest_p1_a1(ss,rr);
	     
	  }
	}
	for(int ss=1; ss<=ns;ss++){
	  for(int rr=1; rr<=(nr-2);rr++){

	    j+=1;
	    nodes(j)=movest_p2_a1(ss,rr);

	  }
	}
	for(int ss=1; ss<=ns;ss++){
	  for(int rr=1; rr<=(nr-2);rr++){

	    j+=1;
	    nodes(j)=movest_p1_a2(ss,rr);

	  }
	}
	for(int ss=1; ss<=ns;ss++){
	  for(int rr=1; rr<=(nr-2);rr++){

	    j+=1;
	    nodes(j)=movest_p2_a2(ss,rr);

	  }
	}
	for(int ss=1; ss<=ns;ss++){
	  for(int rr=1; rr<=(nr-2);rr++){

	    j+=1;
	    nodes(j)=movest_p1_a3(ss,rr);

	  }
	}
	for(int ss=1; ss<=ns;ss++){
	  for(int rr=1; rr<=(nr-2);rr++){

	    j+=1;
	    nodes(j)=movest_p2_a3(ss,rr);

	  }
	}
	
	for(int pp=1; pp<=np; pp++){
	
	  for(int ss=1; ss<=ns;ss++){
          
	   j+=1;
	   nodes(j)=visc(pp,ss);
	  
	  }
		
	}
		
	for(int ff=1; ff<=nE;ff++){
	  j+=1;
	  nodes(j)=lnqE(ff);
	}
	
	for(int ff=1; ff<=nI;ff++){
	  j+=1;
	  nodes(j)=log(qI(ff));
	}
	
	for(int ff=1; ff<=nCPUEq;ff++){
          j+=1;
	  nodes(j)=log(qCPUE(ff));
	}
	
	for(int ii=1; ii<=(ns*nr);ii++){
	  j+=1;
          nodes(j)=Fmod(ii);
	}
	
	for(int sr=1; sr<=nSR; sr++){
			
	  int nRDs_s=nRDs(sr);

	  for(int rno=1; rno<=nRDs_s; rno++){

	   j+=1;
	   nodes(j)=lnRDmat(sr,rno);

	  }
		  
	}
	
	for(int rr=1; rr<=nr;rr++){
	  for(int ss=1; ss<=ns;ss++){
	    for(int yy=1; yy<=ny;yy++){
	      j+=1;
	      nodes(j)=FDYt(rr,ss,yy);
	    }
	  }
	}
	
	if(debug)cout<<"--- Finished popnodes ---"<<endl;
  }



REPORT_SECTION
  {
  	report <<"np, number of stocks"<<endl;
  	report <<np<<endl;
  	
  	report <<"nHy, number of historical (SRA) years"<<endl;
  	report <<nHy<<endl;
  	
  	report <<"ny, number of years"<<endl;
  	report <<ny<<endl;
  	
  	report <<"ns, numnber of subyears"<<endl;
  	report <<ns<<endl;
  	
  	report <<"nr, number of areas"<<endl;
  	report <<nr<<endl;
  	
  	report <<"nf, number of fleets"<<endl;
  	report <<nf<<endl;
  	
  	report <<"na, number of age classes"<<endl;
  	report <<na<<endl;
  	
  	report <<"nl, number of length classes"<<endl;
  	report <<nl<<endl;
  	
  	report <<"nma, number of age classes"<<endl;
  	report <<nma<<endl;
  	
  	report <<"SSB (p,y,s) spawning stock biomass"<<endl;
  	report <<SSB<<endl;
  	
  	report <<"hSSB (p,hy,s) historical spawning stock biomass"<<endl;
  	report <<hSSB<<endl;
  	  	
  	report <<"Fishing mortality rate at length FL (y s r f l)"<<endl;
  	for(int yy=1;yy<=ny;yy++){
  	  report <<FL(yy)<<endl;
  	}
  	
  	report <<"HCobs (Hy x s x a x r) observed historical catches (numbers)"<<endl;
  	for(int yy=1;yy<=nHy;yy++){
  	  report <<HCobs(yy)<<endl;
  	}
  	
  	report <<"nCobs"<<endl;
  	report <<nCobs<<endl;
  	
  	report <<"Cobs (y-s-r-f) observed catches (weight)"<<endl;
  	report <<Cobs<<endl;
  	
  	report <<"CWtotpred (y,s,r,f) predicted catches (weight)"<<endl;
  	for(int yy=1;yy<=ny;yy++){
  	  report <<CWtotpred(yy)<<endl;
  	}
  	
  	report <<"nCLobs"<<endl;
  	report <<nCLobs<<endl;
  	
  	report <<"CLobs (y-s-r-f-l) observed catch at length (numbers)"<<endl;
  	report <<CLobs<<endl;
  	
  	report <<"CLprop (nCLobs) proportion predicted at length"<<endl;
  	report <<CLprop<<endl;
  	
  	report <<"CLtotpred (y,s,r,f,l)"<<endl;
  	for(int yy=1;yy<=ny;yy++){
  	  report <<CLtotpred(yy)<<endl;
  	}
  	
  	report <<"VL (y,s,r,f,l)"<<endl;
	for(int yy=1;yy<=ny;yy++){
	  report <<VL(yy)<<endl;
  	}
  	
  	report <<"movement age groups (p,a)"<<endl;
  	report <<ma<<endl;
  	
  	report <<"mov (p,s,a,r,r) Markov movement matrix"<<endl;
  	for(int pp=1;pp<=np;pp++){
  	  report <<mov(pp)<<endl;
  	}
  	
  	report <<"sel (f,l) selectivity by fleet and length class"<<endl;
  	report <<sel<<endl;
  	
  	report<<"RAI (r,s,y)"<<endl;
  	report<<RAI<<endl;
  	
  	report<<"ml (l) mean length of each length bin"<<endl;
  	report<<ml<<endl;
  	
  	report<<"VB (y,s,r,f) vulnerable biomass by fleet"<<endl;
  	report<<VB<<endl;
  	
        report<<"B (y,s,r) biomass"<<endl;
        report<<B<<endl; 
  	
  	report<<"N (p,y,s,a,r)"<<endl;
  	for(int pp=1;pp<=np;pp++){
	  report <<N(pp)<<endl;
  	}
  	
  	report<<"SSB_mu (p,y) biomass"<<endl;
        report<<SSB_mu<<endl;
        
        report<<"Rec_mu (p,y) biomass"<<endl;
        report<<Rec_mu<<endl;
        
        report<<"Rec_wd (p,y) biomass"<<endl;
        report<<Rec_wd<<endl;
  	
  	report<<"lwa (p) length-weight paramter alpha"<<endl;
  	report<<lwa<<endl;
  	
        report<<"lwb (p) length-weight paramter beta"<<endl;
        report<<lwb<<endl;
        
        report<<"len_age (y,a,p) length at age (pass through) used to calculate iALK and B independently"<<endl;
        report<<len_age<<endl;
        
        report<<"wt_age (y,a,p) weight at age (pass through) used to calculate iALK and B independently"<<endl;
	report<<wt_age<<endl;
	
	report<<"nMP"<<endl;
	report<< nMP <<endl;                        // Number of movement parameters estimated
		
	report<<"nmovind"<<endl;
	report<<nmovind<<endl;
	
	report<<"movind (nmovind, 4)"<<endl;
	report<<movind<<endl;
		
	report<<"nmov1"<<endl;
	report<<nmov1<<endl;
		                  
	report<<"mov1(nmov1,4)"<<endl;       // Index of first non-estimated movement parameter (fixed to zero)
	report<<mov1<<endl; 
	
	report<<"movtype"<<endl;             // 1: gravity, 2: markov
	report<<movtype<<endl;
	
	report<<"Ma (p,a)"<<endl;
	report<<Ma<<endl;
		
	report<<"Fec (p,a)"<<endl;
	report<<Fec<<endl;
	
	report<<"Steep (p)"<<endl;
	report<<steep<<endl;
		                
	report<<"nsel number of estimated selectivities"<<endl;
	report<<nsel<<endl;
	
	report<<"seltype (nsel): logistic, 3: Thompson dome-shaped"<<endl;
	report<<seltype<<endl;
	
	report<<"selind (f) which selectivity is assigned to each fleet"<<endl;
	report<<selind<<endl;
	
	report<<"LB (f) the lower bound of selectivity by fleet"<<endl; // note mismatch with nsel - here assuming nf = nsel
	report<<LB<<endl;
	
	report<<"UB (f) the upper bound of selectivity by fleet"<<endl; // note mismatch with nsel - here assuming nf = nsel
	report<<UB<<endl;
		
	report<<"spawns (p) the subyear for spawning"<<endl;
	report<<spawns<<endl;
	
	report<<"ALK (p,y,a,l) inverse age-length key p(l|a)"<<endl;
	for(int pp=1;pp<=np;pp++){
            report <<ALK(pp)<<endl;
  	}
	
	report<<"lnqE (f) the estimated log qs"<<endl;
	report<<lnqE<<endl;
	
	report<<"lnqI (nI) the estimated log qs"<<endl;
	report<<log(qI)<<endl;
	
	report<<"lnqI (nCPUEq) the estimated log qs"<<endl;
	report<<log(qCPUE)<<endl;
	
	report<<"Ilencat"<<endl;
	report<<Ilencat<<endl;
				
	report<<"hZ (p,hy,s,a,r)"<<endl;
	for(int pp=1;pp<=np;pp++){
	  report <<hZ(pp)<<endl;
  	}
  			
	report<<"nydist number of year iterations to get initial spatial distribution"<<endl;
	report<<nydist<<endl;
		
	report<<"SSB0: unfished spawning stock biomass"<<endl;
	report<<SSB0<<endl;
	
	report<<"R0: unfished recruitment(stock,year)"<<endl;
	report<<R0<<endl;
	
	report<<"steep: steepness(stock,year)"<<endl;
	report<<steep<<endl;
	
	report<<"nRD: number of estimated recruitment deviations by stock"<<endl;
	report<<nRD<<endl;
	
	report<<"lnRD: log recruitment deviations"<<endl; 
	report<<log(Rec)<<endl;
	
	report<<"D_ini: simulated depletion"<<endl;
	report<<D_ini<<endl;
	
	report<<"D: depletion SSB(last year, last subyear)/SSB0"<<endl;
	report<<D<<endl;
	
	report<<"Dt: depletion SSB(last year, last subyear)/SSB(first year, first subyear)"<<endl;
	report<<Dt<<endl;
	
	report<<"SSBnow: current spawning biomass by stock"<<endl;
	report<<SSBnow<<endl;
	
	//names(LHs)<- c("Catch", "CPUE", "Survey", "L Comp", "SOO", "PSAT",  "Rec Dev", "Mov Pr", "Sel Pr", "SRA pen", "SSB Pr", "Dep Pr",      "MI Pr", "R0 diff","TOT")

	report<<"objC"<<" objCPUE"<<" objI"<<" objCL"<<" objSOO"<<" objPSAT"<<" objRD"<<" objmov"<<" objsel"<<" objSRA"<<" objSSB"<<" objDep"<<" objFmod"<<" objR0"<<" objG"<<" objSOOm"<<" objSOOg"<<endl;
	report<<objC<<" "<<objCPUE<<" "<<objI<<" "<<objCL<<" "<<objSOO<<" "<<objPSAT<<" "<<objRD<<" "<<objmov<<" "<<objsel<<" "<<objSRA*LHw(11)<<" "<<objSSB<<" "<<objDep<<" "<<objFmod<<" "<<objR0<<" "<<objG<<" "<<objSOOm<<" "<<objSOOg<<endl;
		
	report<<"SOOpred: logit-space predictions of stock of origin paired to OMI@SOOobs"<<endl;
	report<<SOOpred<<endl;
	
	report<<"CPUEpred_vec: paired CPUE index predictions (nCPUEobs)"<<endl;
	report<<CPUEpred_vec<<endl;
	
	report<<"Ipred_vec: paired fishery independent index predictions (nIobs)"<<endl;
	report<<Ipred_vec<<endl;
	
	report<<"PSATpred: predictions of electronic tag transitions (known stock of origin) paired to PSAT"<<endl;
	report<<PSATpred<<endl;
	
	report<<"PSAT2pred: predictions of electronic tag transitions (unknown stock of origin) paired to PSAT"<<endl;
	report<<PSAT2pred<<endl;
	
	report<<"Fmod (vector s row then r col): the seasonal / spatial deviation from the MI x q x sel predicted F"<<endl;
	report<<Fmod<<endl;
	
	report<<"FDYt (r,s,y): the extra-master index annual variability in F=qE"<<endl;
	report<<FDYt<<endl;
	
	report<<"SPpred_vec"<<endl;
	report<<SPpred_vec<<endl;
	
  	report<<"datacheck"<<endl;
  	report<<datacheck<<endl;
  	
  }


RUNTIME_SECTION
    maximum_function_evaluations 10000
    convergence_criteria 1.e-6


TOP_OF_MAIN_SECTION
	arrmblsize = 70000000;
	gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
	gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
	gradient_structure::set_MAX_NVAR_OFFSET(5000);
	gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
	

GLOBALS_SECTION
        
        #include <admodel.h>
	#include "stats.cxx"
	#include <fstream>
        ofstream nodesout("nodes.cha");
       	
	
	

