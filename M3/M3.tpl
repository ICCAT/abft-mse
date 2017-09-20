// =========================================================================================================================================== 
//
//                         		           Modifiable Multistock Model (M3)                      
//
//                                	           	        v1.6                                   
//
//                                   		          5th September 2017                           
//                                                                     
//                           		            ICCAT GBYP, Tom Carruthers UBC 
//
// ===========================================================================================================================================
//
//
// -- About M3 --
//
// M3 is a spatial, multi-stock, subyear, statistical catch-at-length stock assessment model. M3 was designed to be used as an operating model 
// that could be fitted to various data to predict spatial stock structure of a multi-stock fishery (specifically Atlantic bluefin tuna). In
// this regard M3 includes several simplifications over a conventional age-structured stock assessment in order to reduce calculations and 
// ensure that the estimation problem is well defined. 
//
// M3 is currently in a developmental (beta) stage. Any comments, bugs or otherwise can be forwarded to t.carruthers@fisheries.ubc.ca. 
//
// M3 was compiled using ADMB 11.5 (64 bit) under Windows 10 using mingw64 
//
//
// -- Acknowledgements --
//
// Model structure and assumptions follow the recommendations of the ICCAT MSE core modelling group.
//
//    ------------------------------------------------------------------------------------------ 
//       ---------------------------- ICCAT CORE MODELLING GROUP ----------------------------
//              Laurie Kell, Antonio Di Natale, Doug Butterworth, Harritz Arrizabalaga, 
//            Yukio Takeuchi, Sylvain Bonhommeau, Toshi Kitakado, Clay Porch, David Die,
//                 Miguel Santos, Paul de Bruyn, Polina Levontin, Richard Hillary  
//          
//                      Previous collaborators: Joe Powers, Campbell Davies 
//    ------------------------------------------------------------------------------------------
//
//
// M3 uses Steve Martell's stats.cxx library (publically available as part of the iSCAM stock assessment package, https://github.com/smartell/iSCAM)
//
//
// -- Some conventions --
//
// 1) Matrices are arranged in order of population, year, season
// 2) Timesteps include subyears and are indexed to year and subyear by yind and sind, respectively
// 3) Stock separation is not explicit in the data except for stock of origin, some PSAT (not PSAT2) and some conventional tags
// 4) Movement by subyear is the movement into an area in that subyear: models runs F(y-1) M(y-1), mov(y), F(y), M(y), mov(y+1),...
// 5) Recapture probabilities are 100% assigned to the release area in first time step after which they are affected by movement e.g. for 
//    a 5 area model (0,1,0,0,0) (0.05,0.7,0.1,0.1,0.5),...
// 6) First subyear of first year is for initialisation and there is no recorded fishing
// 7) Rows in movement matrix with no recorded movements are assigned 1/nareas probability to avoid the vanishing fish problem (which 
//    would occur if all areas were assigned zero probability)
//
//
// -- To do minor -- 
//
// * infleclim is no longer used in the code below - update model writing functions etc
// * Investigate speed improvements from assigning data predictions to vector and using vectorized likelihoods
//
//
// -- To do major --
//
// * Dynamic age-length key based on linear interpolation between discrete growth groups
// * Conditional stock of origin by PSAT tag track according to SOO data
// * Predicted stock of origin by fleet type
// * Spawning stock is SSB only in spawning area
// 
//
// -- Change log since v0.16 (ie post CMG Monterey Jan 2016) --   
//
// * Fractional movement model (26-1-2016)
// * Movement by age class (28-1-2016)
// * Spool-up with equilibrium Z over first nZeq years (29-1-2016)
// * Lag Rec-SSB by one year (29-1-2016)
// * Plus group calculation (29-1-2016)
// * Recapture probability by age (1-2-2016)
// * Fixed bug with spool up that ignored age structures greater than 1 (5-2-2016)
// * Added pass-through of equilibrium F constants nZeq, nydist, nyeq (report section)
// 
//
// -- Change log since v0.18 (ie post ICCAT data prep July 2016) --   
//
// * Sim testing of age-based movement (reparameterization and tweaking of initialization)
// * Tweaking of plus group initialization
// * Canspawn has been added which is a matrix of 1s and 0s denoting if a stock spawns or not in each area
// * Model predicted SSB depletion is now calculated and outputted in verbose=T to examine fit to simulations
// * Reparameterization of selectivity estimation (current ogives could produce maximum selectivity at age much less than 1 and were therefore confounded with catchability)
// * Likelihoods reweighted for realistic quantity of various data (following data prep)
//
//
// -- Change log since v1.0 (post ICCAT species group meeting)
// 
// * MCMC outputs
// * Alternative index pass through
// * Pass through of OM naming
// * Historical spool nyeq up must be less than na at the moment (due to allocation of recruitment deviations to bottom triangle of N matrix) 
//
//
// -- Change log since v1.1 (post ICCAT species group meeting)
// 
// * Re parameterizationa of Thompson selectivity
//
//
// -- Change log since v1.2 (post ICCAT core modelling group Nov 2016)
// 
// * Added historical stock reduction analysis approach, penalty for U > 90% added to likelihood function
// * Multiple fleet indices added
// * Added absolute abundance prior
//
// -- Change log since v1.5 (post ICCAT bluefin assessment meeting July 2017)
//
// * Added prior to mean SSB
// * Added prior for SSB trajectory (SSBinc, SSBy, SSBincstock)
//
//
// -- Dev notes --
//
// * inflect lim was commented out.. maybe for real data... hmm
// * make sure pass through works for new movement estimation and nZeq etc
// * adjustment factor was used for reconstructing historical eastern catches - validate
// * remove 5th column from Eobs dataset (and adjust code)
// * add absolute abundance prior
// * Historical SRA years current are fixed to have the same spawning season, this might not be the case in other applications - recoding needed
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
	init_vector RDblock(1,ny);             // Recruitment blocks for simple recruitment estimation
	init_int nRD;                          // Number of estimated recruitment dev parameters per stock                   
	
	
	// -- Growth --
	init_4darray ALK(1,np,1,ny,1,na,1,nl); // Age-Length Key by stock
	init_vector lwa(1,np);                 // Length-weight conversion factor a w=al^b
		
	init_vector lwb(1,np);                 // Length-weight conversion factor b w=al^b
	init_3darray len_age(1,ny,1,na,1,np);  // Length at age (pass through for independently calculating biomass from .rep file)
	init_3darray wt_age(1,ny,1,na,1,np);   // Weight at age (pass through for independently calculating biomass from .rep file)
		
	// -- Maturity --
	init_matrix Fec(1,np,1,na);            // SSB by age by stock (previously a fecundity calculation)
	//init_vector steep(1,np);               // Steepness of the stock-recruitment curve
	
	// -- Spawning --
	init_vector spawns(1,np);              // The subyear in which the stock spawns
	init_matrix canspawn(1,np,1,nr);       // The areas in which spawning occurs (1) or not (0) for each stock
	
	// -- Natural Mortality rate --
	init_matrix Ma(1,np,1,na);             // Instantaneous natural mortality rate at age by stock
		
	// -- Fishery data --
	init_int nCobs;                        // Number of catch observations by year, subyear, area, fleet 
	init_matrix Cobs(1,nCobs,1,5);         // Catch observations
	
	init_int nCPUEq;                       // Number of estimated catchabilities for CPUE indices
	init_int nCPUEobs;                     // Number of rows of CPUE data 
	init_matrix CPUEobs(1,nCPUEobs,1,7);   // A table of CPUE index observations by year, subyear, area, fleet, index No. (nE)
		
	init_int nE;                           // Number of effort series used / partial F's
	init_int nEobs;                        // Number of effort observations by year, subyear, area, fleet, index No. (nE) 
	init_matrix Eobs(1,nEobs,1,6);         // Observed effort observations
	
	init_int nCLobs;                       // Number of Catch-at-length observations
	init_matrix CLobs(1,nCLobs,1,6);       // Catch-at-length observations by year, subyear, area, fleet, length category, N
	
	init_4darray HCobs(1,nHy,1,ns,1,na,1,nr); // Historical catch observations Hy, subyear, area, fleet
	
	init_3darray RAI(1,nr,1,ns,1,ny);      // The real relative abundance index from the simulation / cpue index (this is pass-through data so that output fit can be summarized without other files)
	
	// -- Fishery independent indices --
	init_int nI;                           // Number of fishery independent indices
	init_int nIobs;                        // Number of relative abundance index observations by timestep, area 
	init_matrix Iobs(1,nIobs,1,7);         // Observed relative index observations year, subyear, area, stock, index number (nI), type (1) biomass (2) SSB, index
	
	// -- PSAT tags --
	init_int nPSAT;                        // Number of PSAT recapture events (known SOO)
	init_matrix PSAT(1,nPSAT,1,7);         // PSAT release-recapture events from stock, age class, subyear, timestep, from-area, to-area, N
	init_int nPSAT2;                       // Number of PSAT recapture events (not known SOO)
	init_matrix PSAT2(1,nPSAT2,1,5+np);    // PSAT2 release-recapture events from age class, subyear, timestep, from-area, to-area, prob p=1, p=2...
	
	// -- Conventional tags --
	init_int nTag;                         // Number of conventional tag observations
	init_matrix Tag(1,nTag,1,10);          // Tag release-recpature from Year/subyear/area/age to Year/subyear/area/fleet/age, N 
		
	// -- Stock of origin --
	init_int nSOOobs;                      // Number of stock of origin observations by stock, age, year, subyear, area, fleet, N
	init_matrix SOOobs(1,nSOOobs,1,6);     // Stock of origin data
	
	
	// -- Selectivity controls --
	init_int nsel;                         // Number of estimated selectivities
	init_ivector seltype(1,nsel);          // 2: logistic, 3: Thompson dome-shaped
	init_vector selind(1,nf);              // Which selectivity is assigned to each fleet
	init_vector ratiolim(1,2);             // Limits on logistic slope parameter relative to inflection point
	init_vector infleclim(1,2);            // Limits on modal selectivity 
		
	// -- Movement estimation --
	init_int nMP;                          // Number of movement parameters estimated
	init_int nma;                          // Number of movement age-classes
	init_matrix ma(1,np,1,na);             // Movement age classes by age for each stock
	init_int nmovind;                      // Number of non-residency paramters estimated              
	init_matrix movind(1,nmovind,1,5);     // Index of estimable movements (stock, age class, subyear, area from, area to)
	init_int nmov1;                        // Number of unestimated zero (logspace) movements
	init_matrix mov1(1,nmov1,1,5);         // Index of first non-estimated movement parameter (fixed to zero)
	init_int movtype;                      // 1: gravity (nr), 2: Markov ((nr-1) x nr), 3: fractional (nr-1, no viscosity)
	
	// -- Indexing ---
	init_matrix Ilencat(1,5,1,np);          // the upper and lower bounds of length categories for the 5 vulnerability schedules
	
	// -- Observation errors --
	init_vector CobsCV(1,nf);              // Catch observation error   
	init_vector CPUEobsCV(1,nCPUEq);       // CPUE observation error
	init_vector IobsCV(1,nI);              // Fishery independent index obs error (only if complexF = 1)
		
	// -- Priors --
	init_number RDCV;                      // Recruitment deviation penalty	
	init_vector SSBprior(1,np);            // SSBnow prior
	init_number SSBCV;                     // SSBnow prior CV
	init_int SSBfit;                       // Type of SSB fitting: 1-SSB0 2-SSBnow
	init_number SSBinc;                    // SSB[SSBy[2]]/SSB[SSBy[1]]
	init_vector SSBy(1,2);                 // Years for SSBinc calculation
	init_number SSBincstock;               // Stock for SSBinc calculation
	init_number FCV;                       // F prior 0.2
	init_number movCV;                     // Movement prior 1.
	init_number selCV;                     // Selectivity prior 0.9
	init_number SSBincCV;                  // SSB ratio CV 0.01
	
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
	init_int debug;                        // 1 = run with initial values
	init_int verbose;                      // 1 = print handy text
	init_number datacheck;                 // Validates length of dat file read, 991199
	
	// -- Integer definitions --
	int mi; 			       // Movement index used only in the gravity model to keep track of where the viscosity parameters are
	int nodemax;                           // Temporary integer for use in defining nodes (MCMC)
	int j;                                 // Index location for allocating variables to node (MCMC)
	
PARAMETER_SECTION
		
	// -- Estimated parameters --
	//init_bounded_vector lnR0(1,np,11.,17.,1);               // Unfished recruitment
	init_bounded_number lnmuRT(11.5,16.5,1);                  // Total historical mean recruitment 
	init_number lnPrat(1);                                    // East - west fraction
	init_bounded_vector lnHR1(1,np,-2,2,1);                   // Historical mean recruitment 
	init_bounded_vector lnHR2(1,np,-2,2,1);                   // Historical mean recruitment 
	init_bounded_matrix selpar(1,nsel,1,seltype,-3.,3.,1);    // Selectivity parameters
	init_bounded_dev_vector lnRD1(1,nRD,-6.,6.,1);
	init_bounded_dev_vector lnRD2(1,nRD,-6.,6.,1);
	init_bounded_vector movest(1,nMP,-8.,8.,1);               // Movement parameters
	init_bounded_vector lnqE(1,nE,-10.,1.,1);                 // q estimates for E fleets
        init_bounded_vector lnqI(1,nI,-2.3,2.3,1);                // q estimates for Fish. Ind. indices
        init_bounded_vector lnqCPUE(1,nCPUEq,-6.,4.,1);           // q estimates for Fish. Dep. indices
	init_bounded_dev_vector Fmod(1,ns*nr,-10.,10.,1);         // F modifier by season and area
	
	LOCAL_CALCS
	  nodemax = np+sum(seltype)+np*nRD+nMP+nCPUEq+nI;
	  //cout<<Ilencat<<endl;
	 // exit(1);
	END_CALCS
	
	vector nodes(1,nodemax);                    // Parameter values stored in nodes vector for mcmc output
	
	// -- Objective function values --
	objective_function_value objG;              // Global objective function value
        number objC;                                // Catch observations
        number objCPUE;                             // Standardized cpue indices 
        number objI;                                // Fishery independent indices
        number objCL;                               // Length composition
        number objSOO;                              // Stock of origin
        number objRD;                               // Recruitment deviations
        number objmov;                              // Priors on movement
        number objsel;                              // Priors on selectivity
        number objPSAT;                             // PSAT tags with certain stock of origin
        number objPSAT2;                            // PSAT tags with uncertain stock of origin
        number objSRA;                              // The penalty for F exceeding Fmax 
        number objSSB;                              // The prior on current SSB
        number objFmod;                             // The Fmod prior
        number objRat;                              // The SSB ratio prior

        // -- Transitions --
        5darray N(1,np,1,ny,1,ns,1,na,1,nr);        // Stock numbers
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
        3darray B(1,ny,1,ns,1,nr);                  // Biomass
        vector lnmuR(1,np);
        vector SSB0(1,np);                          // Unfished spawning stock biomass
        vector D(1,np);                             // Stock depletion SSB now / SSB0
        vector Dt(1,np);                            // Stock depletion SSB now / first assessment year
        vector SSBnow(1,np);                        // SSB now
        vector SSB_EW(1,2);			    // Temporary East- West division 5-10, 1-4 areas
      
        matrix RD(1,np,1,nRD);                      // Recruitment deviations from historical average
        matrix Rec(1,np,1,ny);                      // Recruitment 
	vector muR(1,np);                           // Historical recruitment
	5darray CTL(1,np,1,ny,1,ns,1,nr,1,nl);      // Catch at length
	5darray CTA(1,np,1,ny,1,ns,1,nr,1,na);      // Catch at age
        matrix Btrend(1,np,1,ny);                   // Total biomass trend
        matrix meanF(1,np,1,ny);                    // Average F-at-age 
        
        // -- Exploitation rates --
        vector F(1,nF);                                // Estimated fishing mortality rate
        5darray FAT(1,np,1,ny,1,ns,1,nr,1,na);         // Population level F at Age Total
        4darray hFAT(1,nHy,1,ns,1,na,1,nr);            // Historical F at Age (don't need disagg by pop as SRA using catch at age obs)
        5darray hZ(1,np,1,nHy,1,ns,1,na,1,nr);         // Historical Z at Age (don't need disagg by pop as SRA using catch at age obs)
       
        5darray FL(1,ny,1,ns,1,nr,1,nf,1,nl);          // Fishing mortality rate at length
        4darray FT(1,ny,1,ns,1,nr,1,nl);               // Total fishing mortality rate at length all fleets
        5darray Z(1,np,1,ny,1,ns,1,na,1,nr);           // Total mortality rate at age
        vector qCPUE(1,nCPUEq);                        // Catchability of fishery CPUE indices CPUE=qVB
        vector qI(1,nI);                               // Catchability of fishery independent indices (1) I=q*B  (2) I=q*SSB                  
        vector qE(1,nE);                               // Catchability F=qE
        4darray Ipred(1,ny,1,ns,1,nr,1,np);            // Predicted fishery independent index
        
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
        
        // -- Temporary arrays --
        3darray stemp(1,np,1,ns,1,nr);                 // Temporary season index
        vector sind(1,nRPT);                           // Temporary season index for calculation of recapture probabilities
        vector NLtemp(1,nl);                           // Stores a temporary vector of numbers at length (efficiency)
        number Ntemp;                                  // A temporarly record of stock numbers (SRA)
        vector tempR(1,nr);
        
        // -- Observations --
        5darray CWpred(1,np,1,ny,1,ns,1,nr,1,nf);      // Catches by fleet (weight) and stock
        4darray CWtotpred(1,ny,1,ns,1,nr,1,nf);        // Total catches by weight
        4darray CNpred(1,np,1,ny,1,ns,1,nr);           // Catches by all fleets (numbers) and stock
        6darray CLpred(1,np,1,ny,1,ns,1,nr,1,nf,1,nl); // Catches by fleet (weight) and stock
        5darray CLtotpred(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet
        5darray CLtotfrac(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet fraction
       	
       	sdreport_number temp;                          // Requirement of mceval for producing posterior estimates of model parameters/variables                   


PROCEDURE_SECTION
	
	if(debug)cout<<"datacheck: "<<datacheck<<endl; // Were the data the correct length?
	
	calcSurvival();                  // Calculate survival
		
	calcMovement();                  // Calculate movement
         
        calcSelectivities();             // Calculate selectivities
	
	assignPars();                    // Assigns estimates of R0, F, iRD, RD, qCPUE, qI

        if(debug)cout<<"Pars assigned"<<endl; 
	
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
	
	muR(1)=mfexp(lnmuRT)*mfexp(2.1+lnPrat)/(1+mfexp(2.1+lnPrat));              // Initializes at 8 times larger in the east 
	muR(2)=mfexp(lnmuRT)*mfexp(-(2.1+lnPrat))/(1+mfexp(-(2.1+lnPrat)));              // Initializes at 8 times larger in the east 
	lnmuR=log(muR);	
	//muR=mfexp(lnmuR);                   // Assign historical recruitment
	qCPUE=mfexp(lnqCPUE);               // Assign catchability for CPUE indices
	qI=mfexp(lnqI);                     // Assign fishery independent catchabilities
	qE=mfexp(lnqE);                     // Assign catchability of partial F series' 
		
	RD(1)=mfexp(lnRD1);
	RD(2)=mfexp(lnRD2);
	
	for(int pp=1;pp<=np;pp++){        // Loop over stocks
	   
	  for(int yy=1;yy<=ny;yy++){      // Loop over years
	    
	    int ry = RDblock(yy);         // Find the correct reference block for this year
	     
	    Rec(pp,yy)=RD(pp,ry)*muR(pp);
	      
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



FUNCTION calcSurvival
  {
	// -- Calculate survival --
	for(int pp=1;pp<=np;pp++){                   // Loop over stocks      
	 
	  surv(pp,1)=1.;                             // Survival to age 1 is 100%
	  
	  for(int aa=1;aa<=na;aa++){             // Loop over age classes
	    
	    surv(pp,aa)=exp(-sum(Ma(pp)(1,aa-1)));   // Calculate survivial
	  
	  }                                          // End of age classes
	
	  //surv(pp,na)*=exp(-Ma(pp,na))/(1-exp(-Ma(pp,na))); // final plus group survival is multiplied by the indefinite intergral  
	
	}                                            // End of stocks
	
	if(debug)cout<<"--- Finished calcSurvival ---"<<endl;
  }

 

FUNCTION calcMovement
  {
  		
	//  -- Movement modelling -----------
	for(int pp=1;pp<=np;pp++){                 // Loop over stocks
	  
	  for(int ss=1;ss<=ns;ss++){               // Loop over subyears
	    
	    for(int aa=1;aa<=nma;aa++){ 
	      
	      movcalc(pp)(ss)(aa)=-10.;            // Set all movements to be unlikely (logit space)
	  
	    }
	  
	  }                                        // End of subyears
	
	}                                          // End of stock
	
	switch(movtype){                           // What type of movement model?
	
	  case 1:                                  // -- Gravity model ---------------------------------------------------------------------
	    
	    for(int mm=1;mm<=nmov1;mm++){          // Set the first possible movement to be fixed at zero
	    	      
	      int pp=mov1(mm,1);                   // Stock
	      int aa=mov1(mm,2);                   // Movement age class
	      int ss=mov1(mm,3);                   // Subyear
	      int tr=mov1(mm,4);                   // To area
	    	
	      for(int fr=1;fr<=nr;fr++){           // Loop over from-areas
	    	  
	    	movcalc(pp,ss,aa,fr,tr)=0.;        // Set to zero
	    	
	      }                                    // End of from-areas
	    	
	    }                                      // End of first possible movement
	   
	    for(int mm=1;mm<=nmovind;mm++){        // Loop over estimated movement params (first np*ns*nma are residency viscosity parameters)
	      
	      int pp=movind(mm,1);                 // Stock
	      int aa=movind(mm,2);                 // Movement age class
	      int ss=movind(mm,3);                 // Subyear
	      int tr=movind(mm,4);                 // To area
	    
	      for(int fr=1;fr<=nr;fr++){           // Loop over from-areas
	          
	        movcalc(pp,ss,aa,fr,tr)=movest(mm+np*ns*nma); // Assign estimated parameter
	       
	      }                                    // End of from-areas
	      
	    }                                      // End of estimated movements
	      
	    mi=0;
	    
	    for(int pp=1;pp<=np;pp++){                           // Loop over stocks
		        
	      for(int aa=1;aa<=nma;aa++){                        // Loop over age classes
	        
	        for(int ss=1;ss<=ns;ss++){                       // Loop over subyears
	        
	          mi+=1;                                         // Keep track of viscosity parameter number
	          
	          for(int rr=1;rr<=nr;rr++){                     // Loop over areas
	          	          
	            movcalc(pp,ss,aa,rr,rr)+=mfexp(movest(mi)/24);      // Add viscosity
	      
	          }
	        
	        }                                               // End of age class
	         
	      }                                                 // End of subyear
	      
	    }                                                   // End of stock 
	     
	  break;                                   // End of gravity model
	  
	  case 2:                                  // -- Fully prescribed (Markov) movement matrix ------------------------------------------     
	   
	    for(int mm=1;mm<=nmov1;mm++){          // Set the first possible movement to be fixed at zero
	      
	      int pp=mov1(mm,1);                   // Stock
	      int aa=mov1(mm,2);                   // Movement age class
	      int ss=mov1(mm,3);                   // Subyear
	      int fr=mov1(mm,4);                   // From area
	      int tr=mov1(mm,5);                   // To area
	      
	      movcalc(pp,ss,aa,fr,tr)=0.;             // Set to zero
	    
	    }
	     
	    for(int mm=1;mm<=nMP;mm++){            // Assign all other logit space movement parameters to the mov array
	      
	      int pp=movind(mm,1);                 // Stock         
	      int aa=movind(mm,2);                 // Movement age class
	      int ss=movind(mm,3);                 // Subyear
	      int fr=movind(mm,4);                 // From area
	      int tr=movind(mm,5);                 // To area
	      
	      movcalc(pp,ss,aa,fr,tr)=movest(mm);  // Set to estimated parameter
	    
	    }
	    
	  break;                                   // End of fully prescribed (Markov) movement matrix
	
	  case 3:                                  // -- Fractional model (essentially gravity model with no viscosity parameter) -----------
		    
	    for(int mm=1;mm<=nmov1;mm++){          // Set the first possible movement to be fixed at zero
		    	      
	      int pp=mov1(mm,1);                   // Stock
	      int aa=mov1(mm,2);                   // Movement age class
	      int ss=mov1(mm,3);                   // Subyear
	      int tr=mov1(mm,4);                   // To area
		    	
	      for(int fr =1;fr<=nr;fr++){          // Loop over from-areas
		    	  
		movcalc(pp,ss,aa,fr,tr)=0.;           // Set to zero
		    	
	      }                                    // End of from-areas
		    	
	    }                                      // End of first possible movement 
		   
	    for(int mm=1;mm<=nmovind;mm++){        // Other possible movements are set to estimated parameters
		      
	      int pp=movind(mm,1);                 // Stock
	      int aa=movind(mm,2);                 // Movement age class
	      int ss=movind(mm,3);                 // Subyear
	      int tr=movind(mm,4);                 // To area
		    
	      for(int fr =1;fr<=nr;fr++){          // Loop over from-areas 
		        
		movcalc(pp,ss,aa,fr,tr)=movest(mm);   // Assign estimated parameter
		      
	      }                                    // End of from-areas
	      
	    }                                      // End of possible movement 
	     
	  break;                                   // End of fractional movement model 
	
	}                                          // End of types of movement models case
	
	
	// -- Logit transformation ----------------------------------------------
	
	movm=exp(movcalc);                         // Make positive
	
	for(int pp=1;pp<=np;pp++){                 // Stock       
	  
	  for(int ss=1;ss<=ns;ss++){               // Subyear
	    
	    for(int aa=1;aa<=nma;aa++){            // Movement age class
	    
	      for(int fr = 1; fr<=nr;fr++){        // From area
	      
	        movm(pp)(ss)(aa)(fr)/=sum(movm(pp)(ss)(aa)(fr));  // Normalize to sum to 1 (inv logit)
            
              }                                    // End of area
            
            }					   // End of movement age class   	
          
          }                                        // End of subyear
        
        }                                          // End of stock                                
  	
	
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
	     
	      spar(2)=ml(nl)*(0.05+0.85*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));        // Inflection point (2) as a fraction of largest length I(0.1|0.8)
	      spar(1)=ml(nl)*(0.005+0.11*(mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)))));    // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
	      
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	      
	        msel(ss,ll)=1/(1+mfexp((spar(2)-ml(ll))/spar(1)));                         // Logistic selectivity function
	         
	      } 
	      
	      msel(ss)/=max(msel(ss)); // Need to normalize at least one index to max 1 or face counfounding with q
	      // End of length classes
	    
	    break;                                                                         // End of logistic selectivity
		   
            case 3: // Double normal selectivity
	      
	      spar(1)=ml(nl)*(0.1+0.8*mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)))); // Max selectivity bounded between 5 and 95 percent of length range
	      spar(2)=spar(1)*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2)));       // Lower sd (divided by 4 just to start at a reasonable guess)
	      spar(3)=ml(nl)*mfexp(selpar(ss,3));         // Upper sd
	     
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	        
	        if(ml(ll)<spar(1)){
	            
	          msel(ss,ll)=pow(2.0,-(ml(ll)-spar(1))/spar(2)*(ml(ll)-spar(1))/spar(2)); 
	        
	        }else{
	        
	          msel(ss,ll)=pow(2.0,-(ml(ll)-spar(1))/spar(3)*(ml(ll)-spar(1))/spar(3));
	        
	        }
	      
	      }									           // End of length classes
	      
	      
	      msel(ss)/=max(msel(ss));                                                     // Need to normalize to max 1 or face counfounding with q
	          
	    break;
	    
	    /*case 4: // Thompson dome-shaped selectivity
	      
	      spar(1)=0.01+pow((selpar(ss,1)+2.)/5,3.);            // Dome-shape parameter 
	      spar(2)=0.01+0.01*pow((selpar(ss,2)+2.),3.);         // Precision as the ratio of the inflection point
	      spar(3)=ml(nl)*(0.1+0.8*(selpar(ss,3)+2)/4);         // Inflection point as a fraction of largest length
	     
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	        
	        msel(ss,ll)=(1/(1-spar(1)))*pow(
	        ((1-spar(1))/spar(1)),spar(1)) * mfexp(spar(2)*spar(1)*(spar(3)-ml(ll)))/(1+mfexp(spar(2)*(spar(3)-ml(ll))));	// Thompson selectivity function	
	      
	      }									           // End of length classes
	      
	      msel(ss)/=max(msel(ss));                                                     // Need to normalize to max 1 or face counfounding with q
	      
	    break;*/									   // End of Thompson selectivity
		      
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
	     
	  FL(yy)(ss)(rr)(ff)= sel(ff)*Eobs(i,6)*qE(ff)*mfexp(Fmod(ll));  // Calculate fishing mortality rate at length 
	    
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
	hZ.initialize();               // Historical Z = 0
	SSB.initialize();              // Spawning stock biomass = 0
	hSSB.initialize();             // historical Spawning stock biomass = 0
	SSBdist.initialize();          // Spawning distribution = 0
	SSB0.initialize();             // Unfished spawning stock biomass = 0
	SSB_EW.initialize();           // SSB by area initialized at 0
	//SSBpR.initialize();          // SSB per recruit = 0
	CWtotpred.initialize();        // Total catch (weight) = 0
	CWpred.initialize();           // Catch (weight) = 0
	CNpred.initialize();           // Catch (numbers) = 0
	CLpred.initialize();           // Catch (length class) = 0
	CLtotpred.initialize();        // Total catch (length class) = 0
	CLtotfrac.initialize();        // Total catch fractions (length class) = 0
	CTA.initialize();              // Temporary catch at age = 0
	VB.initialize();               // Vulnerable biomass = 0
	VBi.initialize();              // Vulnerable biomass index = 0
	VL.initialize();               // Vulnerable length
	D.initialize();                // Spawning Stock depletion = 0
	Btrend.initialize();           // Trend in biomass = 0
	objSRA.initialize();           // Penalty for historical F's over 0.9 = 0
	
	for(int pp=1;pp<=np;pp++){
	
	  SSB0(pp)=muR(pp)*mfexp(lnHR1(pp))*sum(elem_prod(surv(pp),Fec(pp)));     // Unfished Spawning Stock Biomass
	  SSB0(pp)+=muR(pp)*mfexp(lnHR1(pp))*Fec(pp,na)*surv(pp,na)*mfexp(-Ma(pp,na))/(1-mfexp(-Ma(pp,na))); // indefinite integral of surv added to get plus group SSB0
	
	}
	
	//cout<<Fec(1)<<endl;
	//cout<<muR<<endl;
	//cout<<lnHR1<<endl;
	//cout<<surv(1)<<endl;
	//cout<<sum(elem_prod(surv(1),Fec(1)))<<endl;
	//cout<<SSB0<<endl;
	//exit(1);
	
	
	for(int pp=1;pp<=np;pp++){                              // Loop over stocks
	
	  // -- Initial guess at stock distribution -------------------------------------------
	  
	  stemp(pp)(ns)=1./nr;                                  // Distribute a fish evenly over areas                     
	  
	  for(int ii=1;ii<=nydist;ii++){                        // Loop over nydist initial spatial distribution (stabilizes within 3 usually)
	    
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
	        
	        N(pp,1,ss,aa,rr)=mfexp(lnHR1(pp))*muR(pp)*surv(pp,aa)*stemp(pp,ss,rr);    // Stock numbers are spatial distribution multiplied by Survival and R0
	        hSSB(pp,1,ss)+=N(pp,1,ss,aa,rr)*Fec(pp,aa);           // Historical spawning stock biomass
	        
	      }                                                       // end of ages
	      
	      N(pp,1,ss,na,rr)+=mfexp(lnHR1(pp))*muR(pp)*surv(pp,na)*stemp(pp,ss,rr)*mfexp(-Ma(pp,na))/(1-mfexp(-Ma(pp,na))); // Indefinite intergral for plus group
	      hSSB(pp,1,ss)+=mfexp(lnHR1(pp))*muR(pp)*surv(pp,na)*stemp(pp,ss,rr)*mfexp(-Ma(pp,na))/(1-mfexp(-Ma(pp,na)))*Fec(pp,na);
	      
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
		    hFAT(yy,ss-1,aa,rr)=(-log(posfun(1-(HCobs(yy,ss-1,aa,rr)/(Ntemp+tiny)),1-0.9,pen)));            // F estimate based on half of M
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
		 
		  if(yy<(nHy-10)){
		    N(pp)(1)(ss)(1)=spawnr(pp)*mfexp(lnHR1(pp))*muR(pp); //(0.8*R0(pp)*steep(pp)*sum(SSBdist(pp)))/(0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * sum(SSBdist(pp))); // Recruitment
		  }
		  else{
		    N(pp)(1)(ss)(1)=spawnr(pp)*mfexp(lnHR2(pp))*muR(pp); //(0.8*R0(pp)*steep(pp)*sum(SSBdist(pp)))/(0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * sum(SSBdist(pp))); // Recruitment
		  }
		 
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
	      
	      B(1,ss,rr)+=sum(elem_prod(NLtemp,wl(pp)));      // Biomass summed over stocks	
	      Btrend(pp)(1)+=B(1,ss,rr)/ns;                   // Record first year of biomass trend (debugging)
	      
	      for(int ff=1;ff<=nf;ff++){                      // Loop over fleet types
						  
		VB(1,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));   // Vulnerable biomass summed over stocks
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
	      
	      CNpred(pp,1,ss,rr)=sum(CTL(pp)(1)(ss)(rr));                                                // total predicted catches (numbers) of all fleets

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
	            
	            if(rr<5){
	              SSB_EW(2)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa);
	            }
	            if(rr>4){
	              SSB_EW(1)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa);
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
	          
                  N(pp)(yy)(ss)(1)=spawnr(pp)*Rec(pp,yy);//*(0.8*R0(pp)*steep(pp)*sum(SSBdist(pp)))/(0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * sum(SSBdist(pp))); // Recruitment (SSB lag 1 year)
	       	 	       	  
	       	  for(int aa=1;aa<=na;aa++){  // Loop over age classes
		                
		     for(int rr=1;rr<=nr;rr++){ // Loop over areas
		  		      
		  	SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
		  	
		  	if(rr<5){
			  SSB_EW(2)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa);
			}
			if(rr>4){
			  SSB_EW(1)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa);
	                }
		  	
		     }                          // End of areas
		  		    
		  }                            // End of age classes
	       	  
	       	  
                    
	        }else{                          // Not a spawning season ----------------------------------------------------------------
	        
	          for(int aa=1;aa<=na;aa++){    // Loop over age classes
		    
		    N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss)(aa); // M then move	        
		    
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
		    
		      SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
		    
		      if(rr<5){
		    	SSB_EW(2)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa);
		      }
		      if(rr>4){
		    	SSB_EW(1)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa);
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
	       
		B(yy,ss,rr)+=sum(elem_prod(NLtemp,wl(pp)));           // Biomass summed over stocks				  
	        Btrend(pp)(yy)+=B(yy,ss,rr)/ns;                       // Record biomass trend (debugging)
	     
		for(int ff=1;ff<=nf;ff++){                            // Loop over fleets
		   
		  VB(yy,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));    // Vulnerable biomass summed over stocks
		  VL(yy)(ss)(rr)(ff)+=elem_prod(NLtemp,sel(ff)); 
		  
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
		
		CNpred(pp,yy,ss,rr)=sum(CTL(pp)(yy)(ss)(rr));        // Total predicted catch numbers (all fleets)
			    
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
  	
  	for(int pp=1;pp<=np;pp++){                // Stocks
  	  
  	  for(int ss=1; ss<=ns;ss++){             // Subyears
  	     
  	    int s2 = RPTind(ss,2);                // Retrieve the correct subyear for timestep tt and release subyear ss
  	    
  	    for(int aa=1; aa<=nma; aa++){         // loop over movement age classes
  	      
  	      for(int rr=1;rr<=nr;rr++){            // Regions
  	      
  	        RecapP(pp)(ss)(aa)(1)(rr)=0.;           // Set the area vector to all zeros
  	        RecapP(pp,ss,aa,1,rr,rr)=1.;           // Recapture probability is 100% for same area in the same timestep
  	      
  	        //for(int tt=2;tt<=nRPT;tt++){      // Timesteps (incremental subyears)
  	                                          
  	          RecapP(pp)(ss)(aa)(2)(rr)=RecapP(pp)(ss)(aa)(1)(rr)*mov(pp)(s2)(aa);   // Recalculate recapture probability in next timestep given movement
  	     
  	        //} // timestep (nRPt)
  	      }
  	      
  	    }                                    // End of areas
  	  
  	  }                                      // End of subyears
  	
  	}                                        // End of stocks
    
        if(debug)cout<<"--- Finished calcRecaptureProb ---"<<endl;
    
  }

  

FUNCTION calcObjective
  {
	objG.initialize();                      // Global 
	objC.initialize();                      // Catch data
	objCPUE.initialize();                   // Standardized cpue indices 
	objI.initialize();                      // Fishery independent indices
	objCL.initialize();                     // Length composition
	objSOO.initialize();                    // Stock of origin
	objRD.initialize();                     // Recruitment deviations
	objmov.initialize();                    // Priors on movement
	objsel.initialize();			// Priors on selectivity
	objPSAT.initialize();                   // PSAT tags certain stock of origin
	objPSAT2.initialize();                  // PSAT tags w uncertain stock of origin
	objSSB.initialize();                    // SSB prior
	objFmod.initialize();                   // Fmod prior
	objRat.initialize();                    // Ratio on unfished spawning stock size
	
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
  	  
  	  LHtemp=dnorm(log(CWtotpred(yy,ss,rr,ff)+tiny),
  	  log(Cobs(i,5)+tiny),CobsCV(ff)); // Log-normal LHF
  	  //LHtemp=pow(log(CWtotpred(yy,ss,rr,ff)+tiny)-log(Cobs(i,5)+tiny),2);// SSQ test
  	  objC+=LHtemp*LHw(1);                                                           // Weighted likelihood contribution
  	  objG+=LHtemp*LHw(1);  
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
	    	  		    
	        CPUEtemp=VBi(yy,ss,rr,ff)*qCPUE(ii);                                       // Calculate vulnerable biomass
	            	  	    
	    }else{// length category according to Ilencat
	      
	      CPUEtemp=0.;  		   
	      	
	      for(int yy=1;yy<=ny;yy++){
	
	    	CPUEtemp+=sum(VL(yy)(ss)(rr)(ff)(Ilencat(lt,1),Ilencat(lt,2)))/ny; // mean index over time
	    	   
	      }
	      
	      LHtemp=sum(VL(yy)(ss)(rr)(ff)(Ilencat(lt,1),Ilencat(lt,2)))/CPUEtemp; // the prediced index
	      //cout<<LHtemp<<endl;
	      //exit(1);
	      CPUEtemp=qCPUE(ii)*LHtemp;                                       // Calculate vulnerable biomass
	            
	    }
	    
	    LHtemp=dnorm(log(CPUEtemp+tiny),log(CPUEobs(i,6)+tiny),CPUEobsCV(ii));    // Log-normal LHF
	    objCPUE+=LHtemp*LHw(2);                                                   // Weighted likelihood contribution
	    objG+=LHtemp*LHw(2);                                                      // Weighted likelihood contribution
  	  
	  }
	
	  if(debug)cout<<"---  * Finished CPUE LHF ---"<<endl;
	   
	}
	
	// -- Fishery independent indices --
	
	dvariable Itemp;              // Create a dummy variable cor calculating a normalilized (mean 1) index 
	
	for(int i=1; i<=nIobs;i++){   // Loop over fishery - independent indices
	
	  int yy=Iobs(i,1);   // Year
	  int ss=Iobs(i,2);   // Subyear
	  int rr=Iobs(i,3);   // Region
	  int pp=Iobs(i,4);   // stock
	  int ii=Iobs(i,5);   // q index (often stock for SSB types)
	  int tt=Iobs(i,6);   // Type
	  
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" q index ="<<ii<<" type="<<tt<<endl;
	
	  switch(tt){
	  		    
	    case 1:  // Biomass
	  	      
	      Ipred(yy,ss,rr,pp)= B(yy,ss,rr)*qI(ii);     // Predicted index
	  	    
	      break;
	  		   
	    case 2:  // SSB
	       
	      Ipred(yy,ss,rr,pp)=qI(ii)*(SSB(pp,yy,ss)/mean(extract_row(trans(SSB(pp)),ss)));   // Predicted index normalized to mean 1
	         	      
	      break;
	     
	    case 3:  // Biomass first two stocks
	     
	      Ipred(yy,ss,rr,pp)=qI(ii)*(B(yy,1,rr)+B(yy,2,rr))/mean(B);  // Predicted index normalized to mean 1
	      
	      break;
	  		      
	  }
	  
	  LHtemp=dnorm(log(Ipred(yy,ss,rr,pp)+tiny),log(Iobs(i,7)+tiny),IobsCV(ii)); // Log-normal LHF
	  objI+=LHtemp*LHw(3);                                          // Weighted likelihood contribution
	  objG+=LHtemp*LHw(3);                                          // Weighted likelihood contribution
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
	  CLn(ff)+=1;
	  CLvar(ff)+=pow(sqrt(CLprop(i)+tiny)-sqrt(CLobs(i,6)+tiny),2.);
	  
	}
	
	for(int ff=1;ff<=nf;ff++){
	
	  CLsd(ff)=sqrt(CLvar(ff)/CLn(ff));
	  
	}
		
	for(int i=1;i<=nCLobs;i++){  // Loop over catch at length observations
	
	  int yy=CLobs(i,1);   // Year
	  int ss=CLobs(i,2);   // Subyear
	  int rr=CLobs(i,3);   // Region
	  int ff=CLobs(i,4);   // Fleet type
	  int ll=CLobs(i,5);   // Length class
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" l="<<ll<<" CLobs="<<CLobs(i,6)<<" CLpred="<<CLtotpred(yy,ss,rr,ff,ll)<<endl;
	  //LHtemp=dnorm(log(CLtotpred(yy)(ss)(rr)(ff)(ll)),log(CLobs(i,6)),1);
	  //LHtemp=(-CLobs(i,6)*log((CLtotfrac(yy,ss,rr,ff,ll)+tiny)/CLobs(i,6))); // Multinomial LHF (A.Punt fix for stability)
	  //LHtemp=(-CLobs(i,6)*log((CLtotfrac(yy,ss,rr,ff,ll)+tiny)));
	  LHtemp=(-CLobs(i,6)*log(CLprop(i)+tiny));
	  //LHtemp=log(CLsd(ff))+pow(sqrt(CLobs(i,6)+tiny)-sqrt(CLprop(i)+tiny),2)/(2.*CLsd(ff)); // new log(p) formulation
	  objCL+=LHtemp*LHw(4);                                     // Weighted likelihood contribution
	  objG+=LHtemp*LHw(4);                                      // Weighted likelihood contribution
	}
	
	if(debug)cout<<"---  * Finished length composition LHF ---"<<endl;
	
	
	// -- Stock of origin data -- 
	
	dvariable SOOpred;   // Calculated predicted fraction of each stock of origin (there aren't that many stock of origin observations so it involves less computation to do this as needed)
	dvariable SOOtot;    // For storing the sum of SOO numbers
	SOOtot.initialize(); // SSOtot = 0
	
	for(int i=1;i<=nSOOobs;i++){ // Loop over stock of origin observations
	  
	  int pp=SOOobs(i,1);   // Population
	  int aa=SOOobs(i,2);   // Age class
	  int yy=SOOobs(i,3);   // Year
	  int ss=SOOobs(i,4);   // Subyear
	  int rr=SOOobs(i,5);   // Region
	 
	  SOOtot=0.;            // Reset sum
	  
	  for(int pp=1;pp<=np;pp++){
	  
	    SOOtot+=CNpred(pp,yy,ss,rr);
	    
	  }
	  
	  SOOpred=CNpred(pp,yy,ss,rr)/(SOOtot+tiny); // Calculate predicted fraction
	  
	  LHtemp=(-SOOobs(i,6)*log(SOOpred+tiny));   // Multinomial LHF
	  objSOO+=LHtemp*LHw(5);                     // Weighted likelihood contribution
	  objG+=LHtemp*LHw(5);                       // Weighted likelihood contribution
		
	}
	
	if(debug)cout<<"---  * Finished SOO LHF ---"<<endl;
	
	
	// -- PSAT tagging --
	
	for(int i=1;i<=nPSAT;i++){ // Tags with certain stock of origin
	
	  int pp=PSAT(i,1);   // Population 
	  int aa=PSAT(i,2);   // Movement age class
	  int ss=PSAT(i,3);   // Subyear
	  int tt=PSAT(i,4);   // Time at liberty (subyears)
	  int rr=PSAT(i,5);   // Region from
	  int r2=PSAT(i,6);   // Region to
	  
	  LHtemp=(-PSAT(i,7)*log(RecapP(pp,ss,aa,tt,rr,r2)+tiny)); // Multinomial LHF
	  objPSAT+=LHtemp*LHw(6);                               // Weighted likelihood contribution
	  objG+=LHtemp*LHw(6);                                  // weighted likelihood contribution
	}
	
	if(debug)cout<<"---  * Finished PSAT known SOO LHF ---"<<endl;
	
	
	for(int i=1;i<=nPSAT2;i++){ // Individual tags with uncertain stock of origin
		
          int ss=PSAT2(i,1);   // Subyear
	  int aa=PSAT2(i,2);   // Movement age class
	  int tt=PSAT2(i,3);   // Time at liberty (subyears)
	  int rr=PSAT2(i,4);   // Region from
	  int r2=PSAT2(i,5);   // Region to
	  
	  for(int pp=1;pp<=np;pp++){
	  
	    LHtemp=(-log(RecapP(pp,ss,aa,tt,rr,r2)+tiny)*PSAT2(i,5+pp)); // Multinomial LHF
	    objPSAT2+=LHtemp*LHw(7);                                  // Weighted likelihood contribution
		  
	  }
			
	}
	
	if(debug)cout<<"---  * Finished PSAT unknown SOO LHF ---"<<endl;
	
	// -- Recruitment deviations --
	
	temp=(ny+na)/nRD;
	
	for(int pp=1;pp<=np;pp++){  // Loop over stocks
	  
	  for(int yy=1;yy<=nRD;yy++){ // Loop over years (or blocks of recruitment deviations if complexRD=0)
	    
	    LHtemp=dnorm(log(RD(pp,yy)),0.,RDCV);   // Recruitment deviations
	    
	    objRD+=LHtemp*LHw(8);                // Weighted likelihood contribution
	    objG+=LHtemp*LHw(8);                 // Weighted likelihood contribution
	    
	  }
	  
        }  
	
	for(int pp=1;pp<=np;pp++){  // Loop over stocks
	 
	  LHtemp=dnorm(lnHR1(pp),0.,RDCV/10); 
	  objRD+=LHtemp*LHw(8);
	  objG+=LHtemp*LHw(8);                 // Weighted likelihood contribution
	  LHtemp=dnorm(lnHR2(pp),0.,RDCV/10); 
	  objRD+=LHtemp*LHw(8);
	  objG+=LHtemp*LHw(8);  
	
	}
	
	// -- Movement parameters ---
	
	for(int mm=1;mm<=nMP;mm++){
	     
	  LHtemp=dnorm(movest(mm),0.,movCV);        // Weak(ish) prior 
          objmov+=LHtemp*LHw(9);                  // Weighted likelihood contribution
	  objG+=LHtemp*LHw(9);                    // Weighted likelihood contribution
	  
	}
	
	// -- Selectivity parameters ---
	
	for(int i=1;i<=nsel;i++){ 
	  
	  objsel+=dnorm(selpar(i),0,selCV)*LHw(10);  
	  objG+=dnorm(selpar(i),0,selCV)*LHw(10);   // Prior on selectivity to add numerical stability
	
	}
	
	if(last_phase()){
	
	  objG+=objSRA*LHw(11);                      // Add the posfun penalty for SRA harvest rates over 90%
	
	}
	
	/*cout<<SSB_EW<<endl;
	cout<<((ny-1)*ns)<<endl;
	cout<<SSB_EW/((ny-1)*ns)<<endl;
	cout<<SSBprior<<endl;
	exit(1);*/
	
	
	for(int pp=1;pp<=np;pp++){
	
	  switch(SSBfit){
	  	  		    
	    case 1:  // SSBnow
	  	  	      
	      objSSB+=dnorm(log(SSBnow(pp)+tiny),log(SSBprior(pp)+tiny),SSBCV)*LHw(12);   
	  	  	    
	      break;
	  	  		   
	    case 2:  // SSB0
	  	       
	      objSSB+=dnorm(log(SSB0(pp)+tiny),log(SSBprior(pp)+tiny),SSBCV)*LHw(12);  
	  	         	      
	      break;
	      
	    case 3:  // meanSSB
	    		  		      
	      SSBnow(pp)=SSB_EW(pp)/((ny-1)*ns);         // mean SSB (reusing variable SSBnow)
              objSSB+=dnorm(log(SSBnow(pp)+tiny),log(SSBprior(pp)+tiny),SSBCV)*LHw(12); 
	    	  	         	      
	      break;
	      
	   }
	  
	}
	
	objG+=objSSB*LHw(12);
	
	objRat=dnorm(log(SSB(SSBincstock,SSBy[2],ns)/SSB(SSBincstock,SSBy[1],ns)),log(SSBinc),SSBincCV);
		
	objG+=objRat*LHw(13);
	
	for(int ll=1;ll<=ns*nr;ll++){
	  
	  objFmod+=dnorm(Fmod(ll),0,FCV);
	  
	}
	
	objG+=objFmod*LHw(14);
	
	
	
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
	if(verbose)cout<<"SSB penalty "<<objSSB<<endl;        // Report penalty for current SSB prior
	if(verbose)cout<<"Fmod prior "<<objFmod<<endl;        // Report penalty for Fmod prior
	if(verbose)cout<<"SSB0 ratio prior"<<objRat*50<<endl; // Report penalty for unfished SSB ratio (East / West)
	if(verbose)cout<<"Global objective "<<objG<<endl;     // Report Global objective function
	
  }
	


FUNCTION simsam
  {
        // If working with simulated data do some printing
        cout<<"canspawn = "<<canspawn<<endl;
        cout<<"surv = "<<surv<<endl;
        cout<<"Fec ="<<Fec<<endl;
        //cout<<"muR sim = "<<log(muR_ini)<<endl;                // Simulated R0
        cout<<"muR sam = "<<log(muR)<<endl;                    // Estimated mu r
        cout<<"lnHR1 sam = "<<lnHR1<<endl;                    // Estimated mu r deviant
        cout<<"lnHR2 sam = "<<lnHR2<<endl;                    // Estimated mu r deviant
      
        cout<<"selpar = "<<selpar<<endl;                     // Estimated R0
        for(int ff=1;ff<=nf;ff++){
          //cout<<"sel sim f1= "<<sel_ini(1)<<endl;            // Simulated selectivity fleet 1
          cout<<"sel sam "<<ff<<" "<<sel(ff)<<endl;            // Estimated selectivity fleet 1
        }
        cout<<"RD= "<<RD<<endl;
        //cout<<"Rec1= "<<RD(1)*muR(1)<<endl;
        //cout<<"Rec2= "<<RD(2)*muR(2)<<endl;
       
        //cout<<"RD sam 1 = "<<RD(1)<<endl;                       // Estimated recruitment deviations
	//cout<<"RD sam 2 = "<<RD(2)<<endl;                       // Estimated recruitment deviations
	cout<<"movest= "<<movest<<endl;                         // Estimated recruitment deviations
	
	//cout<<"mov sim p1 s2 m2= "<<endl;                       // Simulated movement probabilities for stock 1 in subyear 1
	//cout<<mov_ini(1)(2)(2)<<endl;                           // Simulated movement probabilities for stock 1 in subyear 1
	cout<<"East"<<endl;
	cout<<"mov sam p1 s1 a3 = "<<mov(1)(1)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s2 a3 = "<<mov(1)(2)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s3 a3 = "<<mov(1)(3)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s4 a3 = "<<mov(1)(4)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"West"<<endl;
	cout<<"mov sam p1 s1 a3 = "<<mov(2)(1)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s2 a3 = "<<mov(2)(2)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s3 a3 = "<<mov(2)(3)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s4 a3 = "<<mov(2)(4)(3)(1)<<endl;                        // Estimated movement probabilities for stock 1 in subyear 1
		
	//cout<<"mov sam p1 s3 m2= "<<endl;                       // Estimated movement probabilities for stock 1 in subyear 1
	//cout<<mov(1)(3)(2)<<endl;                               // Estimated movement probabilities for stock 1 in subyear 1
	//cout<<"mov sim p2 s2 m2= "<<endl;                       // Simulated movement probabilities for stock 2 in subyear 1
	//cout<<mov_ini(2)(2)(2)<<endl;                           // Simulated movement probabilities for stock 2 in subyear 1
	//cout<<"mov sam p2 s2 m2= "<<endl;                       // Estimated movement probabilities for stock 2 in subyear 1
	//cout<<mov(2)(2)(2)<<endl;                               // Estimated movement probabilities for stock 2 in subyear 1
	//cout<<"mov sam p2 s3 m2= "<<endl;                       // Estimated movement probabilities for stock 2 in subyear 1
	//cout<<mov(2)(3)(2)<<endl;                               // Estimated movement probabilities for stock 2 in subyear 1
	//cout<<"qCE sim= "<<exp(lnqCPUE_ini)<<endl;           // Simulated catchabilities
	cout<<"qCPUE sam= "<<qCPUE<<endl;                      // Estimated catchabilities
	cout<<"qE sam= "<<qE<<endl;                             // Simulated FI index catchability
	cout<<"lnqE sam= "<<lnqE<<endl; 
	cout<<"qI sam= "<<qI<<endl;                          // Estimated FI index catchability
	cout<<"Biomass trend = "<<Btrend<<endl;              // Biomass trend
	for(int pp =1;pp<=np;pp++){
	  Btrend(pp)=trans(SSB(pp))(1)/SSB0(pp);
	}
	cout<<Btrend<<endl;
	cout<<"Mean F ="<<meanF<<endl;                       // Mean F
	cout<<"D sim= "<<D_ini<<endl;                        // Simulated depletion
	cout<<"D sam= "<<D<<endl;			     // Estimated depletion SSB/SSB0
	cout<<"Dt sam= "<<Dt<<endl;			     // Estimated depletion SSB over time series	
	cout<<"SSBnow= "<<SSBnow<<endl;                      // Current SSB by stock
	cout<<"------------------------------------"<<endl; 
  }



FUNCTION popnodes
  {
	// -- Populate nodes for mcmc output --
	j=0;
	
	for(int pp=1;pp<=np;pp++){
	  j+=1;
	  nodes(j)=lnmuR(pp);
	}
	
	for(int ff=1; ff<=nsel;ff++){
	  for(int sp=1; sp<=seltype(ff);sp++){
	    j+=1;
	    nodes(j)=selpar(ff,sp);
	  }   
	}
	
	for(int pp=1; pp<=np;pp++){
	  for(int yy=1; yy<=nRD;yy++){
	    j+=1;
            nodes(j)=RD(pp,yy);
          }
	}
	
	for(int mm=1; mm<=nMP;mm++){
	  j+=1;
          nodes(j)=movest(mm);
	}
	
	for(int ff=1; ff<=nCPUEq;ff++){
          j+=1;
	  nodes(j)=lnqCPUE(ff);
	}
	
	for(int pp=1; pp<=nI;pp++){
	  j+=1;
          nodes(j)=lnqI(pp);
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
	
	report<<"Ilencat"<<endl;
	report<<Ilencat<<endl;
	
	report<<"Ma (p,a)"<<endl;
	report<<Ma<<endl;
	
	report<<"RDblock (y)"<<endl;
	report<<RDblock<<endl;
	
	report<<"Fec (p,a)"<<endl;
	report<<Fec<<endl;
	                
	report<<"nsel number of estimated selectivities"<<endl;
	report<<nsel<<endl;
	
	report<<"seltype (nsel): logistic, 3: Thompson dome-shaped"<<endl;
	report<<seltype<<endl;
	
	report<<"selind (f) which selectivity is assigned to each fleet"<<endl;
	report<<selind<<endl;
	
	report<<"spawns (p) the subyear for spawning"<<endl;
	report<<spawns<<endl;
	
	report<<"ALK (p,y,a,l) inverse age-length key p(l|a)"<<endl;
	for(int pp=1;pp<=np;pp++){
            report <<ALK(pp)<<endl;
  	}
	
	report<<"lnqE (f) the estimated log qs"<<endl;
	report<<lnqE<<endl;
	
	report<<"lnqI (nI) the estimated log qs"<<endl;
	report<<lnqI<<endl;
	
	report<<"lnqI (nCPUEq) the estimated log qs"<<endl;
	report<<lnqCPUE<<endl;
			
	report<<"hZ (p,hy,s,a,r)"<<endl;
	for(int pp=1;pp<=np;pp++){
	  report <<hZ(pp)<<endl;
  	}
  	
  	report<<"Ipred (y,s,r,i)"<<endl;
	for(int yy=1;yy<=ny;yy++){
	  report <<Ipred(yy)<<endl;
  	}
			
	report<<"nydist number of year iterations to get initial spatial distribution"<<endl;
	report<<nydist<<endl;
		
	report<<"SSB0: unfished spawning stock biomass"<<endl;
	report<<SSB0<<endl;
	
	report<<"muR: mean historical recruitment"<<endl;
	report<<muR<<endl;
	
	report<<"lnHR1: mean historical recruitment deviations before initial year-10"<<endl;
	report<<lnHR1<<endl;
	
	report<<"lnHR1: mean historical recruitment deviations after initial year-10"<<endl;
	report<<lnHR2<<endl;
		
	report<<"nRD: number of estimated recruitment deviations by stock"<<endl;
	report<<nRD<<endl;
	
	report<<"lnRD: log recruitment deviations"<<endl; 
	report<<log(RD)<<endl;
	
	report<<"D_ini: simulated depletion"<<endl;
	report<<D_ini<<endl;
	
	report<<"D: depletion SSB(last year, last subyear)/SSB0"<<endl;
	report<<D<<endl;
	
	report<<"Dt: depletion SSB(last year, last subyear)/SSB(first year, first subyear)"<<endl;
	report<<Dt<<endl;
	
	report<<"SSBnow: current spawning biomass by stock"<<endl;
	report<<SSBnow<<endl;
	
	report<<"objC"<<" objCPUE"<<" objI"<<" objCL"<<" objSOO"<<" objPSAT"<<" objRD"<<" objmov"<<" objsel"<<" objSRA"<<" objSSB"<<" objFmod"<<" objRat"<<" objG"<<endl;
	report<<objC<<" "<<objCPUE<<" "<<objI<<" "<<objCL<<" "<<objSOO<<" "<<objPSAT<<" "<<objRD<<" "<<objmov<<" "<<objsel<<" "<<objSRA*LHw(11)<<" "<<objSSB<<" "<<objFmod<<" "<<objRat*50<<" "<<objG<<endl;
			
  	report<<"datacheck"<<endl;
  	report<<datacheck<<endl;
  	
  }



RUNTIME_SECTION
    maximum_function_evaluations 5000
    convergence_criteria 1.e-4


TOP_OF_MAIN_SECTION
	arrmblsize = 70000000;
	gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
	gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
	gradient_structure::set_MAX_NVAR_OFFSET(5000);
	gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
	

GLOBALS_SECTION
        
        #include <admodel.h>
	#include "stats.cxx"
	//#include <fstream>
        //ofstream nodesout("nodes.cha");
       	
	
	

