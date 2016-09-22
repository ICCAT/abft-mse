// =========================================================================================================================================== 
//
//                         		           Modifiable Multistock Model (M3)                      
//
//                                	           	    v1.0 (beta)                                   
//
//                                   		          12th September 2016                           
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
//                       Previous coordinators: Joe Powers, Campbell Davies 
//    ------------------------------------------------------------------------------------------
//
//
// M3 uses Steve Martell's stats.cxx library (publically available as part of the iSCAM package, https://github.com/smartell/iSCAM)
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
// -- Dev notes --
//
// * inflect lim was commented out.. maybe for real data... hmm
// * make sure pass through works for new movement estimation and nZeq etc

// ==========================================================================================================================================


DATA_SECTION
	
	// -- Model Dimensions --
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
	init_int nZeq;                         // Number of years at the start of the model to calculate equilibrium Z from (mean of nZep years)
	init_int nydist;                       // Number of years over which initial stock distribution is calculated (prior to spool up)
	init_int nyeq;                         // Number of spool-up years over which the stock is subject to nZeq, used to define equilibrium conditions
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
	init_vector steep(1,np);               // Steepness of the stock-recruitment curve
	
	// -- Spawning --
	init_vector spawns(1,np);              // The subyear in which the stock spawns
	init_matrix canspawn(1,np,1,nr);       // The areas in which spawning occurs (1) or not (0) for each stock
	
	// -- Natural Mortality rate --
	init_matrix Ma(1,np,1,na);             // Instantaneous natural mortality rate at age by stock
		
	// -- Fishery data --
	init_int nCobs;                        // Number of catch observations by year, subyear, area, fleet 
	init_matrix Cobs(1,nCobs,1,5);         // Catch observations
	init_int nCPUE;                        // Number of CPUE series used / partial F's
	init_int nCPUEobs;                     // Number of relative abundance index observations by year, subyear, area, fleet, index No. (nCPUE) 
	init_matrix CPUEobs(1,nCPUEobs,1,6);   // Observed relative index observations
	init_int nCLobs;                       // Number of Catch-at-length observations
	init_matrix CLobs(1,nCLobs,1,6);       // Catch-at-length observations by year, subyear, area, fleet, length category, N
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
	
	// -- Observation errors --
	init_vector CobsCV(1,nf);              // Catch observation error   
	init_vector CPUEobsCV(1,nCPUE);        // CPUE observation error
	init_vector IobsCV(1,nI);              // Fishery independent index obs error (only if complexF = 1)
		
	// -- Priors --
	init_number RDCV;                      // Recruitment deviation penalty	
	
	// -- Likelihood weights --
	init_int nLHw;                         // Number of likelihood weights
	init_vector LHw(1,nLHw);               // Likelihood weights (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel)
	
	// -- Initial values --
	init_vector R0_ini(1,np);                         // Unfished recruitment
	init_matrix sel_ini(1,nf,1,nl);                   // Selectivity 
	init_matrix selpars_ini(1,nf,1,3);                // Selectivity parameters
	init_vector lnF_ini(1,nCobs);                     // Effort (complexF=0) or log fishing mortality rate (complexF=1) 
	init_matrix ilnRD_ini(1,np,2,na);                 // Recruitment deviations (initial)
	init_matrix lnRD_ini(1,np,1,ny);                  // Recruitment deviations
	init_5darray mov_ini(1,np,1,ns,1,na,1,nr,1,nr);   // Movement parameters
	init_vector lnqCPUE_ini(1,nCPUE);                 // q estimates for CPUE fleets
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
		
	
PARAMETER_SECTION
		
	// -- Estimated parameters --
	init_bounded_vector lnR0(1,np,8.,18.,1);                  // Unfished recruitment
	init_bounded_matrix selpar(1,nsel,1,seltype,-3.,3.,1);      // Selectivity parameters
	// init_bounded_matrix ilnRD(1,np,2,na,-1.,1.,2);            // Recruitment deviations (years prior to initial year)
	init_bounded_matrix lnRD(1,np,1,nRD,-6.,6.,1);              // Recruitment deviations
	init_bounded_vector movest(1,nMP,-6.,6.,1);                 // Movement parameters
	init_bounded_vector lnqCPUE(1,nCPUE,-9.,2,1);            // q estimates for CPUE fleets UB mu F=0.3
        init_bounded_vector lnqI(1,nI,-2.3,2.3,1);                  // q estimates for Fish. Ind. Indices
	
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
        
        // -- Transitions --
        5darray N(1,np,1,ny,1,ns,1,na,1,nr);        // Stock numbers
        matrix NLA(1,na,1,nl);                      // Stock numbers by age and length
        matrix surv(1,np,1,na);                     // Survival
        3darray SSB(1,np,1,ny,1,ns);                // Spawning stock biomass
        3darray SSBi(1,np,1,ny,1,ns);               // Spawning stock biomass index
        matrix SSBdist(1,np,1,nr);                  // A temporary vector storing the spatial distribution of spawning biomass
        matrix spawnr(1,np,1,nr);                   // The fraction of spawning in each area
        4darray VB(1,ny,1,ns,1,nr,1,nf);            // Vulnerable biomass
        3darray B(1,ny,1,ns,1,nr);                  // Biomass
        vector R0(1,np);                            // Unfished recruitment
        vector SSB0(1,np);                          // Unfished spawning stock biomass
        vector D(1,np);                             // Stock depletion SSB now / SSB0
        vector SSBpR(1,np);                         // Unfished spawning stock biomass per recruit
        matrix iRD(1,np,2,na);                      // Recruitment deviations (years prior to initial year)
	matrix RD(1,np,1,ny);                       // Recruitment deviations
	5darray CTL(1,np,1,ny,1,ns,1,nr,1,nl);      // Catch at length
	5darray CTA(1,np,1,ny,1,ns,1,nr,1,na);      // Catch at age
        
        //LOCAL_CALCS
        //  cout<<"got to here"<<endl;
        //END_CALCS
        
        // -- Exploitation rates --
        vector F(1,nF);                                // Estimated fishing mortality rate
        5darray FAT(1,np,1,ny,1,ns,1,nr,1,na);         // Population level F at Age Total
        5darray FL(1,ny,1,ns,1,nr,1,nf,1,nl);          // Fishing mortality rate at length
        4darray FT(1,ny,1,ns,1,nr,1,nl);               // Total fishing mortality rate at length all fleets
        5darray Z(1,np,1,ny,1,ns,1,na,1,nr);           // Total mortality rate at age
        4darray Zeq(1,np,1,ns,1,na,1,nr);              // Equilibrium Z calculation
        vector qCPUE(1,nCPUE);                         // Catchability of CPUE indices CPUE=qVB
        vector qI(1,nI);                               // Catchability of fishery independent indices (1) I=qB  (2) I=qSSB                  
        
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
        
        // -- Tagging --
        6darray RecapP(1,np,1,ns,1,nma,1,nRPT,1,nr,1,nr);    // Recapture probabilities
        
        // -- Temporary arrays --
        3darray stemp(1,np,1,ns,1,nr);                 // Temporary season index
        vector sind(1,nRPT);                           // Temporary season index for calculation of recapture probabilities
        vector NLtemp(1,nl);                           // Stores a temporary vector of numbers at length (efficiency)
         
        // -- Observations --
        5darray CWpred(1,np,1,ny,1,ns,1,nr,1,nf);      // Catches by fleet (weight) and stock
        4darray CWtotpred(1,ny,1,ns,1,nr,1,nf);        // Total catches by weight
        4darray CNpred(1,np,1,ny,1,ns,1,nr);           // Catches by all fleets (numbers) and stock
        6darray CLpred(1,np,1,ny,1,ns,1,nr,1,nf,1,nl); // Catches by fleet (weight) and stock
        5darray CLtotpred(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet
        5darray CLtotfrac(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet fraction
       	//sdreport_number R0(1);                       // Requirement of mceval for producing posterior estimates of model parameters/variables                   


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



FUNCTION assignPars
  {
	// -- Assign estimated parameters --
	
	R0=mfexp(lnR0);                     // Assign unfished recruitment
	qCPUE=mfexp(lnqCPUE);               // Assign catchability for CPUE indices
	
	if(complexRD){                      // If annual recruitment deviations are to be estimated
	 
	  RD=mfexp(lnRD)/mean(mfexp(lnRD)); // Ensure mean 1 recruitment deviations
	  
	}
	else{                               // If blocks of recruitment deviations are to be estiamted
	
	  for(int pp=1;pp<=np;pp++){        // Loop over stocks
	   
	    for(int yy=1;yy<=ny;yy++){      // Loop over years
	    
	      int ry = RDblock(yy);         // Find the correct reference block for this year
	      RD(pp,yy)=mfexp(lnRD(pp,ry)); // Assign recruitment deviation accordingly
	      
	    }
	    
	    RD(pp)/=mean(RD(pp));           // Ensure recruitment deviations sum to 1
	    
	  }                                 // End of stocks
	  
	}                                   // End of recruitment estimation type (complexRD)
	
	//iRD=mfexp(ilnRD_ini);             // Assign initial recruitment deviations
	qI=mfexp(lnqI);                     // Assign fishery independent catchabilities
	
	for(int pp=1;pp<=np;pp++){          // Loop over stocks
	
	  wl(pp)=lwa(pp)*pow(ml,lwb(pp));   // Calcualte weight at length
	
	}                                   // End of stocks
	
	if(debug)cout<<"--- Finished assignPars ---"<<endl;
  }



FUNCTION assignInits
  {
	// -- Assign initial (starting) values --
	R0=R0_ini;                          // Assign unfished recruitment
	//iRD=mfexp(ilnRD_ini);             // Assign initial recruitment deviations  
	RD=mfexp(lnRD_ini);                 // Assign recruitment deviations
	if(complexF)F=mfexp(lnF_ini);       // Assign fishing mortality rates
	sel=sel_ini;                        // Assign selectivitiese
	mov=mov_ini;                        // Assign movement
	qCPUE=mfexp(lnqCPUE_ini);           // Assign CPUE index catchability
	qI=mfexp(lnqI_ini);                 // Assign fishery independent index catchability
	
	if(debug)cout<<"--- Finished assignInits ---"<<endl;
  }



FUNCTION calcSurvival
  {
	// -- Calculate survival --
	for(int pp=1;pp<=np;pp++){                   // Loop over stocks      
	 
	  surv(pp,1)=1.;                             // Survival to age 1 is 100%
	  
	  for(int aa=1;aa<=(na-1);aa++){             // Loop over age classes
	    
	    surv(pp,aa+1)=exp(-sum(Ma(pp)(1,aa)));   // Calculate survivial
	  
	  }                                          // End of age classes
	
	  surv(pp,na)*=exp(-Ma(pp,na))/(1-exp(-Ma(pp,na))); // final plus group survival is multiplied by the indefinite intergral  
	
	}                                            // End of stocks
	
	if(debug)cout<<"--- Finished calcSurvival ---"<<endl;
  }

 

FUNCTION calcMovement
  {
  		
	//  -- Movement modelling -----------
	for(int pp=1;pp<=np;pp++){                 // Loop over stocks
	  
	  for(int ss=1;ss<=ns;ss++){               // Loop over subyears
	    
	    for(int aa=1;aa<=nma;aa++){ 
	      
	      movcalc(pp)(ss)(aa)=-10.;                  // Set all movements to be unlikely (logit space)
	  
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
	          	          
	            movcalc(pp,ss,aa,rr,rr)+=mfexp(movest(mi))/3;      // Add viscosity
	      
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
	      int aa=movind(mm,2);                   // Movement age class
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
	     
	      spar(2)=ml(nl)*(0.2+0.5*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));        // Inflection point (2) as a fraction of largest length I(0.1|0.8)
	      spar(1)=ml(nl)*(0.02+0.08*(mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)))));    // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
	      
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	      
	        msel(ss,ll)=1/(1+mfexp((spar(2)-ml(ll))/spar(1)));                         // Logistic selectivity function
	         
	      } 
	      
	      msel(ss)=msel(ss)/max(msel(ss)); // Need to normalize at least one index to max 1 or face counfounding with q
	      // End of length classes
	    
	    break;                                                                         // End of logistic selectivity
		   
            case 3: // Thompson dome-shaped selectivity
	      
	      spar(1)=0.2*mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)));                     // Dome-shape parameter I(0|0.2)
	      spar(2)=0.1+(0.15*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));              // Precision as the ratio of the inflection point I(0.1|0.7)
	      spar(3)=ml(nl)*(0.15+(0.65*mfexp(selpar(ss,3))/(1+mfexp(selpar(ss,3)))));    // Inflection point as a fraction of largest length I(0.15|0.9)
	     
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	        
	        msel(ss,ll)=(1/(1-spar(1)))*pow(((1-spar(1))/spar(1)),spar(1)) * mfexp(spar(2)*spar(1)*(spar(3)-ml(ll)))/(1+mfexp(spar(2)*(spar(3)-ml(ll))));	// Thompson selectivity function	
	      
	      }                                                                            // End of length classes
	       
	    break;									   // End of Thompson selectivity
		      
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
  	Zeq.initialize();     // Equilibrium Z = 0
  	
  	if(complexF){         // -- Estimate a fishing mortality rate for each catch observation ----------------
  	
  	  for(int i=1; i<=nCobs; i++){ // Only calculate F's when catches are observed
  	
  	    int yy=Cobs(i,1); // Year
	    int ss=Cobs(i,2); // Subyear
	    int rr=Cobs(i,3); // Region
  	    int ff=Cobs(i,4); // Fleet
  	
  	    FL(yy)(ss)(rr)(ff)= sel(ff)*F(i); // Assign estimated fishing mortality rate-at-length to array
  	
  	  }                   // End of catch observations
  	  
  	}else{                // -- Alternatively use an index of fishing effort -------------------------------
  	
  	  for(int i=1;i<=nCPUEobs;i++){
			
	    int yy=CPUEobs(i,1);    // Year
            int ss=CPUEobs(i,2);    // Subyear
            int rr=CPUEobs(i,3);    // Region
	    int ff=CPUEobs(i,4);    // Fleet
	     
	    FL(yy)(ss)(rr)(ff)= sel(ff)*CPUEobs(i,6)*qCPUE(ff);  // Calculate fishing mortality rate at length 
	    
	  }
	  
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
	        
	        for(int aa=1;aa<=na;aa++){             // Loop over ages
	        
	          Z(pp)(yy)(ss)(aa)(rr)=FAT(pp)(yy)(ss)(rr)(aa)+(Ma(pp,aa)*sdur(ss)); // Calculate total mortality rate
	        
	          if(yy<=nZeq){                        // Equilibrium Z calculation
	          
	            Zeq(pp)(ss)(aa)(rr)+=Z(pp)(yy)(ss)(aa)(rr)/nZeq; // Sum up equilibrium Z
	            
	          }
	        
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
	SSB.initialize();              // Spawning stock biomass = 0
	SSBdist.initialize();          // Spawning distribution = 0
	SSB0.initialize();             // Unfished spawning stock biomass = 0
	SSBpR.initialize();            // SSB per recruit = 0
	CWtotpred.initialize();        // Total catch (weight) = 0
	CWpred.initialize();           // Catch (weight) = 0
	CNpred.initialize();           // Catch (numbers) = 0
	CLpred.initialize();           // Catch (length class) = 0
	CLtotpred.initialize();        // Total catch (length class) = 0
	CLtotfrac.initialize();        // Total catch fractions (length class) = 0
	CTA.initialize();              // Temporary catch at age = 0
	VB.initialize();               // Vulnerable biomass = 0
	D.initialize();                // Spawning Stock depletion
	
	for(int pp=1;pp<=np;pp++){
	
	  SSB0(pp)=sum(elem_prod(surv(pp)*R0(pp),Fec(pp)));     // Unfished Spawning Stock Biomass
	  //SSB0(pp)+=R0(pp)*Fec(pp,na)*surv(pp,na)*exp(-Ma(pp,na))/(1-exp(-Ma(pp,aa))); // indefinite integral of surv added to get plus group SSB0
	  SSBpR(pp)=SSB0(pp)/R0(pp);                            // Unfished SSB per recruit
	
	}
	
	for(int pp=1;pp<=np;pp++){                              // Loop over stocks
	
	  // -- Initial guess at stock distribution -------------------------------------------
	  
	  stemp(pp)(ns)=1./nr;                                  // Distribute a fish evenly over areas                     
	  
	  for(int ii=1;ii<=nydist;ii++){                        // Loop over 10 years
	    
	    for(int ss=1;ss<=ns;ss++){                          // Loop over subyears
	        
	      if(ss==1){                                        // Take bits of fish from previous years final subyear
	      
	        stemp(pp)(1)=stemp(pp)(ns)*mov(pp)(ss)(na);     // Move them assuming mature movement
	      
	      }                                                 // End of 'if first subyear'	      
	      else{                                             // Take bits of fish from this years' previous subyear
	        
	        stemp(pp)(ss)=stemp(pp)(ss-1)*mov(pp)(ss)(na);  // Move them assuming mature movement
	      
	      }                                                 // End of 'if not first subyear'
	      
	    }                                                   // End of subyears
	  
	  }                                                     // End of years
	 
	 for(int rr=1;rr<=nr;rr++){  
	    SSB0(pp)+=sum(elem_prod(surv(pp)*R0(pp),Fec(pp)))*canspawn(pp,rr)*stemp(pp,spawns(pp),rr);     // Unfished Spawning Stock Biomass
	  }
	  
	  SSBpR(pp)=SSB0(pp)/R0(pp); 
	  
	  // -- Spool up to equilibrium given total mortality rate over first nZeq years -----------
	  
	  for(int rr=1;rr<=nr;rr++){                                // Loop over areas
	    
	    for(int aa=1;aa<=na;aa++){
	        
	       N(pp,1,ns,aa,rr)=R0(pp)*surv(pp,aa)*stemp(pp,ns,rr);    // Stock numbers are spatial distribution multiplied by Survival and R0
	       
	       SSB(pp,1,ns)+=N(pp,1,ns,aa,rr)*Fec(pp,aa)*canspawn(pp,rr);              // Spawning stock biomass
	      
	    }                                                          // End of subyears  
	
	  }                                                         // End of areas 
	 
	  // Checked line for line to here
	  
	  for(int yy=1;yy<=nyeq;yy++){         // Loop over equilibrium years
		   
	    
	    for(int ss=1;ss<=ns;ss++){         // Loop over seasons
	      
	      SSB(pp,1,ss)=0.;                 // reset SSB counter        
	      
	      if(ss==1){                       // First subyear isn't a spawning season  ------------------------------------------------
		       
		for(int aa=1;aa<=na;aa++){     // Loop over age classes
		        
		  N(pp)(1)(1)(aa)=elem_prod(mfexp(-Zeq(pp)(ns)(aa)),N(pp)(1)(ns)(aa))*mov(pp)(1)(aa); // Mortality then move
		  
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
		          
		    SSB(pp,1,1)+=N(pp,1,1,aa,rr)*Fec(pp,aa)*canspawn(pp,rr); // SSB is summed (+=) over age classes and areas (sum())
		          
		  }                            // End of areas
		        
		}                              // End of ages
		
	      }else{                           // Could be a spawning season ------------------------------
		        	      
		if(ss==spawns(pp)){            // If a spawning season...-------------------------------------------------------------------
		          
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
			  	        
	            SSBdist(pp,rr)=0.;         // Reset SSB distribution counter
			  	        
		  }                            // End of areas
		          
		  for(int aa=1;aa<=na;aa++){   // Loop over age classes
		          
		    N(pp)(1)(ss)(aa)=elem_prod(mfexp(-Zeq(pp)(ss-1)(aa)),N(pp)(1)(ss-1)(aa))*mov(pp)(ss)(aa); // Mortality then move
		    		     
	            for(int rr=1;rr<=nr;rr++){ // Loop over areas
			      
		      SSB(pp,1,ss)+=(N(pp,1,ss,aa,rr)*Fec(pp,aa)*canspawn(pp,rr));    // SSB is summed (+=) over age classes and areas (sum())
		      SSBdist(pp,rr)+=(N(pp,1,ss,aa,rr)*Fec(pp,aa)*canspawn(pp,rr));  // The distribution of SSB among areas
			    	            
		    }                          // End of areas
			    
	          }                            // End of age classes
		         
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
		          
		    spawnr(pp,rr)=SSBdist(pp,rr)/sum(SSBdist(pp));  // Calculate spawning fraction of SSB
		          
		  }
		  
		  N(pp)(1)(ss)(na)+=N(pp)(1)(ss)(na-1);          // Plus group
		  
		  for(int aa=(na-1);aa>=2;aa-=1){                  // Loop down age classes from plusgroup(ns)-1
			   	        
	            N(pp)(1)(ss)(aa)=N(pp)(1)(ss)(aa-1);         // Age fish
			  	        
		  }                                                // End of ages
				  
	          N(pp)(1)(ss)(1)=spawnr(pp)*(0.8*R0(pp)*steep(pp)*SSB(pp,1,ss))/
	                          (0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * SSB(pp,1,ss)); // Recruitment
		  
		  
		   
		}else{                          // Not a spawning season ----------------------------------------------------------------
		        
		  for(int aa=1;aa<=na;aa++){    // Loop over age classes
			    
		    N(pp)(1)(ss)(aa)=elem_prod(mfexp(-Zeq(pp)(ss-1)(aa)),N(pp)(1)(ss-1)(aa))*mov(pp)(ss)(aa); // M then move	        
			    
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
			    
		      SSB(pp,1,ss)+=(N(pp,1,ss,aa,rr)*Fec(pp,aa)*canspawn(pp,rr)); // SSB is summed (+=) over age classes and areas (sum())
			    
		    }	                        // End of areas        
			  
		  }                             // End of ages
		          
		}                               // End of 'not a spawning season'
		     
              }                                 // End of 'could be a spawning season?'
		
	    }                                   // End of subyears
		  	
          }                                     // End of year
	 
	  // -- Initial year catch calculations ----------------------------------------------------------
	  
	  for(int rr=1;rr<=nr;rr++){                                             // Loop over areas
	    
	    for(int ss=1;ss<=ns;ss++){                                           // Loop over seasons
	      
              CTA(pp)(1)(ss)=trans(elem_prod(
	      	      	      elem_prod(N(pp)(1)(ss),1-mfexp(-Z(pp)(1)(ss))),
	      	      	      elem_div(trans(FAT(pp)(1)(ss)),Z(pp)(1)(ss))));    
	      	      	      //elem_prod(N(pp)(1)(ss),mfexp(Z(pp)(1)(ss))-1),
	      	      	      //elem_div(trans(FAT(pp)(1)(ss)),Z(pp)(1)(ss))));    // total catch at age in first year
	   
              CTL(pp)(1)(ss)(rr)=CTA(pp)(1)(ss)(rr)*ALK(pp)(1);                  // Catch at length is catch at age * inverse age length key
              	      
	      for(int aa=1;aa<=na;aa++){                      // Loop over age class
	      
	        NLA(aa)=N(pp)(1)(ss)(aa)(rr)*ALK(pp)(1)(aa);  // Temporarily store numbers at length
	      
	      }                                               // End of age class
	      
	      NLtemp=colsum(NLA);                             // Numbers at length (sum over age classes)
	      
	      B(1,ss,rr)+=sum(elem_prod(NLtemp,wl(pp)));      // Biomass summed over stocks	
	     
	      for(int ff=1;ff<=nf;ff++){                      // Loop over fleet types
						  
		VB(1,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));   // Vulnerable biomass summed over stocks
		 
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
	          
	            SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*canspawn(pp,rr); // SSB is summed (+=) over age classes and areas (sum())
	          
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
		      
		      SSBdist(pp,rr)+=N(pp,yy-1,ss,aa,rr)*Fec(pp,aa)*canspawn(pp,rr); // Calculate distribution of SSB over areas
		    	            
	            }                          // End of areas
		    
		  }                            // End of age classes
	         
	          for(int rr=1;rr<=nr;rr++){   // Loop over areas
	          
	            spawnr(pp,rr)=SSBdist(pp,rr)/sum(SSBdist(pp)); // Calculate spawning fraction
	          
	          }                            // End of areas
	                    
	          
	          
	          N(pp)(yy)(ss)(na)+=N(pp)(yy)(ss)(na-1);  // Plus group calculation
	          
	          for(int aa=(na-1);aa>=2;aa-=1){          // Loop down age classes
		   	        
		    N(pp)(yy)(ss)(aa)=N(pp)(yy)(ss)(aa-1); // Age fish
		  	        
	          }                                        // End of age classes
	          
                  N(pp)(yy)(ss)(1)=spawnr(pp)*RD(pp,yy)*(0.8*R0(pp)*steep(pp)*sum(SSBdist(pp)))/
		                   (0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * sum(SSBdist(pp))); // Recruitment (SSB lag 1 year)
	          
	       	 	       	  
	       	  for(int aa=1;aa<=na;aa++){  // Loop over age classes
		                
		     for(int rr=1;rr<=nr;rr++){ // Loop over areas
		  		      
		  	SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*canspawn(pp,rr); // SSB is summed (+=) over age classes and areas (sum())
		  		    	            
		     }                          // End of areas
		  		    
		  }                            // End of age classes
	       	  
	       	  
                    
	        }else{                          // Not a spawning season ----------------------------------------------------------------
	        
	          for(int aa=1;aa<=na;aa++){    // Loop over age classes
		    
		    N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss)(aa); // M then move	        
		    
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
		    
		      SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa)*canspawn(pp,rr); // SSB is summed (+=) over age classes and areas (sum())
		    
		    }	                        // End of areas 
		  
		  }                             // End of ages
	          
	        }                               // Not a spawning season
	     
	      }                                 // Could be a spawning season?
	     
	      CTA(pp)(yy)(ss)=trans(elem_prod(
	        elem_prod(N(pp)(yy)(ss),1-mfexp(-Z(pp)(yy)(ss))),
	      	elem_div(trans(FAT(pp)(yy)(ss)),Z(pp)(yy)(ss)))); // Calculate catch at age
	     
	      //elem_prod(N(pp)(yy)(ss),mfexp(Z(pp)(yy)(ss))-1),
	      //elem_div(trans(FAT(pp)(yy)(ss)),Z(pp)(yy)(ss)))); // Calculate catch at age
	     	
	      for(int rr=1;rr<=nr;rr++){                              // Loop over areas
		
		CTL(pp)(yy)(ss)(rr)=CTA(pp)(yy)(ss)(rr)*ALK(pp)(yy);  // Calculate catch at length
		
		for(int aa=1;aa<=na;aa++){                            // Loop over age classes
			      
		  NLA(aa)=N(pp)(yy)(ss)(aa)(rr)*ALK(pp)(yy)(aa);      // Temporarily store numbers at length
			      
		}                                                     // End of age classes
			      
	        NLtemp=colsum(NLA);                                   // Calculate numbers by length class (sum over ages)
	       
		B(yy,ss,rr)+=sum(elem_prod(NLtemp,wl(pp)));           // Biomass summed over stocks				  
	      
		for(int ff=1;ff<=nf;ff++){                            // Loop over fleets
		   
		  VB(yy,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));    // Vulnerable biomass summed over stocks
		  	  
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
	  
	    //if(yy==3){
	    //cout<<SSB(1)(yy)<<endl;
	    	//cout<<CTA(1)(1)<<endl;
	    //exit(1);
	    //}
	  }                                                          // End of year
	
	  SSBi(pp)=SSB(pp)/(sum(SSB(pp))/(ny*ns));                   // Calculate SSB index normalized to 1
	  
	  for(int pp=1;pp<=np;pp++){  // Loop over stocks
	  		  		      
	     D(pp)=SSB(pp,ny,ns)/SSB(pp,1,1); // SSB depletion over time series
	  		  		    
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
	
	dvariable LHtemp;                       // Temporary store of the calculated likelihood values
        double tiny=1E-10;                      // Create a small constant to avoid log(0) error
	
	// -- Catch observations --
	
	for(int i=1;i<=nCobs;i++){              // Loop over catch observations
	
	  int yy=Cobs(i,1); // Year
	  int ss=Cobs(i,2); // Subyear
	  int rr=Cobs(i,3); // Region
  	  int ff=Cobs(i,4); // Fleet
  	  
  	  LHtemp=dnorm(log(CWtotpred(yy,ss,rr,ff)+tiny),log(Cobs(i,5)+tiny),CobsCV(ff)); // Log-normal LHF
  	  //LHtemp=pow(log(CWtotpred(yy,ss,rr,ff)+tiny)-log(Cobs(i,5)+tiny),2);// SSQ test
  	  objC+=LHtemp*LHw(1);                                                           // Weighted likelihood contribution
  	  objG+=LHtemp*LHw(1);  
  	  //objC+=LHtemp*1/nCobs;                                                           // Weighted likelihood contribution
  	  //objG+=LHtemp*1/nCobs;   // Weighted likelihood contribution
  	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" Cobs="<<Cobs(i,5)<<" Cpred="<<CWtotpred(yy,ss,rr,ff)<<endl;
	}
	//exit(1);
	if(debug)cout<<"---  * Finished Catch LHF ---"<<endl;
	
	// -- CPUE observations --
	
	dvariable CPUEtemp;
	
	if(complexF){                   // The CPUE indices are contributing to the likelihood function
	  
	  for(int i=1;i<=nCPUEobs;i++){ // Loop over catch rate indices
		
	    int yy=CPUEobs(i,1);    // Year
	    int ss=CPUEobs(i,2);    // Subyear
	    int rr=CPUEobs(i,3);    // Region
	    int ff=CPUEobs(i,4);    // index ID
	    int CPUEi=CPUEobs(i,5); // q index
	    	  
	    CPUEtemp=VB(yy,ss,rr,ff)*qCPUE(CPUEi);                                    // Calculate vulnerable biomass
	    LHtemp=dnorm(log(CPUEtemp+tiny),log(CPUEobs(i,6)+tiny),CPUEobsCV(CPUEi)); // Log-normal LHF
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
	  	      
	      Itemp= B(yy,ss,rr)*qI(ii);     // Predicted index
	  	    
	      break;
	  		   
	    case 2:  // SSB
	       
	      Itemp=qI(ii)*(SSB(pp,yy,ss)/mean(extract_row(trans(SSB(pp)),ss)));   // Predicted index normalized to mean 1
	         	      
	    break;
	  		      
	  }
	  
	  LHtemp=dnorm(log(Itemp+tiny),log(Iobs(i,7)+tiny),IobsCV(ii)); // Log-normal LHF
	  objI+=LHtemp*LHw(3);                                          // Weighted likelihood contribution
	  objG+=LHtemp*LHw(3);                                          // Weighted likelihood contribution
	  //objI+=LHtemp*1/nIobs;                                          // Weighted likelihood contribution
	  //objG+=LHtemp*1/nIobs; 	          
	}
	
	if(debug)cout<<"---  * Finished FI index LHF ---"<<endl;
	
	// -- Length composition data --  
	
	for(int i=1;i<=nCLobs;i++){  // Loop over catch at length observations
	
	  int yy=CLobs(i,1);   // Year
	  int ss=CLobs(i,2);   // Subyear
	  int rr=CLobs(i,3);   // Region
	  int ff=CLobs(i,4);   // Fleet type
	  int ll=CLobs(i,5);   // Length class
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" l="<<ll<<" CLobs="<<CLobs(i,6)<<" CLpred="<<CLtotpred(yy,ss,rr,ff,ll)<<endl;
	  
	  LHtemp=(-CLobs(i,6)*log(CLtotfrac(yy,ss,rr,ff,ll)+tiny)); // Multinomial LHF
	  objCL+=LHtemp*LHw(4);                                     // Weighted likelihood contribution
	  objG+=LHtemp*LHw(4);                                      // Weighted likelihood contribution
	  //objCL+=LHtemp*1/nCLobs;                                     // Weighted likelihood contribution
	  //objG+=LHtemp*1/nCLobs;  
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
	  //objSOO+=LHtemp*1/nSOOobs;                     // Weighted likelihood contribution
	  //objG+=LHtemp*1/nSOOobs;  
	
	
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
	  //objPSAT+=LHtemp*1/nPSAT;                               // Weighted likelihood contribution
	  //objG+=LHtemp*1/nPSAT;  
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
	    //objG+=LHtemp*LHw(7);                                    // Weighted likelihood contribution
	    //objPSAT2+=LHtemp*1/nPSAT2;                                  // Weighted likelihood contribution
	    //objG+=LHtemp*1/nPSAT2;                                    // Weighted likelihood contribution
	  
	  }
			
	}
	
	if(debug)cout<<"---  * Finished PSAT unknown SOO LHF ---"<<endl;
	
	
	// -- Recruitment deviations --
	
	for(int pp=1;pp<=np;pp++){  // Loop over stocks
	
	  /*for(int aa=2;aa<=na;aa++){
	  
	    LHtemp=dnorm(ilnRD(pp,aa),0.,RDCV);   // Initial age-structure deviations (currently disabled)
	    objRD+=LHtemp*LHw(8);                 // Weighted likelihood contribution
	    objG+=LHtemp*LHw(8);                  // Weighted likelihood contribution
	    
	  }*/
	  
	  for(int yy=1;yy<=nRD;yy++){ // Loop over years (or blocks of recruitment deviations if complexRD=0)
	    
	    LHtemp=dnorm(lnRD(pp,yy),0.,RDCV);   // Recruitment deviations
	    objRD+=LHtemp*LHw(8);                // Weighted likelihood contribution
	    objG+=LHtemp*LHw(8);                 // Weighted likelihood contribution
	    //objRD+=LHtemp*1/nRD;                // Weighted likelihood contribution
	    //objG+=LHtemp*1/nRD;                 // Weighted likelihood contribution
	
	  }
	  
        }  
	
	// -- Movement parameters ---
	
	for(int mm=1;mm<=nMP;mm++){
	     
	  LHtemp=dnorm(mfexp(movest(mm)),0.,10.); // Weak prior 
          objmov+=LHtemp*LHw(9);                  // Weighted likelihood contribution
	  objG+=LHtemp*LHw(9);                    // Weighted likelihood contribution
	  
	}
	
	for(int i=1;i<=nsel;i++){ 
	  
	  objsel+=dnorm(selpar(i),0,2.)*LHw(10);  
	  objG+=dnorm(selpar(i),0,2.)*LHw(10);   // Weak prior on selectivity
	
	}
	
	if(debug)cout<<"---  * Finished rec dev penalty ---"<<endl;
	
	//objG+=dnorm(lnF(1),log(0.1),4);          // Temporary fix to allow F estimation to be simplied to a master index and partial F approach (complexF = 0)
	
	if(debug)cout<<"--- Finished calcObjective ---"<<endl;
	
	if(verbose)cout<<"Catch LHF "<<objC<<endl;           // Report catch likelihood component
	if(verbose)cout<<"CPUE LHF "<<objCPUE<<endl;         // Report CPUE likelihood component
	if(verbose)cout<<"FI index LHF "<<objI<<endl;        // Report FI index likelihood component
	if(verbose)cout<<"Length comp LHF "<<objCL<<endl;    // Report catch at length likelihood component
        if(verbose)cout<<"SOO LHF "<<objSOO<<endl;           // Report stock of origin likelihood component
	if(verbose)cout<<"PSAT LHF "<<objPSAT<<endl;         // Report PSAT likelihood component
	if(verbose)cout<<"PSAT uSOO LHF "<<objPSAT2<<endl;   // Report PSAT2 likelihood component
	if(verbose)cout<<"Rec dev LHF "<<objRD<<endl;        // Report Rec dev likelihood component
	if(verbose)cout<<"mov prior "<<objmov<<endl;         // Report Rec dev likelihood component
	if(verbose)cout<<"selectivity prior "<<objsel<<endl; // Report Rec dev likelihood component
	if(verbose)cout<<"Global objective "<<objG<<endl;    // Report Global objective function
	
  }
	


FUNCTION simsam
  {
        // If working with simulated data do some printing
        cout<<"R0 sim = "<<log(R0_ini)<<endl;                // Simulated R0
        cout<<"R0 sam = "<<log(R0)<<endl;                    // Estimated R0
        cout<<"sel sim f1= "<<sel_ini(1)<<endl;              // Simulated selectivity fleet 1
        cout<<"sel sam f1= "<<sel(1)<<endl;                  // Estimated selectivity fleet 1
        cout<<"sel sim f2= "<<sel_ini(2)<<endl;              // Simulated selectivity fleet 2
	cout<<"sel sam f2= "<<sel(2)<<endl;                  // Estimated selectivity fleet 2
	//cout<<"RDi sim= "<<exp(ilnRD_ini(1))<<endl;        // Simulated initial recruitment deviations
	//cout<<"RDi sam= "<<iRD(1)<<endl;                   // Estimated initial recruitment deviations
	cout<<"RD sim= "<<exp(lnRD_ini(1))<<endl;            // Simulated recruitment deviations 
        cout<<"RD sam= "<<RD(1)<<endl;                       // Estimated recruitment deviations
	cout<<"mov sim p1 s2 m2= "<<endl;                       // Simulated movement probabilities for stock 1 in subyear 1
	cout<<mov_ini(1)(2)(2)<<endl;                           // Simulated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s2 m2= "<<endl;                       // Estimated movement probabilities for stock 1 in subyear 1
	cout<<mov(1)(2)(2)<<endl;                               // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sim p2 s2 m2= "<<endl;                       // Simulated movement probabilities for stock 2 in subyear 1
	cout<<mov_ini(2)(2)(2)<<endl;                           // Simulated movement probabilities for stock 2 in subyear 1
	cout<<"mov sam p2 s2 m2= "<<endl;                       // Estimated movement probabilities for stock 2 in subyear 1
	cout<<mov(2)(2)(2)<<endl;                               // Estimated movement probabilities for stock 2 in subyear 1
	cout<<"qCE sim= "<<exp(lnqCPUE_ini)<<endl;           // Simulated catchabilities
	cout<<"qCE sam= "<<qCPUE<<endl;                      // Estimated catchabilities
	cout<<"qI sim= "<<exp(lnqI_ini)<<endl;               // Simulated FI index catchability
	cout<<"qI sam= "<<qI<<endl;                          // Estimated FI index catchability
	cout<<"D sim= "<<D_ini<<endl;                        // Simulated depletion
	
	
	cout<<"D sam= "<<D<<endl;			     // Estimated depletion	
	
  }

REPORT_SECTION
  {
  	report <<"np, number of stocks"<<endl;
  	report <<np<<endl;
  	
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
  	
  	report <<"Fishing mortality rate at length FL (y s r f l)"<<endl;
  	for(int yy=1;yy<=ny;yy++){
  	  report <<FL(yy)<<endl;
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
	
	report<<"movtype"<<endl;                    // 1: gravity, 2: markov
	report<<movtype<<endl;
	
	report<<"Ma (p,a)"<<endl;
	report<<Ma<<endl;
	
	report<<"steep (p)"<<endl;
	report<<steep<<endl;
	
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
	
	report<<"lnqCPUE (f) the estimated log qs"<<endl;
	report<<lnqCPUE<<endl;
		
	report<<"nZeq number of initial years to average over to get equilibrium Z"<<endl;
	report<<nZeq<<endl;
		
	report<<"nydist number of year iterations to get initial spatial distribution"<<endl;
	report<<nZeq<<endl;
		
	report<<"nyeq number of initial year iterations used to calculation initial stock size and distribution"<<endl;
	report<<nyeq<<endl;
	
	report<<"SSB0: unfished spawning stock biomass"<<endl;
	report<<SSB0<<endl;
	
	report<<"R0: unfished spawning stock biomass"<<endl;
	report<<R0<<endl;
	
	report<<"D_ini: simulated depletion"<<endl;
	report<<D_ini<<endl;
	
	report<<"D: depletion SSB(last year, last subyear)/SSB0"<<endl;
	report<<D<<endl;
		
  	report<<"datacheck"<<endl;
  	report<<datacheck<<endl;
  	
  }



RUNTIME_SECTION
    maximum_function_evaluations 5000
    convergence_criteria 1.e-3


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
       	
	
	

