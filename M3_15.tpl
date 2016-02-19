// =========================================================================================================================================== 
//                         		   Modifiable Multistock Model (M3)                    
//
//                                       		v0.15                                   
//
//                                   		    11th November 2015                           
//                                                                     
//                           		    ICCAT GBYP, Tom Carruthers UBC                                                  
//
//
// -- Some conventions --
//
// 1) Matrices are arranged in order of population, year, season
// 2) Timesteps include subyears and are indexed to year and subyear by yind and sind, respectively
// 3) Population separation is not explicit in the data except for stock of origin, some PSAT (not PSAT2) and some conventional tags
// 4) Movement by subyear is the movement into an area in that subyear: models runs F(y-1) M(y-1), mov(y), F(y), M(y), mov(y+1),...
// 5) Recapture probabilities are 100% in first time step after which they are affected by movement e.g. (0,1,0,0,0) (0.1,0.7,0.3,0.4,0.1),...
// 6) First subyear of first year is for initialisation and there is no recorded fishing
// 7) Rows in movement matrix with no recorded movements are assigned 1/nareas probability to avoid the vanishing fish problem (if it were all zeros)
//
//
// -- To do minor -- 
// * Swap to a catch-at-age predictor and conv catch-at-age to length by the key (less computation)
// * Investigate speed improvements from assigning data predictions to vector and using vectorized likelihoods
// * you are assigning integer values to continuous data! e.g. int CPUEobs = CPUEdat[i,6]
//
// -- To do major --
//
// * Full age-structured initialization with SSB depletion by stock
// * Dynamic age-length key based on linear interpolation between discrete growth groups
// * Conditional stock of origin by PSAT tag track according to SOO data
// * Add an option to simplify F=qE based on c/I = E by fleet, a local calcs
// * Fix gravity model
// * Stock of origin by fleet type?
// 
//
// -- Change log --   
//
//
//
// ==========================================================================================================================================
//
//
// ------------------------------------------------------------------------------------------------------------------ 


DATA_SECTION
	
	// -- Model Dimensions --
	init_int ny;                           // Number of years
	init_int ns;                           // Number of sub-year time steps
	init_int np;                           // Number of populations
	init_int na;                           // Maximum age
	init_int nr;                           // Number of regions
	init_int nf;	                       // Number of fleets			
	init_int nl;                           // Number of length classes 
	init_int nRPT;                         // Maximimum number of repapture probability time-steps (seasons)
	init_matrix RPTind(1,ns,1,nRPT);       // The correct subyear index for timesteps after initial subyear e.g. for release in s=2: 2,3,4,1,2,3...
	
	init_vector sdur(1,ns);                // Subyear duration e.g. 0.3,0.1,0.2,0.4
	init_vector ml(1,nl);                  // Mean length in each length class (used for calculating selectivity only) 
	
	init_vector RDblock(1,ny);             // Recruitment blocks for simple recruitment estimation
	init_int nRD;                          // Number of estimated recruitment dev parameters per population                   
	
	// -- Growth --
	init_4darray ALK(1,np,1,ny,1,na,1,nl); // Age-Length Key by population
	init_vector lwa(1,np);                 // Length-weight conversion factor a w=al^b
	init_vector lwb(1,np);                 // Length-weight conversion factor b w=al^b
	init_3darray len_age(1,ny,1,na,1,np);  // Length at age (pass through for independently calculating biomass from .rep file)
	init_3darray wt_age(1,ny,1,na,1,np);   // Weight at age (pass through for independently calculating biomass from .rep file)
		
	// -- Maturity --
	init_matrix Fec(1,np,1,na);            // Fecundity (SSB by age) by population
	init_vector steep(1,np);               // Steepness of the stock-recruitment curve
	
	// -- Spawning --
	init_vector spawns(1,np);              // The subyear in which the stock spawns
	
	// -- Natural Mortality rate --
	init_matrix Ma(1,np,1,na);             // Instantaneous natural mortality rate at age by population
	
	// -- Fishery data --
	init_int nCobs;                           // Number of catch observations by year, subyear, region, fleet 
	init_matrix Cobs(1,nCobs,1,5);            // Catch observations
	init_int nCPUE;                           // Number of CPUE series used / partial F's
	init_int nCPUEobs;                        // Number of relative abundance index observations by year, subyear, region, fleet, index No. (nCPUE) 
	init_matrix CPUEobs(1,nCPUEobs,1,6);      // Observed relative index observations
	init_int nCLobs;                          // Number of Catch-at-length observations
	init_matrix CLobs(1,nCLobs,1,6);          // Catch-at-length observations by year, subyear, region, fleet, length category, N
		
	init_3darray RAI(1,nr,1,ns,1,ny);         // -- The real relative abundance index from the simulation / cpue index   
	
	// -- Fishery independent indices --
	init_int nI;                         // Number of fishery independent indices
	init_int nIobs;                      // Number of relative abundance index observations by timestep, region 
	init_matrix Iobs(1,nIobs,1,7);       // Observed relative index observations year, subyear, region, population, index number (nI), type (1) biomass (2) SSB, index
	
	// -- PSAT tags --
	init_int nPSAT;                      // Number of PSAT recapture events (known SOO)
	init_matrix PSAT(1,nPSAT,1,6);       // PSAT release-recapture events from population, subyear, timestep, from-region, to-region, N
	init_int nPSAT2;                     // Number of PSAT recapture events (not known SOO)
	init_matrix PSAT2(1,nPSAT2,1,4+np);  // PSAT2 release-recapture events from subyear, timestep, from-region, to-region, prob p=1, p=2...
	
	// -- Conventional tags --
	init_int nTag;                       // Number of conventional tag observations
	init_matrix Tag(1,nTag,1,10);        // Tag release-recpature from Year/subyear/region/age to Year/subyear/region/fleet/age, N 
		
	// -- Stock of origin --
	init_int nSOOobs;                    // Number of stock of origin observations by stock, year, subyear, region, fleet, N
	init_matrix SOOobs(1,nSOOobs,1,5);   // Stock of origin data
	
	// -- Selectivity controls --
	init_int nsel;                       // Number of estimated selectivities
	init_ivector seltype(1,nsel);        // 2: logistic, 3: Thompson dome-shaped
	init_vector selind(1,nf);            // Which selectivity is assigned to each fleet
	init_vector ratiolim(1,2);           // Limits on logistic slope parameter relative to inflection point
	init_vector infleclim(1,2);          // Limits on modal selectivity 
		
	// -- Movement estimation --
	init_int nMP;                        // Number of movement parameters estimated
	init_int nmovind;                    // Number of non-residency paramters estimated              
	init_matrix movind(1,nmovind,1,4);       // Index of estimable movements (population, subyear, region from, region to)
	init_int nmov1;                      // Number of unestimated zero (logspace) movements
	init_matrix mov1(1,nmov1,1,4);       // Index of first non-estimated movement parameter (fixed to zero)
	init_int movtype;                    // 1: gravity, 2: markov
	
	// -- Observation errors --
	init_vector CobsCV(1,nf);            // Catch observation error   
	init_vector CPUEobsCV(1,nCPUE);      // CPUE observation error
	init_vector IobsCV(1,nI);            // Fishery independent index obs error
		
	// -- Priors --
	init_number RDCV;                    // Recruitment deviation penalty	
				
	// -- Initial values --
	init_vector R0_ini(1,np);                     // unfished recruitment
	init_matrix sel_ini(1,nf,1,nl);               // Selectivity 
	init_matrix selpars_ini(1,nf,1,3);            // Selectivity parameters
	init_vector lnF_ini(1,nCobs);                 // Effort (complexF=0) or log fishing mortality rate (complexF=1) 
	init_matrix ilnRD_ini(1,np,2,na);             // Recruitment deviations (initial)
	init_matrix lnRD_ini(1,np,1,ny);              // Recruitment deviations
	init_4darray mov_ini(1,np,1,ns,1,nr,1,nr);    // Movement parameters
	init_vector lnqCPUE_ini(1,nCPUE);             // q estimates for CPUE fleets
	init_vector lnqI_ini(1,nI);                   // q estimates for Fish. Ind. Indices
     
	// -- Misc --
	init_int complexRD;                        // 0: estimate five year blocks of recruitment 1: estimate all annual recruitment deviations
	init_int complexF;                         // 0: Estimate a q by fleet using C/index as a covariate of effort (nf params e.g. 5) 1: Estimate an F for every Cobs (nCobs params e.g. 1200)
	init_int nF;                               // nCobs if complexF=1, or 1 if complexF=0
	init_int debug;                            // 1= run with initial values
	init_int verbose;                          // 1= print handy text
	init_number datacheck;                     // validates lenght of dat file read, 991199
	int mi; 
		
	
PARAMETER_SECTION
		
	// -- Estimated parameters --
	init_bounded_vector lnR0(1,np,10.,16.5,1);                       // Unfished recruitment
	init_bounded_matrix selpar(1,nsel,1,seltype,-4.,3.,1);       // Selectivity parameters
	//init_bounded_matrix ilnRD(1,np,2,na,-1.,1.,2);               // Recruitment deviations (years prior to initial year)
	init_bounded_matrix lnRD(1,np,1,nRD,-2.,2.,1);                 // Recruitment deviations
	init_bounded_vector movest(1,nMP,-5.,5.,1);                    // Movement parameters
	init_bounded_vector lnqCPUE(1,nCPUE,-8.,-2.3,1);                // q estimates for CPUE fleets UB mu F=0.3
        init_bounded_vector lnqI(1,nI,-2.3,2.3,1);                   // q estimates for Fish. Ind. Indices
	
	// -- Objective function values --
	objective_function_value objG;              // Global objective function value
        number objC;                                // Catch observations
        number objCPUE;                             // Standardized cpue indices 
        number objI;                                // Fishery independent indices
        number objCL;                               // Length composition
        number objSOO;                              // Stock of origin
        number objRD;                               // Recruitment deviations
        number objmov;                              // Priors on movement
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
        vector SSBpR(1,np);                         // Unfished spawning stock biomass per recruit
        matrix iRD(1,np,2,na);                      // Recruitment deviations (years prior to initial year)
	matrix RD(1,np,1,ny);                       // Recruitment deviations
	5darray CTL(1,np,1,ny,1,ns,1,nr,1,nl);      // Catch at length
	5darray CTA(1,np,1,ny,1,ns,1,nr,1,na);      // Catch at age
        
        // -- Exploitation rates --
        vector F(1,nF);                                // Estimated fishing mortality rate
        5darray FAT(1,np,1,ny,1,ns,1,nr,1,na);         // Population level F at Age Total
        5darray FL(1,ny,1,ns,1,nr,1,nf,1,nl);          // Fishing mortality rate at length
        4darray FT(1,ny,1,ns,1,nr,1,nl);               // Total fishing mortality rate at length all fleets
        5darray Z(1,np,1,ny,1,ns,1,na,1,nr);           // Total mortality rate at age
        vector qCPUE(1,nCPUE);                         // Catchability of CPUE indices CPUE=qVB
        vector qI(1,nI);                               // Catchability of fishery independent indices (1) I=qB  (2) I=qSSB                  
        
        // -- Selectivity --
        matrix msel(1,nsel,1,nl);                      // Master (estimated) length selectivities
        matrix sel(1,nf,1,nl);                         // Fleet selectivities assigned from master
        vector spar(1,3);                              // Vector of transformed selectivity parameters
        
        // -- Growth -- 
        matrix wl(1,np,1,nl);                          // Weight at length group
                
        // -- Movement --
        4darray movcalc(1,np,1,ns,1,nr,1,nr); 
        4darray mov(1,np,1,ns,1,nr,1,nr);              // Movement matrix
        
        // -- Tagging --
        5darray RecapP(1,np,1,ns,1,nRPT,1,nr,1,nr);    // Recapture probabilities
        
        // -- Temporary arrays --
        3darray stemp(1,np,1,ns,1,nr);                 // Temporary season index
        vector sind(1,nRPT);                           // Temporary season index for calculation of recapture probabilities
        vector NLtemp(1,nl);                           // Stores a temporary vector of numbers at length (efficiency)
         
        // -- Observations --
        5darray CWpred(1,np,1,ny,1,ns,1,nr,1,nf);      // Catches by fleet (weight) and population
        4darray CWtotpred(1,ny,1,ns,1,nr,1,nf);        // Total catches by weight
        4darray CNpred(1,np,1,ny,1,ns,1,nr);           // Catches by all fleets (numbers) and population
        6darray CLpred(1,np,1,ny,1,ns,1,nr,1,nf,1,nl); // Catches by fleet (weight) and population
        5darray CLtotpred(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet
        5darray CLtotfrac(1,ny,1,ns,1,nr,1,nf,1,nl);   // Catch composition by fleet fraction
       	//sdreport_number R0(1);                       // Requirement of mceval                     


PROCEDURE_SECTION
	
	
	if(debug)cout<<"datacheck: "<<datacheck<<endl;
	
	calcSurvival();         // written
	
	calcMovement();       // written

        calcSelectivities();    // written
	
	assignPars();           // assigns estimates of R0, F, iRD, RD, qCPUE, qI

        if(debug==1)      assignInits(); // Overwrite R0, sel, F, iRD, RD, mov, qCPUE, qI to simulated values
			
	calcF();                // written
	
	initModel();            // written
		
	//calcDynALK();         // later
		
	calcTransitions();      // written
	
	calcRecaptureProb();    // written
	
	calcObjective();	// written
	
	if(verbose)simsam();     // print out simulation versus estimated
		
	if(debug==1) exit(1);


FUNCTION assignPars
  {
	
	R0=mfexp(lnR0);
	qCPUE=mfexp(lnqCPUE);
	
	// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(complexRD){
	 
	  RD=mfexp(lnRD)/mean(mfexp(lnRD));
	  
	}
	else{
	
	  for(int pp=1;pp<=np;pp++){
	  
	    for(int yy=1;yy<=ny;yy++){
	    
	      int ry = RDblock(yy);
	      RD(pp,yy)=mfexp(lnRD(pp,ry));
	      
	    }
	    
	    RD(pp)/=mean(RD(pp));
	    
	  }
	}
	
	iRD=mfexp(ilnRD_ini);
	qI=mfexp(lnqI);
	
	for(int pp=1;pp<=np;pp++){
	
	  wl(pp)=lwa(pp)*pow(ml,lwb(pp));
	
	}
	
	if(debug)cout<<"--- Finished assignPars ---"<<endl;
  }



FUNCTION assignInits
  {
	R0=R0_ini;
	iRD=mfexp(ilnRD_ini);
	RD=mfexp(lnRD_ini);
	if(complexF)F=mfexp(lnF_ini);
	sel=sel_ini;
	mov=mov_ini;
	qCPUE=mfexp(lnqCPUE_ini);
	qI=mfexp(lnqI_ini);
	
	if(debug)cout<<"--- Finished assignInits ---"<<endl;
  }



FUNCTION calcSurvival
  {
	for(int pp=1;pp<=np;pp++){
	 
	  surv(pp,1)=1.;
	  
	  for(int aa=1;aa<=(na-1);aa++){
	    
	    surv(pp,aa+1)=exp(-sum(Ma(pp)(1,aa)));
	  
	  }
	
	}
	
	if(debug)cout<<"--- Finished calcSurvival ---"<<endl;
  }


 

FUNCTION calcMovement
  {
  		
	//  Movement modelling =========================================================
	for(int pp=1;pp<=np;pp++){             // Populations
	  
	  for(int ss=1;ss<=ns;ss++){ 
	    
	    movcalc(pp)(ss)=-10.;                               // Set all movements to be unlikely (logit space)
	  
	  }
	
	}
	
	
	switch(movtype){                           // What type of movement model?
	
	  case 1:                                  // -- Gravity model ----------------------
	    
	    for(int mm=1;mm<=nmov1;mm++){          // Set the first possible movement to be fixed at zero
	    	      
	      int pp=mov1(mm,1);                   // Population
	      int ss=mov1(mm,2);                   // Subyear
	      int tr=mov1(mm,3);                   // To region
	    	
	      for(int fr =1;fr<=nr;fr++){
	    	  
	    	movcalc(pp,ss,fr,tr)=0.;          // Set to zero
	    	
	      }  
	    	
	    }
	   
	    for(int mm=1;mm<=nmovind;mm++){           // over movement params first np*ns are residency viscosity parameters
	      
	      int pp=movind(mm,1);                // Population
	      int ss=movind(mm,2);                // Subyear
	      int tr=movind(mm,3);                // To region
	    
	      for(int fr =1;fr<=nr;fr++){ 
	        
	        movcalc(pp,ss,fr,tr)=movest(mm+np*ns); 
	      
	      }
	    }
	      
	    mi=0;
	    
	    for(int pp =1;pp<=np;pp++){  
		
	      for(int ss =1;ss<=ns;ss++){
	      
	        mi+=1;
	         
	        for(int rr =1;rr<=nr;rr++){ 
	          
	          movcalc(pp,ss,rr,rr)+=mfexp(movest(mi));      // Add residency
	      
	        } // area
	         
	      } // subyear
	      
	    } // pop
	     
	  break;
	  
	  case 2:                                  // -- Fully prescribed movement matrix --     
	   
	   for(int mm=1;mm<=nmov1;mm++){        // Set the first possible movement to be fixed at zero
	      
	      int pp=mov1(mm,1);                   // Population
	      int ss=mov1(mm,2);                   // Subyear
	      int fr=mov1(mm,3);                   // From region
	      int tr=mov1(mm,4);                   // To region
	      movcalc(pp,ss,fr,tr)=0.;                 // Set to zero
	    
	    }
	     
	    for(int mm=1;mm<=nMP;mm++){            // Assign all other logit space movement parameters to the mov array
	      
	      int pp=movind(mm,1);                 // Population         
	      int ss=movind(mm,2);                 // Subyear
	      int fr=movind(mm,3);                 // From region
	      int tr=movind(mm,4);                 // To region
	      movcalc(pp,ss,fr,tr)=movest(mm);         // Set to estimated parameter
	    
	    }
	    
	  break;
	
	}                                          // end of case
	
	mov=exp(movcalc);                          // convert to positive space
	
	for(int pp=1;pp<=np;pp++){                 // Population       
	  
	  for(int ss=1;ss<=ns;ss++){               // Subyear
	    
	    for(int rr = 1; rr<=nr;rr++){          // From region
	      
	      mov(pp)(ss)(rr)/=sum(mov(pp)(ss)(rr));  // Normalize to sum to 1 (inv logit)
            
            }
          
          }
        
        }
  	
	if(debug)cout<<"--- Finished calcMovement ---"<<endl;
  	
  }
	


FUNCTION calcSelectivities
  {
	
	// Selectivity calculations =======================================================
	
	// -- Master selectivities --
	for(int ss=1;ss<=nsel;ss++){ // Loop over estimated selectivities
		  
	  switch(seltype(ss)){
		    
	    case 2: // Logistic selectivity
	     
	      spar(2)=ml(nl)*(0.1+0.7*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));       // Inflection point (2) as a fraction of largest length I(0.1|0.8)
	       
	      spar(1)=spar(2)*(0.01+0.49*(mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)))));   // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
	      
	      for(int ll=1;ll<=nl;ll++){ 
	      
	        msel(ss,ll)=1/(1+mfexp((spar(2)-ml(ll))/spar(1)));
	         
	      }
	    
	    break;
		   
            case 3: // Thompson dome-shaped selectivity
	      
	      spar(1)=0.2*mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)));                 // Dome-shape parameter I(0|0.2)
	      spar(2)=0.1+(0.6*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));         // Precision as the ratio of the inflection point I(0.1|0.7)
	      spar(3)=ml(nl)*(0.1+(0.8*mfexp(selpar(ss,3))/(1+mfexp(selpar(ss,3)))));   // Inflection point as a fraction of largest length I(0.15|0.9)
	     
	      for(int ll=1;ll<=nl;ll++){                                                        // Loop over length classes
	        
	        msel(ss,ll)=(1/(1-spar(1)))*pow(((1-spar(1))/spar(1)),spar(1)) * mfexp(spar(2)*spar(1)*(spar(3)-ml(ll)))/(1+mfexp(spar(2)*(spar(3)-ml(ll))));		
	      
	      }
	       
	    break;
		      
	  }
	          
	}
		
	// -- Fleet specific selectivities --
	  
	for(int ff=1;ff<=nf;ff++){   // Map master selectivities onto fleet-specific selectivities
	  
	  int si=selind(ff);
	  sel(ff)=msel(si);
	
	}
		
	if(debug)cout<<"--- Finished calcSelectivities ---"<<endl;
	
  }	



FUNCTION calcF
  {
  	double tiny=1E-10;	
  	
  	FL.initialize();
  	FT.initialize();
  	FAT.initialize();
  	Z.initialize();
  	
  	if(complexF){
  	  for(int i=1; i<=nCobs; i++){ // Only calculate F's when catches are observed
  	
  	    int yy=Cobs(i,1); // Year
	    int ss=Cobs(i,2); // Subyear
	    int rr=Cobs(i,3); // Region
  	    int ff=Cobs(i,4); // Fleet
  	
  	    FL(yy)(ss)(rr)(ff)= sel(ff)*F(i); // Assign estimated fishing mortality rate-at-length to array
  	
  	  } // Catch observations
  	}else{ // alternatively use an index of fishing effort
  	
  	  for(int i=1;i<=nCPUEobs;i++){
			
	    int yy=CPUEobs(i,1);    // Year
            int ss=CPUEobs(i,2);    // Subyear
            int rr=CPUEobs(i,3);    // Region
	    int ff=CPUEobs(i,4);    // Fleet
	     
	    FL(yy)(ss)(rr)(ff)= sel(ff)*CPUEobs(i,6)*qCPUE(ff);
	    
	  }
	}
  	//cout<<FL(1)(1)(1)<<endl;
  	//exit(1);
        for(int yy=1; yy<=ny;yy++){              // Loop over populations
	  
	  for(int ss=1;ss<=ns;ss++){             // Loop over seasons
	    
	    for(int rr =1;rr<=nr;rr++){          // Loop over years
	      
	      for(int ll=1;ll<=nl;ll++){         // Loop over length bins
	        
	        FT(yy,ss,rr,ll)=tiny;            // Avoids division by zero problems
	        
	        for(int ff=1;ff<=nf;ff++){       // Loop over fleets
	          
	          FT(yy,ss,rr,ll)+=FL(yy,ss,rr,ff,ll); // Summation of F-at-length
	        
	        } // Fleets
	      
	      } // Length classes
	    
	    } // Regions
	  
	  } // Subyears
	
	} // Years
   
        
        // -- Calculate F at age --
	for(int pp=1;pp<=np;pp++){ 
		
	  for(int yy=1;yy<=ny;yy++){  
	          
	    for(int ss=1;ss<=ns;ss++){ 
	      
	      for(int rr=1;rr<=nr;rr++){ 
	      
	        FAT(pp)(yy)(ss)(rr)=FT(yy)(ss)(rr)*trans(ALK(pp)(yy))+tiny; // F(a) = sigma(l) P(l|a)*F(l)
	        
	        for(int aa=1;aa<=na;aa++){
	        
	          Z(pp)(yy)(ss)(aa)(rr)=FAT(pp)(yy)(ss)(rr)(aa)+(Ma(pp,aa)*sdur(ss));  
	        
	        }
	      
	      }
	      
	    }
		    
          }
		  
	}
        
        if(debug)cout<<"--- Finished calcF ---"<<endl;
  
  }



FUNCTION initModel
  {
	double tiny=1E-10;

	N.initialize();
	B.initialize();
	SSB.initialize();
	SSBdist.initialize();
	SSB0.initialize();
	SSBpR.initialize();
	CWtotpred.initialize();
	CWpred.initialize();
	CNpred.initialize();
	CLpred.initialize();
	CLtotpred.initialize();
	CLtotfrac.initialize();
	CTA.initialize();
	VB.initialize();
	
			
	for(int pp=1;pp<=np;pp++){
	
	  SSB0(pp)=sum(elem_prod(surv(pp)*R0(pp),Fec(pp)));     // Unfished Spawning Stock Biomass
	  SSBpR(pp)=SSB0(pp)/R0(pp);                            // Unfished SSB per recruit
	
	}
	
	for(int pp=1;pp<=np;pp++){                              // Loop over populations
	
	  stemp(pp)(ns)=1./nr;                                      // Distribute a fish evenly over regions                     
	  
	  for(int ii=1;ii<=30;ii++){                            // Loop over 50 years
	    
	    for(int ss=1;ss<=ns;ss++){                          // Loop over subyears
	        
	      if(ss==1){                                        // Take bits of fish from previous years final subyear
	      
	        stemp(pp)(1)=stemp(pp)(ns)*mov(pp)(ss);                 // Move them
	      
	      } 	      
	      else{                                             // Take bits of fish from this years' previous subyear
	        
	        stemp(pp)(ss)=stemp(pp)(ss-1)*mov(pp)(ss);              // Move them
	      
	      }
	      
	    } // Subyears
	  
	  } // Years
	  
	  	  
	  for(int rr=1;rr<=nr;rr++){                                                             // Loop over regions
	    
	    // Initial timestep yy=1, ss=1
	   
	    for(int ss=1;ss<=ns;ss++){                                               // Loop over seasons
	  
	      N(pp,1,ss,1,rr)=R0(pp)*RD(pp,1)*surv(pp,1)*stemp(pp,ss,rr); // Stock numbers are spatial distribution multiplied by Survival and R0
	      //CTA(pp,1,ss,rr,1)=N(pp,1,ss,1,rr)*(1-mfexp(-FAT(pp,1,ss,rr,1)));    // Catch at age 
              SSB(pp,1,ss)+=N(pp,1,ss,1,rr)*Fec(pp,1);
	      
	      for(int aa=2;aa<=na;aa++){                                             // Loop over ages
	      
	        N(pp,1,ss,aa,rr)=R0(pp)*iRD(pp,aa)*surv(pp,aa)*stemp(pp,ss,rr);         // Stock numbers are spatial distribution multiplied by Survival and R0
                SSB(pp,1,ss)+=N(pp,1,ss,aa,rr)*Fec(pp,aa);
                //CTA(pp,1,ss,rr,aa)=N(pp,1,ss,aa,rr)*(1-mfexp(-FAT(pp,1,ss,rr,aa)));    // Catch at age 
              
              } // Ages
              
              CTA(pp)(1)(ss)=trans(elem_prod(
	      	      	      elem_prod(N(pp)(1)(ss),mfexp(Z(pp)(1)(ss))-1),
	      	      	      elem_div(trans(FAT(pp)(1)(ss)),Z(pp)(1)(ss))));
	   
              CTL(pp)(1)(ss)(rr)=CTA(pp)(1)(ss)(rr)*ALK(pp)(1); 
              	      
	      for(int aa=1;aa<=na;aa++){
	      
	        NLA(aa)=N(pp)(1)(ss)(aa)(rr)*ALK(pp)(1)(aa);  // Temporarily store numbers at length
	      
	      }
	      
	      NLtemp=colsum(NLA);
	      
	      B(1,ss,rr)+=sum(elem_prod(NLtemp,wl(pp))); // Biomass summed over populations	
	     
	      for(int ff=1;ff<=nf;ff++){
						  
		VB(1,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));                  // Vulnerable biomass summed over populations
		 
	      } // Fleets
	  	
	      for(int ff=1;ff<=nf;ff++){
	  	
	  	for(int ll=1;ll<=nl;ll++){
	          
	          CLpred(pp)(1)(ss)(rr)(ff)(ll)=CTL(pp)(1)(ss)(rr)(ll)*(FL(1)(ss)(rr)(ff)(ll)/FT(1)(ss)(rr)(ll));  // Catch at length by population                                    // Length composition catches
	      	  CLtotpred(1)(ss)(rr)(ff)(ll)+=CLpred(pp)(1)(ss)(rr)(ff)(ll);                                     // Total predicted length composition of catches
	        
	        }
	        
	        CWpred(pp,1,ss,rr,ff)=sum(elem_prod(CLpred(pp)(1)(ss)(rr)(ff),wl(pp))); // Total catch weight by fleet and population
	        CWtotpred(1,ss,rr,ff)+=CWpred(pp,1,ss,rr,ff);                                            // Total predicted catch weight by fleet
	       	CLtotfrac(1)(ss)(rr)(ff)=CLtotpred(1)(ss)(rr)(ff)/(sum(CLtotpred(1)(ss)(rr)(ff))+tiny);
	      
	      } // Fleets
	      
	      CNpred(pp,1,ss,rr)=sum(CTL(pp)(1)(ss)(rr));                                            // total predicted catches (numbers) of all fleets

            } // Seasons          
          
          } // Regions
             
        } // Populations
	
	if(debug)cout<<"--- Finished initModel ---"<<endl;
	
  }


	
FUNCTION calcTransitions
  {
        double tiny=1E-10;
        
        // Order of operations Catch(y-1), M(y-1), move(y), Catch(y), M(y)... 
          
	for(int pp=1;pp<=np;pp++){           // Loop over populations
	  
	  for(int yy=2;yy<=ny;yy++){          // Loop over years
	    
	   
	    for(int ss=1;ss<=ns;ss++){        // Loop over seasons
	        
	      if(ss==1){                      // First subyear isn't a spawning season  ------------------------------------------------
	        
	        for(int aa=1;aa<=na;aa++){ 
	        
	          N(pp)(yy)(1)(aa)=elem_prod(mfexp(-Z(pp)(yy-1)(ns)(aa)),N(pp)(yy-1)(ns)(aa))*mov(pp)(1);
	          for(int rr=1;rr<=nr;rr++){
	          
	            SSB(pp,yy,1)+=N(pp,yy,1,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and regions (sum())
	          
	          }
	        
	        } // Ages
	          
	      }else{                           // Could be a spawning season ------------------------------
	        	      
	        if(ss==spawns(pp)){            // If a spawning season...-------------------------------------------------------------------
	          
	          for(int rr=1;rr<=nr;rr++){
		  	        
		    SSBdist(pp,rr)=0.;               // Reset SSB distribution counter
		  	        
	          }
	          
	          for(int aa=1;aa<=na;aa++){ 
	          
	            N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss);
	            
		    for(int rr=1;rr<=nr;rr++){
		      
		      SSB(pp,yy,ss)+=(N(pp,yy,ss,aa,rr)*Fec(pp,aa)); // SSB is summed (+=) over age classes and regions (sum())
		      SSBdist(pp,rr)+=(N(pp,yy,ss,aa,rr)*Fec(pp,aa));
		    	            
	            }    
		    
		  } 
	         
	          for(int rr=1;rr<=nr;rr++){
	          
	            spawnr(pp,rr)=SSBdist(pp,rr)/sum(SSBdist(pp));
	          
	          }
	                
	          for(int aa=na;aa>=2;aa-=1){ 
		   	        
		    N(pp)(yy)(ss)(aa)=N(pp)(yy)(ss)(aa-1);
		  	        
	          } // Ages
	          
                  N(pp)(yy)(ss)(1)=spawnr(pp)*RD(pp,yy)*(0.8*R0(pp)*steep(pp)*SSB(pp,yy,ss))/(0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * SSB(pp,yy,ss));
	          
	       	          	        
                    
	        }else{                          // Not a spawning season ----------------------------------------------------------------
	        
	          for(int aa=1;aa<=na;aa++){ 
		    
		    N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss); // M then move	        
		    
		    for(int rr=1;rr<=nr;rr++){
		    
		      SSB(pp,yy,ss)+=(N(pp,yy,ss,aa,rr)*Fec(pp,aa)); // SSB is summed (+=) over age classes and regions (sum())
		    
		    }	        
		  
		  } // Ages
	          
	        }  // Not a spawning season
	     
	      } // Could be a spawning season?
	     
	      CTA(pp)(yy)(ss)=trans(elem_prod(
	      elem_prod(N(pp)(yy)(ss),mfexp(Z(pp)(yy)(ss))-1),
	      elem_div(trans(FAT(pp)(yy)(ss)),Z(pp)(yy)(ss))));
	     	
	      for(int rr=1;rr<=nr;rr++){      // Loop over regions
		
		CTL(pp)(yy)(ss)(rr)=CTA(pp)(yy)(ss)(rr)*ALK(pp)(yy); 
		
		for(int aa=1;aa<=na;aa++){
			      
		  NLA(aa)=N(pp)(yy)(ss)(aa)(rr)*ALK(pp)(yy)(aa);  // Temporarily store numbers at length
			      
		}
			      
	        NLtemp=colsum(NLA);
	       
		B(yy,ss,rr)+=sum(elem_prod(NLtemp,wl(pp))); // Biomass summed over populations				  
	      
		for(int ff=1;ff<=nf;ff++){
		   
		  VB(yy,ss,rr,ff)+=sum(elem_prod(elem_prod(NLtemp,wl(pp)),sel(ff)));                          // Vulnerable biomass summed over populations
		  	  
		}
		
		//cout<<"here3 region "<<rr<<endl;
		
		for(int ff=1;ff<=nf;ff++){
		  
		  for(int ll=1;ll<=nl;ll++){
		  	          
		    CLpred(pp)(yy)(ss)(rr)(ff)(ll)=CTL(pp)(yy)(ss)(rr)(ll)*(FL(yy)(ss)(rr)(ff)(ll)/FT(yy)(ss)(rr)(ll));  // Catch at length by population                                    // Length composition catches
		    CLtotpred(yy)(ss)(rr)(ff)(ll)+=CLpred(pp)(yy)(ss)(rr)(ff)(ll);                                       // Total predicted length composition of catches
		  	        
		  }
	         
	       	  CLtotfrac(yy)(ss)(rr)(ff)=CLtotpred(yy)(ss)(rr)(ff)/(sum(CLtotpred(yy)(ss)(rr)(ff))+tiny);
	      	 
	      	  CWpred(pp,yy,ss,rr,ff)=sum(elem_prod(CLpred(pp)(yy)(ss)(rr)(ff),wl(pp)));              // Total catch weight by fleet by population
	      	  CWtotpred(yy,ss,rr,ff)+=CWpred(pp,yy,ss,rr,ff);                                        // Total (over ages and populations) predicted catches by fleet
	      	      	
		} // Fleets
		
		CNpred(pp,yy,ss,rr)=sum(CTL(pp)(yy)(ss)(rr));                                            // Total predicted catch numbers (all fleets)
			    
	      } // Regions
	     
	    } // Subyear
	  	
	  } // Year
	
	  SSBi(pp)=SSB(pp)/(sum(SSB(pp))/(ny*ns));
	
	} // Population (stock)
		
	if(debug)cout<<"--- Finished calcTransitions ---"<<endl;
	
  }



FUNCTION calcRecaptureProb
  {
  	
  	for(int pp=1;pp<=np;pp++){                // Populations
  	  
  	  for(int ss=1; ss<=ns;ss++){             // Subyears
  	     
  	    int s2 = RPTind(ss,2); 
  	    
  	    for(int rr=1;rr<=nr;rr++){            // Regions
  	      
  	      RecapP(pp)(ss)(1)(rr)=0.;           // Set the region vector to all zeros
  	      RecapP(pp,ss,1,rr,rr)=1.;           // Recapture probability is 100% for same area in the same timestep
  	      
  	      //for(int tt=2;tt<=nRPT;tt++){        // Timesteps (incremental subyears)
  	                                              // Retrieve the correct subyear for timestep tt and release subyear ss
  	        RecapP(pp)(ss)(2)(rr)=RecapP(pp)(ss)(1)(rr)*mov(pp)(s2);   // Recalculate recapture probability in next timestep given movement
  	     
  	      //} // timestep (nRPt)
  	    
  	    } // Region
  	  
  	  } // Subyears
  	
  	} // Population
    
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
	objPSAT.initialize();                   // PSAT tags certain stock of origin
	objPSAT2.initialize();                  // PSAT tags w uncertain stock of origin
	
	dvariable LHtemp;                       // Temporary store of the calculated likelihood values
        double tiny=1E-10;
	
	// -- Catch observations --
	
	for(int i=1;i<=nCobs;i++){
	
	  int yy=Cobs(i,1); // Year
	  int ss=Cobs(i,2); // Subyear
	  int rr=Cobs(i,3); // Region
  	  int ff=Cobs(i,4); // Fleet
  	  
  	  LHtemp=dnorm(log(CWtotpred(yy,ss,rr,ff)+tiny),log(Cobs(i,5)+tiny),CobsCV(ff)); // Log-normal LHF
  	  objC+=LHtemp/10;
  	  objG+=LHtemp/10;
  	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" Cobs="<<Cobs(i,5)<<" Cpred="<<CWtotpred(yy,ss,rr,ff)<<endl;
	}
	//exit(1);
	if(debug)cout<<"---  * Finished Catch LHF ---"<<endl;
	
	// -- CPUE observations --
	
	dvariable CPUEtemp;
	
	if(complexF){
	  
	  for(int i=1;i<=nCPUEobs;i++){
		
	    int yy=CPUEobs(i,1);    // Year
	    int ss=CPUEobs(i,2);    // Subyear
	    int rr=CPUEobs(i,3);    // Region
	    int ff=CPUEobs(i,4);    // index ID
	    int CPUEi=CPUEobs(i,5); // q index
	    	  
	    CPUEtemp=VB(yy,ss,rr,ff)*qCPUE(CPUEi);
	    LHtemp=dnorm(log(CPUEtemp+tiny),log(CPUEobs(i,6)+tiny),CPUEobsCV(CPUEi)); // Log-normal LHF
	    objCPUE+=LHtemp;
	    objG+=LHtemp;
	  	  
	  }
	
	  if(debug)cout<<"---  * Finished CPUE LHF ---"<<endl;
	   
	}
	
	// -- Fishery independent indices --
	
	dvariable Itemp;
	
	for(int i=1; i<=nIobs;i++){
	
	  int yy=Iobs(i,1);   // Year
	  int ss=Iobs(i,2);   // Subyear
	  int rr=Iobs(i,3);   // Region
	  int pp=Iobs(i,4);   // population
	  int ii=Iobs(i,5);   // q index (often population for SSB types)
	  int tt=Iobs(i,6);   // Type
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" q index ="<<ii<<" type="<<tt<<endl;
	
	  switch(tt){
	  		    
	    case 1:  // Biomass
	  	      
	      Itemp= B(yy,ss,rr)*qI(ii);     
	  	    
	      break;
	  		   
	    case 2:  // SSB
	  	      
	      
	      Itemp=qI(ii)*(SSB(pp,yy,ss)/mean(extract_row(trans(SSB(pp)),ss)));  
	         	      
	    break;
	  		      
	  }
	  
	  LHtemp=dnorm(log(Itemp+tiny),log(Iobs(i,7)+tiny),IobsCV(ii)); // Log-normal LHF
	  objI+=LHtemp;
	  objG+=LHtemp;
	  	          
	}
	
	if(debug)cout<<"---  * Finished FI index LHF ---"<<endl;
	
	// -- Length composition data --  
	
	for(int i=1;i<=nCLobs;i++){
	
	  int yy=CLobs(i,1);   // Year
	  int ss=CLobs(i,2);   // Subyear
	  int rr=CLobs(i,3);   // Region
	  int ff=CLobs(i,4);   // Fleet type
	  int ll=CLobs(i,5);   // Length class
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" l="<<ll<<" CLobs="<<CLobs(i,6)<<" CLpred="<<CLtotpred(yy,ss,rr,ff,ll)<<endl;
	  
	  LHtemp=(-CLobs(i,6)*log(CLtotfrac(yy,ss,rr,ff,ll)+tiny)); // Multinomial LHF
	  objCL+=LHtemp/1000;
	  objG+=LHtemp/1000;
	
	}
		if(debug)cout<<"---  * Finished length composition LHF ---"<<endl;
	
	
	// -- Stock of origin data -- 
	
	dvariable SOOpred;   // There aren't that many stock of origin observations so it involves less computation to do this as needed
	dvariable SOOtot;
	SOOtot.initialize();
	
	for(int i=1;i<=nSOOobs;i++){
	  
	  int pp=SOOobs(i,1);   // Population
	  int yy=SOOobs(i,2);   // Year
	  int ss=SOOobs(i,3);   // Subyear
	  int rr=SOOobs(i,4);   // Region
	 
	  SOOtot=0.;
	  
	  for(int pp=1;pp<=np;pp++){
	  
	    SOOtot+=CNpred(pp,yy,ss,rr);
	    
	  }
	  
	  SOOpred=CNpred(pp,yy,ss,rr)/(SOOtot+tiny);
	  
	  LHtemp=(-SOOobs(i,5)*log(SOOpred+tiny)); // Multinomial LHF
	  objSOO+=LHtemp;
	  objG+=LHtemp;
	
	}
	
	if(debug)cout<<"---  * Finished SOO LHF ---"<<endl;
	
	
	// -- PSAT tagging --
	
	for(int i=1;i<=nPSAT;i++){ // Tags with certain stock of origin
	
	  int pp=PSAT(i,1);   // Population
	  int ss=PSAT(i,2);   // Subyear
	  int tt=PSAT(i,3);   // Time at liberty (subyears)
	  int rr=PSAT(i,4);   // Region from
	  int r2=PSAT(i,5);   // Region to
	  
	  LHtemp=(-PSAT(i,6)*log(RecapP(pp,ss,tt,rr,r2)+tiny)); // Multinomial LHF
	  objPSAT+=LHtemp;
	  objG+=LHtemp;
	
	}
	
	if(debug)cout<<"---  * Finished PSAT known SOO LHF ---"<<endl;
	
	
	for(int i=1;i<=nPSAT2;i++){ // Individual tags with uncertain stock of origin
		
          int ss=PSAT2(i,1);   // Year
	  int tt=PSAT2(i,2);   // Subyear
	  int rr=PSAT2(i,3);   // Region from
	  int r2=PSAT2(i,4);   // Region to
	  
	  for(int pp=1;pp<=np;pp++){
	  
	    LHtemp=(-log(RecapP(pp,ss,tt,rr,r2)+tiny)*PSAT2(i,4+pp)); // Weighted multinomial LHF
	    objPSAT2+=LHtemp;
	    //objG+=LHtemp;
	  
	  }
			
	}
	
	if(debug)cout<<"---  * Finished PSAT unknown SOO LHF ---"<<endl;
	
	
	// -- Recruitment deviations --
	
	for(int pp=1;pp<=np;pp++){
	
	  /*for(int aa=2;aa<=na;aa++){
	  
	    LHtemp=dnorm(ilnRD(pp,aa),0.,RDCV);
	    objRD+=LHtemp;
	    objG+=LHtemp;
	    
	  }*/
	  
	  for(int yy=1;yy<=nRD;yy++){
	    
	    LHtemp=dnorm(lnRD(pp,yy),0.,RDCV);
	    objRD+=LHtemp;
	    objG+=LHtemp;
	
	  }
	
        }  
	
	// -- Movement parameters ---
	
	for(int mm=1;mm<=nMP;mm++){
	     
	  LHtemp=dnorm(mfexp(movest(mm)),-5.,3.);
          objmov+=LHtemp;
	  objG+=LHtemp;
	  
	}
	
	for(int i=1;i<=nsel;i++){
	
	  objG+=dnorm(selpar(i),0,0.5);
	
	}
	
	if(debug)cout<<"---  * Finished rec dev penalty ---"<<endl;
	
	//objG+=dnorm(lnF(1),log(0.1),0.2); // Temporary fix to allow F estimation to be simplied to F=qE
	
	if(debug)cout<<"--- Finished calcObjective ---"<<endl;
	
	if(verbose)cout<<"Catch LHF "<<objC<<endl;
	if(verbose)cout<<"CPUE LHF "<<objCPUE<<endl;
	if(verbose)cout<<"FI index LHF "<<objI<<endl;
	if(verbose)cout<<"Length comp LHF "<<objCL<<endl;
        if(verbose)cout<<"SOO LHF "<<objSOO<<endl;
	if(verbose)cout<<"PSAT LHF "<<objPSAT<<endl;
	if(verbose)cout<<"PSAT uSOO LHF "<<objPSAT2<<endl;
	if(verbose)cout<<"Rec dev LHF "<<objRD<<endl;
	if(verbose)cout<<"Global objective "<<objG<<endl;
	
	
	
  }
	


FUNCTION simsam
  {
        
        cout<<"R0 sim = "<<log(R0_ini)<<endl;
        cout<<"R0 sam = "<<log(R0)<<endl;
        cout<<"sel sim f1= "<<sel_ini(1)<<endl;
        cout<<"sel sam f1= "<<sel(1)<<endl;
        cout<<"sel sim f2= "<<sel_ini(2)<<endl;
	cout<<"sel sam f2= "<<sel(2)<<endl;
        
        //init_vector lnF_ini(1,nCobs);                 // Effort (complexF=0) or log fishing mortality rate (complexF=1) 
	
	cout<<"RDi sim= "<<exp(ilnRD_ini(1))<<endl;
	cout<<"RDi sam= "<<iRD(1)<<endl;
	cout<<"RD sim= "<<exp(lnRD_ini(1))<<endl;
        cout<<"RD sam= "<<RD(1)<<endl;
	cout<<"mov sim p1 s1= "<<endl;
	cout<<mov_ini(1)(1)<<endl;
	cout<<"mov sam p1 s1= "<<endl;
	cout<<mov(1)(1)<<endl;
	cout<<"mov sim p2 s1= "<<endl;
	cout<<mov_ini(2)(1)<<endl;
	cout<<"mov sam p2 s1= "<<endl;
	cout<<mov(2)(1)<<endl;
	cout<<"qCE sim= "<< exp(lnqCPUE_ini)<<endl;
	cout<<"qCE sam= "<< qCPUE<<endl;
	cout<<"qI sim= "<<exp(lnqI_ini)<<endl;
	cout<<"qI sam= "<<qI<<endl;
	
  }

REPORT_SECTION
  {
  	report <<"np, number of populations/stocks"<<endl;
  	report <<np<<endl;
  	
  	report <<"ny, number of years"<<endl;
  	report <<ny<<endl;
  	
  	report <<"ns, numnber of subyears"<<endl;
  	report <<ns<<endl;
  	
  	report <<"nr, number of regions/areas"<<endl;
  	report <<nr<<endl;
  	
  	report <<"nf, number of fleets"<<endl;
  	report <<nf<<endl;
  	
  	report <<"na, number of age classes"<<endl;
  	report <<na<<endl;
  	
  	report <<"nl, number of length classes"<<endl;
  	report <<nl<<endl;
  	
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
  	
  	report <<"mov (p,s,r,r) Markov movement matrix"<<endl;
  	report <<mov<<endl;
  	
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
       	
	
	

