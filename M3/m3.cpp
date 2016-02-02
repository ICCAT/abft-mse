        
        #include <admodel.h>
	#include "stats.cxx"
	//#include <fstream>
        //ofstream nodesout("nodes.cha");
       	
	
	
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <m3.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  ny.allocate("ny");
  ns.allocate("ns");
  np.allocate("np");
  na.allocate("na");
  nr.allocate("nr");
  nf.allocate("nf");
  nl.allocate("nl");
  nRPT.allocate("nRPT");
  RPTind.allocate(1,ns,1,nRPT,"RPTind");
  sdur.allocate(1,ns,"sdur");
  nZeq.allocate("nZeq");
  nydist.allocate("nydist");
  nyeq.allocate("nyeq");
  ml.allocate(1,nl,"ml");
  RDblock.allocate(1,ny,"RDblock");
  nRD.allocate("nRD");
  ALK.allocate(1,np,1,ny,1,na,1,nl,"ALK");
  lwa.allocate(1,np,"lwa");
  lwb.allocate(1,np,"lwb");
  len_age.allocate(1,ny,1,na,1,np,"len_age");
  wt_age.allocate(1,ny,1,na,1,np,"wt_age");
  Fec.allocate(1,np,1,na,"Fec");
  steep.allocate(1,np,"steep");
  spawns.allocate(1,np,"spawns");
  Ma.allocate(1,np,1,na,"Ma");
  nCobs.allocate("nCobs");
  Cobs.allocate(1,nCobs,1,5,"Cobs");
  nCPUE.allocate("nCPUE");
  nCPUEobs.allocate("nCPUEobs");
  CPUEobs.allocate(1,nCPUEobs,1,6,"CPUEobs");
  nCLobs.allocate("nCLobs");
  CLobs.allocate(1,nCLobs,1,6,"CLobs");
  RAI.allocate(1,nr,1,ns,1,ny,"RAI");
  nI.allocate("nI");
  nIobs.allocate("nIobs");
  Iobs.allocate(1,nIobs,1,7,"Iobs");
  nPSAT.allocate("nPSAT");
  PSAT.allocate(1,nPSAT,1,7,"PSAT");
  nPSAT2.allocate("nPSAT2");
  PSAT2.allocate(1,nPSAT2,1,5+np,"PSAT2");
  nTag.allocate("nTag");
  Tag.allocate(1,nTag,1,10,"Tag");
  nSOOobs.allocate("nSOOobs");
  SOOobs.allocate(1,nSOOobs,1,5,"SOOobs");
  nsel.allocate("nsel");
  seltype.allocate(1,nsel,"seltype");
  selind.allocate(1,nf,"selind");
  ratiolim.allocate(1,2,"ratiolim");
  infleclim.allocate(1,2,"infleclim");
  nMP.allocate("nMP");
  nma.allocate("nma");
  ma.allocate(1,np,1,na,"ma");
  nmovind.allocate("nmovind");
  movind.allocate(1,nmovind,1,5,"movind");
  nmov1.allocate("nmov1");
  mov1.allocate(1,nmov1,1,5,"mov1");
  movtype.allocate("movtype");
  CobsCV.allocate(1,nf,"CobsCV");
  CPUEobsCV.allocate(1,nCPUE,"CPUEobsCV");
  IobsCV.allocate(1,nI,"IobsCV");
  RDCV.allocate("RDCV");
  nLHw.allocate("nLHw");
  LHw.allocate(1,nLHw,"LHw");
  R0_ini.allocate(1,np,"R0_ini");
  sel_ini.allocate(1,nf,1,nl,"sel_ini");
  selpars_ini.allocate(1,nf,1,3,"selpars_ini");
  lnF_ini.allocate(1,nCobs,"lnF_ini");
  ilnRD_ini.allocate(1,np,2,na,"ilnRD_ini");
  lnRD_ini.allocate(1,np,1,ny,"lnRD_ini");
  mov_ini.allocate(1,np,1,ns,1,na,1,nr,1,nr,"mov_ini");
  lnqCPUE_ini.allocate(1,nCPUE,"lnqCPUE_ini");
  lnqI_ini.allocate(1,nI,"lnqI_ini");
  complexRD.allocate("complexRD");
  complexF.allocate("complexF");
  nF.allocate("nF");
  debug.allocate("debug");
  verbose.allocate("verbose");
  datacheck.allocate("datacheck");
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  lnR0.allocate(1,np,10.,16.5,1,"lnR0");
  selpar.allocate(1,nsel,1,seltype,-4.,3.,1,"selpar");
  lnRD.allocate(1,np,1,nRD,-2.,2.,1,"lnRD");
  movest.allocate(1,nMP,-5.,5.,1,"movest");
  lnqCPUE.allocate(1,nCPUE,-8.,-2.3,1,"lnqCPUE");
  lnqI.allocate(1,nI,-2.3,2.3,1,"lnqI");
  objG.allocate("objG");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  objC.allocate("objC");
  #ifndef NO_AD_INITIALIZE
  objC.initialize();
  #endif
  objCPUE.allocate("objCPUE");
  #ifndef NO_AD_INITIALIZE
  objCPUE.initialize();
  #endif
  objI.allocate("objI");
  #ifndef NO_AD_INITIALIZE
  objI.initialize();
  #endif
  objCL.allocate("objCL");
  #ifndef NO_AD_INITIALIZE
  objCL.initialize();
  #endif
  objSOO.allocate("objSOO");
  #ifndef NO_AD_INITIALIZE
  objSOO.initialize();
  #endif
  objRD.allocate("objRD");
  #ifndef NO_AD_INITIALIZE
  objRD.initialize();
  #endif
  objmov.allocate("objmov");
  #ifndef NO_AD_INITIALIZE
  objmov.initialize();
  #endif
  objPSAT.allocate("objPSAT");
  #ifndef NO_AD_INITIALIZE
  objPSAT.initialize();
  #endif
  objPSAT2.allocate("objPSAT2");
  #ifndef NO_AD_INITIALIZE
  objPSAT2.initialize();
  #endif
  N.allocate(1,np,1,ny,1,ns,1,na,1,nr,"N");
  #ifndef NO_AD_INITIALIZE
    N.initialize();
  #endif
  NLA.allocate(1,na,1,nl,"NLA");
  #ifndef NO_AD_INITIALIZE
    NLA.initialize();
  #endif
  surv.allocate(1,np,1,na,"surv");
  #ifndef NO_AD_INITIALIZE
    surv.initialize();
  #endif
  SSB.allocate(1,np,1,ny,1,ns,"SSB");
  #ifndef NO_AD_INITIALIZE
    SSB.initialize();
  #endif
  SSBi.allocate(1,np,1,ny,1,ns,"SSBi");
  #ifndef NO_AD_INITIALIZE
    SSBi.initialize();
  #endif
  SSBdist.allocate(1,np,1,nr,"SSBdist");
  #ifndef NO_AD_INITIALIZE
    SSBdist.initialize();
  #endif
  spawnr.allocate(1,np,1,nr,"spawnr");
  #ifndef NO_AD_INITIALIZE
    spawnr.initialize();
  #endif
  VB.allocate(1,ny,1,ns,1,nr,1,nf,"VB");
  #ifndef NO_AD_INITIALIZE
    VB.initialize();
  #endif
  B.allocate(1,ny,1,ns,1,nr,"B");
  #ifndef NO_AD_INITIALIZE
    B.initialize();
  #endif
  R0.allocate(1,np,"R0");
  #ifndef NO_AD_INITIALIZE
    R0.initialize();
  #endif
  SSB0.allocate(1,np,"SSB0");
  #ifndef NO_AD_INITIALIZE
    SSB0.initialize();
  #endif
  SSBpR.allocate(1,np,"SSBpR");
  #ifndef NO_AD_INITIALIZE
    SSBpR.initialize();
  #endif
  iRD.allocate(1,np,2,na,"iRD");
  #ifndef NO_AD_INITIALIZE
    iRD.initialize();
  #endif
  RD.allocate(1,np,1,ny,"RD");
  #ifndef NO_AD_INITIALIZE
    RD.initialize();
  #endif
  CTL.allocate(1,np,1,ny,1,ns,1,nr,1,nl,"CTL");
  #ifndef NO_AD_INITIALIZE
    CTL.initialize();
  #endif
  CTA.allocate(1,np,1,ny,1,ns,1,nr,1,na,"CTA");
  #ifndef NO_AD_INITIALIZE
    CTA.initialize();
  #endif
  F.allocate(1,nF,"F");
  #ifndef NO_AD_INITIALIZE
    F.initialize();
  #endif
  FAT.allocate(1,np,1,ny,1,ns,1,nr,1,na,"FAT");
  #ifndef NO_AD_INITIALIZE
    FAT.initialize();
  #endif
  FL.allocate(1,ny,1,ns,1,nr,1,nf,1,nl,"FL");
  #ifndef NO_AD_INITIALIZE
    FL.initialize();
  #endif
  FT.allocate(1,ny,1,ns,1,nr,1,nl,"FT");
  #ifndef NO_AD_INITIALIZE
    FT.initialize();
  #endif
  Z.allocate(1,np,1,ny,1,ns,1,na,1,nr,"Z");
  #ifndef NO_AD_INITIALIZE
    Z.initialize();
  #endif
  Zeq.allocate(1,np,1,ns,1,na,1,nr,"Zeq");
  #ifndef NO_AD_INITIALIZE
    Zeq.initialize();
  #endif
  qCPUE.allocate(1,nCPUE,"qCPUE");
  #ifndef NO_AD_INITIALIZE
    qCPUE.initialize();
  #endif
  qI.allocate(1,nI,"qI");
  #ifndef NO_AD_INITIALIZE
    qI.initialize();
  #endif
  msel.allocate(1,nsel,1,nl,"msel");
  #ifndef NO_AD_INITIALIZE
    msel.initialize();
  #endif
  sel.allocate(1,nf,1,nl,"sel");
  #ifndef NO_AD_INITIALIZE
    sel.initialize();
  #endif
  spar.allocate(1,3,"spar");
  #ifndef NO_AD_INITIALIZE
    spar.initialize();
  #endif
  wl.allocate(1,np,1,nl,"wl");
  #ifndef NO_AD_INITIALIZE
    wl.initialize();
  #endif
  movcalc.allocate(1,np,1,ns,1,nma,1,nr,1,nr,"movcalc");
  #ifndef NO_AD_INITIALIZE
    movcalc.initialize();
  #endif
  movm.allocate(1,np,1,ns,1,nma,1,nr,1,nr,"movm");
  #ifndef NO_AD_INITIALIZE
    movm.initialize();
  #endif
  mov.allocate(1,np,1,ns,1,na,1,nr,1,nr,"mov");
  #ifndef NO_AD_INITIALIZE
    mov.initialize();
  #endif
  RecapP.allocate(1,np,1,ns,1,nma,1,nRPT,1,nr,1,nr,"RecapP");
  #ifndef NO_AD_INITIALIZE
    RecapP.initialize();
  #endif
  stemp.allocate(1,np,1,ns,1,nr,"stemp");
  #ifndef NO_AD_INITIALIZE
    stemp.initialize();
  #endif
  sind.allocate(1,nRPT,"sind");
  #ifndef NO_AD_INITIALIZE
    sind.initialize();
  #endif
  NLtemp.allocate(1,nl,"NLtemp");
  #ifndef NO_AD_INITIALIZE
    NLtemp.initialize();
  #endif
  CWpred.allocate(1,np,1,ny,1,ns,1,nr,1,nf,"CWpred");
  #ifndef NO_AD_INITIALIZE
    CWpred.initialize();
  #endif
  CWtotpred.allocate(1,ny,1,ns,1,nr,1,nf,"CWtotpred");
  #ifndef NO_AD_INITIALIZE
    CWtotpred.initialize();
  #endif
  CNpred.allocate(1,np,1,ny,1,ns,1,nr,"CNpred");
  #ifndef NO_AD_INITIALIZE
    CNpred.initialize();
  #endif
  CLpred.allocate(1,np,1,ny,1,ns,1,nr,1,nf,1,nl,"CLpred");
  #ifndef NO_AD_INITIALIZE
    CLpred.initialize();
  #endif
  CLtotpred.allocate(1,ny,1,ns,1,nr,1,nf,1,nl,"CLtotpred");
  #ifndef NO_AD_INITIALIZE
    CLtotpred.initialize();
  #endif
  CLtotfrac.allocate(1,ny,1,ns,1,nr,1,nf,1,nl,"CLtotfrac");
  #ifndef NO_AD_INITIALIZE
    CLtotfrac.initialize();
  #endif
}

void model_parameters::userfunction(void)
{
  objG =0.0;
	if(debug)cout<<"datacheck: "<<datacheck<<endl; // Were the data the correct length?
	calcSurvival();                  // Calculate survival
	calcMovement();                  // Calculate movement
        calcSelectivities();             // Calculate selectivities
	assignPars();                    // Assigns estimates of R0, F, iRD, RD, qCPUE, qI
        if(debug==1)	assignInits();   // Overwrite R0, sel, F, iRD, RD, mov, qCPUE, qI to simulated values
	calcF();                         // Calculate fishing mortality rate at age / length
	initModel();                     // Initialize the model (numbers / catches in first year)
	//calcDynALK();                  // Dynamically calculate inverse age-length key based on predicted fishing mortality rate
	calcTransitions();               // Move/kill/reproduce fish over model years
	calcRecaptureProb();             // Calcualte recapture probabilities
	calcObjective();	         // Calculate objective function
	if(verbose)simsam();             // Print out simulated values versus estimated values for each function evaluation
	if(debug==1) exit(1);            // Exit prior to first function evaluation if in debugging mode
}

void model_parameters::assignPars(void)
{
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
}

void model_parameters::assignInits(void)
{
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
}

void model_parameters::calcSurvival(void)
{
  {
	// -- Calculate survival --
	for(int pp=1;pp<=np;pp++){                   // Loop over stocks      
	  surv(pp,1)=1.;                             // Survival to age 1 is 100%
	  for(int aa=1;aa<=(na-1);aa++){             // Loop over age classes
	    surv(pp,aa+1)=exp(-sum(Ma(pp)(1,aa)));   // Calculate survivial
	  }                                          // End of age classes
	}                                            // End of stocks
	if(debug)cout<<"--- Finished calcSurvival ---"<<endl;
  }
}

void model_parameters::calcMovement(void)
{
  {
	//  -- Movement modelling -----------
	for(int pp=1;pp<=np;pp++){                 // Loop over stocks
	  for(int ss=1;ss<=ns;ss++){               // Loop over subyears
	    movcalc(pp)(ss)=-10.;                  // Set all movements to be unlikely (logit space)
	  }                                        // End of subyears
	}                                          // End of stock
	switch(movtype){                           // What type of movement model?
	  case 1:                                  // -- Gravity model ---------------------------------------------------------------------
	    for(int mm=1;mm<=nmov1;mm++){          // Set the first possible movement to be fixed at zero
	      int pp=mov1(mm,1);                   // Stock
	      int ss=mov1(mm,2);                   // Subyear
	      int aa=mov1(mm,3);                   // Movement age class
	      int tr=mov1(mm,4);                   // To area
	      for(int fr=1;fr<=nr;fr++){           // Loop over from-areas
	    	movcalc(pp,ss,aa,fr,tr)=0.;        // Set to zero
	      }                                    // End of from-areas
	    }                                      // End of first possible movement
	    for(int mm=1;mm<=nmovind;mm++){        // Loop over estimated movement params (first np*ns*nma are residency viscosity parameters)
	      int pp=movind(mm,1);                 // Stock
	      int ss=movind(mm,2);                 // Subyear
	      int aa=movind(mm,3);                 // Movement age class
	      int tr=movind(mm,4);                 // To area
	      for(int fr=1;fr<=nr;fr++){           // Loop over from-areas
	        movcalc(pp,ss,aa,fr,tr)=movest(mm+np*ns*nma); // Assign estimated parameter
	      }                                    // End of from-areas
	    }                                      // End of estimated movements
	    mi=0;
	    for(int pp=1;pp<=np;pp++){                           // Loop over stocks
	      for(int ss=1;ss<=ns;ss++){                         // Loop over subyears
	        mi+=1;                                           // Keep track of viscosity parameter number
	        for(int aa=1;aa<=nma;aa++){                      // Loop over age classes
	          for(int rr=1;rr<=nr;rr++){                     // Loop over areas
	            movcalc(pp,ss,aa,rr,rr)+=mfexp(movest(mi));      // Add viscosity
	          }
	        }                                               // End of age class
	      }                                                 // End of subyear
	    }                                                   // End of stock 
	  break;                                   // End of gravity model
	  case 2:                                  // -- Fully prescribed (Markov) movement matrix ------------------------------------------     
	    for(int mm=1;mm<=nmov1;mm++){          // Set the first possible movement to be fixed at zero
	      int pp=mov1(mm,1);                   // Stock
	      int ss=mov1(mm,2);                   // Subyear
	      int aa=mov1(mm,3);                   // Movement age class
	      int fr=mov1(mm,4);                   // From area
	      int tr=mov1(mm,5);                   // To area
	      movcalc(pp,ss,aa,fr,tr)=0.;             // Set to zero
	    }
	    for(int mm=1;mm<=nMP;mm++){            // Assign all other logit space movement parameters to the mov array
	      int pp=movind(mm,1);                 // Stock         
	      int ss=movind(mm,2);                 // Subyear
	      int aa=mov1(mm,3);                   // Movement age class
	      int fr=movind(mm,4);                 // From area
	      int tr=movind(mm,5);                 // To area
	      movcalc(pp,ss,aa,fr,tr)=movest(mm);  // Set to estimated parameter
	    }
	  break;                                   // End of fully prescribed (Markov) movement matrix
	  case 3:                                  // -- Fractional model (essentially gravity model with no viscosity parameter) -----------
	    for(int mm=1;mm<=nmov1;mm++){          // Set the first possible movement to be fixed at zero
	      int pp=mov1(mm,1);                   // Stock
	      int ss=mov1(mm,2);                   // Subyear
	      int aa=mov1(mm,3);                   // Movement age class
	      int tr=mov1(mm,4);                   // To area
	      for(int fr =1;fr<=nr;fr++){          // Loop over from-areas
		movcalc(pp,ss,aa,fr,tr)=0.;           // Set to zero
	      }                                    // End of from-areas
	    }                                      // End of first possible movement 
	    for(int mm=1;mm<=nmovind;mm++){        // Other possible movements are set to estimated parameters
	      int pp=movind(mm,1);                 // Stock
	      int ss=movind(mm,2);                 // Subyear
	      int aa=mov1(mm,3);                   // Movement age class
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
}

void model_parameters::calcSelectivities(void)
{
  {
	// Selectivity calculations =======================================================
	// -- Master selectivities (can be mirrored across fleets) --
	for(int ss=1;ss<=nsel;ss++){ // Loop over estimated selectivities
	  switch(seltype(ss)){       // Cases match number of estimated parameters for simplicity
	    case 2: // Logistic selectivity
	      spar(2)=ml(nl)*(0.1+0.7*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));        // Inflection point (2) as a fraction of largest length I(0.1|0.8)
	      spar(1)=spar(2)*(0.01+0.49*(mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)))));   // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	        msel(ss,ll)=1/(1+mfexp((spar(2)-ml(ll))/spar(1)));                         // Logistic selectivity function
	      }                                                                            // End of length classes
	    break;                                                                         // End of logistic selectivity
            case 3: // Thompson dome-shaped selectivity
	      spar(1)=0.2*mfexp(selpar(ss,1))/(1+mfexp(selpar(ss,1)));                     // Dome-shape parameter I(0|0.2)
	      spar(2)=0.1+(0.6*mfexp(selpar(ss,2))/(1+mfexp(selpar(ss,2))));               // Precision as the ratio of the inflection point I(0.1|0.7)
	      spar(3)=ml(nl)*(0.1+(0.8*mfexp(selpar(ss,3))/(1+mfexp(selpar(ss,3)))));      // Inflection point as a fraction of largest length I(0.15|0.9)
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
}

void model_parameters::calcF(void)
{
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
	            Zeq(pp)(ss)(aa)(rr)+=Z(pp)(yy)(ss)(aa)(rr); // Sum up equilibrium Z
	          }
	        }                                      // End of ages
	      }                                        // End of areas
	    }                                          // End of subyears
          }                                            // End of years
	}                                              // End of stocks
        Zeq/=nZeq;                                     // Divide by number of equilibrium Z years to calculate mean equilibrium Z
        if(debug)cout<<"--- Finished calcF ---"<<endl;
  }
}

void model_parameters::initModel(void)
{
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
	for(int pp=1;pp<=np;pp++){
	  SSB0(pp)=sum(elem_prod(surv(pp)*R0(pp),Fec(pp)));     // Unfished Spawning Stock Biomass
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
	  // -- Spool up to equilibrium given total mortality rate over first nZeq years -----------
	  for(int rr=1;rr<=nr;rr++){                                // Loop over areas
	    for(int ss=1;ss<=ns;ss++){                              // Loop over subyears
	      N(pp,1,ns,1,rr)=R0(pp)*surv(pp,1)*stemp(pp,ns,rr);    // Stock numbers are spatial distribution multiplied by Survival and R0
	      SSB(pp,1,ns)+=N(pp,1,ns,1,rr)*Fec(pp,1);              // Spawning stock biomass
	    }                                                       // End of subyears   
	  }                                                         // End of areas 
	  for(int yy=1;yy<=nyeq;yy++){         // Loop over equilibrium years
	    for(int ss=1;ss<=ns;ss++){         // Loop over seasons
	      if(ss==1){                       // First subyear isn't a spawning season  ------------------------------------------------
		for(int aa=1;aa<=na;aa++){     // Loop over age classes
		  N(pp)(1)(1)(aa)=elem_prod(mfexp(-Zeq(pp)(ns)(aa)),N(pp)(1)(ns)(aa))*mov(pp)(1)(aa); // Mortality then move
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
		    SSB(pp,1,1)+=N(pp,1,1,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
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
		      SSB(pp,1,ss)+=(N(pp,1,ss,aa,rr)*Fec(pp,aa));    // SSB is summed (+=) over age classes and areas (sum())
		      SSBdist(pp,rr)+=(N(pp,1,ss,aa,rr)*Fec(pp,aa));  // The distribution of SSB among areas
		    }                          // End of areas
	          }                            // End of age classes
		  for(int rr=1;rr<=nr;rr++){   // Loop over areas
		    spawnr(pp,rr)=SSBdist(pp,rr)/sum(SSBdist(pp));  // Calculate spawning fraction of SSB
		  }
		  N(pp)(yy)(ss)(na)+=N(pp)(yy)(ss)(na-1);          // Plus group
		  for(int aa=(na-1);aa>=2;aa-=1){                  // Loop down age classes from plusgroup(ns)-1
	            N(pp)(yy)(ss)(aa)=N(pp)(yy)(ss)(aa-1);         // Age fish
		  }                                                // End of ages
	          N(pp)(1)(ss)(1)=spawnr(pp)*(0.8*R0(pp)*steep(pp)*SSB(pp,1,ss))/
	                          (0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * SSB(pp,1,ss)); // Recruitment
		}else{                          // Not a spawning season ----------------------------------------------------------------
		  for(int aa=1;aa<=na;aa++){    // Loop over age classes
		    N(pp)(1)(ss)(aa)=elem_prod(mfexp(-Zeq(pp)(ss-1)(aa)),N(pp)(1)(ss-1)(aa))*mov(pp)(ss)(aa); // M then move	        
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
		      SSB(pp,1,ss)+=(N(pp,1,ss,aa,rr)*Fec(pp,aa)); // SSB is summed (+=) over age classes and areas (sum())
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
	      	      	      elem_prod(N(pp)(1)(ss),mfexp(Z(pp)(1)(ss))-1),
	      	      	      elem_div(trans(FAT(pp)(1)(ss)),Z(pp)(1)(ss))));    // total catch at age in first year
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
}

void model_parameters::calcTransitions(void)
{
  {
        // Order of calculations Catch(y-1), M(y-1), move(y), Catch(y), M(y), mov(y+1)... 
        double tiny=1E-10;                    // Create a small constant to avoid the log(0) error.     
	for(int pp=1;pp<=np;pp++){            // Loop over stocks
	  for(int yy=2;yy<=ny;yy++){          // Loop over years
	    for(int ss=1;ss<=ns;ss++){        // Loop over seasons
	      if(ss==1){                      // First subyear isn't a spawning season  ----------------------------------------------------------
	        for(int aa=1;aa<=na;aa++){    // Loop over age classes
	          N(pp)(yy)(1)(aa)=elem_prod(mfexp(-Z(pp)(yy-1)(ns)(aa)),N(pp)(yy-1)(ns)(aa))*mov(pp)(1)(aa); // Mortality then move
	          for(int rr=1;rr<=nr;rr++){  // Loop over areas
	            SSB(pp,yy,1)+=N(pp,yy,1,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
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
		      SSB(pp,yy,ss)+=(N(pp,yy,ss,aa,rr)*Fec(pp,aa)); // SSB is summed (+=) over age classes and areas (sum())
		      SSBdist(pp,rr)+=(N(pp,yy,ss,aa,rr)*Fec(pp,aa)); // Calculate distribution of SSB over areas
	            }                          // End of areas
		  }                            // End of age classes
	          for(int rr=1;rr<=nr;rr++){   // Loop over areas
	            spawnr(pp,rr)=SSBdist(pp,rr)/sum(SSBdist(pp)); // Calculate spawning fraction
	          }                            // End of areas
	          N(pp)(yy)(ss)(na)+=N(pp)(yy)(ss)(na-1);  // Plus group calculation
	          for(int aa=(na-1);aa>=2;aa-=1){          // Loop down age classes
		    N(pp)(yy)(ss)(aa)=N(pp)(yy)(ss)(aa-1); // Age fish
	          }                                        // End of age classes
                  N(pp)(yy)(ss)(1)=spawnr(pp)*RD(pp,yy)*(0.8*R0(pp)*steep(pp)*SSB(pp,yy-1,ss))/
                                   (0.2*SSBpR(pp)*R0(pp)*(1-steep(pp))+(steep(pp)-0.2) * SSB(pp,yy-1,ss)); // Recruitment (SSB lag 1 year)
	        }else{                          // Not a spawning season ----------------------------------------------------------------
	          for(int aa=1;aa<=na;aa++){    // Loop over age classes
		    N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss)(aa); // M then move	        
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
		      SSB(pp,yy,ss)+=(N(pp,yy,ss,aa,rr)*Fec(pp,aa)); // SSB is summed (+=) over age classes and areas (sum())
		    }	                        // End of areas 
		  }                             // End of ages
	        }                               // Not a spawning season
	      }                                 // Could be a spawning season?
	      CTA(pp)(yy)(ss)=trans(elem_prod(
	      elem_prod(N(pp)(yy)(ss),mfexp(Z(pp)(yy)(ss))-1),
	      elem_div(trans(FAT(pp)(yy)(ss)),Z(pp)(yy)(ss)))); // Calculate catch at age
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
	  }                                                          // End of year
	  SSBi(pp)=SSB(pp)/(sum(SSB(pp))/(ny*ns));                   // Calculate SSB index normalized to 1
	}                                                            // End of stock
	if(debug)cout<<"--- Finished calcTransitions ---"<<endl;
  }
}

void model_parameters::calcRecaptureProb(void)
{
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
}

void model_parameters::calcObjective(void)
{
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
        double tiny=1E-10;                      // Create a small constant to avoid log(0) error
	// -- Catch observations --
	for(int i=1;i<=nCobs;i++){              // Loop over catch observations
	  int yy=Cobs(i,1); // Year
	  int ss=Cobs(i,2); // Subyear
	  int rr=Cobs(i,3); // Region
  	  int ff=Cobs(i,4); // Fleet
  	  LHtemp=dnorm(log(CWtotpred(yy,ss,rr,ff)+tiny),log(Cobs(i,5)+tiny),CobsCV(ff)); // Log-normal LHF
  	  objC+=LHtemp*LHw(1);                                                           // Weighted likelihood contribution
  	  objG+=LHtemp*LHw(1);                                                           // Weighted likelihood contribution
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
	}
	if(debug)cout<<"---  * Finished length composition LHF ---"<<endl;
	// -- Stock of origin data -- 
	dvariable SOOpred;   // Calculated predicted fraction of each stock of origin (there aren't that many stock of origin observations so it involves less computation to do this as needed)
	dvariable SOOtot;    // For storing the sum of SOO numbers
	SOOtot.initialize(); // SSOtot = 0
	for(int i=1;i<=nSOOobs;i++){ // Loop over stock of origin observations
	  int pp=SOOobs(i,1);   // Population
	  int yy=SOOobs(i,2);   // Year
	  int ss=SOOobs(i,3);   // Subyear
	  int rr=SOOobs(i,4);   // Region
	  SOOtot=0.;            // Reset sum
	  for(int pp=1;pp<=np;pp++){
	    SOOtot+=CNpred(pp,yy,ss,rr);
	  }
	  SOOpred=CNpred(pp,yy,ss,rr)/(SOOtot+tiny); // Calculate predicted fraction
	  LHtemp=(-SOOobs(i,5)*log(SOOpred+tiny));   // Multinomial LHF
	  objSOO+=LHtemp*LHw(5);                     // Weighted likelihood contribution
	  objG+=LHtemp*LHw(5);                       // Weighted likelihood contribution
	}
	if(debug)cout<<"---  * Finished SOO LHF ---"<<endl;
	// -- PSAT tagging --
	for(int i=1;i<=nPSAT;i++){ // Tags with certain stock of origin
	  int pp=PSAT(i,1);   // Population
	  int ss=PSAT(i,2);   // Subyear
	  int aa=PSAT(i,3);   // Movement age class
	  int tt=PSAT(i,4);   // Time at liberty (subyears)
	  int rr=PSAT(i,5);   // Region from
	  int r2=PSAT(i,6);   // Region to
	  LHtemp=(-PSAT(i,7)*log(RecapP(pp,ss,aa,tt,rr,r2)+tiny)); // Multinomial LHF
	  objPSAT+=LHtemp*LHw(6);                               // Weighted likelihood contribution
	  objG+=LHtemp*LHw(6);                                  // weighted likelihood contribution
	}
	if(debug)cout<<"---  * Finished PSAT known SOO LHF ---"<<endl;
	for(int i=1;i<=nPSAT2;i++){ // Individual tags with uncertain stock of origin
          int ss=PSAT2(i,1);   // Year
	  int aa=PSAT2(i,2);   // Movement age class
	  int tt=PSAT2(i,3);   // Subyear
	  int rr=PSAT2(i,4);   // Region from
	  int r2=PSAT2(i,5);   // Region to
	  for(int pp=1;pp<=np;pp++){
	    LHtemp=(-log(RecapP(pp,ss,aa,tt,rr,r2)+tiny)*PSAT2(i,5+pp)); // Multinomial LHF
	    objPSAT2+=LHtemp*LHw(7);                                  // Weighted likelihood contribution
	    //objG+=LHtemp*LHw(7);                                    // Weighted likelihood contribution
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
	  }
        }  
	// -- Movement parameters ---
	for(int mm=1;mm<=nMP;mm++){
	  LHtemp=dnorm(mfexp(movest(mm)),-5.,5.); // Weak prior (very low movement)
          objmov+=LHtemp*LHw(9);                  // Weighted likelihood contribution
	  objG+=LHtemp*LHw(9);                    // Weighted likelihood contribution
	}
	for(int i=1;i<=nsel;i++){ 
	  objG+=dnorm(selpar(i),0,2.)*LHw(10);   // Weak prior on selectivity
	}
	if(debug)cout<<"---  * Finished rec dev penalty ---"<<endl;
	//objG+=dnorm(lnF(1),log(0.1),2);          // Temporary fix to allow F estimation to be simplied to a master index and partial F approach (complexF = 0)
	if(debug)cout<<"--- Finished calcObjective ---"<<endl;
	if(verbose)cout<<"Catch LHF "<<objC<<endl;           // Report catch likelihood component
	if(verbose)cout<<"CPUE LHF "<<objCPUE<<endl;         // Report CPUE likelihood component
	if(verbose)cout<<"FI index LHF "<<objI<<endl;        // Report FI index likelihood component
	if(verbose)cout<<"Length comp LHF "<<objCL<<endl;    // Report catch at length likelihood component
        if(verbose)cout<<"SOO LHF "<<objSOO<<endl;           // Report stock of origin likelihood component
	if(verbose)cout<<"PSAT LHF "<<objPSAT<<endl;         // Report PSAT likelihood component
	if(verbose)cout<<"PSAT uSOO LHF "<<objPSAT2<<endl;   // Report PSAT2 likelihood component
	if(verbose)cout<<"Rec dev LHF "<<objRD<<endl;        // Report Rec dev likelihood component
	if(verbose)cout<<"Global objective "<<objG<<endl;    // Report Global objective function
  }
}

void model_parameters::simsam(void)
{
  {
        // If working with simulated data do some printing
        cout<<"R0 sim = "<<log(R0_ini)<<endl;                // Simulated R0
        cout<<"R0 sam = "<<log(R0)<<endl;                    // Estimated R0
        cout<<"sel sim f1= "<<sel_ini(1)<<endl;              // Simulated selectivity fleet 1
        cout<<"sel sam f1= "<<sel(1)<<endl;                  // Estimated selectivity fleet 1
        cout<<"sel sim f2= "<<sel_ini(2)<<endl;              // Simulated selectivity fleet 2
	cout<<"sel sam f2= "<<sel(2)<<endl;                  // Estimated selectivity fleet 2
        //init_vector lnF_ini(1,nCobs);                      // Effort (complexF=0) or log fishing mortality rate (complexF=1) 
	//cout<<"RDi sim= "<<exp(ilnRD_ini(1))<<endl;        // Simulated initial recruitment deviations
	//cout<<"RDi sam= "<<iRD(1)<<endl;                   // Estimated initial recruitment deviations
	cout<<"RD sim= "<<exp(lnRD_ini(1))<<endl;            // Simulated recruitment deviations 
        cout<<"RD sam= "<<RD(1)<<endl;                       // Estimated recruitment deviations
	cout<<"mov sim p1 s1= "<<endl;                       // Simulated movement probabilities for stock 1 in subyear 1
	cout<<mov_ini(1)(1)<<endl;                           // Simulated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s1= "<<endl;                       // Estimated movement probabilities for stock 1 in subyear 1
	cout<<mov(1)(1)<<endl;                               // Estimated movement probabilities for stock 1 in subyear 1
	cout<<"mov sim p2 s1= "<<endl;                       // Simulated movement probabilities for stock 2 in subyear 1
	cout<<mov_ini(2)(1)<<endl;                           // Simulated movement probabilities for stock 2 in subyear 1
	cout<<"mov sam p2 s1= "<<endl;                       // Estimated movement probabilities for stock 2 in subyear 1
	cout<<mov(2)(1)<<endl;                               // Estimated movement probabilities for stock 2 in subyear 1
	cout<<"qCE sim= "<<exp(lnqCPUE_ini)<<endl;           // Simulated catchabilities
	cout<<"qCE sam= "<<qCPUE<<endl;                      // Estimated catchabilities
	cout<<"qI sim= "<<exp(lnqI_ini)<<endl;               // Simulated FI index catchability
	cout<<"qI sam= "<<qI<<endl;                          // Estimated FI index catchability
  }
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
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
}

void model_parameters::set_runtime(void)
{
  dvector temp1("{5000}");
  maximum_function_evaluations.allocate(temp1.indexmin(),temp1.indexmax());
  maximum_function_evaluations=temp1;
  dvector temp("{1.e-3}");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
}

void model_parameters::preliminary_calculations(void){
#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
	arrmblsize = 70000000;
	gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
	gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
	gradient_structure::set_MAX_NVAR_OFFSET(5000);
	gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
	
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
