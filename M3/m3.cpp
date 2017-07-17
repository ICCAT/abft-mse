#ifdef DEBUG
  #ifndef __SUNPRO_C
    #include <cfenv>
    #include <cstdlib>
  #endif
#endif
        
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
  nHy.allocate("nHy");
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
  nydist.allocate("nydist");
  ml.allocate(1,nl,"ml");
  RDblock.allocate(1,ny,"RDblock");
  nRD.allocate("nRD");
  ALK.allocate(1,np,1,ny,1,na,1,nl,"ALK");
  lwa.allocate(1,np,"lwa");
  lwb.allocate(1,np,"lwb");
  len_age.allocate(1,ny,1,na,1,np,"len_age");
  wt_age.allocate(1,ny,1,na,1,np,"wt_age");
  Fec.allocate(1,np,1,na,"Fec");
  spawns.allocate(1,np,"spawns");
  canspawn.allocate(1,np,1,nr,"canspawn");
  Ma.allocate(1,np,1,na,"Ma");
  nCobs.allocate("nCobs");
  Cobs.allocate(1,nCobs,1,5,"Cobs");
  nCPUEq.allocate("nCPUEq");
  nCPUEobs.allocate("nCPUEobs");
  CPUEobs.allocate(1,nCPUEobs,1,6,"CPUEobs");
  nE.allocate("nE");
  nEobs.allocate("nEobs");
  Eobs.allocate(1,nEobs,1,6,"Eobs");
  nCLobs.allocate("nCLobs");
  CLobs.allocate(1,nCLobs,1,6,"CLobs");
  HCobs.allocate(1,nHy,1,ns,1,na,1,nr,"HCobs");
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
  SOOobs.allocate(1,nSOOobs,1,6,"SOOobs");
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
  CPUEobsCV.allocate(1,nCPUEq,"CPUEobsCV");
  IobsCV.allocate(1,nI,"IobsCV");
  RDCV.allocate("RDCV");
  SSBprior.allocate(1,np,"SSBprior");
  SSBCV.allocate("SSBCV");
  nLHw.allocate("nLHw");
  LHw.allocate(1,nLHw,"LHw");
  muR_ini.allocate(1,np,"muR_ini");
  sel_ini.allocate(1,nf,1,nl,"sel_ini");
  selpars_ini.allocate(1,nf,1,3,"selpars_ini");
  lnF_ini.allocate(1,nCobs,"lnF_ini");
  lnRD_ini.allocate(1,np,1,ny,"lnRD_ini");
  mov_ini.allocate(1,np,1,ns,1,na,1,nr,1,nr,"mov_ini");
  lnqCPUE_ini.allocate(1,nCPUEq,"lnqCPUE_ini");
  lnqI_ini.allocate(1,nI,"lnqI_ini");
  D_ini.allocate(1,np,"D_ini");
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
  lnmuR.allocate(1,np,9.5,19.,1,"lnmuR");
  lnHR1.allocate(1,np,-2,2,1,"lnHR1");
  lnHR2.allocate(1,np,-2,2,1,"lnHR2");
  selpar.allocate(1,nsel,1,seltype,-2.,2.,1,"selpar");
  lnRD1.allocate(1,nRD,-6.,6.,1,"lnRD1");
  lnRD2.allocate(1,nRD,-6.,6.,1,"lnRD2");
  movest.allocate(1,nMP,-8.,8.,1,"movest");
  lnqE.allocate(1,nE,-6.,1.,1,"lnqE");
  lnqI.allocate(1,nI,-2.3,2.3,1,"lnqI");
  lnqCPUE.allocate(1,nCPUEq,-6.,4.,1,"lnqCPUE");
  Fmod.allocate(1,ns*nr,-2,2,1,"Fmod");
	  nodemax = np+sum(seltype)+np*nRD+nMP+nCPUEq+nI;
	  //cout<<nodemax<<endl;
  nodes.allocate(1,nodemax,"nodes");
  #ifndef NO_AD_INITIALIZE
    nodes.initialize();
  #endif
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
  objsel.allocate("objsel");
  #ifndef NO_AD_INITIALIZE
  objsel.initialize();
  #endif
  objPSAT.allocate("objPSAT");
  #ifndef NO_AD_INITIALIZE
  objPSAT.initialize();
  #endif
  objPSAT2.allocate("objPSAT2");
  #ifndef NO_AD_INITIALIZE
  objPSAT2.initialize();
  #endif
  objSRA.allocate("objSRA");
  #ifndef NO_AD_INITIALIZE
  objSRA.initialize();
  #endif
  objSSB.allocate("objSSB");
  #ifndef NO_AD_INITIALIZE
  objSSB.initialize();
  #endif
  objFmod.allocate("objFmod");
  #ifndef NO_AD_INITIALIZE
  objFmod.initialize();
  #endif
  objRat.allocate("objRat");
  #ifndef NO_AD_INITIALIZE
  objRat.initialize();
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
  hSSB.allocate(1,np,1,nHy,1,ns,"hSSB");
  #ifndef NO_AD_INITIALIZE
    hSSB.initialize();
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
  VBi.allocate(1,ny,1,ns,1,nr,1,nf,"VBi");
  #ifndef NO_AD_INITIALIZE
    VBi.initialize();
  #endif
  B.allocate(1,ny,1,ns,1,nr,"B");
  #ifndef NO_AD_INITIALIZE
    B.initialize();
  #endif
  SSB0.allocate(1,np,"SSB0");
  #ifndef NO_AD_INITIALIZE
    SSB0.initialize();
  #endif
  D.allocate(1,np,"D");
  #ifndef NO_AD_INITIALIZE
    D.initialize();
  #endif
  Dt.allocate(1,np,"Dt");
  #ifndef NO_AD_INITIALIZE
    Dt.initialize();
  #endif
  SSBnow.allocate(1,np,"SSBnow");
  #ifndef NO_AD_INITIALIZE
    SSBnow.initialize();
  #endif
  RD.allocate(1,np,1,nRD,"RD");
  #ifndef NO_AD_INITIALIZE
    RD.initialize();
  #endif
  Rec.allocate(1,np,1,ny,"Rec");
  #ifndef NO_AD_INITIALIZE
    Rec.initialize();
  #endif
  muR.allocate(1,np,"muR");
  #ifndef NO_AD_INITIALIZE
    muR.initialize();
  #endif
  CTL.allocate(1,np,1,ny,1,ns,1,nr,1,nl,"CTL");
  #ifndef NO_AD_INITIALIZE
    CTL.initialize();
  #endif
  CTA.allocate(1,np,1,ny,1,ns,1,nr,1,na,"CTA");
  #ifndef NO_AD_INITIALIZE
    CTA.initialize();
  #endif
  Btrend.allocate(1,np,1,ny,"Btrend");
  #ifndef NO_AD_INITIALIZE
    Btrend.initialize();
  #endif
  meanF.allocate(1,np,1,ny,"meanF");
  #ifndef NO_AD_INITIALIZE
    meanF.initialize();
  #endif
  F.allocate(1,nF,"F");
  #ifndef NO_AD_INITIALIZE
    F.initialize();
  #endif
  FAT.allocate(1,np,1,ny,1,ns,1,nr,1,na,"FAT");
  #ifndef NO_AD_INITIALIZE
    FAT.initialize();
  #endif
  hFAT.allocate(1,nHy,1,ns,1,na,1,nr,"hFAT");
  #ifndef NO_AD_INITIALIZE
    hFAT.initialize();
  #endif
  hZ.allocate(1,np,1,nHy,1,ns,1,na,1,nr,"hZ");
  #ifndef NO_AD_INITIALIZE
    hZ.initialize();
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
  qCPUE.allocate(1,nCPUEq,"qCPUE");
  #ifndef NO_AD_INITIALIZE
    qCPUE.initialize();
  #endif
  qI.allocate(1,nI,"qI");
  #ifndef NO_AD_INITIALIZE
    qI.initialize();
  #endif
  qE.allocate(1,nE,"qE");
  #ifndef NO_AD_INITIALIZE
    qE.initialize();
  #endif
  Ipred.allocate(1,ny,1,ns,1,nr,1,np,"Ipred");
  #ifndef NO_AD_INITIALIZE
    Ipred.initialize();
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
  Ntemp.allocate("Ntemp");
  #ifndef NO_AD_INITIALIZE
  Ntemp.initialize();
  #endif
  tempR.allocate(1,nr,"tempR");
  #ifndef NO_AD_INITIALIZE
    tempR.initialize();
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
  temp.allocate("temp");
}

void model_parameters::userfunction(void)
{
  objG =0.0;
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
}

void model_parameters::assignPars(void)
{
  {
	// -- Assign estimated parameters --
	muR=mfexp(lnmuR);                   // Assign historical recruitment
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
}

void model_parameters::assignInits(void)
{
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
}

void model_parameters::calcSurvival(void)
{
  {
	// -- Calculate survival --
	for(int pp=1;pp<=np;pp++){                   // Loop over stocks      
	  //surv(pp,1)=1.;                             // Survival to age 1 is 100%
	  for(int aa=1;aa<=na;aa++){             // Loop over age classes
	    surv(pp,aa)=exp(-sum(Ma(pp)(1,aa)));   // Calculate survivial
	  }                                          // End of age classes
	  //surv(pp,na)*=exp(-Ma(pp,na))/(1-exp(-Ma(pp,na))); // final plus group survival is multiplied by the indefinite intergral  
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
}

void model_parameters::calcSelectivities(void)
{
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
	      msel(ss)/=max(msel(ss)); // Need to normalize at least one index to max 1 or face counfounding with q
	      // End of length classes
	    break;                                                                         // End of logistic selectivity
            case 3: // Thompson dome-shaped selectivity
	      spar(1)=0.01+pow((selpar(ss,1)+2.)/5,3.);            // Dome-shape parameter 
	      spar(2)=0.01+0.01*pow((selpar(ss,2)+2.),3.);         // Precision as the ratio of the inflection point
	      spar(3)=ml(nl)*(0.1+0.8*(selpar(ss,3)+2)/4);         // Inflection point as a fraction of largest length
	      for(int ll=1;ll<=nl;ll++){                                                   // Loop over length classes
	        msel(ss,ll)=(1/(1-spar(1)))*pow(
	        ((1-spar(1))/spar(1)),spar(1)) * mfexp(spar(2)*spar(1)*(spar(3)-ml(ll)))/(1+mfexp(spar(2)*(spar(3)-ml(ll))));	// Thompson selectivity function	
	      }									           // End of length classes
	      msel(ss)/=max(msel(ss));                                                     // Need to normalize to max 1 or face counfounding with q
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
}

void model_parameters::initModel(void)
{
  {
	double tiny=1E-10;             // Define a small number for ensuring that log(0) doesn't happen
	N.initialize();                // Stock numbers = 0
	B.initialize();                // Stock biomass = 0
	hZ.initialize();               // Historical Z = 0
	SSB.initialize();              // Spawning stock biomass = 0
	hSSB.initialize();             // historical Spawning stock biomass = 0
	SSBdist.initialize();          // Spawning distribution = 0
	SSB0.initialize();             // Unfished spawning stock biomass = 0
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
	          N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy-1)(ns)(aa)),N(pp)(yy-1)(ns)(aa))*mov(pp)(ss)(aa); // Mortality then move
	          for(int rr=1;rr<=nr;rr++){  // Loop over areas
	            SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
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
		     }                          // End of areas
		  }                            // End of age classes
	        }else{                          // Not a spawning season ----------------------------------------------------------------
	          for(int aa=1;aa<=na;aa++){    // Loop over age classes
		    N(pp)(yy)(ss)(aa)=elem_prod(mfexp(-Z(pp)(yy)(ss-1)(aa)),N(pp)(yy)(ss-1)(aa))*mov(pp)(ss)(aa); // M then move	        
		    for(int rr=1;rr<=nr;rr++){  // Loop over areas
		      SSB(pp,yy,ss)+=N(pp,yy,ss,aa,rr)*Fec(pp,aa); // SSB is summed (+=) over age classes and areas (sum())
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
	objsel.initialize();			// Priors on selectivity
	objPSAT.initialize();                   // PSAT tags certain stock of origin
	objPSAT2.initialize();                  // PSAT tags w uncertain stock of origin
	objSSB.initialize();                    // SSB prior
	objFmod.initialize();                   // Fmod prior
	objRat.initialize();                    // Ratio on unfished spawning stock size
	Ipred.initialize();                     // Predicted fishery-independent index
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
	    //cout<<yy<<"-"<<ss<<"-"<<rr<<"-"<<ii<<"-"<<ff<<"-"<<CPUEobs(i,5)<<endl;
	    CPUEtemp=VBi(yy,ss,rr,ff)*qCPUE(ii);                                       // Calculate vulnerable biomass
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
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" l="<<ll<<" CLobs="<<CLobs(i,6)<<" CLpred="<<CLtotpred(yy,ss,rr,ff,ll)<<endl;
	  //LHtemp=dnorm(log(CLtotpred(yy)(ss)(rr)(ff)(ll)),log(CLobs(i,6)),1);
	  //LHtemp=(-CLobs(i,6)*log((CLtotfrac(yy,ss,rr,ff,ll)+tiny)/CLobs(i,6))); // Multinomial LHF (A.Punt fix for stability)
	  LHtemp=(-CLobs(i,6)*log((CLtotfrac(yy,ss,rr,ff,ll)+tiny)));
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
	  LHtemp=dnorm(movest(mm),0.,2);        // Weak(ish) prior 
          objmov+=LHtemp*LHw(9);                  // Weighted likelihood contribution
	  objG+=LHtemp*LHw(9);                    // Weighted likelihood contribution
	}
	// -- Selectivity parameters ---
	for(int i=1;i<=nsel;i++){ 
	  objsel+=dnorm(selpar(i),0,1.25)*LHw(10);  
	  objG+=dnorm(selpar(i),0,1.25)*LHw(10);   // Prior on selectivity to add numerical stability
	}
	objG+=objSRA*LHw(11);                      // Add the posfun penalty for SRA harvest rates over 90%
	for(int pp=1;pp<=np;pp++){
	  objSSB+=dnorm(log(SSBnow(pp)+tiny),log(SSBprior(pp)+tiny),SSBCV)*LHw(12);
	}
	objG+=objSSB*LHw(12);
	for(int ll=1;ll<=ns*nr;ll++){
	  objFmod+=dnorm(Fmod(ll),0,0.5);
	}
	objG+=objFmod;
	objRat=dnorm(log(SSB0(1)/SSB0(2)),2.079,0.15);
	objG+=objRat*50;
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
}

void model_parameters::simsam(void)
{
  {
        // If working with simulated data do some printing
        cout<<"canspawn = "<<canspawn<<endl;
        cout<<"surv = "<<surv<<endl;
        cout<<"Fec ="<<Fec<<endl;
        //cout<<"muR sim = "<<log(muR_ini)<<endl;                // Simulated R0
        cout<<"muR sam = "<<log(muR)<<endl;                    // Estimated mu r
        cout<<"lnHR1 sam = "<<lnHR1<<endl;                    // Estimated mu r deviant
        cout<<"lnHR2 sam = "<<lnHR2<<endl;                    // Estimated mu r deviant
        //cout<<"selpar = "<<selpar<<endl;                     // Estimated R0
        for(int ff=1;ff<=nf;ff++){
          //cout<<"sel sim f1= "<<sel_ini(1)<<endl;            // Simulated selectivity fleet 1
          cout<<"sel sam "<<ff<<" "<<sel(ff)<<endl;            // Estimated selectivity fleet 1
        }
        cout<<"RD= "<<RD<<endl;
        cout<<"Rec1= "<<RD(1)*muR(1)<<endl;
        cout<<"Rec2= "<<RD(2)*muR(2)<<endl;
        //cout<<"RD sam 1 = "<<RD(1)<<endl;                       // Estimated recruitment deviations
	//cout<<"RD sam 2 = "<<RD(2)<<endl;                       // Estimated recruitment deviations
	cout<<"movest= "<<movest<<endl;                         // Estimated recruitment deviations
	//cout<<"mov sim p1 s2 m2= "<<endl;                       // Simulated movement probabilities for stock 1 in subyear 1
	//cout<<mov_ini(1)(2)(2)<<endl;                           // Simulated movement probabilities for stock 1 in subyear 1
	cout<<"mov sam p1 s2 m2= "<<endl;                         // Estimated movement probabilities for stock 1 in subyear 1
	cout<<mov(1)(2)(2)<<endl;                                 // Estimated movement probabilities for stock 1 in subyear 1
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
}

void model_parameters::popnodes(void)
{
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
  	report<<"datacheck"<<endl;
  	report<<datacheck<<endl;
  }
}

void model_parameters::set_runtime(void)
{
  dvector temp1("{5000}");
  maximum_function_evaluations.allocate(temp1.indexmin(),temp1.indexmax());
  maximum_function_evaluations=temp1;
  dvector temp("{1.e-4}");
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
#ifdef DEBUG
  #ifndef __SUNPRO_C
std::feclearexcept(FE_ALL_EXCEPT);
  #endif
#endif
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
#ifdef DEBUG
  #ifndef __SUNPRO_C
bool failedtest = false;
if (std::fetestexcept(FE_DIVBYZERO))
  { cerr << "Error: Detected division by zero." << endl; failedtest = true; }
if (std::fetestexcept(FE_INVALID))
  { cerr << "Error: Detected invalid argument." << endl; failedtest = true; }
if (std::fetestexcept(FE_OVERFLOW))
  { cerr << "Error: Detected overflow." << endl; failedtest = true; }
if (std::fetestexcept(FE_UNDERFLOW))
  { cerr << "Error: Detected underflow." << endl; }
if (failedtest) { std::abort(); } 
  #endif
#endif
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
