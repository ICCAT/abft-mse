#ifdef DEBUG
  #ifndef __SUNPRO_C
    #include <cfenv>
    #include <cstdlib>
  #endif
#endif
        
        #include <admodel.h>
	#include "stats2.cxx"
	#include <fstream>
        ofstream nodesout("nodes.cha");
       	
	
	
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
  ALK.allocate(1,np,1,ny,1,na,1,nl,"ALK");
  lwa.allocate(1,np,"lwa");
  lwb.allocate(1,np,"lwb");
  len_age.allocate(1,ny,1,na,1,np,"len_age");
  wt_age.allocate(1,ny,1,na,1,np,"wt_age");
  Fec.allocate(1,np,1,na,"Fec");
  nSR.allocate("nSR");
  SRminyr.allocate(1,nSR,"SRminyr");
  SRmaxyr.allocate(1,nSR,"SRmaxyr");
  nRDs.allocate(1,4,"nRDs");
  RDts.allocate(1,np,1,ny,"RDts");
  RDno.allocate(1,np,1,ny,"RDno");
  SRp.allocate(1,nSR,"SRp");
  SRpar.allocate(1,nSR,"SRpar");
  SSBpR.allocate(1,np,"SSBpR");
  spawns.allocate(1,np,"spawns");
  canspawn.allocate(1,np,1,nr,"canspawn");
  Ma.allocate(1,np,1,na,"Ma");
  nCobs.allocate("nCobs");
  Cobs.allocate(1,nCobs,1,7,"Cobs");
  nCPUEq.allocate("nCPUEq");
  nCPUEobs.allocate("nCPUEobs");
  CPUEobs.allocate(1,nCPUEobs,1,10,"CPUEobs");
  nE.allocate("nE");
  nEobs.allocate("nEobs");
  Eobs.allocate(1,nEobs,1,6,"Eobs");
  nCLobs.allocate("nCLobs");
  CLobs.allocate(1,nCLobs,1,7,"CLobs");
  HCobs.allocate(1,nHy,1,ns,1,na,1,nr,"HCobs");
  RAI.allocate(1,nr,1,ns,1,ny,"RAI");
  nI.allocate("nI");
  nIobs.allocate("nIobs");
  Iobs.allocate(1,nIobs,1,12,"Iobs");
  nPSAT.allocate("nPSAT");
  PSAT.allocate(1,nPSAT,1,8,"PSAT");
  nPSAT2.allocate("nPSAT2");
  PSAT2.allocate(1,nPSAT2,1,8,"PSAT2");
  nTag.allocate("nTag");
  Tag.allocate(1,nTag,1,10,"Tag");
  nSOOobs.allocate("nSOOobs");
  SOOobs.allocate(1,nSOOobs,1,9,"SOOobs");
  nsel.allocate("nsel");
  seltype.allocate(1,nsel,"seltype");
  selind.allocate(1,nf,"selind");
  ratiolim.allocate(1,2,"ratiolim");
  infleclim.allocate(1,2,"infleclim");
  LB.allocate(1,nf,"LB");
  UB.allocate(1,nf,"UB");
  nma.allocate("nma");
  ma.allocate(1,np,1,na,"ma");
  macat.allocate(1,nma,1,2,"macat");
  movtype.allocate("movtype");
  nMovExc.allocate("nMovExc");
  MovExc.allocate(1,nMovExc,1,6,"MovExc");
  nIlencat.allocate("nIlencat");
  Ilencat.allocate(1,nIlencat,1,2,"Ilencat");
  CLCV_num.allocate("CLCV_num");
  RDCV.allocate("RDCV");
  nSSBprior.allocate("nSSBprior");
  SSBprior.allocate(1,nSSBprior,1,3,"SSBprior");
  SSBCV.allocate("SSBCV");
  nDepprior.allocate("nDepprior");
  Depprior.allocate(1,nDepprior,1,3,"Depprior");
  DepCV.allocate("DepCV");
  BSfrac.allocate(1,np,1,ns,"BSfrac");
  FCV.allocate("FCV");
  movCV.allocate("movCV");
  selCV.allocate("selCV");
  R0diffCV.allocate("R0diffCV");
  BSfracCV.allocate("BSfracCV");
  nSpatPrq.allocate("nSpatPrq");
  nSpatPr.allocate("nSpatPr");
  SpatPr.allocate(1,nSpatPr,1,10,"SpatPr");
  nLHw.allocate("nLHw");
  LHw.allocate(1,nLHw,"LHw");
  nF.allocate("nF");
  MICV.allocate("MICV");
  Phase1.allocate("Phase1");
  Phase2.allocate("Phase2");
  Phase3.allocate("Phase3");
  Phase4.allocate("Phase4");
  ET_LHF.allocate("ET_LHF");
  LC_LHF.allocate("LC_LHF");
  debug.allocate("debug");
  verbose.allocate("verbose");
  datacheck.allocate("datacheck");
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  lnR0.allocate(1,nSR,12,14.5,Phase1,"lnR0");
  selpar.allocate(1,nsel,1,seltype,-5.,5.,Phase2,"selpar");
  movest_p1_a1.allocate(1,ns,1,nr-2,-8.,8.,Phase3,"movest_p1_a1");
  movest_p2_a1.allocate(1,ns,1,nr-2,-8.,8.,Phase3,"movest_p2_a1");
  movest_p1_a2.allocate(1,ns,1,nr-2,-8.,8.,Phase1,"movest_p1_a2");
  movest_p2_a2.allocate(1,ns,1,nr-2,-8.,8.,Phase1,"movest_p2_a2");
  movest_p1_a3.allocate(1,ns,1,nr-2,-8.,8.,Phase3,"movest_p1_a3");
  movest_p2_a3.allocate(1,ns,1,nr-2,-8.,8.,Phase3,"movest_p2_a3");
  lnqE.allocate(1,nE,-10.,1.,Phase1,"lnqE");
  Fmod.allocate(1,ns*nr,-10.,10.,Phase2,"Fmod");
  visc.allocate(1,np,1,ns,-3.,3,Phase2,"visc");
	  //cout<<"here 0"<<endl;
	  int nRD1=nRDs(1);      // Note here that there are up to four recruitment deviation vectors
	  int nRD2=nRDs(2);
	  int nRD3=nRDs(3);
	  int nRD4=nRDs(4);
	  nRD=sum(nRDs);
	  	   
  lnRD1.allocate(1,nRD1,-1.5,1.5,Phase1,"lnRD1");
  lnRD2.allocate(1,nRD2,-1.5,1.5,Phase1,"lnRD2");
  lnRD3.allocate(1,nRD3,-1.5,1.5,Phase1,"lnRD3");
  lnRD4.allocate(1,nRD4,-1.5,1.5,Phase1,"lnRD4");
  FDY_1_1.allocate(1,ny,-5.,5.,Phase3,"FDY_1_1");
  FDY_1_2.allocate(1,ny,-5.,5.,Phase3,"FDY_1_2");
  FDY_1_3.allocate(1,ny,-5.,5.,Phase3,"FDY_1_3");
  FDY_1_4.allocate(1,ny,-5.,5.,Phase3,"FDY_1_4");
  FDY_2_1.allocate(1,ny,-5.,5.,Phase3,"FDY_2_1");
  FDY_2_2.allocate(1,ny,-5.,5.,Phase3,"FDY_2_2");
  FDY_2_3.allocate(1,ny,-5.,5.,Phase3,"FDY_2_3");
  FDY_2_4.allocate(1,ny,-5.,5.,Phase3,"FDY_2_4");
  FDY_3_1.allocate(1,ny,-5.,5.,Phase3,"FDY_3_1");
  FDY_3_2.allocate(1,ny,-5.,5.,Phase3,"FDY_3_2");
  FDY_3_3.allocate(1,ny,-5.,5.,Phase3,"FDY_3_3");
  FDY_3_4.allocate(1,ny,-5.,5.,Phase3,"FDY_3_4");
  FDY_4_1.allocate(1,ny,-5.,5.,Phase3,"FDY_4_1");
  FDY_4_2.allocate(1,ny,-5.,5.,Phase3,"FDY_4_2");
  FDY_4_3.allocate(1,ny,-5.,5.,Phase3,"FDY_4_3");
  FDY_4_4.allocate(1,ny,-5.,5.,Phase3,"FDY_4_4");
  FDY_5_1.allocate(1,ny,-5.,5.,Phase3,"FDY_5_1");
  FDY_5_2.allocate(1,ny,-5.,5.,Phase3,"FDY_5_2");
  FDY_5_3.allocate(1,ny,-5.,5.,Phase3,"FDY_5_3");
  FDY_5_4.allocate(1,ny,-5.,5.,Phase3,"FDY_5_4");
  FDY_6_1.allocate(1,ny,-5.,5.,Phase3,"FDY_6_1");
  FDY_6_2.allocate(1,ny,-5.,5.,Phase3,"FDY_6_2");
  FDY_6_3.allocate(1,ny,-5.,5.,Phase3,"FDY_6_3");
  FDY_6_4.allocate(1,ny,-5.,5.,Phase3,"FDY_6_4");
  FDY_7_1.allocate(1,ny,-5.,5.,Phase3,"FDY_7_1");
  FDY_7_2.allocate(1,ny,-5.,5.,Phase3,"FDY_7_2");
  FDY_7_3.allocate(1,ny,-5.,5.,Phase3,"FDY_7_3");
  FDY_7_4.allocate(1,ny,-5.,5.,Phase3,"FDY_7_4");
	  nodemax = nSR + sum(seltype) + (np*ns*nma*(nr-2)) + nE + nI + nCPUEq + ns*nr + nRD + ns*2 + nr*ns*ny;
  lnRDmat.allocate(1,4,1,nRDs,"lnRDmat");
  #ifndef NO_AD_INITIALIZE
    lnRDmat.initialize();
  #endif
  RDmat.allocate(1,4,1,nRDs,"RDmat");
  #ifndef NO_AD_INITIALIZE
    RDmat.initialize();
  #endif
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
  objSOOm.allocate("objSOOm");
  #ifndef NO_AD_INITIALIZE
  objSOOm.initialize();
  #endif
  objSOOg.allocate("objSOOg");
  #ifndef NO_AD_INITIALIZE
  objSOOg.initialize();
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
  objDep.allocate("objDep");
  #ifndef NO_AD_INITIALIZE
  objDep.initialize();
  #endif
  objFmod.allocate("objFmod");
  #ifndef NO_AD_INITIALIZE
  objFmod.initialize();
  #endif
  objR0.allocate("objR0");
  #ifndef NO_AD_INITIALIZE
  objR0.initialize();
  #endif
  objBSfrac.allocate("objBSfrac");
  #ifndef NO_AD_INITIALIZE
  objBSfrac.initialize();
  #endif
  objFDY.allocate("objFDY");
  #ifndef NO_AD_INITIALIZE
  objFDY.initialize();
  #endif
  objSP.allocate("objSP");
  #ifndef NO_AD_INITIALIZE
  objSP.initialize();
  #endif
  qI.allocate(1,nI,"qI");
  #ifndef NO_AD_INITIALIZE
    qI.initialize();
  #endif
  qCPUE.allocate(1,nCPUEq,"qCPUE");
  #ifndef NO_AD_INITIALIZE
    qCPUE.initialize();
  #endif
  qSP.allocate(1,nSpatPrq,"qSP");
  #ifndef NO_AD_INITIALIZE
    qSP.initialize();
  #endif
  N.allocate(1,np,1,ny,1,ns,1,na,1,nr,"N");
  #ifndef NO_AD_INITIALIZE
    N.initialize();
  #endif
  Rec_wd.allocate(1,np,1,ny,"Rec_wd");
  #ifndef NO_AD_INITIALIZE
    Rec_wd.initialize();
  #endif
  Rec_mu.allocate(1,np,1,ny,"Rec_mu");
  #ifndef NO_AD_INITIALIZE
    Rec_mu.initialize();
  #endif
  SSB_mu.allocate(1,np,1,ny,"SSB_mu");
  #ifndef NO_AD_INITIALIZE
    SSB_mu.initialize();
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
  VL.allocate(1,ny,1,ns,1,nr,1,nf,1,nl,"VL");
  #ifndef NO_AD_INITIALIZE
    VL.initialize();
  #endif
  VBL.allocate(1,ny,1,ns,1,nr,1,nf,1,nl,"VBL");
  #ifndef NO_AD_INITIALIZE
    VBL.initialize();
  #endif
  B.allocate(1,ny,1,ns,1,nr,"B");
  #ifndef NO_AD_INITIALIZE
    B.initialize();
  #endif
  BL.allocate(1,ny,1,ns,1,nr,1,nl,"BL");
  #ifndef NO_AD_INITIALIZE
    BL.initialize();
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
  SSB_EW.allocate(1,np,1,ny,"SSB_EW");
  #ifndef NO_AD_INITIALIZE
    SSB_EW.initialize();
  #endif
  movest.allocate(1,np*ns*nma*nr,"movest");
  #ifndef NO_AD_INITIALIZE
    movest.initialize();
  #endif
      	  if(debug)cout<<"here2"<<endl;
      	  // exit(1);
  Rec.allocate(1,np,1,ny,"Rec");
  #ifndef NO_AD_INITIALIZE
    Rec.initialize();
  #endif
  R0.allocate(1,np,1,ny,"R0");
  #ifndef NO_AD_INITIALIZE
    R0.initialize();
  #endif
  steep.allocate(1,np,1,ny,"steep");
  #ifndef NO_AD_INITIALIZE
    steep.initialize();
  #endif
  CTL.allocate(1,np,1,ny,1,ns,1,nr,1,nl,"CTL");
  #ifndef NO_AD_INITIALIZE
    CTL.initialize();
  #endif
  CTA.allocate(1,np,1,ny,1,ns,1,nr,1,na,"CTA");
  #ifndef NO_AD_INITIALIZE
    CTA.initialize();
  #endif
	   if(debug)cout<<"here3"<<endl;
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
  qE.allocate(1,nE,"qE");
  #ifndef NO_AD_INITIALIZE
    qE.initialize();
  #endif
  Ipred.allocate(1,nIobs,"Ipred");
  #ifndef NO_AD_INITIALIZE
    Ipred.initialize();
  #endif
  Ipred_vec.allocate(1,nIobs,"Ipred_vec");
  #ifndef NO_AD_INITIALIZE
    Ipred_vec.initialize();
  #endif
  Ipredtot.allocate(1,2,1,nI,"Ipredtot");
  #ifndef NO_AD_INITIALIZE
    Ipredtot.initialize();
  #endif
  CPUEpred.allocate(1,nCPUEobs,"CPUEpred");
  #ifndef NO_AD_INITIALIZE
    CPUEpred.initialize();
  #endif
  CPUEpred_vec.allocate(1,nCPUEobs,"CPUEpred_vec");
  #ifndef NO_AD_INITIALIZE
    CPUEpred_vec.initialize();
  #endif
  CPUEpredtot.allocate(1,2,1,nCPUEq,"CPUEpredtot");
  #ifndef NO_AD_INITIALIZE
    CPUEpredtot.initialize();
  #endif
  SPpred.allocate(1,nSpatPr,"SPpred");
  #ifndef NO_AD_INITIALIZE
    SPpred.initialize();
  #endif
  SPpred_vec.allocate(1,nSpatPr,"SPpred_vec");
  #ifndef NO_AD_INITIALIZE
    SPpred_vec.initialize();
  #endif
  SPpredtot.allocate(1,2,1,nSpatPrq,"SPpredtot");
  #ifndef NO_AD_INITIALIZE
    SPpredtot.initialize();
  #endif
	  if(debug)cout<<"here4"<<endl;
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
  CLprop.allocate(1,nCLobs,"CLprop");
  #ifndef NO_AD_INITIALIZE
    CLprop.initialize();
  #endif
  RecapP.allocate(1,np,1,ns,1,nma,1,nRPT,1,nr,1,nr,"RecapP");
  #ifndef NO_AD_INITIALIZE
    RecapP.initialize();
  #endif
	   if(debug)cout<<"here5"<<endl;
  FDYt.allocate(1,nr,1,ns,1,ny,"FDYt");
  #ifndef NO_AD_INITIALIZE
    FDYt.initialize();
  #endif
  stemp.allocate(1,np,1,ns,1,nr,"stemp");
  #ifndef NO_AD_INITIALIZE
    stemp.initialize();
  #endif
  BSfrac_pred.allocate(1,np,1,ns,"BSfrac_pred");
  #ifndef NO_AD_INITIALIZE
    BSfrac_pred.initialize();
  #endif
  NLtemp.allocate(1,nl,"NLtemp");
  #ifndef NO_AD_INITIALIZE
    NLtemp.initialize();
  #endif
  Ntemp.allocate("Ntemp");
  #ifndef NO_AD_INITIALIZE
  Ntemp.initialize();
  #endif
	  if(debug)cout<<"here6"<<endl;
  CWpred.allocate(1,np,1,ny,1,ns,1,nr,1,nf,"CWpred");
  #ifndef NO_AD_INITIALIZE
    CWpred.initialize();
  #endif
  CWtotpred.allocate(1,ny,1,ns,1,nr,1,nf,"CWtotpred");
  #ifndef NO_AD_INITIALIZE
    CWtotpred.initialize();
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
  SOOpred.allocate(1,nSOOobs,"SOOpred");
  #ifndef NO_AD_INITIALIZE
    SOOpred.initialize();
  #endif
  PSATpred.allocate(1,nPSAT,"PSATpred");
  #ifndef NO_AD_INITIALIZE
    PSATpred.initialize();
  #endif
  PSAT2pred.allocate(1,nPSAT2,"PSAT2pred");
  #ifndef NO_AD_INITIALIZE
    PSAT2pred.initialize();
  #endif
	  if(debug)cout<<"here7"<<endl;
  temp.allocate("temp");
}

void model_parameters::userfunction(void)
{
  objG =0.0;
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
}

void model_parameters::calcFDY(void)
{
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
}

void model_parameters::calcSurvival(void)
{
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
}

void model_parameters::calcMovement(void)
{
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
}

void model_parameters::calcSelectivities(void)
{
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
}

void model_parameters::calcF(void)
{
  {
  	double tiny=1E-10;	
  	FL.initialize();      // Fishing mortality rate at length = 0
  	FT.initialize();      // Total fishing mortality rate = 0
  	FAT.initialize();     // Total fishing mortality rate at age = 0
  	Z.initialize();       // Total mortality = 0
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
	VL.initialize();               // Vulnerable length
	VBL.initialize();              // Vulnerable biomass at length
	D.initialize();                // Spawning Stock depletion = 0
	objSRA.initialize();           // Penalty for historical F's over 0.9 = 0
	for(int pp=1;pp<=np;pp++){
	  SSB0(pp)=R0(pp,1)*SSBpR(pp);
	}
	for(int pp=1;pp<=np;pp++){                              // Loop over stocks
	  // -- Initial guess at stock distribution -------------------------------------------
	  stemp(pp)(ns)=1./nr;                                  // Distribute a fish evenly over areas                     
	  for(int ii=1;ii<=20;ii++){                            // Loop over nydist initial spatial distribution (stabilizes within 3 usually)
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
	        N(pp,1,ss,aa,rr)=R0(pp,1)*surv(pp,aa)*stemp(pp,ss,rr); // Stock numbers are spatial distribution multiplied by Survival and R0
	        hSSB(pp,1,ss)+=N(pp,1,ss,aa,rr)*Fec(pp,aa);            // Historical spawning stock biomass
	      }                                                        // end of ages
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
		  objSRA+=pen/1000000.;
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
}

void model_parameters::calcObjective(void)
{
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
	qCPUE.initialize();                     // Catchability MLE calc for CPUE indices is a product
	qI.initialize();                        // Catchability MLE calc for Fishery Independent indices is a product
	qSP.initialize();                       // Catchability MLE calc for Spatial priors is a product
	CPUEpredtot.initialize();               // Record of fishery CPUE indices for normalization (dropping the leading qs)
	CPUEpred_vec.initialize();              // Paired CPUE index predictions 
	Ipredtot.initialize();                  // Record of fishery independent indices for normalization (dropping the leading qs)
	Ipred_vec.initialize();                 // Paired fishery independent index predictions
	SPpredtot.initialize();                 // Record of spatial seasonal prior totals for normalization
	SPpred_vec.initialize();                // Paired spatial seasonal prior predictions               
	Ipred.initialize();                     // Predicted fishery-independent index
	CLprop.initialize();                    // Catch at length composition predicted proportions
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
	for(int ii=1; ii<=nCPUEq;ii++){
	    qCPUE(ii)=1.0; // these are products so need to start at 1
	}
	if(nCPUEobs>0){                   // The CPUE indices are contributing to the likelihood function
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
	      CPUEpred(i)=VB(yy,ss,rr,ff);                                       // Calculate vulnerable biomass
	    }else{// 
	      CPUEpred(i)=sum(VBL(yy)(ss)(rr)(ff)(Ilencat(lt,1),Ilencat(lt,2))); // the prediced index
	    }
	    CPUEpredtot(1,ii)+=CPUEpred(i);
	    CPUEpredtot(2,ii)+=CPUEobs(i,6);
	    qCPUE(ii)*=	pow((CPUEobs(i,6)/CPUEpred(i)),CPUEobs(i,10)); //CPUEobs(i,10)= w_i = (1/sigma_i^2) / SUM_i{ (1/sigma_i^2) }
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
	for(int ii=1; ii<=nI;ii++){
		  qI(ii)=1.0; // new q derivation is a product so requires initialization at 1
	}
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
	      Ipred(i)= VB(yy,ss,rr,ff); // CPUEpred(i)=sum(VL(yy)(ss)(rr)(ff));//(Ilencat(lt,1),Ilencat(lt,2)));B(yy,1,rr)+B(yy,2,rr);  // Predicted index 
	      break;  
	    case 5:  // Biomass first two stocks
	      Ipred(i)= sum(VL(yy)(ss)(rr)(ff)(Ilencat(lt,1),Ilencat(lt,2))); // CPUEpred(i)=sum(VL(yy)(ss)(rr)(ff));//(Ilencat(lt,1),Ilencat(lt,2)));B(yy,1,rr)+B(yy,2,rr);  // Predicted index 
	      break;  
	  }
	  Ipredtot(1,ii)+=Ipred(i);
	  Ipredtot(2,ii)+=Iobs(i,7);
	  qI(ii)*= pow((Iobs(i,7)/Ipred(i)),Iobs(i,12)); //Iobs(i,12)= w_i = (1/sigma_i^2) / SUM_i{ (1/sigma_i^2) }
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
	  //cout<<"y="<<yy<<" s="<<ss<<" r="<<rr<<" f="<<ff<<" l="<<ll<<" CLobs="<<CLobs(i,6)<<" CLpred="<<CLtotpred(yy,ss,rr,ff,ll)<<endl;
	  CLprop(i)=CLtotfrac(yy)(ss)(rr)(ff)(ll);
	  switch(LC_LHF){       // Cases match number of estimated parameters for simplicity
	    case 1:  // lognormal
	      LHtemp=dnorm(log(CLprop(i)+tiny),log(CLobs(i,6)+tiny),sqrt(CLCV_num/(CLobs(i,6)+tiny))); // Log-normal LHF
	    break;
	    case 2: // lognormal with no constant p(u,l) * {ln(p(u,l)) – ln(phat(u,l))}^2
	      LHtemp=CLobs(i,6) * square(log(CLobs(i,6)+tiny) - log(CLprop(i)+tiny));
	    break;
	    case 3: // Sqrt difference 
	      //LHtemp= square(pow(CLobs(i,6)+tiny,0.5)-pow(CLprop(i)+tiny,0.5)); //sqrt multinomial is approximately normal 
	    break;
	  }
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
	if(debug)cout<<"---  * Finished SOO LHF ---"<<endl;
	// -- PSAT tagging --
	for(int i=1;i<=nPSAT;i++){ // Tags with certain stock of origin
	  int pp=PSAT(i,1);   // Population 
	  int aa=PSAT(i,2);   // Movement age class
	  int ss=PSAT(i,3);   // Subyear
	  int tt=PSAT(i,4);   // Time at liberty (subyears)
	  int rr=PSAT(i,5);   // Region from
	  int r2=PSAT(i,6);   // Region to
	  switch(ET_LHF){       // Cases match number of estimated parameters for simplicity
	    case 1:  // multinomial
	   	LHtemp=(-PSAT(i,7)*log(movm(pp,ss,aa,rr,r2)+tiny)); // Multinomial LHF
	    break;
	    case 2: // approximately multinomial and zero when fit perfect: -[ET*ln(mov) – ET*ln(mov$)] 
	        LHtemp=( PSAT(i,7)*log(PSAT(i,8)) - PSAT(i,7)*log(movm(pp,ss,aa,rr,r2)+tiny) ); // Multinomial LHF
	    break;
	    case 3: // sqrt difference
	    	//LHtemp=square(pow(PSAT(i,8)+tiny,0.5)-pow(movm(pp,ss,aa,rr,r2)+tiny,0.5)); // multinomial approximation
	    break; 
	  }
	  PSATpred(i)=movm(pp,ss,aa,rr,r2);
	  objPSAT+=LHtemp*LHw(6);                               // Weighted likelihood contribution
	  objG+=LHtemp*LHw(6);                                  // weighted likelihood contribution
	}
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
       for(int ii=1; ii<=nSpatPrq;ii++){
          qSP(ii)=1.0; // new derivation is a product so requires initialization at 1
       }
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
       	  qSP(ii)*= pow((SpatPr(i,5)/SPpred(i)),SpatPr(i,10)); //Iobs(i,12)= w_i = (1/sigma_i^2) / SUM_i{ (1/sigma_i^2) }
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
}

void model_parameters::simsam(void)
{
  {
        // If working with simulated data do some printing
        cout<<"log R0 = " <<lnR0<<endl;                    // Estimated R0 
        cout<<"selpar = "<<selpar<<endl;                     // Estimated R0
        for(int ff=1;ff<=nf;ff++){
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
}

void model_parameters::popnodes(void)
{
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
	report<<"D: depletion SSB(last year, last subyear)/SSB0"<<endl;
	report<<D<<endl;
	report<<"Dt: depletion SSB(last year, last subyear)/SSB(first year, first subyear)"<<endl;
	report<<Dt<<endl;
	report<<"SSBnow: current spawning biomass by stock"<<endl;
	report<<SSBnow<<endl;
	//names(LHs)<- c("Catch", "CPUE", "Survey", "L Comp", "SOO", "PSAT",  "Rec Dev", "Mov Pr", "Sel Pr", "SRA pen", "SSB Pr", "Dep Pr",      "MI Pr", "R0 diff","TOT")
        report<<"LHw"<<endl;
        report<<LHw<<endl;
	report<<"objC"  <<" objCPUE"<<" objI"  <<" objCL"  <<" objSOO"  <<" objPSAT"  <<" objRD"  <<" objmov"<<" objsel"    <<" objSRA"           <<" objSSB" <<" objDep"   <<" objFmod" <<" objR0"   <<" objG" <<" objSOOm"  <<" objSOOg"   <<" objSP"<<endl;
	report<<objC<<" "<<objCPUE<<" "<<objI<<" "<<objCL<<" "<<objSOO<<" "<<objPSAT<<" "<<objRD<<" "<<objmov<<" "<<objsel<<" "<<objSRA*LHw(11)<<" "<<objSSB<<" "<<objDep<<" "<<objFmod<<" "<<objR0<<" "<<objG<<" "<<objSOOm<<" "<<objSOOg<<" "<<objSP<<endl;
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
}

void model_parameters::set_runtime(void)
{
  dvector temp1("{100000}");
  maximum_function_evaluations.allocate(temp1.indexmin(),temp1.indexmax());
  maximum_function_evaluations=temp1;
  dvector temp("{1.e-6}");
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
