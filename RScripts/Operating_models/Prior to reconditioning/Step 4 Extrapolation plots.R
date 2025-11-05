
library(ABTMSE)
loadABT()
packageVersion('ABTMSE')

addlab<-function(){
  mtext("Year",1,outer=T,line=0.3)
  mtext("Biomass",2,outer=T,line=0.3)
}

dopar<-function() par(mfrow=c(4,3),mai=c(0.4,0.3,0.4,0.01),omi=c(0.4,0.4,0.01,0.01))

# Do Deterministic Calculations ------------------------------------------------

  MPs<-list(c('CurC100','CurC100'))
  outdir<-"C:/Users/tcarruth/Dropbox/abft-mse/Results/Extrapolation/"
  figres<-400
  figw<-8
  figh<-10

  MSEs<-list()
  OMnos<-1:12
  OMs<-paste0("OM_",OMnos,"d")
  for(i in OMnos){
    MSEs[[i]]<-new('MSE',OM=get(OMs[i]),Obs=Perfect_Obs,MPs=MPs,interval=2,IE="Umax_90", MPareas=NA,Fdistyrs=3,maxTAC=c(10,10),MSEparallel=F)
    print(paste(i,"complete"))
  }
  save(MSEs,file=paste0(outdir,"MSEs_d"))



  # --- Plot results ---------------------------------------------------

  OMcodes<-apply(Design$Design_Ref,1,FUN=function(x)paste(x,collapse="-"))

  # --- Deterministic -------------------

  #load(paste0(outdir,"MSEs_d"))

  # West stock scenarios

  jpeg(paste0(outdir,"West_deterministic2.jpg"),height=figh,width=figw,res=figres,units='in')

    dopar()
    for(i in OMnos[1:11])Explot_SS(MSEs[[i]],plotnam=paste0("OM_",i,"d: ",OMcodes[i]))
    Explot_SS(MSEs[[12]],plotnam=paste0("OM_",12,"d: ",OMcodes[12]),legendy=T,MPnams=c("Zero catch","Current catch"))
    addlab()

  dev.off()

  # East stock scenarios

  jpeg(paste0(outdir,"East_deterministic2.jpg"),height=figh,width=figw,res=figres,units='in')

    dopar()
    for(i in OMnos[1:11])Explot_SS(MSEs[[i]],plotnam=paste0("OM_",i,"d: ",OMcodes[i]),S_A=1)
    Explot_SS(MSEs[[12]],plotnam=paste0("OM_",12,"d: ",OMcodes[12]),legendy=T,MPnams=c("Zero catch","Current catch"))
    addlab()

  dev.off()


  # Do Stochastic Calculations ------------------------------------------------

  MPs<-list(c('CurC100','CurC100'))
  outdir<-"C:/Users/tcarruth/Dropbox/abft-mse/Results/Extrapolation/"
  figres<-400
  figw<-8
  figh<-10

  MSEs<-list()
  OMnos<-1:12
  OMs<-paste0("OM_",OMnos)
  for(i in OMnos){
    MSEs[[i]]<-new('MSE',OM=get(OMs[i]),Obs=Perfect_Obs,MPs=MPs,interval=2,IE="Umax_90", MPareas=NA,Fdistyrs=3,maxTAC=c(10,10),MSEparallel=F)
    print(paste(i,"complete"))
  }
  save(MSEs,file=paste0(outdir,"MSEs_s"))



  # --- Plot results ---------------------------------------------------

  OMcodes<-apply(Design$Design_Ref,1,FUN=function(x)paste(x,collapse="-"))

  # --- Deterministic -------------------

  #load(paste0(outdir,"MSEs_s"))

  # West stock scenarios

  jpeg(paste0(outdir,"West_stochastic.jpg"),height=figh,width=figw,res=figres,units='in')

    dopar()
    for(i in OMnos[1:11])Explot_SS(MSEs[[i]],plotnam=paste0("OM_",i,"d: ",OMcodes[i]))
    Explot_SS(MSEs[[12]],plotnam=paste0("OM_",12,"d: ",OMcodes[12]),legendy=T,MPnams=c("Zero catch","Current catch"))
    addlab()

  dev.off()

  # East stock scenarios

  jpeg(paste0(outdir,"East_stochastic.jpg"),height=figh,width=figw,res=figres,units='in')

    dopar()
    for(i in OMnos[1:11])Explot_SS(MSEs[[i]],plotnam=paste0("OM_",i,"d: ",OMcodes[i]),S_A=1)
    Explot_SS(MSEs[[12]],plotnam=paste0("OM_",12,"d: ",OMcodes[12]),legendy=T,MPnams=c("Zero catch","Current catch"))
    addlab()

  dev.off()

