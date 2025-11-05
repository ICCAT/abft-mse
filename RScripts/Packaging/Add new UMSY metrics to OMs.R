# ============================================================
# === Copy datafiles to the  R package
# ============================================================


library('ABTMSE')
loadABT()

setwd("C:/Users/tcarruth/Dropbox/abft-mse")
setwd("C:/Users/tcar_/Dropbox/abft-mse")

datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")

OMs<-list.files(datadir)
getfile<-(grepl("ROM_",OMs)|grepl("OM_",OMs))&grepl("d",OMs)
fulldir<-list.files(datadir,full.names=T)[getfile]
TDrefs<-OMs[getfile]
OMcode<-unlist(strsplit(TDrefs,"d"))


# Operating models and operating model input files
doU<-function(i,fulldir,TDrefs,OMcode,datadir,OMs){
  
  U3<-function(x,dset,Urat=0.03)dset$Bt_PI[x]*Urat
  U4<-function(x,dset,Urat=0.04)dset$Bt_PI[x]*Urat
  U5<-function(x,dset,Urat=0.05)dset$Bt_PI[x]*Urat
  U6<-function(x,dset,Urat=0.06)dset$Bt_PI[x]*Urat
  U7<-function(x,dset,Urat=0.07)dset$Bt_PI[x]*Urat
  U8<-function(x,dset,Urat=0.08)dset$Bt_PI[x]*Urat
  U9<-function(x,dset,Urat=0.09)dset$Bt_PI[x]*Urat
  U10<-function(x,dset,Urat=0.1)dset$Bt_PI[x]*Urat
  
  class(U3)<-class(U4)<-class(U5)<-class(U6)<-class(U7)<-class(U8)<-class(U9)<-class(U10)<-'MP'
  
  load(fulldir[i])
  OM<-get(TDrefs[i])
  myMPs<-list(c("U3","U3"),c("U4","U4"),c("U5","U5"),
       c("U6","U6"),c("U7","U7"),c("U8","U8"),c("U9","U9"),c("U10","U10"))
  nMPs<-length(myMPs)
  MSE<-new('MSE',OM,MPs=myMPs)
  
  pp<-1
  EBr<-(MSE@SSB[,,pp,MSE@nyears+MPlag+50]/array(rep(MSE@dynB0[,pp,MPlag+50]*MSE@SSBMSY_SSB0[,pp],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim)))[2:9,1]
  EUMSY<-approx(EBr,seq(0.03,0.1,by=0.01),1)$y
  pp<-2
  WBr<-(MSE@SSB[,,pp,MSE@nyears+MPlag+50]/array(rep(MSE@dynB0[,pp,MPlag+50]*MSE@SSBMSY_SSB0[,pp],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim)))[2:9,1]
  WUMSY<-approx(WBr,seq(0.03,0.1,by=0.01),1)$y
  
  nsim<-OM@nsim
  OM@UMSY<-matrix(rep(c(EUMSY,WUMSY),nsim),byrow=T,nrow=nsim)
  assign(TDrefs[i],OM)
  do.call(save, list(TDrefs[i],file=fulldir[i]))
  
  othOMs<-match(OMcode[i],OMs)
  othdir<-list.files(datadir,full.names=T)[othOMs]
  load(fulldir[i])
  OMstoch<-get(OMcode[i])
  nsim<-OMstoch@nsim
  OMstoch@UMSY<-matrix(rep(c(EUMSY,WUMSY),nsim),byrow=T,nrow=nsim)
  assign(OMcode[i],OMstoch)
  do.call(save, list(OMcode[i],file=othdir))
  print(c(EUMSY,WUMSY))
  
}

sfInit(parallel=T,cpus=8)
sfLibrary(ABTMSE)
sfExport(list=list("Perfect_Obs","Ann_Cat","U3","U4","U5","U6","U7","U8","U9","U10"))
sapply(1:length(OMcode),doU,fulldir=fulldir,TDrefs=TDrefs,OMcode=OMcode,datadir=datadir,OMs=OMs)





# END OF CREATE EXAMPLES =========================================================================================



