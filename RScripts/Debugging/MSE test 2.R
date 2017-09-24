
library(ABTMSE)
loadABT()

sfInit(parallel=T,cpus=detectCores())


memory.limit(size = 24000)

OM2<-new('OM',OMd="C:/GitHub/abft-mse/Objects/OMs/36",nsim=70,proyears=50,seed=1)

#MSE<-new('MSE',OM=OM,MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI")),interval=3,IE="Umax_90")

#MSE<-new('MSE',OM=OM,MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC100","EMP1w")),interval=3,IE="Umax_90")

#MSE<-new('MSE',OM=OM,MPs=list(c("CurC100","EMP1w")),interval=3,IE="Umax_90")


MSE2<-new('MSE',OM=OM2,MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC150","CurC150"),c("EMP1e","EMP1w"),c("EMP2e","EMP2w")),interval=3,IE="Umax_90")

save(MSE2,file="C:/temp/MSE2.Rdata")
load(file="C:/temp/MSE2.Rdata")

jpeg("C:/GitHub/abft-mse/Outputs/Example PPlot.jpg",res=300,width=7,height=10,units='in')
PPlot(MSE)
dev.off()


jpeg("C:/GitHub/abft-mse/Outputs/Example plot.jpg",res=600,width=7,height=10,units='in')
plot(MSE,rev=F)
dev.off()


jpeg("C:/GitHub/abft-mse/Outputs/Example Tplot2.jpg",res=600,width=7.5,height=5.5,units='in')
Tplot2(MSE)
dev.off()


jpeg("C:/GitHub/abft-mse/Outputs/Example TplotS2.jpg",res=600,width=7.5,height=5.5,units='in')
TplotS2(MSE)
dev.off()


plot(MSE)


setMethod("initialize", "MSE", function(.Object,OM,Obs,MPs=list(c("UMSY","UMSY")),interval=3,IE="Umax_90",
                                        curTAC=c(13500000,2000000),Allocation=NA,MPareas=NA,Fdistyrs=3){
  .Object})

  .Object<-new('MSE')


  Obs=Bad_Obs
  MPs=list(c("UMSY","UMSY"))
  MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC100","EMP1w"))
  MPs=list( c("CurC100","EMP1w"))
  interval=3
  IE="Umax_90"
  curTAC=c(13500000,2000000)
  Allocation=NA
  MPareas=NA
  Fdistyrs=3



