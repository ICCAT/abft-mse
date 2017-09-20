
library(ABTMSE)
loadABT()

sfInit(parallel=T,cpus=detectCores())

OM<-new('OM',OMd="C:/GitHub/abft-mse/Objects/OMs/1",nsim=48,proyears=30,seed=1)

MSE<-new('MSE',OM=OM,Obs=Good_Obs,MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI")),interval=3,IE="Umax_90")



setMethod("initialize", "MSE", function(.Object,OM,Obs,MPs=list(c("UMSY","UMSY")),interval=3,IE="Umax_90",
                                        curTAC=c(13500000,2000000),Allocation=NA,MPareas=NA,Fdistyrs=3){
  .Object})

  .Object<-new('MSE')

  OM=OM_example
  Obs=Bad_Obs
  MPs=list(c("UMSY","UMSY"))
  interval=3
  IE="Umax_90"
  curTAC=c(13500000,2000000)
  Allocation=NA
  MPareas=NA
  Fdistyrs=3



