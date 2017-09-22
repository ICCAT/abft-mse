
library(ABTMSE)
loadABT()

sfInit(parallel=T,cpus=detectCores())




OM<-new('OM',OMd="C:/GitHub/abft-mse/Objects/OMs/1",nsim=16,proyears=30,seed=1)

#MSE<-new('MSE',OM=OM,MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI")),interval=3,IE="Umax_90")

#MSE<-new('MSE',OM=OM,MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC100","EMP1w")),interval=3,IE="Umax_90")

MSE<-new('MSE',OM=OM,MPs=list(c("CurC100","EMP1w")),interval=3,IE="Umax_90")


MSE<-new('MSE',OM=OM,MPs=list(c("EMP1e","EMP1w"),c("EMP2e","EMP2w")),interval=3,IE="Umax_90")



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



