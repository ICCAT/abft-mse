
library(ABTMSE)
loadABT()

sfInit(parallel=T,cpus=detectCores())


memory.limit(size = 24000)

OM2<-new('OM',OMd="C:/GitHub/abft-mse/Objects/OMs/1",nsim=53,proyears=10,seed=1)

#MSE<-new('MSE',OM=OM,MPs=list(c("UMSY","UMSY")),interval=3,IE="Umax_90")

#MSE<-new('MSE',OM=OM,MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC100","EMP1w")),interval=3,IE="Umax_90")

#MSE<-new('MSE',OM=OM,MPs=list(c("CurC100","EMP1w")),interval=3,IE="Umax_90")


MSE3<-new('MSE',OM=OM2,MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC150","CurC150"),c("EMP1e","EMP1w"),c("EMP2e","EMP2w")),interval=5,IE="Umax_90")

save(MSE3,file="C:/temp/MSE3.Rdata")
#load(file="C:/temp/MSE3.Rdata")

jpeg("C:/GitHub/abft-mse/Outputs/Example PPlot.jpg",res=300,width=7,height=10,units='in')
PPlot(MSE3)
dev.off()


jpeg("C:/GitHub/abft-mse/Outputs/Example plot.jpg",res=600,width=7,height=10,units='in')
plot(MSE3,rev=F)
dev.off()


jpeg("C:/GitHub/abft-mse/Outputs/Example Tplot2.jpg",res=600,width=7.5,height=5.5,units='in')
Tplot2(MSE3)
dev.off()


jpeg("C:/GitHub/abft-mse/Outputs/Example TplotS2.jpg",res=600,width=7.5,height=5.5,units='in')
TplotS2(MSE3)
dev.off()


perf<-getperf(MSE3)
write.csv(perf[[1]],"C:/GitHub/abft-mse/Outputs/East_perf.csv")
write.csv(perf[[2]],"C:/GitHub/abft-mse/Outputs/West_perf.csv")

write.csv(t(rbind(apply(MSE3@Istats[,,3],2,quantile,p=c(0.05,0.5,0.95)), # sd 1
                  apply(MSE3@Istats[,,2],2,quantile,p=c(0.05,0.5,0.95)), # ac 1
                  apply(MSE3@Istats[,,6],2,quantile,p=c(0.05,0.5,0.95)), # sd 2
                  apply(MSE3@Istats[,,5],2,quantile,p=c(0.05,0.5,0.95)), # ac 2
                  apply(MSE3@Istats[,,1],2,quantile,p=c(0.05,0.5,0.95))  # beta
                  )),"C:/GitHub/abft-mse/Outputs/Istats.csv")

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

OM4<-new('OM',OMd="C:/GitHub/abft-mse/Objects/OMs/36",nsim=5,proyears=50,seed=1)

sfStop()

system.time(
  MSE4<-new('MSE',OM=OM4,MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC150","CurC150"),c("EMP1e","EMP1w"),c("EMP2e","EMP2w")),interval=3,IE="Umax_90")
)


sfInit(parallel=T,cpus=detectCores())

system.time(
  MSE4<-new('MSE',OM=OM4,MPs=list(c("CurC50","CurC50"),c("CurC100","CurC100"),c("CurC150","CurC150"),c("EMP1e","EMP1w"),c("EMP2e","EMP2w")),interval=3,IE="Umax_90")
)









