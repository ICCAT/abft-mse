
# MSE test


library(ABTMSE)
loadABT()
sfInit(parallel=T,cpus=detectCores())

MPs<-list(c('UMSY','UMSY'),
            c('UMSY_PI','UMSY_PI'))
OM=OM_example
Obs=Bad_Obs
interval=5
IE="Umax_90"
curTAC=c(13500000,2000000)
Allocation=NA
MPareas=NA
Fdistyrs=3



myMSE<-new('MSE',OM=OM_example,Obs=Bad_Obs,MPs=MPs,interval=5,IE="Umax_90")



plot(myMSE)


