# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Run MSEs
# models

# Tom Carruthers UBC
# Laurie Kell ICCAT

# 8th August 2016

rm(list=ls(all=TRUE))                             # Remove all existing objects from environment (or they get sent to the cluster and it could take a long time)
setwd("C:/ABT-MSE/")                              # The working directory

library(ABTMSE)
loadABT()

OM<-new('OM',OMd="C:/ABT-MSE/Objects/OMs/1",nsim=48,proyears=30,seed=1,targpop=NA)

sfInit(parallel=T,cpus=detectCores())
MSE<-new('MSE',OM,Obs=Bad_Obs,MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI")),interval=3,IE="Umax_90")

getperf(MSE)
plot(MSE)
TOplot(MSE)


