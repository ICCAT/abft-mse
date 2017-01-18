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


# --- Source MSE functions and objects ------

source("Source/MSE_source.r")
source("Source/Objects.r")


# -- Load bad observation model ------

#load(paste0(getwd(),"/Objects/Observation models/Good_Obs"))# make the good obs model!
load(paste0(getwd(),"/Objects/Observation models/Bad_Obs"))
MPs="UMSY"
interval=3
IE="Umax"

#out<-M3read(OMDir="C:/ABT-MSE/Objects/OMs/1")
OM<-new('OM',OMd="C:/ABT-MSE/Objects/OMs/1",nsim=8,proyears=30,seed=1,targpop=NA)

sfInit(parallel=T,cpus=detectCores())
MSE<-new('MSE',OM,Obs,MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI")),interval=3,IE="Umax")

getperf(MSE)
plot(MSE)
TOplot(MSE)



avail('ABT_MP')

