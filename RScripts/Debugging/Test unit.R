
library(ABTMSE)
loadABT()

OMd="C:/M3"

OM<-new('OM',OMd,nsim=32)
load("G:/ABT-MSE/Objects/OMs/Test_OM") # OMI

Obs=Bad_Obs
Testdir="C:/TEST"
M3dir="C:/M3"
ploty=F

sfInit(parallel=TRUE, cpus=detectCores())

TEST1<-new('TEST',OM,OMI,Bad_Obs,Testdir="C:/TEST",M3dir="C:/M3")

plot(TEST1)


OMI_test<-OMI
OM_test<-OM
TEST<-TEST1

save(OMI_test,file="G:/ABT-MSE/R_package/ABTMSE/Data/OMI_test")
save(OM_test,file="G:/ABT-MSE/R_package/ABTMSE/Data/OM_test")
save(TEST,file="G:/ABT-MSE/R_package/ABTMSE/Data/TEST")

