# MP testing

library(ABTMSE)
loadABT()
sfInit(parallel=T,cpus=detectCores())
sfExport(list=c("XSA","DD","CDD","DD_R","UMSY","tiny"))

dset<-dset_example_East

nsim<-dim(dset$Iobs)[1]

sapply(1:nsim,CDD_i4,dset)
sfSapply(1:nsim,CDD_i4,dset)
sfSapply(1:nsim,DD_i4,dset_example_West)


paste()

apply(((MSE@C[,sim,pp,ind]-MSE@C[,sim,pp,ind1])^2)^0.5/MSE@C[,sim,pp,ind1],1:2,mean)

C<-MSE@C[3,39,pp,]
C[C==0]<-tiny
mean(((C[ind]-C[ind1])^2)^0.5/C[ind1])
