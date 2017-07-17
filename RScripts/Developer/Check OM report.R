
library(ABTMSE)
loadABT()
dirs="C:/Users/tcar_/Documents/abft-mse/M3"
addlab=TRUE


make_fit_reports(dirs="C:/Users/tcar_/Documents/abft-mse/M3",addlab=TRUE)

nOMs<-length(dirs)
Obs<-Bad_Obs #load(paste0(getwd(),"/Objects/Observation_models/Bad_Obs"))

i<-1
input_dir<-dirs[i]

out<-M3read(input_dir)
load(paste(input_dir,"/OMI",sep=""))
load(system.file("ts.Rdata", package="ABTMSE"))
dat<-subset(ts,catchScenario=="Reported")


wta<-t(out$wt_age[33,,])
surv<-t(exp(-apply(out$M_age,1,cumsum)))
Fec<-out$mat_age*wta
na<-dim(Fec)[2]

SSB0=out$muR*exp(out$lnHR1)*apply(surv*Fec,1,sum)#     // Unfished Spawning Stock Biomass
SSB0=SSB0+out$muR*exp(out$lnHR1)*Fec[,na]*surv[,na]*exp(-out$M_age[,na])/(1-exp(-out$M_age[,na]))# // indefinite integral of surv added to get plus group SSB0
