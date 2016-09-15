
# ============================================================================================================================================
# === Build a preliminary Observation model object 'Obs' ============================================================================
# ============================================================================================================================================

# Indexing: S sim   P pop   A age   Y year   M subyear   R region   rng sample range

# --- Set working directory ------

setwd("G:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")

# --- Create a blank Obs definition object

Obs<-new('Obs')

Obs@Ccv<-Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.1,0.3)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.1
Obs@hbcv<-Obs@Btbcv<-Obs@Dbcv<-0.5
Obs@Kbcv<-Obs@t0bcv<-Obs@Linfbcv<-0.025
Obs@FMSYbcv<-Obs@FMSY_Mbcv<-Obs@BMSY_B0bcv<-0.1
Obs@nCAAobs<-c(500,1000)
Obs@nCALobs<-1000
Obs@Lcv<-c(0.05,0.1)
Obs@Ibeta<-c(0.75,1.25)#exp(runif(nsim,log(0.75),log(1.25)))

save(Obs,file=paste(getwd(),"/Objects/Bad_Obs",sep=""))

Obs<-new('Obs')

