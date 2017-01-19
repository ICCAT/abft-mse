
# ============================================================================================================================================
# === Build a preliminary Observation model object 'Obs' ============================================================================
# ============================================================================================================================================

# --- Set working directory ------

setwd("C:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")
MPind<-read.csv("Data/Processed/MP Indices/MP indices compiled.csv")


# --- Create a bad observation error model

Obs<-new('Obs')

Obs@Ccv<-Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.2,0.4)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.25
Obs@hbcv<-Obs@Btbcv<-Obs@Dbcv<-1
Obs@Kbcv<-Obs@t0bcv<-Obs@Linfbcv<-0.05
Obs@FMSYbcv<-Obs@FMSY_Mbcv<-Obs@BMSY_B0bcv<-0.3
Obs@nCAAobs<-c(50,100)
Obs@nCALobs<-100
Obs@Lcv<-c(0.05,0.1)
Obs@Ibeta<-c(0.5,2)#exp(runif(nsim,log(0.75),log(1.25)))
Obs@MPind=MPind
Bad_Obs<-Obs
save(Bad_Obs,file=paste(getwd(),"/Objects/Observation models/Bad_Obs",sep=""))


# --- Create a good observation error model

Obs<-new('Obs')

Obs@Ccv<-Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.1,0.2)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.1
Obs@hbcv<-Obs@Btbcv<-Obs@Dbcv<-0.5
Obs@Kbcv<-Obs@t0bcv<-Obs@Linfbcv<-0.025
Obs@FMSYbcv<-Obs@FMSY_Mbcv<-Obs@BMSY_B0bcv<-0.1
Obs@nCAAobs<-c(500,1000)
Obs@nCALobs<-1000
Obs@Lcv<-c(0.05,0.1)
Obs@Ibeta<-c(0.75,1.25)#exp(runif(nsim,log(0.75),log(1.25)))
Obs@MPind=MPind
Good_Obs<-Obs
save(Good_Obs,file=paste(getwd(),"/Objects/Observation models/Good_Obs",sep=""))


# --- Create a perfect observation error model

Obs<-new('Obs')

Obs@Ccv<-Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.01,0.02)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.01
Obs@hbcv<-Obs@Btbcv<-Obs@Dbcv<-0.01
Obs@Kbcv<-Obs@t0bcv<-Obs@Linfbcv<-0.001
Obs@FMSYbcv<-Obs@FMSY_Mbcv<-Obs@BMSY_B0bcv<-0.01
Obs@nCAAobs<-c(5000,10000)
Obs@nCALobs<-10000
Obs@Lcv<-c(0.05,0.1)
Obs@Ibeta<-c(0.99,1.01)#exp(runif(nsim,log(0.75),log(1.25)))
Obs@MPind=MPind
Perfect_Obs<-Obs
save(Perfect_Obs,file=paste(getwd(),"/Objects/Observation models/Perfect_Obs",sep=""))


