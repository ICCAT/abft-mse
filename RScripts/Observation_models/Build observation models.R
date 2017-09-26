
# ============================================================================================================================================
# === Build a preliminary Observation model object 'Obs' ============================================================================
# ============================================================================================================================================

# --- Set working directory ------

setwd("C:/GitHub/abft-mse/")#setwd("C:/Users/tcar_/Documents/GitHub/abft-mse")
source("Source/MSE_source.r")
source("Source/Objects.r")
#MPind2<-read.csv("Data/Processed/MP Indices/MP indices compiled assessment 2017.csv")
load(file=paste(getwd(),"/Objects/OMs/Base_OM",sep=""))

CPUEdat<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE indices compiled 2017 assessment.csv",sep=""))
Iobs<-read.csv(paste(getwd(),"/Data/Raw/SSB/FI_indices_compiled_assessment_2017.csv",sep=""))


CPUEdat<-cbind(CPUEdat,rep(1,nrow(CPUEdat)),rep("Kimoto",nrow(CPUEdat)))
names(CPUEdat)[10:11]<-c("Type","Source")

Iobs<-cbind(Iobs,rep("Kimoto",nrow(Iobs)))
names(Iobs)<-c("Year","Quarter","Area","Stock","Ino","Type","Index","CV","Name","Source")

MPind<-rbind(Iobs[,c(1,9,7,8,6,10,3)],CPUEdat[,c(1,9,6,8,10,11,3)])

ToKeep<-c("JPN_LL_NEAtl2","FR_AER_SUV","MED_LAR_SUV","MED_AER_SUV","JPN_LL2","US_RR_115_144","GOM_LAR_SUV")
MPind<-subset(MPind,MPind$Name%in%ToKeep)
No<-match(MPind$Name,ToKeep)
MPind<-MPind[order(No),]
Stock<-c(2,2,2,2,1,1,1,1,1,1)[MPind$Area]
MPind<-data.frame(Year=MPind$Year,No=No[order(No)],Name=MPind$Name,Index=MPind$Index,CV=MPind$CV,Stock=Stock,Type=MPind$Type,Source=MPind$Source,Areas=MPind$Area)
names(MPind)<-c('Year','No','Name','Index','CV','Stock','Type','Source','Areas')

MPind<-subset(MPind,MPind$Year>=OMI@years[1]&MPind$Year<=OMI@years[2])
MPind$Year<-MPind$Year-OMI@years[1]+1

Indices<-MPind[match(ToKeep,MPind$Name),c(2,3,6)]
save(Indices,file=paste0(getwd(),"/R_package/ABTMSE/data/Indices"))

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
save(Bad_Obs,file=paste(getwd(),"/Objects/Observation_models/Bad_Obs",sep=""))


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
save(Good_Obs,file=paste(getwd(),"/Objects/Observation_models/Good_Obs",sep=""))


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
save(Perfect_Obs,file=paste(getwd(),"/Objects/Observation_models/Perfect_Obs",sep=""))


