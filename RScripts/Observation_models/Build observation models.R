
# ============================================================================================================================================
# === Build a preliminary Observation model object 'Obs' ============================================================================
# ============================================================================================================================================

# --- Set working directory ------

setwd("C:/Users/tcar_/Dropbox/abft-mse/")#
setwd("C:/Users/tcarruth/Dropbox/abft-mse/")#
#source("Source/MSE_source.r")
#source("Source/Objects.r")
#MPind2<-read.csv("Data/Processed/MP Indices/MP indices compiled assessment 2017.csv")
load(file=paste(getwd(),"/Objects/OMs/Base_OM",sep=""))

CPUEdat<-read.csv(file=paste(getwd(),"/Data/ICCAT_2019_2/CPUE_indices compiled_2019OM_3.csv",sep=""))
Iobs<-read.csv(paste(getwd(),"/Data/ICCAT_2019_2/FI_indices_compiled_OM2019.csv",sep=""))

CPUEdat<-cbind(CPUEdat,rep(1,nrow(CPUEdat)),rep("Kimoto",nrow(CPUEdat)))
names(CPUEdat)[10:11]<-c("Type","Source")
CPUEdat$Fleet<-paste0("Fleet_",CPUEdat$Fleet)

Iobs<-cbind(Iobs,rep("Kimoto",nrow(Iobs)))
types<-c("B","SSB")
names(Iobs)<-c("Year","Quarter","Area","Stock","Ino","Fleet","Index","CV","Name","Source") # 'fleet' is B or SSB
Iobs$Fleet<-types[Iobs$Fleet]
Iobs$Fleet[grepl("FR_AER",Iobs$Name)]<-"Fleet_15"

# Year Name Index CV Fleet Source Area
MPind<-rbind(Iobs[,c(1,2,9,7,8,6,10,3)],CPUEdat[,c(1,2,9,6,8,5,11,3)])

ToKeep<-c("JPN_LL_NEAtl2","FR_AER_SUV2","MED_LAR_SUV","GBYP_AER_SUV_BAR","JPN_LL_West2","US_RR_66_114","GOM_LAR_SUV","CAN_ACO_SUV")
CPUEind<-c(1,0,0,0,1,1,0,0)
Fleet<-c(2,0,0,0,2,15,0,0) # fleet selectivities

MPind<-subset(MPind,MPind$Name%in%ToKeep)
#ToKeep<-unique(MPind$Name)
No<-match(MPind$Name,ToKeep)
MPind<-MPind[order(No),]

Stock<-c(2,2,2,1,1,1,1)[MPind$Area]
MPind<-data.frame(Year=MPind$Year,Quarter=MPind$Quarter,No=No[order(No)],Name=MPind$Name,Index=MPind$Index,CV=MPind$CV,Stock=Stock,Fleet=MPind$Fleet,Source=MPind$Source,Areas=MPind$Area)
names(MPind)<-c('Year','Quarter','No','Name','Index','CV','Stock','Fleet','Source','Areas')

MPind<-subset(MPind,MPind$Year>=OMI@years[1]) # &MPind$Year<2017)
MPind$Year<-MPind$Year-OMI@years[1]+1

Indices<-MPind[match(ToKeep,MPind$Name),c(3,4,7,2,10)]
Indstats<-MPind[MPind$Name%in%ToKeep,]

min<-aggregate(Indstats$Index,by=list(Indstats$Name),min)
max<-aggregate(Indstats$Index,by=list(Indstats$Name),max)
mu<-aggregate(Indstats$Index,by=list(Indstats$Name),mean)
med<-aggregate(Indstats$Index,by=list(Indstats$Name),quantile,p=0.5)
minyr<-aggregate(Indstats$Year+1964,by=list(Indstats$Name),min)
maxyr<-aggregate(Indstats$Year+1964,by=list(Indstats$Name),max)
#maxyr[as.numeric(as.character(maxyr))>2016]<-2016

reord<-match(Indices$Name,min[,1])
Indices<-cbind(Indices,min[reord,2],max[reord,2],mu[reord,2],med[reord,2],minyr[reord,2],maxyr[reord,2])
Indices[,4:7]<-round(Indices[,4:7],3)
names(Indices)<-c("No","Name","Stock","Quarter","Area","Min","Max","Mean","Median","First year","Last year")

save(Indices,file=paste0(getwd(),"/R_package/ABTMSE/data/Indices"))

# --- Create a bad observation error model

Obs<-new('Obs')
Obs@Ccv<-c(0.1,0.15)
Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.2,0.4)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.25
Obs@Cbias<-1
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

Obs@Ccv<-c(0.025,0.025)
Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.1,0.2)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.001
Obs@Cbias<-1
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



# --- Create a good observation error model with 20% under reporting (20% overages)

Obs<-new('Obs')
Obs@Ccv<-c(0.025,0.025)
Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(0.1,0.2)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-0.001
Obs@Cbias<-5/6 # this is 5/6 since the implementation overages are 20% = 6/5
Obs@hbcv<-Obs@Btbcv<-Obs@Dbcv<-0.5
Obs@Kbcv<-Obs@t0bcv<-Obs@Linfbcv<-0.025
Obs@FMSYbcv<-Obs@FMSY_Mbcv<-Obs@BMSY_B0bcv<-0.1
Obs@nCAAobs<-c(500,1000)
Obs@nCALobs<-1000
Obs@Lcv<-c(0.05,0.1)
Obs@Ibeta<-c(0.75,1.25)#exp(runif(nsim,log(0.75),log(1.25)))
Obs@MPind=MPind
Unreported_20<-Obs
save(Unreported_20,file=paste(getwd(),"/Objects/Observation_models/Unreported_20",sep=""))



# --- Create a perfect observation error model

Obs<-new('Obs')

Obs@Ccv<-Obs@Icv<-Obs@Dcv<-Obs@Btcv<-Obs@Ftcv<-c(1E-10,1E-9)
Obs@Cbcv<-Obs@Mbcv<-Obs@LFCbcv<-Obs@LFSbcv<-Obs@ageMbcv<-Obs@Ftbcv<-Obs@Recbcv<-Obs@IMSYbcv<-Obs@MSYbcv<-Obs@BMSYbcv<-1E-10
Obs@Cbias<-1
Obs@hbcv<-Obs@Btbcv<-Obs@Dbcv<-1E-10
Obs@Kbcv<-Obs@t0bcv<-Obs@Linfbcv<-1E-10
Obs@FMSYbcv<-Obs@FMSY_Mbcv<-Obs@BMSY_B0bcv<-1E-10
Obs@nCAAobs<-c(5000,10000)
Obs@nCALobs<-10000
Obs@Lcv<-c(0.05,0.1)
Obs@Ibeta<-c(0.99999,1.000001)#exp(runif(nsim,log(0.75),log(1.25)))
Obs@MPind=MPind
Perfect_Obs<-Obs
save(Perfect_Obs,file=paste(getwd(),"/Objects/Observation_models/Perfect_Obs",sep=""))


