# graphing of t2sz data
rm(list=ls(all=TRUE))                  # Remove all existing objects from environment
set.seed(2)                            # Ensure reproducible results by setting random seed
setwd("G:/ABT-MSE/")                   # Set the working directory
source("Source/MSE_source.r")          # Load the source code

setwd("G:/ABT-MSE/Data/Task2/")
dat<-read.csv('t2szBFT-all_v1 (1).csv')
dat<-dat[,c(3,6,7,15,20,21,22,23,26,28)]
dat<-ICCATtoGEO2(dat)

Area_names<-c("GOM_11","GSL_11", "CAR_11", "WATL_11", "SCATL_11", "NCATL_11", 
                   "NEATL_11", "EATL_11", "SEATL_11", "WMED_11", "EMED_11")                     # The 11 area model (areas of the Monterey CMG originally proposed by M.Lauretta)
comp<-


na<-length(Area_names)

Area_defs<-new('list')
for(i in 1:na)Area_defs[[i]]<-AreaDefs[[match(Area_names[i],AreaNames)]]    # Obtain polygon definition files

dat<-assign_area(dat,Area_defs)

names(dat)[4]<-"TimePeriodID"
dat<-assign_quarter(dat)
attach(dat)


Years<-unique(YearC)
ny<-length(Years)

fleets<-c('PS','LL','TP','BB','RR','OTH')
nf<-length(fleets)
dat$GearGrpCode<-as.character(dat$GearGrpCode)
dat$GearGrpCode[!dat$GearGrpCode%in%fleets]<-'OTH'

nl<-ceiling(quantile(dat$ClassFrq,0.95))
len<-1:nl
dat<-subset(dat,dat$ClassFrq<nl)
Fleet<-match(dat$GearGrpCode,fleets)
Year<-match(dat$YearC,Years)

CS<-array(0,c(ny,4,na,nf,nl))

agg<-aggregate(dat$Nr,by=list(Year,dat$Subyear,dat$Area,Fleet,dat$ClassFrq),sum)
names(agg)<-c("Year","Subyear","Area","Fleet","Len","N")
agg$N<-agg$N+1
CS[as.matrix(agg[,1:5])]<-agg[,6]
ny
apply(agg,2,max)




