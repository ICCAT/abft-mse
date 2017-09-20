# Length observations.r
# August 2016
# R Script for formatting the length observations by fleet

dat<-read.csv(paste(getwd(),"/Data/Raw/Task2/t2szBFT-all_v1.csv",sep=""))
dat<-dat[dat$YearC>(Base@years[1]-1)&dat$YearC<(Base@years[2]+1),]
dat$FleetCode[dat$FleetCode=="USA-Rec"]<-"USA"
dat$FleetCode[dat$FleetCode=="USA-Com"]<-"USA"


names(dat)[names(dat)=="YearC"]<-"Year"
names(dat)[names(dat)=="TimePeriodCatch"]<-"TimePeriodID"
names(dat)[names(dat)=="GearGrpCode"]<-"GearGrp"

dat<-ICCATtoGEO2(dat)
dat<-assign_area(dat,Base@area_defs)

dat<-assign_quarter(dat)
names(dat)[names(dat)=="Subyear"]<-"Quarter"

dat<-AssignFleet(dat,Base@Fleets)

#test<-aggregate(rep(1,nrow(dat)),by=list(dat$Fleet),sum)
#cbind(Base@Fleets$name[test[,1]],test)
#test<-aggregate(rep(1,nrow(dat)),by=list(dat$FleetCode,dat$GearGrp),sum)
#test[order(test$x,decreasing=T),]

dat$Year<-dat$Year-Base@years[1]+1


Lencat<-rep(NA,nrow(dat))
for(i in 1:Base@nl){

  Lencat[dat$ClassFrq>(Base@lenbins[i]-0.01)&dat$ClassFrq<Base@lenbins[i+1]]<-i

}
cond<-!is.na(Lencat)
Lencat<-Lencat[cond]
dat<-dat[cond,]


CLobs<-aggregate(dat$Nr,by=list(dat$Year,dat$Quarter,dat$Area,dat$Fleet,Lencat),sum)
CLtot<-aggregate(dat$Nr,by=list(dat$Year,dat$Fleet),sum)
CLtot2<-aggregate(dat$Nr,by=list(dat$Year,dat$Quarter,dat$Area,dat$Fleet),sum)


CLtota<-array(NA,c(Base@ny,Base@nf))
CLtota[as.matrix(CLtot[,1:2])]<-CLtot[,3]

CLtota2<-array(NA,c(Base@ny,Base@ns, Base@nr, Base@nf))
CLtota2[as.matrix(CLtot2[,1:4])]<-CLtot2[,5]


names(CLobs)<-c("Year","Subyear","Area","Fleet","Length_category","N")
#CLobs$N<-(CLobs$N/CLtota[as.matrix(CLobs[,c(1,4)])])*log(CLtota[as.matrix(CLobs[,c(1,4)])])
CLobs$N<-CLobs$N/CLtota2[as.matrix(CLobs[,c(1:4)])]

CLobs<-subset(CLobs,CLobs$N>0)

save(CLobs,file=paste(getwd(),"/Data/Processed/Conditioning/CLobs",sep=""))





























