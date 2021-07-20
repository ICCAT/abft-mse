# Length observations.r
# August 2016
# R Script for formatting the length observations by fleet

dat<-read.csv(paste(getwd(),"/Data/Raw/Task2/t2szBFT-all_v1.csv",sep=""))

# Temporary fix for new data of wrong format
IDgrep<-c("JP","US","CA")
Fleetcody<-c("JPN","USA","CAN")
FleetCode<-rep("UKN",nrow(dat))
for(ff in 1:length(IDgrep))FleetCode[grepl(IDgrep[ff],dat$FleetID)]<-Fleetcody[ff]
                 
dat<-dat[dat$YearC>(Base@years[1]-1)&dat$YearC<(Base@years[2]+1),]
#dat$FleetCode[dat$FleetCode%in%c("USA-Rec" ,"USA-Com")]<-"USA"
#dat$GearCode[dat$GearCode%in%c("RRFB","RRFS")]<-"RR"
# temp<-dat[dat$FleetCode=="USA",] unique(temp$FleetCode) unique(dat$GearGrp)
#dat<-dat[dat$FleetCode=="USA"&dat$GearGrp=="RR",]
#cond<-dat$SampAreaCode%in%c("BF55","BF61","BF51","BF56","BF67")#dat$FleetCode=="USA"&dat$GearGrpCode=="RR"&
#dat$Lat[cond]<-35
#dat$Lon[cond]<-70
#dat$QuadID[cond]<-4
#dat$GeoStrata[cond]<-"5x5"

# Standardization of names
names(dat)[names(dat)=="YearC"]<-"Year"
names(dat)[names(dat)=="TimePeriodCatch"]<-"TimePeriodID"
names(dat)[names(dat)=="GearGrpCode"]<-"GearGrp"
names(dat)[names(dat)=="SquareTypeCode"]<-"GeoStrata"

# sum(dat$GeoStrata%in%c("1x1","5x5"))/nrow(dat)*100 # percentage reduction due to 1x1 5x5 stipulation
dat<-dat[dat$GeoStrata%in%c("1x1","5x5"),]
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

diag<-F
if(diag){

  Fnams<-c(Fleets$name,"OTH")
  Flabs<-Fnams[CLobs[,4]]
  CLobs_lab<-cbind(CLobs,Flabs)
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_wrap(~Year , ncol=10)
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_grid(~Flabs)
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_grid(~Subyear)
  #ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_grid(~Area,~Subyear)
  
  
}  
  
  
  


























