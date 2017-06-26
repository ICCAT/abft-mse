# CATDIS.r
# August 2016
# R Script for correctly formatting the CATDIS data into the correct spatio-temporal strata

dat<-read.csv(paste(getwd(),"/Data/Raw/Catch/cdis5015_BFT.csv",sep="")) #
dat<-dat[,c(2,5,7,9,13,14,15)]
names(dat)[c(1,4,5,6,7)]<-c("Year","Quarter","Lat","Lon","Catch")
dat<-subset(dat,dat$Year>(Base@years[1]-1)&dat$Year<(Base@years[2]+1))
dat<-assign_area(dat,Base@area_defs)
dat<-AssignFleet(dat,Base@Fleets)
dat$Year<-dat$Year-Base@years[1]+1

Cobs<-dat[,c(1,4,8,9,7)]
Cobs[,5]<-Cobs[,5]*1000 # convert tonnes to kg
save(Cobs,file=paste(getwd(),"/Data/Processed/Conditioning/Cobs",sep=""))

somechecks<-F
if(somechecks){
  sum((1:Base@nf)%in%unique(dat$Fleet))==Base@nf
  test<-cbind(c(Base@Fleets$name,'OTH'),aggregate(dat$Catch,by=list(dat$Fleet),sum))
  test<-test[order(test$x,decreasing=T),]
  test<-cbind(test,round(test$x/sum(test$x)*100,2),round(cumsum(test$x/sum(test$x))*100,2))
  names(test)<-c("Fleet","Code","Catch(t)","%","Cum %")
  test
}
