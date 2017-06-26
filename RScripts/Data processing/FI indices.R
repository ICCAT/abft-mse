# FI indices.r
# August 2016
# R Script for formatting fishery independen indices

MED<-read.csv(paste(getwd(),"/Data/Raw/SSB/Balearic larval 2014.csv",sep=""))
MED<-MED[MED$Survey.Year>(Base@years[1]-1)&MED$Survey.Year<(Base@years[2]+1),]
MED$Survey.Year<-MED$Survey.Year-Base@years[1]+1
ng<-nrow(MED)
IobsMED<-cbind(MED$Survey.Year,rep(Base@spawns[1],ng),rep(match("MED",Base@areanams),ng),rep(1,ng),rep(1,ng),rep(2,ng),MED$Index.1)


GOM<-read.csv(paste(getwd(),"/Data/Raw/SSB/Ingram 2014 index.csv",sep=""))
GOM<-GOM[GOM$Year>(Base@years[1]-1)&GOM$Year<(Base@years[2]+1),]
GOM$Year<-GOM$Year-Base@years[1]+1
ng<-nrow(GOM)
IobsGOM<-cbind(GOM$Year,rep(Base@spawns[2],ng),rep(match("GOM",Base@areanams),ng),rep(2,ng),rep(2,ng),rep(2,ng),GOM$ZIDL)

#Year (1), subyear (2), area (3), stock (4)  index number (matching nI, 5), index type (biomass=1, SSB=2, column 6), the observed relative abundance index (column 7) 


RAIind<-TEG(dim(Base@RAI))
IobsRAI<-cbind(RAIind,rep(1,nrow(RAIind)),rep(3,nrow(RAIind)),rep(3,nrow(RAIind)),Base@RAI[RAIind])


#Iobs<-rbind(IobsMED,IobsGOM,IobsRAI) # alternatively add the master index to the biomass index. 
Iobs<-rbind(IobsMED,IobsGOM)

save(Iobs,file=paste(getwd(),"/Data/Processed/Conditioning/Iobs",sep=""))

