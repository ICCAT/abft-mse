# FI indices.r
# August 2016
# R Script for formatting fishery independen indices


RAIind<-TEG(dim(Base@RAI))
IobsRAI<-cbind(RAIind,rep(1,nrow(RAIind)),rep(3,nrow(RAIind)),rep(3,nrow(RAIind)),Base@RAI[RAIind])

#Iobs<-read.csv(paste(getwd(),"/Data/Raw/SSB/FI_indices_compiled_assessment_2017.csv",sep=""))
Iobs<-read.csv(paste(getwd(),"/data/ICCAT_2019_2/FI_indices_compiled_OM2019.csv",sep=""))
Iobs<-Iobs[Iobs$Year>=Base@years[1]&Iobs$Year<=Base@years[2],]
Iobs$Year<-Iobs$Year-Base@years[1]+1
Iobs$CV[Iobs$CV<0.25]<-0.25
Iobscv<-aggregate(Iobs$CV,list(Iobs$Ino),mean)
Iobscv<-Iobscv$x[order(Iobscv[,1])]

for(i in 1:max(Iobs$Ino)){
  cond<-Iobs$Ino==i
  Iobs$index[cond]<-Iobs$index[cond]/mean(Iobs$index[cond])
}

Inames<-as.character(unique(Iobs[,9]))
wt<-1/(Iobs$CV^2)
Ft<-rep(0,nrow(Iobs))
FRadj<-Iobs[,5]%in%grep("FR_AER",Inames)
Ft[FRadj]<-match("RRUSAFS",Fleets$name)
Iobs[FRadj,6]<-4

Iobs<-as.matrix(cbind(Iobs[,1:8],Ft,wt))

save(Iobs,file=paste(getwd(),"/Data/Processed/Conditioning/Iobs 2019",sep=""))

