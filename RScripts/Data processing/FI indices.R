# FI indices.r
# April 2021
# R Script for formatting fishery independent indices


RAIind<-TEG(dim(Base@RAI)) # TEG (rather self centrically) stands for 'Toms Expanded Grid' and expands from a vector of dim lengths
IobsRAI<-cbind(RAIind,rep(1,nrow(RAIind)),rep(3,nrow(RAIind)),rep(3,nrow(RAIind)),Base@RAI[RAIind])

Iobs<-read.csv(paste(getwd(),"/data/ICCAT_2021_3/FI_indices_compiled_OM2021Apr19.csv",sep=""))
Iobs<-Iobs[Iobs$Year>=Base@years[1]&Iobs$Year<=Base@years[2],]
Iobs$Year<-Iobs$Year-Base@years[1]+1
Iobs$CV[Iobs$CV<0.25]<-0.25 # Note that this value is for conditioning - the future projection of indices uses the statistical specifications outlined in Table 7.1

for(i in 1:max(Iobs$Ino)){ # standardization to mean 1
  cond<-Iobs$Ino==i
  Iobs$index[cond]<-Iobs$index[cond]/mean(Iobs$index[cond])
}

Inames<-as.character(unique(Iobs[,9]))
#wt<-1/(Iobs$CV^2) # inverse variance weights
wt<-rep(1,length(Iobs$CV))   # even weights
lowwt<-c("GBYP_AER_SUV_BAR","CAN_ACO_SUV2") # !TSD Table 2.2
wt[Iobs[,9]%in%lowwt]<-0.01 # low weights for some series'

Ft<-rep(0,nrow(Iobs)) # Fleet mirror (where applicable)
lt<-rep(0,nrow(Iobs)) # Length vulnerability mirror

# French Aerial Survey mirroring
FRadj<-Iobs[,5]%in%grep("FR_AER",Inames)
Ft[FRadj]<-match("RRUSAFS",Fleets$name) # mirrors the RRUSAFS
Iobs[FRadj,6]<-4 # Type is biomass first two stocks (given RRUSAFS selectivity)

# Canadian Acoustic Survey mirrioring
CANadj<-Iobs[,5]%in%grep("CAN_ACO_SUV",Inames)
Ft[CANadj]<-match("RRCAN",Fleets$name)   # mirrors the RRCAN with length cat numbers
Iobs[CANadj,6]<-5 # Numbers first two stocks (given RRCAN selectivity)
lt[CANadj]<-6  # sixth length category

for_wt<-1/Iobs$CV^2
sum_for_wt<-aggregate(for_wt,list(Iobs$Ino),sum)$x
w<-for_wt/sum_for_wt[Iobs$Ino] # this is used in the MLE q estimation

tab<-cbind(aggregate(Iobs$Year+Base@years[1]-1,FUN=min,by=list(Iobs$Name)),
  aggregate(Iobs$Year+Base@years[1]-1,FUN=max,by=list(Iobs$Name))$x,
  aggregate(Iobs$subyear,FUN=min,by=list(Iobs$Name))$x,
  aggregate(Iobs$subyear,FUN=max,by=list(Iobs$Name))$x,
  Base@areanams[aggregate(Iobs$area,FUN=min,by=list(Iobs$Name))$x],
  Base@areanams[aggregate(Iobs$area,FUN=max,by=list(Iobs$Name))$x],
  aggregate(Iobs$stock,FUN=min,by=list(Iobs$Name))$x,
  aggregate(Iobs$Ino,FUN=min,by=list(Iobs$Name))$x,
  c("Biomass","SSB","Biomass","Vulnerable B","Vulnerable N")[aggregate(Iobs$type,FUN=min,by=list(Iobs$Name))$x],
  aggregate(Iobs$index,FUN=mean,by=list(Iobs$Name))$x,
  aggregate(Iobs$CV,FUN=mean,by=list(Iobs$Name))$x,
  aggregate(wt,FUN=mean,by=list(Iobs$Name))$x)

names(tab)<-c("Name","Start Yr","End Yr","Start q","End q","Min area","Max area","Stock","Ino","type","mu Index","mu CV","mu wt")

tab # !TSD verify with Table 2.2

Iobs<-as.matrix(cbind(Iobs[,1:8],Ft,wt,lt,w))

hist(Iobs[,10],breaks=seq(0,16,length.out=50)) # plot weights

save(Iobs,file=paste(getwd(),"/Data/Processed/Conditioning/Iobs",sep=""))


