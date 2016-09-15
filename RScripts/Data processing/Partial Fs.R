# Partial Fs.r
# August 2016
# R Script for deriving partial Fs from a master index and fleet-specific catches

Catches<-CPUEobs<-array(NA,c(Base@ny,Base@ns,Base@nr,Base@nf))
Catches[as.matrix(Base@Cobs[,1:4])]<-Base@Cobs[,5]
Catches<-(Catches/(mean(Catches,na.rm=T)))*0.001
ind<-TEG(c(Base@ny,Base@ns,Base@nr,Base@nf))
CPUEobs[ind]<--log((1-Catches[ind]/Base@RAI[ind[,1:3]]))

CPUEobs<-cbind(ind,ind[,4],CPUEobs) # y s r f i cpue/pf
CPUEobs<-CPUEobs[!is.na(CPUEobs[,6]),]
mubyfleet<-aggregate(CPUEobs[,6],by=list(CPUEobs[,4]),FUN=mean)
CPUEobs[,6]<-CPUEobs[,6]/mubyfleet[CPUEobs[,4],2]

save(CPUEobs,file=paste(getwd(),"/Data/Processed/Conditioning/CPUEobs",sep=""))

#aggregate(Base@CPUEobs[,6],by=list(Base@CPUEobs[,4]),FUN=mean)