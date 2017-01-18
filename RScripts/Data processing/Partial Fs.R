# Partial Fs.r
# August 2016
# R Script for deriving partial Fs from a master index and fleet-specific catches

Catches<-Eobs<-array(NA,c(Base@ny,Base@ns,Base@nr,Base@nf))
Catches[as.matrix(Base@Cobs[,1:4])]<-Base@Cobs[,5]
Catches<-(Catches/(mean(Catches,na.rm=T)))*0.01
ind<-TEG(c(Base@ny,Base@ns,Base@nr,Base@nf))
#CPUEobs[ind]<--log((1-Catches[ind]/Base@RAI[ind[,1:3]]))
Eobs[ind]<-Catches[ind]/Base@RAI[ind[,1:3]]


Eobs<-cbind(ind,ind[,4],Eobs) # y s r f i cpue/pf
Eobs<-Eobs[!is.na(Eobs[,6]),]
mubyfleet<-aggregate(Eobs[,6],by=list(Eobs[,4]),FUN=mean)
Eobs[,6]<-Eobs[,6]/mubyfleet[Eobs[,4],2]

save(Eobs,file=paste(getwd(),"/Data/Processed/Conditioning/Eobs",sep=""))

#aggregate(CPUEobs[,6],by=list(CPUEobs[,4]),FUN=mean)