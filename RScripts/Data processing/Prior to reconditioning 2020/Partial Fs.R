# Partial Fs.r
# August 2016
# R Script for deriving partial Fs from a master index and fleet-specific catches

Catches<-Eobs<-array(0,c(Base@ny,Base@ns,Base@nr,Base@nf))
Catches[as.matrix(Base@Cobs[,1:4])]<-Base@Cobs[,5]
Catches<-(Catches/(mean(Catches,na.rm=T)))*0.001
Catches[is.na(Catches)]<-0
cond<-Catches>0
ind<-TEG(c(Base@ny,Base@ns,Base@nr,Base@nf))[cond,]

Eobs[ind]<--log((1-Catches[ind]/Base@RAI[ind[,1:3]]))
#Eobs[ind]<-Catches[ind]/Base@RAI[ind[,1:3]]


Eobs2<-cbind(ind,ind[,4],Eobs[ind]) # y s r f i cpue/pf
Eobs2<-Eobs2[!is.na(Eobs2[,6]),]
mubyfleet<-aggregate(Eobs2[,6],by=list(Eobs2[,4]),FUN=mean)
Eobs2[,6]<-Eobs2[,6]/mubyfleet[Eobs2[,4],2]*0.2
Eobs<-Eobs2

save(Eobs,file=paste(getwd(),"/Data/Processed/Conditioning/Eobs",sep=""))

#aggregate(CPUEobs[,6],by=list(CPUEobs[,4]),FUN=mean)
