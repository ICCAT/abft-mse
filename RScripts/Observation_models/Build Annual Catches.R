#setwd("C:/Users/tcar_/Dropbox/abft-mse/")#
#setwd("C:/Users/tcarruth/Dropbox/abft-mse/")#

load(file=paste(getwd(),"/objects/OMs/1/OMI",sep=""))
Ctemp<-OMI@Cobs
Atemp<-Ctemp[,3]
Ctemp[Atemp<4,3]<-2  # West stock
Ctemp[Atemp>3,3]<-1  # East stock
Ann_Cat<-aggregate(Ctemp[,5],by=list(Ctemp[,1],Ctemp[,3]),sum)
names(Ann_Cat)<-c("Year","MArea","Catch")

save(Ann_Cat,file=paste0(getwd(),"/R_package/ABTMSE/data/Ann_Cat"))
