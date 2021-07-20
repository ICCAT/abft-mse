library(ABTMSE)
loadABT()


MSE<-new('MSE',OM_3d, MPs=list(c("U3","U3")))

Checks<-list()
nchecks<-10
for(i in 1:nchecks)Checks[[i]]<-list()

# Annual Catch length
Checks[[1]]<-nrow(Ann_Cat)==2*OM_1d@nyears
names(Checks[[1]])<-"Ann Cat Len"

# Annual Catch assigned to Cobs
Checks[[2]]<-all(dset[[1]]$Cobs[1,1:OM_1d@nyears]==Ann_Cat$Catch[Ann_Cat$MArea==1])
names(Checks[[2]])<-"Ann Cat assigned to Cobs"

names(Checks)[!unlist(Checks)]
