# Define fleets.r
# August 2016
# R Script for defining fleets for this operating model

CPUEdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_2/CPUE_indices compiled_2019OM_2.csv",sep=""))
aggregate(rep(1,nrow(CPUEdat)),by=list(CPUEdat$Fleet,CPUEdat$Name),sum)
CLdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_2/CLobs_25cm.csv",sep=""))
FleetDefs<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_4/FleetDefs.csv",sep=""),header=F)[1:17,]
nf<-nrow(FleetDefs)

#                1        2        3        4        5          6            7        8       9      10       11       12       13      14     15        16        17
Fleets <-      new('list') # note you must put ALL flag types ahead of specific fleet types of same gear - sequential fleet assignment
Fleets$name <- FleetDefs[,2]
Fleets$LB <-  rep(0,nf)
Fleets$UB <-  rep(500,nf)



