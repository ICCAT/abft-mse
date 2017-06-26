# Fishery indices.r
# November 2016
# R Script for formatting fishery CPUE indices for MSE testing

CPUEdat<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE indices compiled.csv",sep=""))
CPUEobs<-CPUEdat[,1:6]
CPUEobs<-CPUEobs[CPUEobs$Year>=Base@years[1]&CPUEobs$Year<=Base@years[2],]
CPUEobs[,1]<-CPUEobs[,1]-Base@years[1]+1

# Get the CPUE series names
firstrow<-match(1:max(CPUEdat$qNo),CPUEdat$qNo)
CPUEnames<-as.character(CPUEdat$Name[firstrow])

# Standardize to mean 1
mubyF<-aggregate(CPUEobs$Index,by=list(CPUEobs$qNo),mean)
CPUEobs$Index<-CPUEobs$Index/mubyF[CPUEobs$qNo,2]

save(CPUEobs,file=paste(getwd(),"/Data/Processed/Conditioning/CPUEobs",sep=""))

# End of script


