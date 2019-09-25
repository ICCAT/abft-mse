# Fishery indices.r
# May 2019
# R Script for formatting fishery CPUE indices for MSE testing

#CPUEdat<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE indices compiled 2017 assessment.csv",sep=""))
#CPUEdat<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE indices compiled.csv",sep=""))
#CPUEdat<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE_indices compiled_2018OM_2.csv",sep=""))
#CPUEdat_old<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE_indices compiled_2018OM.csv",sep=""))
CPUEdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_2/CPUE_indices compiled_2019OM_3.csv",sep=""))
#CPUEdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_4/CPUE_indices compiled_2019OM_JPLLsplit_with_lt.csv",sep=""))

CPUEdat$CV[CPUEdat$CV<0.25]<-0.25
CPUEcv <- aggregate(CPUEdat$CV,list(CPUEdat$qNo),mean)
CPUEcv<- CPUEcv$x[order(CPUEcv[,1])]
CPUEcv[CPUEcv<0.25]=0.25
#oldareas<-aggregate(CPUEdat_old$Area,by=list(CPUEdat_old$Name),max)
#oldfleets<-aggregate(CPUEdat_old$Fleet,by=list(CPUEdat_old$Name),unique)
#CPUEdat$Area<-oldareas$x[match(CPUEdat$Name,oldareas$Group.1)]
#CPUEdat$Fleet<-oldfleets$x[match(CPUEdat$Name,oldfleets$Group.1)]
CPUEdat$lt[is.na(CPUEdat$lt)]<-0

# Fleet pairings to CPUE indices

pairings<-aggregate(1:nrow(CPUEdat),by=list(CPUEdat$Name, CPUEdat$Fleet),sum)
#cbind(pairings[,1:2],Base@Fleets$name[pairings[,2]])


#jpeg("C:/temp/CAN indices.jpg",width=8,height=5,units='in',res=300)
#par(mfrow=c(1,2),omi=c(0,0,1,0))

#temp<-CPUEdat[CPUEdat$Name=="CAN GSL",]
#plot(temp$Year,temp$Index,ylim=c(0,max(temp$Index)),main="September",type="l")

#CPUEdat<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE_indices compiled_2018OM_2.csv",sep=""))
#temp<-CPUEdat[CPUEdat$Name=="CAN GSL",]
#plot(temp$Year,temp$Index,ylim=c(0,max(temp$Index)),main="November",type="l")
#mtext("CPUE_indices compiled_2018OM.csv",3,outer=T)

#dev.off()

diag<-F

if(diag){

  forAI<-aggregate(CPUEobs$Index,by=list(CPUEobs$qNo,CPUEobs$Fleet),mean)[,1:2]
  forAI<-forAI[order(forAI[,1]),]
  names(forAI)<-c("qNo","Fleet")
  write.csv(forAI,"C:/Users/tcarruth/Dropbox/BFT MSE/Communications/CPUEobs 2019/CPUEobs_Format_ICCAT_2019.csv")

  unique(Base@CLobs[,4])

}


CPUEobs<-CPUEdat[,1:8]
CPUEobs<-CPUEobs[CPUEobs$Year>=Base@years[1]&CPUEobs$Year<=Base@years[2],]
CPUEobs[,1]<-CPUEobs[,1]-Base@years[1]+1

# Get the CPUE series names
firstrow<-match(1:max(CPUEdat$qNo),CPUEdat$qNo)
CPUEnames<-as.character(CPUEdat$Name[firstrow])

# Standardize to mean 1
mubyF<-aggregate(CPUEobs$Index,by=list(CPUEobs$qNo),mean)
CPUEobs$Index<-CPUEobs$Index/mubyF[CPUEobs$qNo,2]
wt<-1/(CPUEobs[,8]^2)
wt[CPUEobs[,4]==match("CAN GSL",CPUEnames)]<-1 # Exception is the CAN GSL
CPUEobs<-cbind(CPUEobs,wt)

diag<-F
if(diag){

  Inams<-CPUEnames
  Ilabs<-Inams[CPUEobs[,4]]
  #Flabs<-Flabs[CLobs$Fleet]
  CPUEobs_lab<-cbind(CPUEobs,Ilabs)
  ggplot(CPUEobs_lab, aes(x=Year,y=Index)) + geom_point() +geom_line() + facet_wrap(~Ilabs , ncol=10)



}

# End of script


