# Fishery indices.r
# April 2021
# R Script for formatting fishery CPUE indices for MSE testing

CPUEdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2021_3/CPUE_indices compiled_OM2021Apr19.csv",sep="")) # !TSD Table 2.1

exclude <- c("US_GOM_PLL1","US_GOM_PLL2")
lowwt<-c("US_RR_115_144","US_RR_66_114")
CPUEdat<-CPUEdat[!(CPUEdat$Name %in% exclude),]
uniqueQs<-unique(CPUEdat$qNo)
CPUEdat$qNo<-match(CPUEdat$qNo,uniqueQs)
CPUEdat[match(1:max(CPUEdat$qNo),CPUEdat$qNo),]
CPUEdat$CV[CPUEdat$CV<0.25]<-0.25
CPUEdat$lt[is.na(CPUEdat$lt)]<-0

CPUEobs<-CPUEdat[,1:8]
CPUEobs<-CPUEobs[CPUEobs$Year>=Base@years[1]&CPUEobs$Year<=Base@years[2],]
CPUEobs[,1]<-CPUEobs[,1]-Base@years[1]+1

# Get the CPUE series names
firstrow<-match(1:max(CPUEdat$qNo),CPUEdat$qNo)
CPUEnames<-as.character(CPUEdat$Name[firstrow])

# Standardize to mean 1
mubyF<-aggregate(CPUEobs$Index,by=list(CPUEobs$qNo),mean)
CPUEobs$Index<-CPUEobs$Index/mubyF[CPUEobs$qNo,2]
# wt<-1/(CPUEobs[,8]^2) # inverse variance weighting
wt<-rep(1,nrow(CPUEobs)) # overide individual observation (inverse variance) weights
lowind<-match(lowwt,CPUEnames)
wt[CPUEobs[,4]%in%lowind]<-0.01

for_wt<-1/CPUEobs$CV^2
sum_for_wt<-aggregate(for_wt,list(CPUEobs$qNo),sum)$x
w<-for_wt/sum_for_wt[CPUEobs$qNo] # this is the normalized weight per index for the correct MLE CPUE q calculation - ie accounts for CV in each observation

CPUEobs<-cbind(CPUEobs,wt,w)

save(CPUEobs,file=paste(getwd(),"/Data/Processed/Conditioning/CPUEobs",sep=""))

diag <- F # Do some diagnostic plots etc?
if(diag){

  if(sum(is.na(CPUEobs[,4]))>0)message("!!!Missing length categories detected!!!")

  # Fleet pairings to CPUE indices
  pairings<-aggregate(1:nrow(CPUEdat),by=list(CPUEdat$Name, CPUEdat$Fleet),sum) # !TSD check against Table 2.1
  cbind(pairings[,1:2],Base@Fleets$name[pairings[,2]])

  forAI<-aggregate(CPUEobs$Index,by=list(CPUEobs$qNo,CPUEobs$Fleet),mean)[,1:2]
  forAI<-forAI[order(forAI[,1]),]
  names(forAI)<-c("qNo","Fleet")
  forAI
  #write.csv(forAI,"C:/Users/tcarruth/Dropbox/BFT MSE/Communications/CPUEobs 2019/CPUEobs_Format_ICCAT_2019.csv")

  Inams<-CPUEnames
  Ilabs<-Inams[CPUEobs[,4]]
  CPUEobs_lab<-cbind(CPUEobs,Ilabs)
  ggplot(CPUEobs_lab, aes(x=Year,y=Index)) + geom_point() +geom_line() + facet_wrap(~Ilabs , ncol=7)

  # Do CPUE index stuff here

}

# End of script


