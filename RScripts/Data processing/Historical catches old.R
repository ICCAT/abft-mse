# Historical catches.R
# April 2019
# R Script for assigning historical annual catches to season, age and area prior to 1965
# (Using Task1 in the west and Trap data in the East)

HCobs_tab<-read.csv(paste(getwd(),"/data/ICCAT_2019/HCobs1864_1964.csv",sep=""))
HCobs_tab<-HCobs_tab[HCobs_tab$Year>=Base@Hyears[1],]
HCobs_tab$Year<-HCobs_tab$Year-Base@Hyears[1]+1
HCobs<-array(0,c(Base@nHy, Base@ns, Base@na, Base@nr))
ind<-as.matrix(HCobs_tab[,c(1,2,4,3)])
HCobs[ind]<-HCobs_tab$Catch*1000 # tonnes to kg
save(HCobs,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs",sep=""))

diag<-F

if(diag){  # some diagnostics
  
  load(file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_old",sep="")); 
 
  # Historical and future catches 
  
  HCobsW<-HCobs[HCobs$Area<4,]
  HCobsWa<-aggregate(HCobsW$Catch,by=list(HCobsW$Year),sum)
  
  HCobsE<-HCobs[HCobs$Area>3,]
  HCobsEa<-aggregate(HCobsE$Catch,by=list(HCobsE$Year),sum)
  
  CobsW<-Cobs[Cobs$Area<4,]
  CobsWa<-aggregate(CobsW$Catch,by=list(CobsW$Year),sum)
  
  CobsE<-Cobs[Cobs$Area>3,]
  CobsEa<-aggregate(CobsE$Catch,by=list(CobsE$Year),sum)
 
  par(mfrow=c(1,2),mai=c(0.5,0.5,0.3,0.3))
  plot(c(HCobsWa[,1],CobsWa[,1]),c(HCobsWa[,2],CobsWa[,2]),yaxs='i',main='West')
  abline(v=1964.5,col='red')
  plot(c(HCobsEa[,1],CobsEa[,1]),c(HCobsEa[,2],CobsEa[,2]),yaxs='i',main="East")
  abline(v=1964.5,col='red')
  mtext("Catch (t)",2,line=0.5,outer=2)
  mtext("Year",1,line=0.5,outer=2)
  
}
