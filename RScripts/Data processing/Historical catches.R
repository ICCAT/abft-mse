# Historical catches.R
# April 2021
# R Script for assigning historical annual catches to season, age and area prior to 1965

HCobs_tab_1<-read.csv(paste(getwd(),"/data/ICCAT_2021_2/HCobs1864_1949_July2019.csv",sep=""))
HCobs_tab_2<-read.csv(paste(getwd(),"/data/ICCAT_2021_2/HCobs5064_Apr2021.csv",sep=""))

HCobs_tab<-rbind(HCobs_tab_1,HCobs_tab_2)

HCobs_tab<-HCobs_tab[HCobs_tab$Year>=Base@Hyears[1],]
HCobs_tab$Year<-HCobs_tab$Year-Base@Hyears[1]+1
HCobs<-array(0,c(Base@nHy, Base@ns, Base@na, Base@nr))
ind<-as.matrix(HCobs_tab[,c(1,2,4,3)])
HCobs[ind]<-HCobs_tab$Catch
HCobs[,,1,]<-0 # remove age 0 catches from historical period.
saveRDS(HCobs,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs.rda",sep=""))

diag<-F

if(diag){  # some diagnostics

 # Plot total catch numbers by quarter
 Wvec<-c(T,T,T,F,F,F,F) # west areas
 HCW<-apply(HCobs*rep(Wvec,each=prod(dim(HCobs)[1:3])),1:2,sum)
 HCE<-apply(HCobs*rep(!Wvec,each=prod(dim(HCobs)[1:3])),1:2,sum)
 par(mfrow=c(2,1),mai=c(0.5,0.4,0.3,0.01))
 cols<-c("black","red","green","blue")
 yrlab=Base@Hyears[1]:Base@Hyears[2]
 matplot(yrlab,HCW,type='l',col=cols); legend('topleft',legend=paste("Q = ",1:4),bty='n',text.col=cols); mtext("West")
 matplot(yrlab,HCE,type='l',col=cols); mtext("East")
 
}
