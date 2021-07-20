# Historical catches 2020 Brazil to East.R
# August 2020

load(file=paste(getwd(),"/Objects/OMs/OMI",sep=""))
Base<-OMI

HCobs_tab_org<-read.csv(paste(getwd(),"/data/ICCAT_2019_3/HCobs5064_final.csv",sep=""))
HCobs_tab<-read.csv(paste(getwd(),"/data/ICCAT_2020_2/HCobs5064_final_BRArobustness.csv",sep=""))

HCobs_tab<-HCobs_tab[HCobs_tab$Year>=Base@Hyears[1],]
HCobs_tab$Year<-HCobs_tab$Year-Base@Hyears[1]+1
HCobs<-array(0,c(Base@nHy, Base@ns, Base@na, Base@nr))
ind<-as.matrix(HCobs_tab[,c(1,2,4,3)])
HCobs[ind]<-HCobs_tab$Catch
HCobs[,,1,]<-0 # remove age 1 catches from historical period.
saveRDS(HCobs,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_BRA.rda",sep=""))





if(diag){  # some diagnostics
  setwd("C:/Users/tcar_/Dropbox/abft-mse")
  
  years<-c(1965,2016)
  
  # Pre 1965 historica catch reconstruction (not brazil reallocation)
  HCobs<-readRDS(file=paste(getwd(),"/Data/Processed/Conditioning/HCobs.rda",sep=""));
  
  # Pre 1965 historical catch reconstruction (brazil reallocation)
  HCobs_BRA<-readRDS(file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_BRA.rda",sep=""));

  # 1965 onwards catches (not brazilreallocatoin)
  Cobs<-read.csv("data/ICCAT_2019_4/Cobs_JPLLsplit.csv")
  Cobs<-Cobs[Cobs$Year>=years[1],]
  Cobs$Year<-Cobs$Year-years[1]+1
  Cobs$Catch<-Cobs$Catch*1000 # tonnes to kg
  CCV<- 0.01 #0.005*(Base@ns*Base@nr/2)^0.5 # per fleet obs annual obs error is ~ 0.02 assuming half coverage
  CV<-rep(CCV,nrow(Cobs))
  wt<-rep(1/(CCV^2),nrow(Cobs))
  Cobs<-cbind(Cobs,CV,wt)
  
  # 1965 onwards catches (brazil reallocation)
  Cobs_BRA<-read.csv("data/ICCAT_2020_2/Cobs_JPLLsplit_BRArobustness.csv")
  Cobs_BRA<-Cobs_BRA[Cobs_BRA$Year>=years[1],]
  Cobs_BRA$Year<-Cobs_BRA$Year-years[1]+1
  Cobs_BRA$Catch<-Cobs_BRA$Catch*1000 # tonnes to kg
  CCV<- 0.01 #0.005*(Base@ns*Base@nr/2)^0.5 # per fleet obs annual obs error is ~ 0.02 assuming half coverage
  CV<-rep(CCV,nrow(Cobs_BRA))
  wt<-rep(1/(CCV^2),nrow(Cobs_BRA))
  Cobs_BRA<-cbind(Cobs_BRA,CV,wt)
  
  # Historical and future catches
  HCobsWb<-apply(HCobs_BRA[,,,1:3],1,sum)
  HCobsEb<-apply(HCobs_BRA[,,,4:7],1,sum)

  HCobsW<-apply(HCobs[,,,1:3],1,sum)
  HCobsE<-apply(HCobs[,,,4:7],1,sum)

  CobsW<-Cobs[Cobs$Area<4,]
  CobsWa<-aggregate(CobsW$Catch,by=list(CobsW$Year),sum)
  CobsE<-Cobs[Cobs$Area>3,]
  CobsEa<-aggregate(CobsE$Catch,by=list(CobsE$Year),sum)

  CobsWb<-Cobs_BRA[Cobs_BRA$Area<4,]
  CobsWab<-aggregate(CobsWb$Catch,by=list(CobsWb$Year),sum)
  CobsEb<-Cobs_BRA[Cobs_BRA$Area>3,]
  CobsEab<-aggregate(CobsEb$Catch,by=list(CobsEb$Year),sum)

  jpeg("C:/Users/tcar_/Dropbox/BFT MSE/Communications/Brazilian Catches/Brazilian catch2.jpg",res=600, width=9,height=9,units='in')

    par(mfrow=c(3,2),mai=c(0.4,0.8,0.1,0.12),omi=c(0,0.5,0.5,0))
    yrs<-1865:1965

    Wb<-apply(HCobs_BRA[,,,1:3],1,sum)
    Eb<-apply(HCobs_BRA[,,,4:7],1,sum)

    W<-apply(HCobs[,,,1:3],1,sum)
    E<-apply(HCobs[,,,4:7],1,sum)

    plot(yrs,W,ylim=c(0,max(W,Wb)),yaxs="i",type='l',ylab="Total numbers of fish")
    lines(yrs,Wb,col='red')
    legend('topleft',legend=c("Brazilian catch scenario","Reference"),text.col=c("red","black"),cex=1.1)
    mtext("(a)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    
    plot(yrs,E,ylim=c(0,max(E,Eb)),yaxs="i",type='l',ylab="Total numbers of fish")
    lines(yrs,Eb,col='red')
    mtext("(b)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    yrs<-1:35

    Wb<-apply(HCobs_BRA[,,,1:3],3,sum)
    Eb<-apply(HCobs_BRA[,,,4:7],3,sum)
    Wb<-Wb/max(Wb)
    Eb<-Eb/max(Eb)
    W<-apply(HCobs[,,,1:3],3,sum)
    E<-apply(HCobs[,,,4:7],3,sum)
    W<-W/max(W)
    E<-E/max(E)

    plot(yrs,W,ylim=c(0,max(W,Wb)),yaxs="i",type='l',ylab="Age composition (1865-1965)")
    lines(yrs,Wb,col='red')
    mtext("(c)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    
    plot(yrs,E,ylim=c(0,max(E,Eb)),yaxs="i",type='l',ylab="Age composition (1865-1965)")
    lines(yrs,Eb,col='red')
    mtext("(d)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    mtext(c("West Area","East Area"),3,line=0.6,adj=c(0.25,0.77),outer=T,font=2)

    # Implied catch weight
    EW<-Base@wt_age[1,,1]/1000 # in tonnes
    WW<-Base@wt_age[2,,1]/1000 # in tonnes

    Wba<-apply(apply(HCobs_BRA[,,,1:3],c(1,3),sum)*rep(WW,each=101),1,sum)
    Eba<-apply(apply(HCobs_BRA[,,,4:7],c(1,3),sum)*rep(EW,each=101),1,sum)

    Wa<-apply(apply(HCobs[,,,1:3],c(1,3),sum)*rep(WW,each=101),1,sum)
    Ea<-apply(apply(HCobs[,,,4:7],c(1,3),sum)*rep(EW,each=101),1,sum)

    yrs<-1865:2017

    plot(yrs,c(Wa,CobsWa[,2]/1000),ylim=c(0,max(Wa,Wba,CobsWab[,2]/1000)*1.1),ylab="Total annual catches (kt)",yaxs="i",type='l',xlim=c(1940,2017))
    abline(v=1965.5,col='#0000ff60')
    lines(yrs,c(Wba,CobsWab[,2]/1000),col='red')
    mtext("(e)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    legend('topleft',legend="End of spool-up",text.col="blue",bty='n',cex=1.1)
    legend('left',legend=c(round(sum(c(Wa,CobsWa[,2]/1000),1)),round(sum(c(Wba,CobsWab[,2]/1000)),1)),text.col=c('black','red'),title="Total Catch")

    plot(yrs,c(Ea,CobsEa[,2]/1000),ylim=c(0,max(Ea,Eba,CobsEa[,2]/1000)*1.1),ylab="Total annual catches (kt)",yaxs="i",type='l',xlim=c(1865,2017))
    abline(v=1965.5,col='#0000ff60')
    lines(yrs,c(Eba,CobsEab[,2]/1000),col='red')
    mtext("(f)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    legend('left',legend=c(round(sum(c(Ea,CobsEa[,2]/1000),1)),round(sum(c(Eba,CobsEab[,2]/1000)),1)),text.col=c('black','red'),title="Total Catch")
   
  dev.off()

}
