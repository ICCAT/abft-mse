# Historical catches.R
# April 2019
# R Script for assigning historical annual catches to season, age and area prior to 1965
# (Using Task1 in the west and Trap data in the East)

# The original historical catch observations
#HCobs_tab<-read.csv(paste(getwd(),"/data/ICCAT_2019/HCobs1864_1964.csv",sep=""))

HCobs_tab_1<-read.csv(paste(getwd(),"/data/ICCAT_2019_3/HCobs1864_1949.csv",sep=""))
HCobs_tab_2<-read.csv(paste(getwd(),"/data/ICCAT_2019_3/HCobs5064_final.csv",sep=""))

HCobs_tab<-rbind(HCobs_tab_1,HCobs_tab_2)

HCobs_tab<-HCobs_tab[HCobs_tab$Year>=Base@Hyears[1],]
HCobs_tab$Year<-HCobs_tab$Year-Base@Hyears[1]+1
HCobs<-array(0,c(Base@nHy, Base@ns, Base@na, Base@nr))
ind<-as.matrix(HCobs_tab[,c(1,2,4,3)])
HCobs[ind]<-HCobs_tab$Catch
HCobs[,,1,]<-0 # remove age 1 catches from historical period.
save(HCobs,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs",sep=""))

diag<-F

if(diag){  # some diagnostics

  load(file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_old",sep=""));
  load(file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_new",sep=""));

  # Historical and future catches
  HCobsWo<-apply(HCobs_new[,,,1:3],1,sum)
  HCobsEo<-apply(HCobs_new[,,,4:7],1,sum)

  HCobsW<-apply(HCobs[,,,1:3],1,sum)
  HCobsE<-apply(HCobs[,,,4:7],1,sum)

  CobsW<-Cobs[Cobs$Area<4,]
  CobsWa<-aggregate(CobsW$Catch,by=list(CobsW$Year),sum)

  CobsE<-Cobs[Cobs$Area>3,]
  CobsEa<-aggregate(CobsE$Catch,by=list(CobsE$Year),sum)


  jpeg("C:/Users/tcar_/Dropbox/BFT MSE/Communications/HCobs Ai 2019/Old-New catch at age.jpg",res=400, width=8,height=10,units='in')

    # old HCobs vs New
    par(mfrow=c(3,2),mai=c(0.4,0.4,0.1,0.12),omi=c(0,0.5,0.5,0))
    yrs<-1865:1965

    Wo<-apply(HCobs_old[,,,1:3],1,sum)
    Eo<-apply(HCobs_old[,,,4:7],1,sum)

    W<-apply(HCobs[,,,1:3],1,sum)
    E<-apply(HCobs[,,,4:7],1,sum)

    plot(yrs,W,ylim=c(0,max(W,Wo)),yaxs="i",type='l')
    lines(yrs,Wo,col='red')
    legend('topleft',legend=paste0("Mean factor = ",round(mean(W)/mean(Wo),2)),bty='n')
    legend('left',legend=c("Old (Tom)","New ICCAT 2019"),text.col=c("red","black"),bty='n')
    mtext("Catch number by year",3,line=0.05,adj=0.07,cex=0.9,font=2)

    plot(yrs,E,ylim=c(0,max(E,Eo)),yaxs="i",type='l')
    lines(yrs,Eo,col='red')
    legend('topright',legend=paste0("Mean factor = ",round(mean(E)/mean(Eo),2)),bty='n')

    yrs<-1:35

    Wo<-apply(HCobs_old[,,,1:3],3,sum)
    Eo<-apply(HCobs_old[,,,4:7],3,sum)
    Wo<-Wo/max(Wo)
    Eo<-Eo/max(Eo)
    W<-apply(HCobs[,,,1:3],3,sum)
    E<-apply(HCobs[,,,4:7],3,sum)
    W<-W/max(W)
    E<-E/max(E)

    plot(yrs,W,ylim=c(0,max(W,Wo)),yaxs="i",type='l')
    lines(yrs,Wo,col='red')
    mtext("Fraction of catch numbers by age",3,line=0.05,adj=0.07,cex=0.9,font=2)

    plot(yrs,E,ylim=c(0,max(E,Eo)),yaxs="i",type='l')
    lines(yrs,Eo,col='red')

     mtext(c("West","East"),3,line=0.6,adj=c(0.25,0.77),outer=T,font=2)


    # Implied catch weight

    EW<-Base@wt_age[1,,1]/1000 # in tonnes
    WW<-Base@wt_age[2,,1]/1000 # in tonnes

    Woa<-apply(apply(HCobs_old[,,,1:3],c(1,3),sum)*rep(WW,each=101),1,sum)
    Eoa<-apply(apply(HCobs_old[,,,4:7],c(1,3),sum)*rep(EW,each=101),1,sum)

    Wa<-apply(apply(HCobs[,,,1:3],c(1,3),sum)*rep(WW,each=101),1,sum)
    Ea<-apply(apply(HCobs[,,,4:7],c(1,3),sum)*rep(EW,each=101),1,sum)

    yrs<-1865:2017

    plot(yrs,c(Wa,CobsWa[,2]/1000),ylim=c(0,max(Wa,Woa,CobsWa[,2]/1000)),yaxs="i",type='l',xlim=c(1940,2017))
    abline(v=1965,col='blue')
    lines(1865:1965,Woa,col='red')
    mtext("Catch weight by year",3,line=0.05,adj=0.07,cex=0.9,font=2)

    plot(yrs,c(Ea,CobsEa[,2]/1000),ylim=c(0,max(Ea,Eoa,CobsEa[,2]/1000)),yaxs="i",type='l',xlim=c(1865,2017))
    abline(v=1965,col='blue')
    lines(1865:1965,Eoa,col='red')




  dev.off()

}
