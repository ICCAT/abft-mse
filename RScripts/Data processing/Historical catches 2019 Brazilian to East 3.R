# Historical catches.R
# April 2019
# R Script for assigning historical annual catches to season, age and area prior to 1965
# (Using Task1 in the west and Trap data in the East)

# The original historical catch observations
#HCobs_tab<-read.csv(paste(getwd(),"/data/ICCAT_2019/HCobs1864_1964.csv",sep=""))
load(file=paste(getwd(),"/Objects/OMs/OMI",sep=""))
Base<-OMI

HCobs_tab_1<-read.csv(paste(getwd(),"/data/ICCAT_2019_3/HCobs1864_1949.csv",sep=""))
HCobs_tab_2<-read.csv(paste(getwd(),"/data/ICCAT_2019_3/HCobs5064_final.csv",sep=""))
HCobs_tab<-rbind(HCobs_tab_1,HCobs_tab_2)

aggtest<-aggregate(HCobs_tab[,5],list(HCobs_tab[,1],HCobs_tab[,3]),sum)


# Brazilian reallocation -----------------

#HCobs_tab_BRA<-read.csv(paste(getwd(),"/data/ICCAT_2020_3/JPN_offBRA_1957_1964.csv",sep=""))

HCobs_tab_BRA<-read.csv(paste(getwd(),"/data/ICCAT_2020_3/HCobs5064_final_BRArobustness.csv",sep=""))
HCobs_tab<-rbind(HCobs_tab_1,HCobs_tab_BRA)

aggtest2<-aggregate(HCobs_tab[,5],list(HCobs_tab[,1],HCobs_tab[,3]),sum)



#par(mfrow=c(1,2),mai=c(0.5,0.5,0.01,0.01))
#plot(aggtest[aggtest[,2]==2,1],aggtest[aggtest[,2]==2,3],ylim=c(0,max(aggtest[aggtest[,2]==2,3])),col="red",type="l")
#lines(aggtest2[aggtest2[,2]==2,1],aggtest2[aggtest2[,2]==2,3],col="blue")
#plot(aggtest2[aggtest2[,2]==4,1],aggtest2[aggtest2[,2]==4,3],ylim=c(0,max(aggtest2[aggtest2[,2]==4,3])),col="blue",type="l")
#lines(aggtest[aggtest[,2]==4,1],aggtest[aggtest[,2]==4,3],col="red")



# End of Brazilian reallocation ----------------

HCobs_tab<-HCobs_tab[HCobs_tab$Year>=Base@Hyears[1],]
HCobs_tab$Year<-HCobs_tab$Year-Base@Hyears[1]+1
HCobs<-array(0,c(Base@nHy, Base@ns, Base@na, Base@nr))
ind<-as.matrix(HCobs_tab[,c(1,2,4,3)])
HCobs[ind]<-HCobs_tab$Catch
HCobs[,,1,]<-0 # remove age 1 catches from historical period.
saveRDS(HCobs,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_BRA2020.rda",sep=""))

diag<-F

if(diag){  # some diagnostics

  HCobs_new<-readRDS(file=paste(getwd(),"/Data/Processed/Conditioning/HCobs.rda",sep=""));
  HCobs_old<-readRDS(file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_BRA2020.rda",sep=""));

  # Historical and future catches
  HCobsWo<-apply(HCobs_old[,,,1:3],1,sum)
  HCobsEo<-apply(HCobs_old[,,,4:7],1,sum)

  HCobsW<-apply(HCobs_new[,,,1:3],1,sum)
  HCobsE<-apply(HCobs_new[,,,4:7],1,sum)

  CobsW<-Cobs[Cobs$Area<4,]
  CobsWa<-aggregate(CobsW$Catch,by=list(CobsW$Year),sum)

  CobsE<-Cobs[Cobs$Area>3,]
  CobsEa<-aggregate(CobsE$Catch,by=list(CobsE$Year),sum)


  jpeg("C:/Users/tcar_/Dropbox/BFT MSE/Communications/Brazilian Catches/Brazilian catch.jpg",res=600, width=9,height=9,units='in')

    # old HCobs vs New
    par(mfrow=c(3,2),mai=c(0.4,0.8,0.1,0.12),omi=c(0,0.5,0.5,0))
    yrs<-1865:1965

    Wo<-apply(HCobs_old[,,,1:3],1,sum)
    Eo<-apply(HCobs_old[,,,4:7],1,sum)

    W<-apply(HCobs_new[,,,1:3],1,sum)
    E<-apply(HCobs_new[,,,4:7],1,sum)

    plot(yrs,W,ylim=c(0,max(W,Wo)),yaxs="i",type='l',ylab="Total numbers of fish")
    lines(yrs,Wo,col='red')
    #legend('topleft',legend=paste0("Mean factor = ",round(mean(W)/mean(Wo),2)),bty='n')
    legend('topleft',legend=c("Brazilian catch scenario","Reference"),text.col=c("red","black"),cex=1.1)
    #mtext("Catch number by year",3,line=0.05,adj=0.07,cex=0.9,font=2)
    mtext("(a)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    
    plot(yrs,E,ylim=c(0,max(E,Eo)),yaxs="i",type='l')
    lines(yrs,Eo,col='red')
    #legend('topright',legend=paste0("Mean factor = ",round(mean(E)/mean(Eo),2)),bty='n')
    mtext("(b)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    yrs<-1:35

    Wo<-apply(HCobs_old[,,,1:3],3,sum)
    Eo<-apply(HCobs_old[,,,4:7],3,sum)
    Wo<-Wo/max(Wo)
    Eo<-Eo/max(Eo)
    W<-apply(HCobs_new[,,,1:3],3,sum)
    E<-apply(HCobs_new[,,,4:7],3,sum)
    W<-W/max(W)
    E<-E/max(E)

    plot(yrs,W,ylim=c(0,max(W,Wo)),yaxs="i",type='l',ylab="Age composition (1865-1965)")
    lines(yrs,Wo,col='red')
    #mtext("Fraction of catch numbers by age",3,line=0.05,adj=0.07,cex=0.9,font=2)
    mtext("(c)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    
    plot(yrs,E,ylim=c(0,max(E,Eo)),yaxs="i",type='l')
    lines(yrs,Eo,col='red')
    mtext("(d)",3,line=0.05,adj=0.07,cex=0.9,font=2)
     mtext(c("West Area","East Area"),3,line=0.6,adj=c(0.25,0.77),outer=T,font=2)


    # Implied catch weight

    EW<-Base@wt_age[1,,1]/1000 # in tonnes
    WW<-Base@wt_age[2,,1]/1000 # in tonnes

    Woa<-apply(apply(HCobs_old[,,,1:3],c(1,3),sum)*rep(WW,each=101),1,sum)
    Eoa<-apply(apply(HCobs_old[,,,4:7],c(1,3),sum)*rep(EW,each=101),1,sum)

    Wa<-apply(apply(HCobs_new[,,,1:3],c(1,3),sum)*rep(WW,each=101),1,sum)
    Ea<-apply(apply(HCobs_new[,,,4:7],c(1,3),sum)*rep(EW,each=101),1,sum)

    yrs<-1865:2017

    plot(yrs,c(Wa,CobsWa[,2]/1000),ylim=c(0,max(Wa,Woa,CobsWa[,2]/1000)),ylab="Total annual catches (kt)",yaxs="i",type='l',xlim=c(1940,2017))
    abline(v=1965.5,col='#0000ff60')
    lines(1865:1965,Woa,col='red')
    mtext("(e)",3,line=0.05,adj=0.07,cex=0.9,font=2)
    legend('topleft',legend="End of spool-up",text.col="blue",bty='n',cex=1.1)

    plot(yrs,c(Ea,CobsEa[,2]/1000),ylim=c(0,max(Ea,Eoa,CobsEa[,2]/1000)),yaxs="i",type='l',xlim=c(1865,2017))
    abline(v=1965.5,col='#0000ff60')
    lines(1865:1965,Eoa,col='red')
    mtext("(f)",3,line=0.05,adj=0.07,cex=0.9,font=2)


  dev.off()

}



# === Catch observations after =============================================================================

Cobs<-read.csv(paste(getwd(),"/data/ICCAT_2020_3/Cobs_JPLLsplit_BRArobustness.csv",sep=""))

#Cobs1<-read.csv("data/ICCAT_2019_4/Cobs_JPLLsplit.csv")
Cobs<-Cobs[Cobs$Year>=Base@years[1],]
Cobs$Year<-Cobs$Year-Base@years[1]+1
Cobs$Catch<-Cobs$Catch*1000 # tonnes to kg
CCV<- 0.01 #0.005*(Base@ns*Base@nr/2)^0.5 # per fleet obs annual obs error is ~ 0.02 assuming half coverage
CV<-rep(CCV,nrow(Cobs))
wt<-rep(1/(CCV^2),nrow(Cobs))
Cobs<-cbind(Cobs,CV,wt)












