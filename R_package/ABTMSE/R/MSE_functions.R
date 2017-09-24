#' Calculate performance metrics.
#'
#' @param MSE an object of class MSE
#' @return a list n stocks long each with a data frame of performance metrics (columns) by management procedure (row) \code{MSE}.
#' \itemize{
#'   \item Y10. Mean yield over the first 10 years of the projection
#'   \item Y20. Mean yield over years 11-20 of the projection
#'   \item Y30. Mean yield over years 21-30 of the projection
#'   \item PGK. Probability of Green Kobe (F<FMSY AND B>BMSY) region over all years of the projection
#'   \item POF. Probability of Over-Fishing (F>FMSY) over all years of the projection
#'   \item POFed. Probability of Over-Fished status (B<BMSY) over all years of the projection
#'   \item D10. Mean depletion (SSB relative to unfished) over the first 10 years of the projection
#'   \item D20. Mean depletion (SSB relative to unfished) over years 11-20 of the projection
#'   \item D30. Mean depletion (SSB relative to unfished) over years 21-30 of the projection
#'   \item LD. Mean depletion (SSB relative to unfished) over all projected years
#'   \item RSSB. Mean Spawning Stock Biomass in final projection year relative to zero fishing
#'   \item LRSSB. Mean Spawning Stock Biomass over all projection years relative to zero fishing
#'   \item AAVY. Mean Average Annual Variability in Yield over all projections
#'   \item carat. weight of the diamond (0.2--5.01)
#'   ...
#' }
#' @examples
#' getperf(MSE_example)
getperf<-function(MSE,rnd=3,outdir=NA){

  nsim<-MSE@nsim
  proyears<-MSE@proyears
  nMPs<-MSE@nMPs

  MPnams<-unlist(MSE@MPs)
  MPnams<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  out<-new('list')

  for(pp in 1:MSE@npop){

    C10a<-C10(MSE,pp=pp)/1E6 # thousand tonnes
    C20a<-C20(MSE,pp=pp)/1E6
    C30a<-C30(MSE,pp=pp)/1E6

    D10a<-D10(MSE,pp=pp)
    D20a<-D20(MSE,pp=pp)
    D30a<-D30(MSE,pp=pp)

    LDa<-LD(MSE,pp=pp)
    DNCa<-DNC(MSE,pp=pp)
    LDNCa<-LDNC(MSE,pp=pp)

    POFa<-POF(MSE,pp=pp)
    POSa<-POS(MSE,pp=pp)
    PGKa<-PGK(MSE,pp=pp)

    AAVCa<-AAVC(MSE,pp)

    out[[pp]]<-data.frame("C10"=apply(C10a,1,mean),
                          "C20"=apply(C20a,1,mean),
                          "C30"=apply(C30a,1,mean),

                          "D10"=apply(D10a,1,mean),
                          "D20"=apply(D20a,1,mean),
                          "D30"=apply(D30a,1,mean),

                          "LD"=apply(LDa,1,mean),
                          "DNC"=apply(DNCa,1,mean),
                          "LDNC"=apply(LDNCa,1,mean),

                          "POF"=apply(POFa,1,mean),
                          "POS"=apply(POSa,1,mean),
                          "PGK"=apply(PGKa,1,mean),

                          "AAVC"=apply(AAVCa,1,mean),
                          row.names=MPnams)
    out[[pp]]<-round(out[[pp]],rnd)

    if(!is.na(outdir))if(file.exists(outdir))write.csv(out[[pp]],paste0(outdir,"/",MSE@Snames[pp],"_perf.csv"))

  }

  names(out)<-MSE@Snames
  out

}

Tplot<-function(MSE){

  MPnams<-matrix(unlist(MSE@MPs),nrow=2)
  MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  par(mfrow=c(2,3),mai=c(0.6,0.6,0.25,0.05),omi=rep(0.05,4))

  for(pp in 1:MSE@npop){

    C30a<-apply(C30(MSE,pp)/1000,1,mean)
    PGKa<-apply(PGK(MSE,pp),1,mean)
    POFa<-apply(POF(MSE,pp),1,mean)
    POSa<-apply(POS(MSE,pp),1,mean)
    D30a<-apply(D30(MSE,pp),1,mean)
    AAVCa<-apply(AAVC(MSE,pp),1,mean)
    TOplt(PGKa,C30a,MPnams[pp,],"PGK","C30",0.5,NA)
    TOplt(POFa,POSa,MPnams[pp,],"POF (prob. overfishing)","POS (prob. overfished)",0.5,0.5)
    mtext(MSE@Snames[pp],3,line=0.3,font=2)
    TOplt(D30a,AAVCa,MPnams[pp,],"Final depletion","AAVC",0.1,0.2)

  }
}

Tplot2<-function(MSE){

  MPnams<-matrix(unlist(MSE@MPs),nrow=2)
  MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  par(mfrow=c(2,3),mai=c(0.6,0.6,0.25,0.05),omi=rep(0.05,4))

  for(pp in 1:MSE@npop){

    C30a<-apply(C30(MSE,pp)/1000,1,mean)
    PGKa<-apply(PGK(MSE,pp),1,mean)
    C10a<-apply(C10(MSE,pp),1,mean)
    LDa<-apply(LD(MSE,pp),1,mean)
    D30a<-apply(D30(MSE,pp),1,mean)
    AAVCa<-apply(AAVC(MSE,pp),1,mean)
    TOplt(LDa,C10a,MPnams[pp,],"LD","C10",0.5,NA)
    TOplt(AAVCa,C30a,MPnams[pp,],"AAVC","C30",0.5,0.5)
    mtext(MSE@Snames[pp],3,line=0.3,font=2)
    TOplt(D30a,C30a,MPnams[pp,],"D30","C30",0.1,0.2)

  }
}

TplotS<-function(MSE){

  MPnams<-matrix(unlist(MSE@MPs),nrow=2)
  MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  par(mfrow=c(2,3),mai=c(0.6,0.6,0.25,0.05),omi=rep(0.05,4))


  C30_1<-apply(C30(MSE,1)/1000,1,mean)
  PGK_1<-apply(PGK(MSE,1),1,mean)
  POF_1<-apply(POF(MSE,1),1,mean)
  POS_1<-apply(POS(MSE,1),1,mean)
  D30_1<-apply(D30(MSE,1),1,mean)
  AAVC_1<-apply(AAVC(MSE,1),1,mean)

  C30_2<-apply(C30(MSE,pp=2)/1000,1,mean)
  PGK_2<-apply(PGK(MSE,pp=2),1,mean)
  POF_2<-apply(POF(MSE,2),1,mean)
  POS_2<-apply(POS(MSE,2),1,mean)
  D30_2<-apply(D30(MSE,2),1,mean)
  AAVC_2<-apply(AAVC(MSE,2),1,mean)

  TOplt(C30_1,C30_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"C30 (mean catch yrs 21-30)")
  TOplt(PGK_1,PGK_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"PGK, P(Green Kobe)")
  TOplt(POF_1,POF_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"POF, P(F>FMSY)")
  TOplt(POS_1,POS_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"POS, P(B<BMSY)")
  TOplt(D30_1,D30_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"D30 (mean depln. yrs 21-30")
  TOplt(AAVC_1,AAVC_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"AAVC (Av. Ann. Var. Catch)")

}

TplotS2<-function(MSE){

  MPnams<-matrix(unlist(MSE@MPs),nrow=2)
  MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  par(mfrow=c(2,3),mai=c(0.6,0.6,0.25,0.05),omi=rep(0.05,4))

  C30_1<-apply(C30(MSE,1)/1000,1,mean)
  LD_1<-apply(LD(MSE,1),1,mean)
  C10_1<-apply(C10(MSE,1),1,mean)
  D10_1<-apply(D10(MSE,1),1,mean)
  D30_1<-apply(D30(MSE,1),1,mean)
  AAVC_1<-apply(AAVC(MSE,1),1,mean)

  C30_2<-apply(C30(MSE,pp=2)/1000,1,mean)
  LD_2<-apply(LD(MSE,pp=2),1,mean)
  C10_2<-apply(C10(MSE,2),1,mean)
  D10_2<-apply(D10(MSE,2),1,mean)
  D30_2<-apply(D30(MSE,2),1,mean)
  AAVC_2<-apply(AAVC(MSE,2),1,mean)

  TOplt(C10_1,C10_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"C10 (mean catch yrs 1-10)")
  TOplt(C30_1,C30_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"C30 (mean catch yrs 21-30)")

  TOplt(LD_1,LD_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"LD, (lowest depletion)")
  TOplt(D10_1,D10_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"D10 (mean depln. yrs 1-10")
  TOplt(D30_1,D30_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"D30 (mean depln. yrs 21-30")
  TOplt(AAVC_1,AAVC_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"AAVC (Av. Ann. Var. Catch)")

}

TOplt<-function(x,y,MPs,xlab,ylab,xref=NA,yref=NA,main=""){
  MPcols<-rep(c("black","blue","orange","green","red","grey"),20)
  plot(x,y,col='white',xlab="",ylab="",
       xlim=range(x)+(max(x)-min(x))/10*c(-1,1),
       ylim=range(y)+(max(y)-min(y))/10*c(-1,1))
  abline(h=yref,lty=2,col="grey")
  abline(v=xref,lty=2,col="grey")
  text(x,y,MPs,col=MPcols)
  mtext(xlab,1,line=2.2,cex=0.85)
  mtext(ylab,2,line=2.2,cex=0.85)
  mtext(main,3,line=0.4,cex=0.85,font=2)
}

TOPNTplt<-function(x,y,MPs,xlab,ylab,xref=NA,yref=NA,main=""){
  MPcols<-makeTransparent(rep(c("black","blue","orange","green","red","grey"),20),95)
  plot(x,y,col='white',xlab="",ylab="",
       xlim=range(x)+(max(x)-min(x))/10*c(-1,1),
       ylim=range(y)+(max(y)-min(y))/10*c(-1,1))
  abline(h=yref,lty=2,col="grey")
  abline(v=xref,lty=2,col="grey")
  for(mp in 1:nrow(x))points(x[mp,],y[mp,],pch=19,col=MPcols[mp])
  mtext(xlab,1,line=2,cex=0.85)
  mtext(ylab,2,line=2,cex=0.85)
  mtext(main,3,line=0.4,cex=0.85,font=2)
}



#' A generic tradeoff plot based on two functions of class PM
#'
#' @param MSE an object of class MSE
#' @param PMs a list of named functions of class PM
#' @return a trade-off figure \code{classy}
#' @examples
#' loadABT()
#' tradeoff(MSE_example,list(POF=POF,Y10=Y10))
tradeoff<-function(MSE,PMs){

  MPnams<-matrix(unlist(MSE@MPs),nrow=2)
  MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")
  nPMs<-length(PMs)
  perf<-array(NA,c(nPMs,MSE@npop,MSE@nMPs,MSE@nsim))
  pmnams<-names(PMs)

  for(pm in 1:nPMs){

    for(pp in 1:MSE@npop){

      perf[pm,pp,,]<-do.call(PMs[[pm]],list(MSE=MSE,pp=pp))

    }

  }
  par(mfrow=c(2,2),mai=c(0.5,0.6,0.4,0.05),omi=c(0.01,0.01,0.01,0.01))

  for(i in 1:2)    TOplt(apply(perf[1,i,,],1,mean),apply(perf[2,i,,],1,mean),MPnamsj,pmnams[1],pmnams[2],main=MSE@Snames[i])
  for(i in 1:2)    TOPNTplt(perf[1,i,,],perf[2,i,,],MPnamsj,pmnams[1],pmnams[2],main=MSE@Snames[i])


}


custombar<-function(dat,MPnams,tickwd1=0.05,tickwd2=0.025,lwd1=2,lwd2=1,xlab=T){

  nMPer<-nrow(dat)
  incr<-(max(dat)-min(dat))*0.05
  plot(dat[,5],ylim=c(min(dat)-incr,max(dat)+incr),xlim=c(0.25,nMPer+0.25),col='white',axes=F,ylab="",xlab="")

  if(xlab){
    axis(1,-1:(nMPer+1),c("","",MPnams,""),las=2,font=2,cex.axis=1.2)
  }else{
    axis(1,-1:(nMPer+1),rep("",nMPer+3))
  }

  incr<-(max(dat)-min(dat))*0.2
  yp<-pretty(seq(min(dat)-incr,max(dat)+incr,length.out=12))
  axis(2,yp,yp,las=2)
  big<-1E20
  polygon(c(-big,big,big,-big),c(-big,-big,big,big),col='grey92')
  points(dat[,3],pch=19,cex=1.1)
  abline(h=yp,col='white')

  for(i in 1:nMPer){

    lines(c(i-tickwd1/2,i+tickwd1/2),c(dat[i,2],dat[i,2]),lwd=lwd1) # lower interquartile
    lines(c(i-tickwd1/2,i+tickwd1/2),c(dat[i,4],dat[i,4]),lwd=lwd1) # upper interquartile

    lines(c(i-tickwd2/2,i+tickwd2/2),c(dat[i,1],dat[i,1]),lwd=lwd1) # lower 80%
    lines(c(i-tickwd2/2,i+tickwd2/2),c(dat[i,5],dat[i,5]),lwd=lwd2) # upper 80%

    lines(c(i,i),c(dat[i,1],dat[i,5]),lwd=lwd2) # 80%
    lines(c(i,i),c(dat[i,2],dat[i,4]),lwd=lwd1) # 80%

  }

}

#' Performance plot
#'
#' @param MSE An MSE object
#' @return a plot showing performance metric statistics accross MPs
#' @examples
#' PPlot()
PPlot<-function(MSE,Pnames=c("C10","C30","D30","LD","DNC","LDNC","PGK","AAVC")){

  nsim<-MSE@nsim
  nMPs<-MSE@nMPs
  npop<-MSE@npop
  nperf<-length(Pnames)

  MPnams_a<-matrix(paste(rep(1:nMPs,each=2),unlist(MSE@MPs),sep=" - "),nrow=npop)

 # MPnams<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")
  #                 pop MP perf quantile
  store<-array(NA,c(2,nMPs,13,5))
  mult=rep(1,nperf)# multiplier for catch biomass scaling

  for(pp in 1:MSE@npop){

    for(i in 1:nperf){

      store[pp,,i,]<-t(apply(do.call(get(Pnames[i]),list(MSE=MSE,pp=pp))*mult[i],1,quantile,p=c(0.05,0.25,0.5,0.75,0.95)))

    }

  }

  par(mfrow=c(nperf,2),mai=c(0.1,0.6,0.01,0.05),omi=c(1.2,0.4,0.4,0.01))

  for(i in 1:nperf){
    for(pp in 1:npop){
      xlab=F
      if(i==nperf)xlab=T
      custombar(dat=store[pp,,i,],MPnams=MPnams_a[pp,],xlab=xlab)
      if(pp==1)mtext(Pnames[i],2,line=5,font=2)
      if(i==1) mtext(MSE@Snames[pp],3,line=1,font=2)
    }
  }

  mtext("Candidate Management Procedure",1,line=6.8,font=2,outer=T)

}


