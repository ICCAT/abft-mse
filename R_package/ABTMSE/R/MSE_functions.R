#' Calculate performance metrics.
#'
#' @param MSE an object of class MSE
#' @param rnd number of significant digits to round tabulated values
#' @param outdir a directory for the table
#' @param quantiles a vector 2 long of upper and lower quantiles to calculate and include in the table if quantile=T
#' @param quantile logical: should quantiles be calculated and included in the table
#' @return a list n stocks long each with a data frame of performance metrics (columns) by management procedure (row) \code{MSE}.
#' \itemize{
#'   \item Y10. Mean yield over the first 10 years of the projection
#'   \item Y20. Mean yield over years 11-20 of the projection
#'   \item Y30. Mean yield over years 21-30 of the projection
#'   \item PGK. Probability of Green Kobe (F<FMSY AND B>BMSY) region over all years of the projection
#'   \item POF. Probability of Over-Fishing (F>FMSY) over all years of the projection
#'   \item POS. Probability of Over-Fished status (B<BMSY) over all years of the projection
#'   \item D10. Mean depletion (SSB relative to unfished) after 10 years of the projection
#'   \item D20. Mean depletion (SSB relative to unfished) after 20 years of the projection
#'   \item D30. Mean depletion (SSB relative to unfished) after 30 years of the projection
#'   \item LD. Mean spawning stock depletion over all projected years
#'   \item DNC Spawning stock Depletion in final projection year relative to zero fishing
#'   \item LDNC. Lowest Spawning Stock depletion over all projection years relative to zero fishing
#'   \item AAVY. Mean Average Annual Variability in Yield over all projections
#'   ...
#' }
#' @examples
#' getperf(MSE_example)
getperf<-function(MSE,rnd=3,outdir=NA,quantiles=c(0.05,0.95),quantile=F){

  p<-0.5
  if(quantile)p<-c(quantiles[1],p,quantiles[2])
  nsim<-MSE@nsim
  proyears<-MSE@proyears
  nMPs<-MSE@nMPs

  MPnams<-unlist(MSE@MPs)
  MPnams<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  out<-new('list')

  for(pp in 1:MSE@npop){

    AvC30a<-AvC30(MSE,pp=pp)/1E6

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

    Br30a<-Br30(MSE,pp)

    if(!quantile){
    out[[pp]]<-data.frame("AvC30"=apply(AvC30a,1,quantile,p),

                          "C10"=apply(C10a,1,quantile,p),
                          "C20"=apply(C20a,1,quantile,p),
                          "C30"=apply(C30a,1,quantile,p),

                          "D10"=apply(D10a,1,quantile,p),
                          "D20"=apply(D20a,1,quantile,p),
                          "D30"=apply(D30a,1,quantile,p),

                          "LD"=apply(LDa,1,quantile,p),
                          "DNC"=apply(DNCa,1,quantile,p),
                          "LDNC"=apply(LDNCa,1,quantile,p),

                          "POF"=apply(POFa,1,quantile,p),
                          "POS"=apply(POSa,1,quantile,p),
                          "PGK"=apply(PGKa,1,quantile,p),

                          "AAVC"=apply(AAVCa,1,quantile,p),
                          "Br30"=apply(Br30a,1,quantile,p),
                          row.names=MPnams)
    }else{
      ptext<-c(p[1]*100,"Med",p[3]*100)
      metrics<-c("AvC30","C10","C20","C30","D10","D20","D30","LD","DNC","LDNC","POF","POS","PGK","AAVC","Br30")
      pnams<-paste(rep(metrics,each=3),rep(ptext,length(metrics)),sep="_")
     dat<-cbind(t(apply(AvC30a,1,quantile,p)),
           t(apply(C10a,1,quantile,p)),
             t(apply(C20a,1,quantile,p)),
               t(apply(C30a,1,quantile,p)),

                 t(apply(D10a,1,quantile,p)),
                   t(apply(D20a,1,quantile,p)),
                     t(apply(D30a,1,quantile,p)),

                       t(apply(LDa,1,quantile,p)),
                         t(apply(DNCa,1,quantile,p)),
                           t(apply(LDNCa,1,quantile,p)),

                             t(apply(POFa,1,quantile,p)),
                               t(apply(POSa,1,quantile,p)),
                                 t(apply(PGKa,1,quantile,p)),

                           t(apply(AAVCa,1,quantile,p)),
                              t(apply(Br30a,1,quantile,p)))

     out[[pp]]<-as.data.frame(dat,row.names=MPnams,names=pnams)
     names(out[[pp]])<-pnams


    }
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
PPlot<-function(MSE,Pnames=c("AvC30","C10","C30","D30","LD","DNC","LDNC","PGK","AAVC","Br30")){

  nsim<-MSE@nsim
  nMPs<-MSE@nMPs
  npop<-MSE@npop
  nperf<-length(Pnames)

  MPnams_a<-matrix(paste(rep(1:nMPs,each=2),unlist(MSE@MPs),sep=" - "),nrow=npop)

 # MPnams<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")
  #                 pop MP perf quantile
  store<-array(NA,c(2,nMPs,13,5))
  mult=rep(1,nperf)# multiplier for catch biomass scaling
  for(i in 1:nperf)if(substr(Pnames[i],1,1)=="C")mult[i]=1/1000

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

#' Run a set of MSE runs
#'
#' @param OMdir A vector of operating model directories (e.g. those found in abft-mse/Objects/OMs/)
#' @param MPs A vector of management procedures for analysis
#' @param nsim The number of simulations
#' @param OMdir Obs an observation error model of class 'Obs' e.g. Perfect_Obs
#' @param rebuildOMs Logical: should the OMs be rebuilt or should those in the directory be used directly
runAll<-function(OMdirs,MPs,Obs,outfolder){
  load(paste0(OMdirs[x],"/OM"))
  MSEobj<-new('MSE',OM=OM,MPs=MPs,Obs=Obs)
  save(MSEobj,file=paste0(outfolder,"/MSE_multi_",OMcode,".Rda"))
}

runMulti<-function(x,OMdirs,MPs,nsim,Obs,rebuildOMs,outfolder,seed,deterministic,Recs, Design){
  OMcode<-strsplit(OMdirs[x],split="//")[[1]][2]
  if(rebuildOMs){
     OM<-new('OM',OMdirs[x],nsim=nsim,Recruitment=Recs[[Design$Design_Ref[x,1]]])
  }else{
    if(deterministic){
      load(paste0(OMdirs[x],"/OMd"))
    }else{
      load(paste0(OMdirs[x],"/OM"))
    }
  }
  OM@seed<-seed
  #if(OMcode=="R1"){
   # MSEobj<-new('MSE',OM=OM,MPs=MPs,Obs=Obs,IE=Overage_20)
  #}else{
  MSEobj<-new('MSE',OM=OM,MPs=MPs,Obs=Obs)
  #}
  save(MSEobj,file=paste0(outfolder,"/MSE_multi_",x,".Rda"))

}

#' Run whole MSE runs in parallel
#'
#' @param OMdir A vector of operating model directories (e.g. those found in abft-mse/Objects/OMs/)
#' @param MPs A vector of management procedures for analysis
#' @param nsim The number of simulations
#' @param Obs an observation error model of class 'Obs' e.g. Perfect_Obs
#' @param rebuildOMs Logical: should the OMs be rebuilt or should those in the directory be used directly
#' @param outfolder the folder where output MSE objects should be placed
#' @param seed the random seed for the MSE
#' @param deterministic Logical: should the deterministic OMd be used
multiMSE<-function(OMdirs,MPs=MPs,nsim=12, Obs, rebuildOMs=T,outfolder,seed=1,deterministic=F,Recs=Recs,Design=Design){

  #sfExport('Obs')

  if(!sfIsRunning())stop("You need to start a cluster with sfInit before running this")

  sfSapply(1:length(OMdirs),runMulti,OMdirs=OMdirs,
           MPs=MPs,nsim=nsim,Obs=Obs,rebuildOMs=rebuildOMs,
           outfolder=outfolder,seed=seed,deterministic=deterministic,Recs=Recs,Design=Design)
  #sapply(1:length(OMdirs),runMulti,OMdirs=OMdirs, MPs=MPs,nsim=nsim,Obs=Obs,rebuildOMs=rebuildOMs,outfolder=outfolder,seed=seed)
  message("multiMSE run complete")

}



