#' Calculate performance metrics.
#'
#' @param MSE an object of class MSE
#' @param rnd number of significant digits to round tabulated values
#' @param outdir a directory for the table
#' @param quantiles a vector 2 long of upper and lower quantiles to calculate and include in the table if quantile=T
#' @param quantile logical: should quantiles be calculated and included in the table
#' @return a list n stocks long each with a data frame of performance metrics (columns) by management procedure (row) \code{MSE}.
#' \itemize{
#'   \item C10. Mean yield over the first 10 years of the projection
#'   \item C20. Mean yield over years 11-20 of the projection
#'   \item C30. Mean yield over years 21-30 of the projection
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

    AvC10a<-AvC10(MSE,pp=pp)
    AvC30a<-AvC30(MSE,pp=pp)

    C10a<-C10(MSE,pp=pp)
    C20a<-C20(MSE,pp=pp)
    C30a<-C30(MSE,pp=pp)

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

    AvgBra<-AvgBr(MSE,pp)
    Br30a<-Br30(MSE,pp)

    PGTa<-PGT(MSE,pp)

    if(!quantile){
    out[[pp]]<-data.frame("AvC10"=apply(AvC10a,1,quantile,p),
                          "AvC30"=apply(AvC30a,1,quantile,p),

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
                          "AvgBr"=apply(AvgBra,1,quantile,p),
                          "Br30"=apply(Br30a,1,quantile,p),
                          "PGT"=apply(PGTa,1,quantile,p),
                          row.names=MPnams)
    }else{
      ptext<-c(p[1]*100,"Med",p[3]*100)
      metrics<-c("AvC30","C10","C20","C30","D10","D20","D30","LD","DNC","LDNC","POF","POS","PGK","AAVC","Br30")
      pnams<-paste(rep(metrics,each=3),rep(ptext,length(metrics)),sep="_")
     dat<-cbind(

       t(apply(AvC10a,1,quantile,p)),
       t(apply(AvC30a,1,quantile,p)),
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
                            t(apply(AvgBra,1,quantile,p)),
                              t(apply(Br30a,1,quantile,p)),
                              t(apply(PGTa,1,quantile,p)))

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



#' Plot the true and simulated indices for an MSE run
#'
#' @param MSEobj An object of class 'MSE'
#' @param index An integer number of the index to be plotted or its name (order is Inames slot of MSE object)
#' @param MPs A numeric vector or vector of names of MPs (order is MPs slot of MSE object)
plot_Indices<-function(MSEobj,index=1,MPs=NA){

  if(is.na(MPs))MPs<-1:MSEobj@nMPs
  if(class(MPs[1])=='character')MPs<-match(MPs,MSEobj@MPs)
  if(class(index)=='character')index=match(index,MSEobj@Inames)

  nMPs<-length(MPs)
  nsim<-dim(MSEobj@VBi)[2]
  nind<-dim(MSEobj@VBi)[3]
  allyears<-MSEobj@nyears+MSEobj@proyears
  ub<-min(nsim,10)
  par(mfcol=c(3,nMPs),mai=c(0.2,0.2,0.1,0.1),omi=c(0.4,0.4,0.45,0.01))
  yrs<-1965+(1:allyears)

  VBylim<-c(0,max(MSEobj@VBi[,,index,]))
  Iobsylim<-c(0,max(MSEobj@Iobs[,,index,],na.rm=T))
  sid<-match(MSEobj@Inames[index],MSEobj@Istats[,1])

  for(mm in 1:nMPs){

    cols<-c('red','blue','dark green','green','grey','purple','brown','orange','pink','black')
    matplot(yrs,t(MSEobj@VBi[mm,1:2,index,]),col=cols,lty=1,type='l',lwd=2,yaxs='i',ylim=Iobsylim)
    matplot(yrs,t(MSEobj@Iobs[mm,1:2,index,]),col=cols,lty=1,add=T,type='l')
    abline(v=2016.5,col='#99999970',lwd=2)
    legend('topright',legend="First two simulations",bty='n')
    if(mm==1)legend('left',legend=c("True Vuln Bio","Index"),lwd=c(2,1),bty='n')
    mtext(MSEobj@MPs[mm],3,line=0.5,cex=0.9)


    matplot(yrs,t(MSEobj@VBi[mm,1:ub,index,]),col=cols,lty=1,type='l',yaxs='i',ylim=VBylim)
    abline(v=2016.5,col='#99999970',lwd=2)
    legend('topright',legend="True Vuln Bio, 10 sims",bty='n')

    matplot(yrs,t(MSEobj@Iobs[mm,1:ub,index,]),col=cols,lty=1,type='l',yaxs='i',ylim=Iobsylim)
    abline(v=2016.5,col='#99999970',lwd=2)
    legend('topright',legend="Indices, 10 sims",bty='n')

  }
  mtext(paste0("Index: ", MSEobj@Inames[index]," (StDev = ",MSEobj@Istats[sid,3],", AC1 = ",MSEobj@Istats[sid,4],")"),3,line=1.25,outer=T,font=2)
  mtext("Year",1,line=1.3,outer=T)
  mtext("Index (or calibrated vulnerable biomass)",2,line=1.6,outer=T)
  mtext(deparse(substitute(MSEobj)),3,line=1,at=0.08,font=2,outer=T)

}




#' Plot the unfished recruitment, mean and stochastic recruitment of a projection
#'
#' @param MSEobj An object of class 'MSE'
#' @param MPs A numeric vector of MPs (order is MPs slot of MSE object)
plot_Recruitment<-function(MSEobj,MPs=1:2){

  startyr=1965
  par(mfcol=c(3,2),mai=c(0.6,0.7,0.05,0.05),omi=c(0.01,0.01,0.4,0.01))
  simlines<-c(1,2)
  simpch<-c(1,4)
  MPcols<-c('red','blue')

  for(pp in 2:1){
    yrs<-startyr+(1:(MSEobj@proyears+MSEobj@nyears))-1

    # SSB
    ylim1<-c(0,1.02*max(MSEobj@SSB_proj[,,pp,],MSEobj@R0_proj[pp,]*MSEobj@SSBpR[1,pp],MSEobj@dynB0[,pp,],na.rm=T))
    plot(yrs,c(MSEobj@dynB0h[1,pp,],MSEobj@dynB0[1,pp,]),ylim=ylim1,lty=1,col="green",type="l",ylab="SSB",yaxs='i',xlab="Year")
    lines(yrs,c(MSEobj@dynB0h[1,pp,]*MSEobj@SSBMSY_SSB0[1,pp],MSEobj@dynB0[1,pp,]*MSEobj@SSBMSY_SSB0[1,pp]),ylim=ylim1,col="green",lty=2)
    lines(yrs,MSEobj@R0_proj[pp,]*MSEobj@SSBpR[1,pp],col="black")
    matplot(yrs,t(MSEobj@SSB_proj[MPs[1],,pp,]),ylim=ylim,lty=simlines,col=MPcols[1],type="l",add=T)
    matplot(yrs,t(MSEobj@SSB_proj[MPs[2],,pp,]),ylim=ylim,lty=simlines,col=MPcols[2],type="l",add=T)
    if(pp==2){
      legend('bottomright',legend=c("Dynamic SSB0 (SSBMSY)","Equilibrium SSB0",MSEobj@MPs[MPs[1]],MSEobj@MPs[MPs[2]]),text.col=c('green','black','red','blue'),cex=0.9,bty='n')
      legend('bottomleft',legend=c("Simulation 1","Simulation 2"),lty=c(1,2),bty='n',cex=0.9)
    }

    # Recruitment
    ylim2=c(0,1.02*max(MSEobj@R0_proj[pp,],MSEobj@Rec_err[MPs,1,pp,],MSEobj@Rec_mu[MPs,1,pp,],na.rm=T))
    plot(yrs,MSEobj@R0_proj[pp,],ylim=ylim2,type='l',ylab="Recruitment (with process error)",yaxs='i',xlab="Year")
    #matplot(yrs,t(MSEobj@Rec_mu[MPs[1],,pp,]),lty=simlines,col=MPcols[1],type="l",add=T)
    #matplot(yrs,t(MSEobj@Rec_mu[MPs[2],,pp,]),lty=simlines,col=MPcols[2],type="l",add=T)
    matplot(yrs,MSEobj@Rec_err[MPs[1],1,pp,],lty=simlines,col=MPcols[1],type="l",add=T)
    matplot(yrs,MSEobj@Rec_err[MPs[2],1,pp,],lty=simlines,col=MPcols[2],type="l",add=T)

    if(dim(MSEobj@AC)[2]>2){
      ind<-(pp*2)-(1:0)
    }else{
      ind<-pp
    }

    legend('topleft',legend=
             c(paste0(rep("SD",length(ind))," = ",round(MSEobj@Reccv[1,ind],3)),
             paste0(rep("AC",length(ind))," = ",round(MSEobj@AC[1,ind],3))),bty='n')

    # S-R
    ylim3=c(0,1.02*max(MSEobj@Rec_mu[,,pp,],na.rm=T))
    for(ss in 1:1){
      for(MP in MPs){
        if(ss==1&MP==MPs[1]){
          plot(MSEobj@SSB_proj[MP,ss,pp,],MSEobj@Rec_mu[MP,ss,pp,],col=MPcols[match(MP,MPs)],pch=simpch[ss],ylim=ylim3,
               xlim=ylim1,xlab="SSB",ylab="Deterministic recruitment",yaxs='i')
          legend('bottomleft',legend=c("Simulation 1","Simulation 2"),pch=simpch,bty='n',cex=0.9)
        }else{
          points(MSEobj@SSB_proj[MP,ss,pp,],MSEobj@Rec_mu[MP,ss,pp,],col=MPcols[match(MP,MPs)],pch=simpch[ss])
        }
      }
    }

  }
  mtext(c("West Stock","East Stock"),3,line=0.4,at=c(0.3,0.8),outer=T,cex=0.9)
  mtext(deparse(substitute(MSEobj)),3,line=1,at=0.08,font=2,outer=T)

}



#' Plot the catch composition during projections r
#'
#' @param MSEobj An object of class 'MSE'
#' @param MPs A numeric vector of MPs (order is MPs slot of MSE object)
plot_CatchComp<-function(MSEobj){
  startyr<-2016
  MP<-2
  sim<-1
  cols=c('black','red','green','blue','orange','grey','pink','purple','brown')
  yrs<-startyr+(1:MSEobj@proyears)
  yrind<-MSEobj@nyears+(1:MSEobj@proyears)
  fleets<-apply(MSEobj@Fleet_cat[sim,MP,,yrind],1,sum)>100
  matplot(yrs,t(MSEobj@Fleet_cat[sim,MP,fleets,yrind])/1000,type='l',col=cols,lty=1,ylab="Catch (t)")
  legend('right', legend=OM_1@Fleets$name[fleets],text.col=cols,bty='n',cex=0.8)
}




#' Compile the performance results for a complete set of MSE runs (all reference and robustness operating models)
#'
#' @param dir Character string, a directory where the MSE objects are saved (e.g. MSE_1.rda, MSE_2.rda, ... MSE_R_1.rda). Defaults to the current working directory
#' @param name character string, the name of your set of MSE runs. Defaults to NULL
#' @param CMPdesc A vector of character strings. These describe the CMPs tested in the MSE runs (not including the zero catch CMP that is run by default). Defaults to NULL.
#' @param CMPcode A vector of 3 character strings that identify your CMP for presentation purposes (e.g. c("TC1","TC2","TC3","TC4"))
Results_compiler<-function(dir=NULL,name=NULL, CMPdesc=NULL, CMPcode=NULL){

  if(!is.null(dir)) setwd(dir)

  files<-list.files()
  isMSE<-grepl("MSE_",files)
  MSEref<-c(paste0("MSE_",1:48),paste0("MSE_R_",1:44))
  nOMs<-length(MSEref)
  fileref<-paste0(MSEref,".rda")
  MSEtab<-data.frame(MSE=MSEref,Available=fileref%in%files)

  if(sum(isMSE)!=92){ # Check 1 - are there enough files?
    message("You currently have ",sum(isMSE), " MSE objects in this folder. You should have ", nOMs, " including all reference OMs and robustness OMs:")
    return(MSEtab)
  }

  if(sum(MSEtab$Available)!=nrow(MSEtab)){ # Check 2 - do they match the required naming convention?
    message("You do not have all the required MSE objects:")
    print(MSEtab)
    return(MSEtab)
  }


  MSEobj<-readRDS(fileref[1])
  MSEnames<-rep(NA,nOMs)
  perf<-getperf(MSEobj)
  pnames<-names(perf[[1]])
  pnames<-pnames[!pnames%in%c("POF","PGK")]
  nmet<-length(pnames)
  nMPs<-MSEobj@nMPs
  nsim<-MSEobj@nsim
  nyears<-MSEobj@nyears+MSEobj@proyears

  message("MSE_1.rda has ",nsim," simulations and ",nMPs, " candidate management procedures (including the Zero catch reference CMP)")

  MET<-array(NA,c(nsim,nOMs,2, nMPs, nmet))
  dynSSB0 <- dynSSBMSY <- array(NA,c(nsim,nOMs,2,nyears))
  R0 <- SSB0 <- array(NA,c(nOMs,2,nyears))
  Rec <-  CW <- CWa <- B_BMSY <- F_FMSY <- array(NA,c(nsim,nOMs,2,nMPs,nyears))

  #system.time({

  for(OM in 1:nOMs){

    MSEobj<-readRDS(fileref[OM])
    test<-strsplit(MSEobj@Name,"/")[[1]]
    #MSEnames[OM]<-paste0(test[length(test)-(1:0)],collapse="")

    for(pp in 1:2){
      for(mt in 1:nmet){
        MET[,OM,pp,,mt]<-t(do.call(pnames[mt], list(MSE=MSEobj,pp=pp)))
      }
    }

    dynSSB0[,OM,,] <-   abind(MSEobj@dynB0h,MSEobj@dynB0) # dynamic SSB0 stored
    dynSSBMSY[,OM,,] <- abind(MSEobj@dynB0h*array(MSEobj@SSBMSY_SSB0,dim(MSEobj@dynB0h)),
                              MSEobj@dynB0*array(MSEobj@SSBMSY_SSB0,dim(MSEobj@dynB0)) ) # SSBmsy dynamic

    SSB0[OM,,] <- MSEobj@R0_proj*MSEobj@SSBpR[1,]
    R0[OM,,] <-   MSEobj@R0_proj
    Rec[,OM,,,] <- aperm(MSEobj@Rec_mu,c(2,3,1,4))

    CW[,OM,,,] <- aperm(MSEobj@CW,c(2,3,1,4))
    CWa[,OM,,,] <- aperm(MSEobj@CWa,c(2,3,1,4))
    B_BMSY[,OM,,,] <- aperm(MSEobj@B_BMSY,c(2,3,1,4))
    F_FMSY[,OM,,,] <- aperm(MSEobj@F_FMSY,c(2,3,1,4))
    print(paste(OM,"/",nOMs,"MSE objects (OMs)"))

  }

  #})

  #OMcheck<-c(paste0("OMs",1:48),paste0("ROMs",1:44))
  #if(sum(MSEnames%in%OMcheck)!=nOMs){
   # message("The names of the MSE objects should correspond with the reference OMs and robustness OMs in the ABTMSE package. Currently they do not match:")
    #diag<-data.frame(Needed = OMcheck, Available = MSEnames%in%OMcheck)
    #print(diag)
    #return(diag)
  #}

  if(any(is.na(MET))){
    message("Warning: there are NA values in the summary performance metrics")
  }

  # performance metrics descriptions
  Rd2list <- function(Rd){
    names(Rd) <- substring(sapply(Rd, attr, "Rd_tag"),2);
    temp_args <- Rd$arguments;

    Rd$arguments <- NULL;
    myrd <- lapply(Rd, unlist);
    myrd <- lapply(myrd, paste, collapse="");

    temp_args <- temp_args[sapply(temp_args , attr, "Rd_tag") == "\\item"];
    temp_args <- lapply(temp_args, lapply, paste, collapse="");
    temp_args <- lapply(temp_args, "names<-", c("arg", "description"));
    myrd$arguments <- temp_args;
    return(myrd);
  }

  getHelpList <- function(...){
    thefile <- help(...)
    myrd <- utils:::.getHelpFile(thefile);
    Rd2list(myrd);
  }

  pdesc<-rep(NA,length(pnames))
  for(pp in 1:length(pnames)){
    x <- getHelpList(pnames[pp])
    pdesc[pp]<-strsplit(strsplit(x$description,"\n")[[1]][2],' \\(a')[[1]][1]
  }

  #getROMnam<-function(x)get(x)@Name
  #ROMcode<- sapply(paste0("ROM_",1:44,"d"),getROMnam)# run a get of the ROM@names c(paste0("Sen_",c(55,56,58,59)),paste0("WGr_",c(55,56,58,59)),paste0("BrC_",c(55,56,58,59)))
  ROMlevs<-c("WstGw","Qinc","CatOver","HiWmix","BrzCt","TVmix","NLindex","PChgMix","TVregime","IntPar","ZeroEmix")
  ROMcode<-paste(rep(ROMlevs,each=4),rep(c(1:4),length(ROMlevs)),sep="_")

  if(is.null(CMPcode)|length(CMPcode)!=(nMPs-1)){
    MPnames<-unlist(lapply(MSEobj@MPs,FUN=function(X)paste0(unlist(X),collapse="-")))
  }else{
    MPnames<-c("ZeroC",CMPcode)
  }


  return(list(MET=MET, dynSSB0=dynSSB0, dynSSBMSY = dynSSBMSY,
              R0 = R0, SSB0 = SSB0,
              Rec = Rec, CW = CW, CWa = CWa,
              B_BMSY = B_BMSY, F_FMSY = F_FMSY,
              name=name, CMPdesc=c("Zero catches",CMPdesc), pdesc=pdesc, ROMcode = ROMcode,
              pnames=pnames, MPnames=MPnames,
              OMnames=c(as.character(1:48),paste0("R",1:44)),
              Design=Design))


}




#' Compile the performance results for a complete set of MSE runs (all reference and robustness operating models)
#'
#' @param ResList A list of results objects compiled by the function Results_compiler()
Join_Results<-function(ResList){
  Res<-ResList[[1]]
  Res$name <- paste("Joined results including:", paste(unlist(lapply(ResList,function(x)x$name)),collapse=", "))

  for(i in 2:length(ResList)){
    print(paste0("-----",i,"------"))
    ResT<-ResList[[i]]
    MPind<-2:dim(ResT$MET)[4]
    Res$MET<-abind(Res$MET,ResT$MET[,,,MPind,,drop=F],along=4)
    Res$Rec<-abind(Res$Rec,ResT$Rec[,,,MPind,,drop=F],along=4)
    Res$CW<-abind(Res$CW,ResT$CW[,,,MPind,,drop=F],along=4)
    Res$CWa<-abind(Res$CWa,ResT$CWa[,,,MPind,,drop=F],along=4)
    Res$B_BMSY<-abind(Res$B_BMSY,ResT$B_BMSY[,,,MPind,,drop=F],along=4)
    Res$F_FMSY<-abind(Res$F_FMSY,ResT$F_FMSY[,,,MPind,,drop=F],along=4)
    Res$CMPdesc<-c(unlist(Res$CMPdesc),unlist(ResT$CMPdesc[MPind]))
    Res$MPnames<-c(Res$MPnames,ResT$MPnames[MPind])
    print(ResT$CMPdesc)
  }

  for(i in 1:length(Res$MPnames))Res$MPnames[i]<-substr(Res$MPnames[i],1,12)
  #for(i in 1:length(Res$CMPdesc))Res$CMPdesc[i]<-substr(Res$CMPdesc[i],1,12)
  names(Res$MPnames)<-rep(NULL,length(Res$MPnames)) # get rid of any vector labels
  Res
}

#' Compile the performance results for a complete set of MSE runs (all reference and robustness operating models)
#'
#' @param ResAll A compiled results object created by Results_compiler() or Join_Results()
#' @param ind A vector as long as the MPs (e.g. length(ResAll$MPnames)) or an integer vector (indicating the MPs to keep)
Sub_Results<-function(ResAll,ind){
  ResNew<-ResAll
  ResNew$MET <- ResAll$MET[,,,ind,]
  ResNew$Rec <- ResAll$Rec[,,,ind,]
  ResNew$CW <- ResAll$CW[,,,ind,]
  ResNew$CWa <- ResAll$CWa[,,,ind,]
  ResNew$B_BMSY <- ResAll$B_BMSY[,,,ind,]
  ResNew$F_FMSY <- ResAll$F_FMSY[,,,ind,]
  ResNew$CMPdesc <- ResAll$CMPdesc[ind]
  ResNew$MPnames <- ResAll$MPnames[ind]
  ResNew
}

#' Get Br30 results accounting for OM weights
#'
#' @param MSElist A list of the deterministic reference set MSE objects
#' @param q The percentile of the weighted distribution
Br30_Wt<-function(MSElist,q=0.5){

  dims<-dim(MSElist[[1]]@SSB)
  nOMs<-length(MSElist)
  nMPs<-dims[1]

  getEB<-function(X)Br30(X,1)[,1]
  getWB<-function(X)Br30(X,2)[,1]

  EBa<-matrix(unlist(lapply(MSElist,getEB)),ncol=nOMs)
  WBa<-matrix(unlist(lapply(MSElist,getWB)),ncol=nOMs)

  E_Br30<-apply(EBa,1,wtd.quantile,q=q,weight=OM_wt[1:nOMs])
  W_Br30<-apply(WBa,1,wtd.quantile,q=q,weight=OM_wt[1:nOMs])

  dat<-data.frame(Western=W_Br30,Eastern=E_Br30)
  row.names(dat)<-unlist(lapply(MSElist[[1]]@MPs,function(X)X[1]))
  dat

}


