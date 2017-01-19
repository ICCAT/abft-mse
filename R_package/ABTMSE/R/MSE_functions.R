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
getperf<-function(MSE){

  nsim<-MSE@nsim
  proyears<-MSE@proyears
  nMPs<-MSE@nMPs

  MPnams<-unlist(MSE@MPs)
  MPnams<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  out<-new('list')

  for(pp in 1:MSE@npop){

    Y10a<-Y10(MSE,pp=pp)/1000
    Y20a<-Y20(MSE,pp=pp)/1000
    Y30a<-Y30(MSE,pp=pp)/1000
    PGKa<-PGK(MSE,pp=pp)
    POFa<-POF(MSE,pp)
    POFeda<-POFed(MSE,pp)
    D10a<-D10(MSE,pp)
    D20a<-D20(MSE,pp)
    D30a<-D30(MSE,pp)
    LDa<-LD(MSE,pp)
    RSSBa<-RSSB(MSE,pp)
    LRSSBa<-LRSSB(MSE,pp)
    AAVYa<-AAVY(MSE,pp)

    out[[pp]]<-data.frame("Y10"=apply(Y10a,1,mean),
                          "Y20"=apply(Y20a,1,mean),
                          "Y30"=apply(Y30a,1,mean),
                          "PGK"=apply(PGKa,1,mean),
                          "POF"=apply(POFa,1,mean),
                          "POFed"=apply(POFeda,1,mean),
                          "D10"=apply(D10a,1,mean),
                          "D20"=apply(D20a,1,mean),
                          "D30"=apply(D30a,1,mean),
                          "LD"=apply(LDa,1,mean),
                          "RSSB"=apply(RSSBa,1,mean),
                          "LRSSB"=apply(LRSSBa,1,mean),
                          "AAVY"=apply(AAVYa,1,mean),row.names=MPnams)

  }

  names(out)<-MSE@Snames
  out

}

Tplot<-function(MSE){

  MPnams<-matrix(unlist(MSE@MPs),nrow=2)
  MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  par(mfrow=c(4,3),mai=c(0.5,0.5,0.25,0.05),omi=rep(0.05,4))

  for(pp in 1:MSE@npop){

    Y30a<-apply(Y30(MSE,pp)/1000,1,mean)
    PGKa<-apply(PGK(MSE,pp),1,mean)
    POFa<-apply(POF(MSE,pp),1,mean)
    POFeda<-apply(POFed(MSE,pp),1,mean)
    D30a<-apply(D30(MSE,pp),1,mean)
    AAVYa<-apply(AAVY(MSE,pp),1,mean)
    TOplt(PGKa,Y30a,MPnams[pp,],"P(Green Kobe)","Long Term Yield",0.5,NA)
    TOplt(POFa,POFeda,MPnams[pp,],"P(F>FMSY)","P(B<BMSY)",0.5,0.5)
    mtext(MSE@Snames[pp],3,line=0.3,font=2)
    TOplt(D30a,AAVYa,MPnams[pp,],"Final depletion","AAVY",0.1,0.2)

  }

  Y30_1<-apply(Y30(MSE,1)/1000,1,mean)
  PGK_1<-apply(PGK(MSE,1),1,mean)
  POF_1<-apply(POF(MSE,1),1,mean)
  POFed_1<-apply(POFed(MSE,1),1,mean)
  D30_1<-apply(D30(MSE,1),1,mean)
  AAVY_1<-apply(AAVY(MSE,1),1,mean)

  Y30_2<-apply(Y30(MSE,pp=2)/1000,1,mean)
  PGK_2<-apply(PGK(MSE,pp=2),1,mean)
  POF_2<-apply(POF(MSE,2),1,mean)
  POFed_2<-apply(POFed(MSE,2),1,mean)
  D30_2<-apply(D30(MSE,2),1,mean)
  AAVY_2<-apply(AAVY(MSE,2),1,mean)

  TOplt(Y30_1,Y30_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"Long term Yield")
  TOplt(PGK_1,PGK_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"P(Green Kobe)")
  TOplt(POF_1,POF_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"P(F>FMSY)")
  TOplt(POFed_1,POFed_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"P(B<BMSY)")
  TOplt(D30_1,D30_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"Final depletion")
  TOplt(AAVY_1,AAVY_2,MPnamsj,MSE@Snames[1],MSE@Snames[2],NA,NA,"Av. Ann. Var. Yld.")

}

TOplt<-function(x,y,MPs,xlab,ylab,xref=NA,yref=NA,main=""){
  MPcols<-rep(c("black","blue","orange","green","red","grey"),20)
  plot(x,y,col='white',xlab="",ylab="",
       xlim=range(x)+(max(x)-min(x))/15*c(-1,1),
       ylim=range(y)+(max(y)-min(y))/15*c(-1,1))
  abline(h=yref,lty=2,col="grey")
  abline(v=xref,lty=2,col="grey")
  text(x,y,MPs,col=MPcols)
  mtext(xlab,1,line=2,cex=0.85)
  mtext(ylab,2,line=2,cex=0.85)
  mtext(main,3,line=0.4,cex=0.85,font=2)
}





