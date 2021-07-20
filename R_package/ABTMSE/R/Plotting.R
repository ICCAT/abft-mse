# Plotting code

plotcomp<-function(out,plotnam=""){

  par(mfrow=c(5,2),mai=c(0.2,0.2,0.1,0.1),omi=c(0.3,0.4,0.3,0.01))

  ys<-c(1980,1990,2000,2010,2016)-1965+1

  for(i  in 1:length(ys)){

    for(pp in 2:1){

      Ns<-apply(out$N[pp,ys[i],,,],2,sum)
      barplot(Ns,col='blue',border="white",names.arg=c(as.character(c(1:34)),"35+"),width=1,cex.axis =0.8)
      legend('top',legend=1964+ys[i],bty='n',cex=1.1)

    }
  }

  mtext(c("West Stock","East Stock"),3,adj=c(0.25,0.8),line=0.3,outer=T)
  mtext(plotnam,3,line=0.5,outer=T,font=2)
  mtext("Ages 1-35+",1,line=0.6,outer=T)
  mtext("Number of fish",2,line=0.4,outer=T)

}


plotindex2<-function(Base,pCPUE){

  anam<-Base@areanams
  par(mfcol=c(Base@nr,Base@ns),mai=c(0.2,0.2,0.1,0.05),omi=c(0.5,1,0.25,0.01))
  ylimy<-c(0,quantile(pCPUE,0.995,na.rm=T))

  for(q in 1:Base@ns){for(a in 1:Base@nr){
    plot(Base@years[1]:Base@years[2],pCPUE[,q,a],ylim=ylimy,type='l',col='red',axes=F)
    if(a==1)mtext(paste("Quarter",q),3,line=1)
    if(q==1)mtext(anam[a],2,line=3,las=1)
    if(q==4){axis(1,prettyaxis(Base@years),prettyaxis(Base@years))
    }else{axis(1,prettyaxis(Base@years))}
    if(a==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
    }else{axis(2,prettyaxis(ylimy))}

  }}
  mtext("Year",1,line=1.5,outer=T)
}

#' Plot area definitions of an OMI, OM or MSE object
#'
#' @param .Object an object of class OMI, OM or MSE
#' @return a map of the areas of \code{classy}
#' @examples
#' loadABT()
#' areaplot(OM_1)
#' areaplot(OMI_1)
#' areaplot(MSE_example)
areaplot<-function(.Object,xadj=NA,yadj=NA){

  if(is.na(xadj[1]))xadj=rep(0,length(.Object@areanams))
  if(is.na(yadj[1]))yadj=rep(0,length(.Object@areanams))

  xlimy<-c(-100,40)
  ylimy<-c(-25,65)
  plot(xlimy,ylimy,col="white",axes=F,xlab="",ylab="")

  abline(v=(-18:18)*10,col='light grey',lwd=0.8)
  abline(h=(-18:18)*10,col='light grey',lwd=0.8)


  map(xlim=xlimy+c(-5,5),ylim=ylimy+c(-5,5),add=T,fill=T,col="light grey")
  map(xlim=xlimy,ylim=ylimy,add=T,fill=F,col="light grey",lwd=4)
   for(i in 1:length(.Object@areanams)){
    polygon(.Object@area_defs[[i]],border='black')
    text(mean(.Object@area_defs[[i]]$x)+xadj[i],mean(.Object@area_defs[[i]]$y)+yadj[i],paste0(i,": ",.Object@areanams[i]),col='red',font=2,cex=0.8)
   }
  axis(1)
  axis(2)

  #map(xlim=xlimy,ylim=ylimy,add=T,fill=T,col="light grey")

}


sdensplot<-function(dens,area_defs,areanams,ncolgrad=200,colpal='heat'){

  xlimy<-c(-95,40)
  ylimy<-c(-25,60)
  plot(xlimy,ylimy,col="white",axes=F,xlab="",ylab="")
  nbins<-ncolgrad
  bins<-seq(min(dens),max(dens),length.out=nbins+1)
  y<-ceiling((dens-min(dens))/(max(dens)-min(dens))*nbins)
  y[y==0]<-1
  if(colpal=="heat")cols<-heat.colors(ncolgrad,alpha=1)[ncolgrad:1]
  if(colpal=="gray")cols<-gray.colors(ncolgrad, start = 0.25, end = 0.985, gamma = 2.2, alpha = NULL)[ncolgrad:1]

  for(i in 1:length(areanames)){
    polygon(area_defs[[i]],col=cols[y[i]],border='white',density=NA)
    #text(mean(OMd@Area_defs[[i]]$x),mean(OMd@Area_defs[[i]]$y),OMd@Area_names[i],col='white',font=2,cex=0.8)
  }
  map(xlim=xlimy,ylim=ylimy,add=T,fill=T,col="light grey")
}


plot_mov_dist<-function(out,OMI,pnam=c("Eastern stock","Western stock"),custom=F){

  its<-10
  acs<-c(1,6,12)

  for(pp in 1:out$np){
    for(ac in 1:out$nma){

      movE<-out$mov[pp,,acs[ac],,]

      nr<-out$nr
      ns<-out$ns

      vecE<--array(NA,c(its,ns,nr))
      NE<-array(NA,c(its,ns,nr,nr))

      for(i in 1:its){

        for(j in 1:ns){

          if(i==1&j==1){
            vecE[1,1,]<-rep(1/nr,nr)
          }else if(j==1){
            vecE[i,1,]<-apply(NE[i-1,ns,,],2,sum)
          }else{
            vecE[i,j,]<-apply(NE[i,j-1,,],2,sum)
          }

          NE[i,j,,]<-vecE[i,j,]*movE[j,,]

        }

      }

      vmovplot(vecE[its,,],NE[its,,,],movE,anam=OMI@areanams,custom=custom)
      legend('topleft',legend=paste0(pnam[pp],", Age class ",ac),bty='n',cex=0.9)
    }
  }

}


vmovplot<-function(vec,N,mov,anam,custom){
  ns<-dim(mov)[1]
  nrr<-dim(mov)[2]

  sref<-(1:ns)[c(2:ns,1)]
  v2<-apply(N,c(1,3),sum)

  xlim<-c(0,(4+nrr)*ns)

  ncol<-100
  vcol<-rainbow(ncol,s=1,v=1,start=0,end=0.45,alpha=1)[ncol:1]
  for(i in 1:50)vcol[i]<-makeTransparent(vcol[i],i/5*10)
  #plot(1:100,pch=19,cex=2,col=vcol)

  colsv<-array(vcol[ceiling((vec/max(vec))*ncol)],dim(vec))
  colsN<-array(vcol[ceiling((N/max(N))*ncol)],dim(N))
  colsv2<-array(vcol[ceiling((v2/max(v2))*ncol)],dim(v2))

  if(!custom)par(mai=rep(0.01,4),omi=rep(0.01,4))
  plot(c(-4,ns*(nrr+4)),c(0,nrr+3.5),col="white",axes=F)

  for(s in 1:ns){

    sc<-(s-1)*(nrr+4)

    for(r in 1:nrr){

      polygon(x=c(sc+1+nrr,sc+2+nrr,sc+2+nrr,sc+1+nrr),y=c(r-1,r-1,r,r),col=colsv[sref[s],r],border="white")

      for(r2 in 1:nrr){

        polygon(x=c(sc+(nrr-r2),sc+1+(nrr-r2),sc+1+(nrr-r2),sc+(nrr-r2)),y=c(r-1,r-1,r,r),col=colsN[s,r,r2],border='white')

      }

      #polygon(x=c(sc+2+(nrr-r),sc+3+(nrr-r),sc+3+(nrr-r),sc+2+(nrr-r)),y=c(0,0,1,1),col=colsv2[s,r],border='white')

    }

    for(r in 1:nrr){
      text(-2.5,r-0.5,anam[r],cex=0.75)
      polygon(x=c(sc+(nrr-r),sc+1+(nrr-r),sc+1+(nrr-r),sc+(nrr-r)),y=c(r-1,r-1,r,r),col=NA,border='light grey')

    }

    text(sc+nrr/2,nrr+0.75,paste0("Season ",s),font=1,cex=0.8)

    arrows(x0=sc+nrr+2.5,x1=sc+nrr+3.5,y0=nrr/2,y1=nrr/2,length=0.1,lwd=2)

  }

}


plot_mov<-function(out,OMI,pnam=c("Eastern stock","Western stock"),custom=F){

  acs<-c(1,6,12)

  for(pp in 1:out$np){
    for(ac in 1:out$nma){

      movE<-out$mov[pp,,acs[ac],,]

      nr<-out$nr
      ns<-out$ns

      movplot(movE,anam=OMI@areanams,custom=custom)
      legend('topleft',legend=paste0(pnam[pp],", Age class ",ac),bty='n',cex=0.9)
    }
  }

}

movplot<-function(mov,anam,custom){

  N<-mov
  ns<-dim(mov)[1]
  nrr<-dim(mov)[2]

  sref<-(1:ns)[c(2:ns,1)]
  v2<-apply(N,c(1,3),sum)

  xlim<-c(0,(4+nrr)*ns)

  ncol<-100
  vcol<-rainbow(ncol,s=1,v=1,start=0,end=0.45,alpha=1)[ncol:1]
  for(i in 1:50)vcol[i]<-makeTransparent(vcol[i],i/5*10)
  #plot(1:100,pch=19,cex=2,col=vcol)

  colsN<-array(vcol[ceiling((N/max(N))*ncol)],dim(N))

  if(!custom)par(mai=rep(0.01,4),omi=rep(0.01,4))
  plot(c(-4,ns*(nrr+4)),c(0,nrr+3.5),col="white",axes=F)

  for(s in 1:ns){

    sc<-(s-1)*(nrr+4)

    for(r in 1:nrr){

      #polygon(x=c(sc+1+nrr,sc+2+nrr,sc+2+nrr,sc+1+nrr),y=c(r-1,r-1,r,r),col=colsv[sref[s],r],border="white")

      for(r2 in 1:nrr){

        polygon(x=c(sc+(nrr-r2),sc+1+(nrr-r2),sc+1+(nrr-r2),sc+(nrr-r2)),y=c(r-1,r-1,r,r),col=colsN[s,r,r2],border='white')

      }

      #polygon(x=c(sc+2+(nrr-r),sc+3+(nrr-r),sc+3+(nrr-r),sc+2+(nrr-r)),y=c(0,0,1,1),col=colsv2[s,r],border='white')

    }

    for(r in 1:nrr){
      text(-2.5,r-0.5,anam[r],cex=0.75)
      polygon(x=c(sc+(nrr-r),sc+1+(nrr-r),sc+1+(nrr-r),sc+(nrr-r)),y=c(r-1,r-1,r,r),col=NA,border='light grey')

    }

    text(sc+nrr/2,nrr+0.75,paste0("Season ",s),font=1,cex=0.8)

    arrows(x0=sc+nrr+2.5,x1=sc+nrr+3.5,y0=nrr/2,y1=nrr/2,length=0.1,lwd=2)

  }

}

plotSSBMSE<-function(MSEs){
  MSE<-MSEs[[1]]
  nsim<-MSE@nsim
  nyears<-MSE@nyears
  proyears<-MSE@proyears
  allyears<-proyears+nyears
  nMPs<-MSE@nMPs

  CC<-MSE@C/1000000

  #somenames=c("Green Kobe","Final depletion","AAV Yield","Yield","Yield 5% DR", "Yield 10% DR", "Yield -5% DR")

  #stats<-getperf(MSE)
  yrs<-startyr:(startyr+MSE@proyears-4)
  refyears<-modelyr:(modelyr+MSE@proyears-4)
  worms<-1:min(nworms,MSE@nsim)

  xtick<-pretty(seq(yrs[1],yrs[length(yrs)],length.out=3))

  SSBcol="light blue"
  Catcol="light grey"

  Cq<-apply(CC,c(1,3,4),quantile,p=quants)
  Clim<-cbind(rep(0,MSE@npop),apply(Cq[3,,,refyears],2,max))
  if(!MSY){
    if(byarea)SSBnorm<-MSE@SSBa/array(MSE@SSBa[,,,MSE@nyears],dim(MSE@SSBa))
    if(!byarea)SSBnorm<-MSE@SSB/array(MSE@SSB[,,,MSE@nyears],dim(MSE@SSB))
  }else{
    SSBnorm<-array(NA,c(nMPs,nsim,2,allyears)) # because you only have dynB0 for future years
    dB0yrs<-(proyears-length(refyears))+1:length(refyears)
    #SSBnorm[,,,refyears]<-MSE@SSBa[,,,refyears]/array(rep(MSE@dynB0[,,dB0yrs],each=nMPs)/rep(rep(MSE@SSBMSY_SSB0,each=nMPs),length(refyears)),c(nMPs,nsim,2,length(refyears)))
    SSBnorm[,,,refyears]<-MSE@SSB[,,,refyears]/array(rep(MSE@dynB0[,,dB0yrs],each=nMPs)*rep(rep(MSE@SSBMSY_SSB0,each=nMPs),length(refyears)),c(nMPs,nsim,2,length(refyears)))
    SSBnorm[,,,1:(modelyr-1)]<-SSBnorm[,,,modelyr]
  }

  SSBq<-apply(SSBnorm,c(1,3,4),quantile,p=quants,na.rm=T)
  SSBlim<-cbind(rep(0,MSE@npop),apply(SSBq,3,max,na.rm=T))

  linecols<-rep(c("black","orange","blue","red","green","light grey","grey","pink","purple","brown"),100)

  MPnams<-unlist(MSE@MPs)
  MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

  par(mfrow=c(MSE@nMPs,MSE@npop*4),mai=c(0.05,0.05,0.35,0.05),omi=c(0.5,0.05,0.15,0.02))
  rsz<-5
  fill<-NA
  rw<-c(fill,rep(1,rsz),rep(2,rsz),fill,rep(3,rsz),rep(4,rsz),fill,fill,rep(5,rsz),rep(6,rsz),fill,rep(7,rsz),rep(8,rsz))
  lmat<-matrix(NA,ncol=rsz*MSE@nMPs,nrow=rsz*8+5)
  for(i in 1:nMPs)lmat[,(i-1)*rsz+1:rsz]<-rw+(i-1)*8
  lmat<-t(lmat)
  lmat[is.na(lmat)]<-max(lmat,na.rm=T)+1
  layout(lmat)

  pind<-1:MSE@npop
  if(rev)pind=MSE@npop:1

  gridcol='light grey'

  for(MP in 1:MSE@nMPs){
    for(pp in pind){
      # Catch projection  ---
      # Col 1: Catch quantiles
      ytick<-pretty(seq(0,Clim[pp,2],length.out=4))
      plot(range(yrs),Clim[pp,],axes=F,col="white",xlab="",ylab="",ylim=Clim[pp,])
      abline(h=ytick,col=gridcol)
      abline(v=xtick,col=gridcol)
      polygon(c(yrs,yrs[length(yrs):1]),
              c(Cq[1,MP,pp,refyears],Cq[3,MP,pp,refyears[length(yrs):1]]),
              col=Catcol,border=F)
      lines(yrs,Cq[2,MP,pp,refyears],lwd=1.5,col="black")
      if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
      if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
      abline(h=0)

      axis(2,ytick,labels=ytick)
      #legend('topright',legend="Catches (t)",bty='n')


      # Col 2: Catch worms
      plot(range(yrs),Clim[pp,],axes=F,col="white",xlab="",ylab="",ylim=Clim[pp,])
      abline(h=ytick,col=gridcol)
      abline(v=xtick,col=gridcol)
      for(i in 1:length(worms))lines(yrs,CC[MP,i,pp,refyears],col=linecols[i],lwd=1.2)
      if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
      if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
      abline(h=0)

      mtext('Catches (kt)',3,adj=-0.8,line=-1,cex=0.7)


      # SSB projection  ---
      # Col 3: SSB quantiles
      ytick<-pretty(seq(0,SSBlim[pp,2],length.out=4))
      plot(range(yrs),SSBlim[pp,],axes=F,col="white",xlab="",ylab="",ylim=SSBlim[pp,])
      abline(h=ytick,col=gridcol)
      abline(v=xtick,col=gridcol)
      polygon(c(yrs,yrs[length(yrs):1]),
              c(SSBq[1,MP,pp,refyears],SSBq[3,MP,pp,refyears[length(yrs):1]]),
              col=SSBcol,border=F)
      lines(yrs,SSBq[2,MP,pp,refyears],lwd=1.5,col="black")
      if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
      if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
      abline(h=0)
      abline(h=1,lty=2)
      axis(2,ytick,labels=ytick)
      #legend('topright',legend="SSB relative to 2018",bty='n')

      mtext(MPnams[(MP-1)*2+pp],3,adj=-0.35,line=0.3,cex=0.9)

      # Col 4: SSB worms
      plot(range(yrs),SSBlim[pp,],axes=F,col="white",xlab="",ylab="",ylim=SSBlim[pp,])
      abline(h=ytick,col=gridcol)
      abline(v=xtick,col=gridcol)
      for(i in 1:length(worms))lines(yrs,SSBnorm[MP,i,pp,refyears],col=linecols[i],lwd=1.2)
      if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
      if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
      abline(h=0)
      abline(h=1,lty=2)

      if(!MSY)mtext('SSB relative to 2018',3,adj=-0.8,line=-1,cex=0.7)
      if(MSY)mtext('SSB relative SSB_MSY',3,adj=-0.8,line=-1,cex=0.7)

    }
  }

  texty<-paste(MSE@Snames[pind],"Area")
  if(!byarea|MSY) texty<-paste(MSE@Snames[pind],"(Catch by area, SSB by stock)")
  mtext(texty,3,adj=c(0.15,0.85),line=-0.45,outer=T,font=2)

}






