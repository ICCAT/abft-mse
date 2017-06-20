# Plotting code


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
areaplot<-function(.Object){

  xlimy<-c(-100,40)
  ylimy<-c(-25,65)
  plot(xlimy,ylimy,col="white",axes=F,xlab="",ylab="")

  abline(v=(-18:18)*10,col='light grey',lwd=0.8)
  abline(h=(-18:18)*10,col='light grey',lwd=0.8)


  map(xlim=xlimy+c(-5,5),ylim=ylimy+c(-5,5),add=T,fill=T,col="light grey")
  map(xlim=xlimy,ylim=ylimy,add=T,fill=F,col="light grey",lwd=4)
   for(i in 1:length(.Object@areanams)){
    polygon(.Object@area_defs[[i]],border='red')
    text(mean(.Object@area_defs[[i]]$x),mean(.Object@area_defs[[i]]$y),.Object@areanams[i],col='RED',font=2,cex=0.8)
  }
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

