# ======================================================================================================================
# ==== ABT MSE Plotting code ====================================================================================================
# ======================================================================================================================

plotindex3<-function(OMI){
  nr<-OMI@nr
  ny<-OMI@ny
  ns<-OMI@ns
  
  yrs<-1959+(1:ny)
  RAIo<-OMI@RAI
  RAIo<-RAIo/mean(RAIo) 

  par(mfcol=c(nr,ns),mai=c(0.05,0.15,0.01,0.01),omi=c(0.4,0.5,0.2,0.05))
  ylim=c(0,max(RAIo,na.rm=T))
  ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))

  subyrnams<-c("Jan-Mar","Apr-Jun","Jul-Sept","Oct-Dec")
  
  for(s in 1:ns){
  
    for(r in 1:nr){
    
      plot(yrs,RAIo[,s,r],col="blue",type='l',ylim=ylim,axes=F)
        
      if(r==nr)axis(1,xs,xs)
      if(r<nr)axis(1,xs,NA)
      if(s==1)axis(2,ys,ys)
      if(s<(ns+1))axis(2,ys,NA)
      if(s==1)mtext(OMI@areanams[r],2,line=2.75)
      if(r==1)mtext(subyrnams[s],3,line=-0.5)
      #if(r==1&s==ns)legend('topright',legend=popnams,text.font=2,text.col=pcols[1:np],bty='n')
    
    }
  
  }
  
  mtext("Year",1,outer=T,line=2)
  #mtext("Predicted (line) versus observed (point) relative abundance of Atlantic bluefin",3,line=1.5,outer=T)

}

plotindex2<-function(OMI,OMI2){
  nr<-OMI@nr
  ny<-OMI@ny
  ns<-OMI@ns
  
  yrs<-1959+(1:ny)
  RAIo<-OMI@RAI
  RAIo<-RAIo/mean(RAIo) 
  
  RAIo2<-OMI2@RAI
  RAIo2<-RAIo2/mean(RAIo2) 
  
  par(mfcol=c(nr,ns),mai=c(0.05,0.15,0.01,0.01),omi=c(0.4,0.5,0.2,0.05))
  ylim=c(0,max(RAIo,RAIo2,na.rm=T))
  ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))
  
  subyrnams<-c("Jan-Mar","Apr-Jun","Jul-Sept","Oct-Dec")
  
  for(s in 1:ns){
    
    for(r in 1:nr){
      
      plot(yrs,RAIo[,s,r],col="#0000ff95",type='l',lwd=1.5,ylim=ylim,axes=F)
      lines(yrs,RAIo2[,s,r],col="#ff000095",type='l',lwd=1.5)
      
      if(r==nr)axis(1,xs,xs)
      if(r<nr)axis(1,xs,NA)
      if(s==1)axis(2,ys,ys)
      if(s<(ns+1))axis(2,ys,NA)
      if(s==1)mtext(OMI@areanams[r],2,line=2.75)
      if(r==1)mtext(subyrnams[s],3,line=-0.5)
      if(r==1&s==ns)legend('topright',legend=c("Index 1","Index 2"),text.font=2,text.col=c("red","blue"),bty='n')
      
    }
    
  }
  
  mtext("Year",1,outer=T,line=2)
  #mtext("Predicted (line) versus observed (point) relative abundance of Atlantic bluefin",3,line=1.5,outer=T)
  
}


plotareas<-function(OMI){
  
  par(mai=c(0.9,0.7,0.01,0.01),omi=rep(0.02,4))
  
  ylim=range(sapply(OMI@area_defs,function(x)range(x$y)))
  xlim=range(sapply(OMI@area_defs,function(x)range(x$x)))
  
  adj<-rep(0,OMI@nr)
  adj[c(2,3,7,10)]<-c(-5,5,7,-5)

  plot(xlim,ylim,col="white",xlab="",ylab="")
  abline(h=c(-100:100)*10,col="light grey")
  abline(v=c(-100:100)*10,col="light grey")
  map(xlim=xlim,ylim=ylim,add=T,fill=T,col="light grey")
  map(xlim=xlim,ylim=ylim,add=T,fill=F,col="light grey",lwd=3)
  
  for(i in 1:OMI@nr){
    polygon(OMI@area_defs[[i]])
    text(mean(OMI@area_defs[[i]]$x),mean(OMI@area_defs[[i]]$y)+adj[i],paste0(i,". ",OMI@areanams[i]),col='red',font=2,cex=0.8)         
  }
  
  mtext(expression(paste("Longitude ",~degree~W,sep="")),side=1,line=2.5,outer=F,font=2,cex=1)
  mtext(expression(paste("Latitude ",~degree~N,sep="")),side=2,line=2.5,outer=F,font=2,cex=1)

}


setMethod("plot",
  signature(x = "MSE"),
  function(x){
    MSEobj<-x
    # Plot the trajectories of F/FMSY and B/BMSY    ============================================
    nr<-2
    nc<-MSEobj@nMPs
    MSEcols<-c('red','green','blue','orange','brown','purple','dark grey','violet','dark red','pink','dark blue','grey')
    allyears<-MSEobj@nyears+MSEobj@proyears
    prjd<-(MSEobj@nyears+1):(MSEobj@nyears+MSEobj@proyears)
    x11(width=nc*3,height=nr*3)
    par(mfcol=c(nr,nc),mai=c(0.2,0.3,0.2,0.01),omi=c(0.4,0.4,0.1,0.05))
    lwdy<-2.5
    FMSYr<-quantile(MSEobj@F_FMSY,c(0.001,0.975),na.rm=T)
    BMSYr<-quantile(MSEobj@B_BMSY,c(0.001,0.975),na.rm=T)
            
    colsse<-rainbow(100,start=0,end=0.36)[1:100]
    colB<-rep(colsse[100],ceiling(BMSYr[2]*100))
    colB[1:100]<-colsse
    colB<-makeTransparent(colB,60)
    colsse<-rainbow(200,start=0,end=0.36)[200:1]
    colF<-rep(colsse[200],ceiling(FMSYr[2]*100))
    colF[1:200]<-colsse
    colF<-makeTransparent(colF,60)
            
    Yd<-rep(NA,MSEobj@nMPs)
    P10<-rep(NA,MSEobj@nMPs)
    P50<-rep(NA,MSEobj@nMPs)
    P100<-rep(NA,MSEobj@nMPs)
    POF<-rep(NA,MSEobj@nMPs)
    yind<-max(allyears-4,1):allyears

    for(mm in 1:MSEobj@nMPs){
      Yd[mm]<-round(mean(MSEobj@C[mm,,MSEobj@targpop,yind],1))
      POF[mm]<-round(sum(MSEobj@F_FMSY[mm,,]>1,na.rm=T)/prod(dim(MSEobj@F_FMSY[mm,,]),na.rm=T)*100,1)
      plot(MSEobj@F_FMSY[mm,1,],ylim=FMSYr,col=colF[mean(MSEobj@F_FMSY[mm,1,],na.rm=T)*100],type='l',lwd=lwdy)
      for(i in 1:MSEobj@nsim)lines(MSEobj@F_FMSY[mm,i,],col=colF[mean(MSEobj@F_FMSY[mm,i,prjd],na.rm=T)*100],lwd=lwdy)
      abline(h=100,col="grey",lwd=3)
      mtext(MSEobj@MPs[mm],3,outer=F,line=1)
      legend('topright',c(paste(POF[mm],"% POF",sep=""),paste(Yd[mm],"% FMSY yield",sep="")),bty='n')
      if(mm==1)mtext("F/FMSY",2,line=2.5,outer=F)
                
      P10[mm]<-round(sum(MSEobj@B_BMSY[,mm,]<0.1,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
      P50[mm]<-round(sum(MSEobj@B_BMSY[,mm,]<0.5,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
      P100[mm]<-round(sum(MSEobj@B_BMSY[,mm,]<1,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
              
      plot(MSEobj@B_BMSY[mm,1,],ylim=BMSYr,col=colB[MSEobj@B_BMSY[mm,1,allyears]*100],type='l',lwd=lwdy)
      for(i in 1:MSEobj@nsim)lines(MSEobj@B_BMSY[mm,i,],col=colB[MSEobj@B_BMSY[mm,i,allyears]*100],lwd=lwdy)
      abline(h=100,col="grey",lwd=3)
              #legend('topleft',MSEobj@meths[mm],bty='n')
      legend('topright',c(paste(P100[mm],"% < BMSY",sep=""),
                          paste(P50[mm],"% < 0.5BMSY",sep=""),
                          paste(P10[mm],"% < 0.1BMSY",sep="")),bty='n')
             
      if(mm==1)mtext("B/BMSY",2,line=2.5,outer=F)
              
   }
            
  mtext("Projection year",1,outer=T,line=1.2)
  
  nr<-floor((MSEobj@nMPs)^0.5)
  nc<-ceiling((MSEobj@nMPs)/nr)
            
  # KOBE plots ==============================================================================
            
  x11(width=nc*3,height=nr*3.6)
  par(mfrow=c(nr,nc),mai=c(0.2,0.3,0.4,0.01),omi=c(0.45,0.3,0.01,0.01))
            
  colsse<-rainbow(MSEobj@proyears,start=0.57,end=0.7)[1:MSEobj@proyears]
  colsse<-makeTransparent(colsse,90)
             
  for(mm in 1:MSEobj@nMPs){
    plot(c(MSEobj@B_BMSY[mm,1,1],MSEobj@B_BMSY[mm,1,2]),
         c(MSEobj@F_FMSY[mm,1,1],MSEobj@F_FMSY[mm,1,2]),xlim=BMSYr,ylim=FMSYr,
          col=colsse[1],type='l')
              
    OO<-round(sum(MSEobj@B_BMSY[mm,,allyears]<1&MSEobj@F_FMSY[mm,,allyears]>1,na.rm=T)/MSEobj@nsim*100,1)
    OU<-round(sum(MSEobj@B_BMSY[mm,,allyears]>1&MSEobj@F_FMSY[mm,,allyears]>1,na.rm=T)/MSEobj@nsim*100,1)
    UO<-round(sum(MSEobj@B_BMSY[mm,,allyears]<1&MSEobj@F_FMSY[mm,,allyears]<1,na.rm=T)/MSEobj@nsim*100,1)
    UU<-round(sum(MSEobj@B_BMSY[mm,,allyears]>1&MSEobj@F_FMSY[mm,,allyears]<1,na.rm=T)/MSEobj@nsim*100,1)
              
    polygon(c(1,-1000,-1000,1),c(1,1,1000,1000),col="orange",border="orange")
    polygon(c(1,1000,1000,1),c(1,1,1000,1000),col="yellow",border="yellow")
    polygon(c(1,-1000,-1000,1),c(1,1,-1000,-1000),col="yellow",border="yellow")
    polygon(c(1,1000,1000,1),c(1,1,-1000,-1000),col="green",border="green")
              
    abline(h=1,col="grey",lwd=3)
    abline(v=1,col="grey",lwd=3)
               
    for(i in 1:MSEobj@nsim){
      for(y in 1:(MSEobj@proyears-1)){
        lines(c(MSEobj@B_BMSY[mm,i,y],MSEobj@B_BMSY[mm,i,y+1]),
              c(MSEobj@F_FMSY[mm,i,y],MSEobj@F_FMSY[mm,i,y+1]),
              col=colsse[y],lwd=1.5)
      }
    }
              
    points(MSEobj@B_BMSY[mm,i,1],MSEobj@F_FMSY[mm,i,1],pch=19,cex=0.8,col=colsse[1])
    points(MSEobj@B_BMSY[mm,i,allyears],MSEobj@F_FMSY[mm,i,allyears],pch=19,cex=0.8,col=colsse[allyears])
              
    if(mm==1)legend('right',c("Start","End"),bty='n',text.col=c(colsse[1],colsse[MSEobj@proyears]),pch=19,col=c(colsse[1],colsse[MSEobj@proyears]))
    legend('topleft',paste(OO,"%",sep=""),bty='n',text.font=2)
    legend('topright',paste(OU,"%",sep=""),bty='n',text.font=2)
    legend('bottomleft',paste(UO,"%",sep=""),bty='n',text.font=2)
    legend('bottomright',paste(UU,"%",sep=""),bty='n',text.font=2)
              
    mtext(MSEobj@MPs[mm],3,line=0.45)
  }
  
  mtext("B/BMSY",1,outer=T,line=1.4)
  mtext("F/FMSY",2,outer=T,line=0.2)
    
})  # End of method plot for object class MSE  

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

plotindex<-function(Base,pCPUE){
  
  anam<-Base@areanams
  par(mfcol=c(Base@nr,Base@ns),mai=c(0.2,0.2,0.1,0.05),omi=c(0.5,1,0.25,0.01))
  ylimy<-c(0,quantile(pCPUE,0.995,na.rm=T))
  
  for(q in 1:Base@ns){for(a in 1:Base@nr){
    plot(Base@years[1]:Base@years[2],pCPUE[,q,a],ylim=ylimy,type='l',col='red',axes=F)
    if(a==1)mtext(paste("Quarter",q),3,line=1)
    if(q==1)mtext(anam[a],2,line=3,las=1)
    if(q==4){axis(1,prettyaxis(years),prettyaxis(years))
    }else{axis(1,prettyaxis(years))}
    if(a==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
    }else{axis(2,prettyaxis(ylimy))}
    
  }}
  mtext("Year",1,line=1.5,outer=T)
}

plot2index<-function(Base,pCPUE,pCPUE2,legend=c("Master","Alternative")){
  
  anam<-Base@areanams
  par(mfcol=c(Base@nr,Base@ns),mai=c(0.2,0.2,0.1,0.05),omi=c(0.5,1,0.25,0.01))
  ylimy<-c(0,quantile(pCPUE,0.995,na.rm=T))
  
  for(q in 1:Base@ns){for(a in 1:Base@nr){
    plot(Base@years[1]:Base@years[2],pCPUE[,q,a],ylim=ylimy,type='l',col='#ff000095',axes=F)
    lines(Base@years[1]:Base@years[2],pCPUE2[,q,a],ylim=ylimy,type='l',col='#0000ff95')
    
    if(a==1)mtext(paste("Quarter",q),3,line=1)
    if(q==1)mtext(anam[a],2,line=3,las=1)
    if(q==4){axis(1,prettyaxis(years),prettyaxis(years))
    }else{axis(1,prettyaxis(years))}
    if(a==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
    }else{axis(2,prettyaxis(ylimy))}
    if(a==1&q==4)legend('topright',legend=legend,text.col=c('red','blue'),bty='n')
  }}
  mtext("Year",1,line=1.5,outer=T)

}

plotMindex<-function(Base,pCPUE,legend=NA,lwd=1.5){
  if(is.na(legend)) legend=c("Master",paste("Alt",1:(length(pCPUE)-1)))
  anam<-Base@areanams
  par(mfcol=c(Base@nr,Base@ns),mai=c(0.2,0.2,0.1,0.05),omi=c(0.5,1,0.25,0.01))
  ylimy<-c(0,quantile(pCPUE[[1]],0.995,na.rm=T))
  
  cols<-c("#ff000095","#0000ff95","#00ff0095","#99999995")
  
  for(q in 1:Base@ns){for(a in 1:Base@nr){
    plot(Base@years[1]:Base@years[2],pCPUE[[1]][,q,a],ylim=ylimy,type='l',col=cols[1],axes=F,lwd=lwd)
    for(ii in 2:length(pCPUE))lines(Base@years[1]:Base@years[2],pCPUE[[ii]][,q,a],ylim=ylimy,type='l',col=cols[ii],lwd=lwd)
    
    if(a==1)mtext(paste("Quarter",q),3,line=1)
    if(q==1)mtext(anam[a],2,line=3,las=1)
    if(q==4){axis(1,prettyaxis(years),prettyaxis(years))
    }else{axis(1,prettyaxis(years))}
    if(a==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
    }else{axis(2,prettyaxis(ylimy))}
    if(a==1&q==4)legend('topright',legend=legend,text.col=cols,cex=0.9,bty='n',text.font=2)
  }}
  mtext("Year",1,line=1.5,outer=T)
  
}

