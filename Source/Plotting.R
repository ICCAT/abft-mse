# ======================================================================================================================
# ==== ABT MSE Plotting code ====================================================================================================
# ======================================================================================================================

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
  par(mfcol=c(Base@nr,Base@ns),mai=c(0.25,0.2,0.2,0.05),omi=c(0.5,1,0.3,0.01))
  ylimy<-c(0,quantile(pCPUE,0.995,na.rm=T))
  
  for(q in 1:Base@ns){for(a in 1:Base@nr){
    plot(Base@years[1]:Base@years[2],pCPUE[,q,a],ylim=ylimy,type='l',col='red',axes=F)
    if(a==1)mtext(paste("Quarter",q),3,line=2)
    if(q==1)mtext(anam[a],2,line=3,las=1)
    if(q==4){axis(1,prettyaxis(years),prettyaxis(years))
    }else{axis(1,prettyaxis(years))}
    if(a==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
    }else{axis(2,prettyaxis(ylimy))}
    
  }}
  mtext("Year",1,line=1.5,outer=T)
}
