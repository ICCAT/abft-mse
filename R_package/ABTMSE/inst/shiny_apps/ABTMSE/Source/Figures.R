# Zeh plots
custombar<-function(dat,MPnams,tickwd1=0.085,tickwd2=0.045,lwd1=3,lwd2=1,xlab=T,ylim=NA,ynam="",cols=NULL,add=F){
  incp<-0
  pch<-19
  if(add){
    incp<-0.3
    pch<-3
  }
  if(is.null(cols))cols=rep('black',dim(dat)[1])
  if(!add)par(mar=c(10,4,0.5,0.5))
  nMPer<-nrow(dat)
 #saveRDS(dat, "C:/temp/dat.rds")
  if(is.na(ylim[1]))ylim=range(dat)
  incr<-(ylim[2]-ylim[1])*0.05
  if(!add){
    plot(1:nMPer,dat[,5],xlim=c(0.5,nMPer+0.5),ylim=c(0,ylim[2]),col='white',axes=F,ylab="",xlab="",xaxs="i")
  }
  if(!add){
    if(xlab){
      axis(1,1:nMPer,rep("",nMPer),las=2,font=2,cex.axis=0.9)
      Map(axis, side=1, at=1:nMPer, col.axis=cols, labels=MPnams, lwd=0, las=2)
      #axis(1,at=1:nMPer,labels=FALSE)
    }else{
      axis(1,1:nMPer,rep("",nMPer))
    }
  }

  if(!add){
    yp<-pretty(seq(0,ylim[2]+2*incr,length.out=12))
    axis(2,yp,yp,las=2)
    big<-1E20
    polygon(c(-big,big,big,-big),c(-big,-big,big,big),col='grey96')
    abline(h=yp,col='white',lwd=2)
    abline(h=0,col="darkgrey",lwd=2)
  }

  points((1:length(dat[,3]))+incp,dat[,3],pch=pch,cex=1.1,col=cols)

  for(i in 1:nMPer){

    lines(c(i-tickwd1/2,i+tickwd1/2)+incp,c(dat[i,2],dat[i,2]),lwd=lwd1,col=cols[i]) # lower interquartile
    lines(c(i-tickwd1/2,i+tickwd1/2)+incp,c(dat[i,4],dat[i,4]),lwd=lwd1,col=cols[i]) # upper interquartile

    lines(c(i-tickwd2/2,i+tickwd2/2)+incp,c(dat[i,1],dat[i,1]),lwd=lwd1,col=cols[i]) # lower 80%
    lines(c(i-tickwd2/2,i+tickwd2/2)+incp,c(dat[i,5],dat[i,5]),lwd=lwd2,col=cols[i]) # upper 80%

    lines(c(i,i)+incp,c(dat[i,1],dat[i,5]),lwd=lwd2,col=cols[i]) # 80%
    lines(c(i,i)+incp,c(dat[i,2],dat[i,4]),lwd=lwd1,col=cols[i]) # 80%

  }

  if(!add)mtext(ynam,2,line=2)

}
#renderPlot(ZehP(MET,MPnames,1,1),height=Zeh_sz*0.8)
# ind<-1:96; pp<-1; Ino=1; PMind<-1; MPind<-1:29
ZehP<-function(MET,Pnam,pp,Ino){

 # temp<-changed()

  PMind<<-match(input$Zeh_PM,pnames)
  if(Ino==1)ind<<-getind1()
  if(Ino==2)ind<<-getind2()
  MPind<-getMPind()
  #if(input$zeh_violin)MPind<-MPind[MPind!=1]
  MET_s<-MET[,,,MPind,]
  res<-MET_s[,ind,pp,,PMind]
  #ylim=range(c(MET_s[,ind,pp,,PMind],MET_s[,ind,pp,,PMind]))
  ndim<-length(dim(res))

  if(input$zeh_violin==0){
    ylim=range(c(MET_s[,getind1(),pp,,PMind],MET_s[,getind2(),pp,,PMind]))
    if(ndim==2)store<-apply(res,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
    if(ndim==3)store<-apply(res,3,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
    custombar(dat=t(store),MPnams=MPnames[MPind],ylim=ylim,ynam=input$Zeh_PM)
  }else{

    if(ndim==3)res0<-array(res,c(dim(res)[1]*dim(res)[2],dim(res)[3]))
    if(ndim==2)res0<-res
    df<-data.frame(y=as.vector(res0),x=rep(MPnames[MPind],each=dim(res0)[1]))
    df<-subset(df,df$x!="ZeroC")
    p<-ggplot(df, aes(x=x, y=y, fill=x)) +  geom_violin(width=1.4)+ scale_fill_viridis(discrete = TRUE) +geom_boxplot(width=0.1, color="black", alpha=0.2)+  theme(legend.position="none",plot.title = element_text(size=11))+xlab("")+ylab("")
    print(p)

  }
  # mtext("Candidate Management Procedure",1,line=6.8,font=2,outer=T)

}


ZehOM<-function(MET,Pnam,pp){

  temp<-changed()

  PMind<<-match(input$Zeh_PM1,pnames)
  MPind1<-match(input$Zeh_MP1,MPnames)
  addMP2<-input$Zeh_MP2!="None"
  if(addMP2)MPind2<-match(input$Zeh_MP2,MPnames)
  ZehFac<-as.numeric(input$Zeh_FacCol)
  ind<<-getind1()

  if(ZehFac==0){ # no factor coloring
    AllCols<-c(rep('black',length(OMcode)),rep('darkgrey',length(ROMcode)))
  }else{ # factor coloring
    ZehCols<-c('Black','Red','Green','Blue')
    #saveRDS(ZehFac,"C:/temp/zehfac.rda")
    AllCols<-c(ZehCols[match(OMgrid[,ZehFac],unique(OMgrid[,ZehFac]))],rep('darkgrey',length(ROMcode)))
  }

  cols<-AllCols[ind]
  res<-MET[,ind,pp,MPind1,PMind]
  ndim<-length(dim(res))

  if(ndim==2){
    store<-apply(res,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
  }else{
    store<-matrix(quantile(res,c(0.05,0.25,0.5,0.75,0.95)),ncol=1)
  }
  custombar(dat=t(store),MPnams=c(OMcode,ROMcode)[ind],ynam=input$Zeh_PM1,cols=cols)


  if(addMP2){

    if(ZehFac==0){ # no factor coloring
      AllCols<-c(rep('red',length(OMcode)),rep('darkred',length(ROMcode)))
    }else{ # factor coloring
      ZehCols<-c('Black','Red','Green','Blue')
      #saveRDS(ZehFac,"C:/temp/zehfac.rda")
      AllCols<-c(ZehCols[match(OMgrid[,ZehFac],unique(OMgrid[,ZehFac]))],rep('darkgrey',length(ROMcode)))
    }

    cols<-AllCols[ind]
    res<-MET[,ind,pp,MPind2,PMind]
    ndim<-length(dim(res))

    if(ndim==2){
      store<-apply(res,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
    }else{
      store<-matrix(quantile(res,c(0.05,0.25,0.5,0.75,0.95)),ncol=1)
    }
    custombar(dat=t(store),MPnams=c(OMcode,ROMcode)[ind],ynam=input$Zeh_PM1,cols=cols,add=T)
    labcol=rep('black',2)
    if(ZehFac==0)labcol=c("black","red")
    legend('topleft',legend=c(input$Zeh_MP1,input$Zeh_MP2),text.col=labcol,col=labcol,pch=c(19,3))
  }

  mtext("Operating model",1,line=5.5,font=1)

}


#    MET<-CompRes$MET; Pnam<-CompRes$pnames; MPnames<-CompRes$MPnames; pp<-1 ; Ino<-1; PMind<-c(1,4,length(Pnam))
#    MPind<-1:3 ;ind<-rep(T,length(CompRes$OMnames)) ;ylim<-range(MET); ZehPMnams<-Pnam[PMind]
# input <- list(zehPM_violin=T)
ZehPM<-function(MET,Pnam,pp,Ino){

  #temp<-changed()

  ZehPMnams<-c(input$ZehPM_PM1,input$ZehPM_PM2,input$ZehPM_PM3)
  PMind<-c(match(ZehPMnams,pnames))

  par(mfrow=c(1,3))
  if(Ino==1)ind<<-getind1()
  if(Ino==2)ind<<-getind2()
  MPind<-c(match(input$ZehPM_MP1,MPnames),match(input$ZehPM_MP2,MPnames),match(input$ZehPM_MP3,MPnames))
  MET_s<-MET[,,,MPind,]

  plist<-list()
  for(i in 1:3){

    res<-MET_s[,ind,pp,,PMind[i]]

    ylim=range(c(MET_s[,getind1(),pp,,PMind[i]],MET_s[,getind2(),pp,,PMind[i]]))
    ndim<-length(dim(res))

    if(input$zehPM_violin==0){

      if(ndim==2)store<-apply(res,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
      if(ndim==3)store<-apply(res,3,quantile,p=c(0.05,0.25,0.5,0.75,0.95))

      custombar(dat=t(store),MPnams=MPnames[MPind],ylim=ylim,ynam=ZehPMnams[i],cols=MPcols[1:3])

    }else{
      #plot(1)
      if(ndim==3)res0<-array(res,c(dim(res)[1]*dim(res)[2],dim(res)[3]))
      if(ndim==2)res0<-res
      df<-data.frame(y=as.vector(res0),x=rep(MPnames[MPind],each=dim(res0)[1]))

      df<-subset(df,df$x!="ZeroC")
      plist[[i]]<-ggplot(df, aes(x=x, y=y, fill=x)) + geom_violin() + scale_fill_viridis(discrete = TRUE) + geom_boxplot(width=0.1, color="black", alpha=0.2)+ theme(legend.position="none",plot.title = element_text(size=11))+xlab("")+ylab(ZehPMnams[i])

    }

  }
  # mtext("Candidate Management Procedure",1,line=6.8,font=2,outer=T)

  if(input$zehPM_violin==1)grid.arrange(plist[[1]],plist[[2]],plist[[3]], nrow = 1)

}


# Trade-off plot
Tplot<-function(x1=1:10,y1=1:10,x2=1:10,y2=1:10,tabno=1,xlab="xlab",ylab="ylab",MPnames=as.character(1:10),bars=F,MPlabs=F){

  temp<-changed()

  par(mai=c(0.8,1,0.01,0.03))


  yinc<-(max(y1,y2)-min(y1,y2))/20
  if(bars)ylim<-range(y1,y2)+c(-yinc,yinc)/3
  if(!bars)ylim<-range(y1[2,],y2[2,])+c(-yinc,yinc)/3
  xinc<-(max(x1,x2)-min(x1,x2))/20
  if(bars)xlim=range(x1,x2)+c(-xinc,xinc)
  if(!bars)xlim=range(x1[2,],x2[2,])+c(-xinc,xinc)

  yls<-pretty(seq(ylim[1]-2*yinc,ylim[2]+2*yinc,length.out=20))
  xls<-pretty(seq(xlim[1]-2*xinc,xlim[2]+2*xinc,length.out=20))

  if(tabno==1){x=x1;y=y1}
  if(tabno==2){x=x2;y=y2}

  if(!MPlabs){
    plot(x[2,],y[2,],col="white",xlab="",ylab="",xlim=xlim,ylim=ylim)
    abline(h=yls,col="lightgrey")
    abline(v=xls,col="lightgrey")
    abline(h=0,col="darkgrey",lwd=2)
    abline(v=0,col="darkgrey",lwd=2)
    points(x[2,],y[2,],col=MPcols,cex=1.3,pch=19,)
  }else{
    plot(x[2,],y[2,],col="white",xlab="",ylab="",xlim=xlim,ylim=ylim)
    abline(h=yls,col="lightgrey")
    abline(v=xls,col="lightgrey")
    abline(h=0,col="darkgrey",lwd=2)
    abline(v=0,col="darkgrey",lwd=2)
    text(x[2,],y[2,],MPnames,col=MPcols,font=2)
  }

  mtext(input$T_PMx,1,line=1.9)
  mtext(input$T_PMy,2,line=1.9)

  if(bars){
    for(MP in 1:ncol(x)){
      lines(c(x[1,MP],x[3,MP]),rep(y[2,MP],2),col=MPcols[MP])
      lines(rep(x[2,MP],2),c(y[1,MP],y[3,MP]),col=MPcols[MP])
    }
  }
}

Tleg<-function(){

  par(mai=rep(0.01,4))
  plot(1,1,col='white',xlab="",ylab="",axes=F)
  MPind<-getMPind()
  if(!input$labs)legend('center',legend=MPnames[MPind],text.col= MPcols,bty='n')
}

Twrap<-function(pp,tabno){

  PMx<-match(input$T_PMx,pnames)
  PMy<-match(input$T_PMy,pnames)
  ind1<-getind1()
  ind2<-getind2()
  MPind<-getMPind()
  MET_s<-MET[,,,MPind,]
  datx1<-MET_s[,ind1,pp,,PMx]
  daty1<-MET_s[,ind1,pp,,PMy]
  datx2<-MET_s[,ind2,pp,,PMx]
  daty2<-MET_s[,ind2,pp,,PMy]

  if(length(dim(datx1))==3){
    x1<-apply(datx1,3,quantile,p=c(0.05,0.5,0.95))
    y1<-apply(daty1,3,quantile,p=c(0.05,0.5,0.95))
  }else{
    x1<-apply(datx1,2,quantile,p=c(0.05,0.5,0.95))
    y1<-apply(daty1,2,quantile,p=c(0.05,0.5,0.95))
  }

  if(length(dim(datx2))==3){
    x2<-apply(datx2,3,quantile,p=c(0.05,0.5,0.95))
    y2<-apply(daty2,3,quantile,p=c(0.05,0.5,0.95))
  }else{
    x2<-apply(datx2,2,quantile,p=c(0.05,0.5,0.95))
    y2<-apply(daty2,2,quantile,p=c(0.05,0.5,0.95))
  }

  Tplot(x1,y1,x2,y2,tabno,xlab=input$T_PMx,ylab=input$T_PMy,MPnames=MPnames[MPind],bars=input$bars,MPlabs=input$labs)
  #Tplot(x,y,xlab="",ylab="",MPnames=1:10,bars=F,MPlabs=F)
}


PROplot<-function(dat,MPnams,ylim,ynam){

  yinc<-(ylim[2]-ylim[1])/10
  yls<-pretty(seq(ylim[1]-yinc,ylim[2]+yinc,length.out=14))
  xls<-seq(1900,2100,20)
  matplot(Syear+(1:ncol(dat))-1,t(dat),type="l",col=MPcols,ylim=ylim+c(0,yinc),ylab="",xlab="",yaxs="i")
  abline(h=yls,col='lightgrey')
  abline(v=xls,col='lightgrey')
  matplot(Syear+(1:ncol(dat))-1,t(dat),type="l",col=MPcols,add=T,lty=1)
  mtext(ynam,3,line=0.1,cex=0.8)

}


YBP<-function(pp,Ino,leg=F){

  temp<-changed()

  ind1<-getind1()
  ind2<-getind2()
  if(Ino==1)ind<-ind1
  if(Ino==2)ind<-ind2
  MPind<-getMPind()
  quant<-input$YB_quant

  par(mfrow=c(1,2),mai=c(0.5,0.5,0.3,0.03))

  res<-CompRes$CWa[,ind,pp,MPind,]/1E6
  ndim<-length(dim(res))

  res1<-CompRes$CWa[,ind1,pp,MPind,]/1E6
  res2<-CompRes$CWa[,ind2,pp,MPind,]/1E6
  ndim1<-length(dim(res1))
  ndim2<-length(dim(res2))

  #saveRDS(MPind,"C:/temp/MPind.rda")

  #saveRDS(resA,"C:/temp/resA.rda")
  #saveRDS(quants,"C:/temp/quants.rda")

  if(ndim1==3)ymax<-max(apply(res1,2:3,quantile,p=quant/100))
  if(ndim1==4)ymax<-max(apply(res1,3:4,quantile,p=quant/100))
  if(ndim2==3)ymax<-max(ymax,max(apply(res2,2:3,quantile,p=quant/100)))
  if(ndim2==4)ymax<-max(ymax,max(apply(res2,3:4,quantile,p=quant/100)))
  ylim<-c(0,ymax)

  if(ndim==3)store<-apply(res,2:3,quantile,p=quant/100)
  if(ndim==4)store<-apply(res,3:4,quantile,p=quant/100)
  PROplot(store,MPnams=MPnames[MPind],ylim=ylim,ynam="Catch by Area (1000t)")

  res<-CompRes$B_BMSY[,ind,pp,MPind,]
  res1<-CompRes$B_BMSY[,ind1,pp,MPind,]
  res2<-CompRes$B_BMSY[,ind2,pp,MPind,]
  ndim1<-length(dim(res1))
  ndim2<-length(dim(res2))

  if(ndim1==3)ymax<-max(apply(res1,2:3,quantile,p=quant/100))
  if(ndim1==4)ymax<-max(apply(res1,3:4,quantile,p=quant/100))
  if(ndim2==3)ymax<-max(ymax,max(apply(res2,2:3,quantile,p=quant/100)))
  if(ndim2==4)ymax<-max(ymax,max(apply(res2,3:4,quantile,p=quant/100)))
  ylim<-c(0,ymax)

  if(ndim==3)store<-apply(res,2:3,quantile,p=quant/100)
  if(ndim==4)store<-apply(res,3:4,quantile,p=quant/100)
  PROplot(store,MPnams=MPnames[MPind],ylim=ylim,ynam="SSB/SSBMSY dynamic")
  if(leg)legend('top',MPnames[MPind],text.col=MPcols,bty='n')

  #res<-CompRes$F_FMSY[,ind,pp,MPind,]
  #resA<-CompRes$F_FMSY[,unique(c(ind1,ind2)),pp,MPind,]
  #if(ndimA==3)ylim=c(0,max(apply(resA,2:3,quantile,p=quant/100)))
  #if(ndimA==4)ylim=c(0,max(apply(resA,3:4,quantile,p=quant/100)))
  #if(ndim==3)store<-apply(res,2:3,quantile,p=quant/100)
  #if(ndim==4)store<-apply(res,3:4,quantile,p=quant/100)
  #PROplot(store,MPnams=MPnames[MPind],ylim=ylim,ynam="U/UMSY2016 (catch/vuln.bio.)")

}

# Stochastic projection


PROplot_s<-function(dat,MPnams,ylim,ynam){

  makeTrans<-function(someColor, alpha=100){
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }

  cols<-makeTrans(MPcols[1:3])
  nyrs<-dim(dat)[3]

  yrs<-Syear+(1:nyrs)-1

  yinc<-(ylim[2]-ylim[1])/10
  yls<-pretty(seq(ylim[1]-yinc,ylim[2]+yinc,length.out=14))
  xls<-seq(1900,2100,20)
  plot(Syear+c(1,nyrs)-1,ylim,type="l",col="white",ylim=ylim+c(0,yinc),ylab="",xlab="",yaxs="i")
  abline(h=yls,col='lightgrey')
  abline(v=xls,col='lightgrey')

  for(i in dim(dat)[2]:1){

    polygon(c(yrs,yrs[nyrs:1]),c(dat[1,i,],dat[2,i,nyrs:1]),col=cols[i],border=MPcols[i])

  }

  mtext(ynam,3,line=0.1,cex=0.8)

}

debugsetup<-function(){
  CompRes<-readRDS("C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/inst/shiny_apps/ABTMSE/data/CompRes.rda")
  ind<-ind1<-ind2<-c(T,F,T,rep(F,length(CompRes$OMnames)-3))
  ind<-ind1<-ind2<-c(F,T,T,rep(F,length(CompRes$OMnames)-3))
  ind<-ind1<-ind2<-rep(T,length(CompRes$OMnames)-3)
  MPind<-1:3
  pp<-2
  input<-list(StochIQR=90,PT2_IQR=90)
  Syear<-1965
}

StochP<-function(pp,Ino,leg=F){

  temp<-changed()
  ind1<-getind1()
  ind2<-getind2()
  if(Ino==1)ind<-ind1
  if(Ino==2)ind<-ind2
  MPind<-match(c(input$S_MP1,input$S_MP2,input$S_MP3),MPnames)
  quants<-0.5+c(-input$StochIQR,input$StochIQR)/200

  par(mfrow=c(1,2),mai=c(0.5,0.5,0.3,0.03))

  res<-CompRes$CWa[,ind,pp,MPind,]/1E6
  ndim<-length(dim(res))
  res1<-CompRes$CWa[,ind1,pp,MPind,]/1E6
  res2<-CompRes$CWa[,ind2,pp,MPind,]/1E6
  ndim1<-length(dim(res1))
  ndim2<-length(dim(res2))

  if(ndim1==3)ymax<-max(apply(res1,2:3,quantile,p=quants))
  if(ndim1==4)ymax<-max(apply(res1,3:4,quantile,p=quants))
  if(ndim2==3)ymax<-max(ymax,max(apply(res2,2:3,quantile,p=quants)))
  if(ndim2==4)ymax<-max(ymax,max(apply(res2,3:4,quantile,p=quants)))
  ylim<-c(0,ymax)

  if(ndim==3)store<-apply(res,2:3,quantile,p=quants)
  if(ndim==4)store<-apply(res,3:4,quantile,p=quants)
  PROplot_s(store,MPnams=MPnames[MPind],ylim=ylim,ynam="Catch by Area (1000t)")

  res<-CompRes$B_BMSY[,ind,pp,MPind,]
  res1<-CompRes$B_BMSY[,ind1,pp,MPind,]
  res2<-CompRes$B_BMSY[,ind2,pp,MPind,]
  ndim1<-length(dim(res1))
  ndim2<-length(dim(res2))

  if(ndim1==3)ymax<-max(apply(res1,2:3,quantile,p=quants))
  if(ndim1==4)ymax<-max(apply(res1,3:4,quantile,p=quants))
  if(ndim2==3)ymax<-max(ymax,max(apply(res2,2:3,quantile,p=quants)))
  if(ndim2==4)ymax<-max(ymax,max(apply(res2,3:4,quantile,p=quants)))
  ylim<-c(0,ymax)

  if(ndim==3)store<-apply(res,2:3,quantile,p=quants)
  if(ndim==4)store<-apply(res,3:4,quantile,p=quants)
  PROplot_s(store,MPnams=MPnames[MPind],ylim=ylim,ynam="SSB/SSBMSY dynamic")

  if(leg)legend('top',MPnames[MPind],text.col=MPcols[1:3],bty='n')


}


PROplot_m<-function(dat,MPnams,ylim,ynam){

  makeTrans<-function(someColor, alpha=100){
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }

  cols<-makeTrans(MPcols[1:3])
  nyrs<-dim(dat)[3]
  yrs<-Syear+(1:nyrs)-1
  yinc<-(ylim[2]-ylim[1])/10
  yls<-pretty(seq(ylim[1]-yinc,ylim[2]+yinc,length.out=14))
  xls<-seq(1900,2100,20)
  plot(Syear+c(1,nyrs)-1,ylim,type="l",col="white",ylim=ylim+c(0,yinc),ylab="",xlab="",yaxs="i")
  abline(h=yls,col='lightgrey')
  abline(v=xls,col='lightgrey')
  matplot(yrs,t(dat[,1,]),col=cols[1],type="l",add=T)
  matplot(yrs,t(dat[,2,]),col=cols[2],type="l",add=T)
  mtext(ynam,3,line=0.1,cex=0.8)

}


StochPm<-function(pp,Ino,leg=F){

  temp<-changed()

  ind1<-getind1()
  ind2<-getind2()


  if(Ino==1)ind<-ind1
  if(Ino==2)ind<-ind2

  if(length(ind)>1){

    plot(1,axes=F,col='white',xlab="",ylab="")
    legend('center',legend="Only works for a single OM",bty='n',text.col='red')

  }else{

    if(length(ind1)>1)ind1<-ind2
    if(length(ind2)>1)ind2<-ind1

    nsim<-dim(MET)[1]
    sims<-sample(dim(MET)[1],min(input$nsim,nsim))
    MPind<-match(c(input$Sm_MP1,input$Sm_MP2),MPnames)

    par(mfrow=c(1,2),mai=c(0.5,0.5,0.3,0.03))

    res<-CompRes$CWa[sims,ind,pp,MPind,]/1E6
    res1<-CompRes$CWa[sims,ind1,pp,MPind,]/1E6
    res2<-CompRes$CWa[sims,ind2,pp,MPind,]/1E6
    ylim<-c(0,max(res1,res2))

    PROplot_m(res,MPnams=MPnames[MPind],ylim=ylim,ynam="Catch by Area (1000t)")

    res<-CompRes$B_BMSY[sims,ind,pp,MPind,]
    res1<-CompRes$B_BMSY[sims,ind1,pp,MPind,]
    res2<-CompRes$B_BMSY[sims,ind2,pp,MPind,]
    ylim<-c(0,max(res1,res2))

    PROplot_m(res,MPnams=MPnames[MPind],ylim=ylim,ynam="SSB/SSBMSY dynamic")

    if(leg)legend('top',MPnames[MPind],text.col=MPcols[1:2],bty='n')
  }

}


radarP<-function(pp,Ino,leg=F){

  #datatable MP x metric
  temp<-changed()
  ind1<-getind1()
  ind2<-getind2()
  MPind<-match(c(input$R_MP1,input$R_MP2,input$R_MP3),MPnames)
  pind<-match(input$R_PMs,pnames)
  plabs<-pnames[pind]

  # ind1<-1:96; ind2<-1:96; ind<-ind1; MPind<-27:29; pind=c(1,12,13); pp=2; Ino=2; plabs<-pnames[pind]
  perf1<-MET[,,pp,,]
  perf1<-perf1[,ind1,MPind,pind,drop=F]
  perf1<-apply(perf1,3:4,mean,na.rm=T)

  perf2<-MET[,,pp,,]
  perf2<-perf2[,ind2,MPind,pind,drop=F]
  perf2<-apply(perf2,3:4,mean,na.rm=T)

  # metric inversions
  invnams<-c("AAVC")

  iind<-plabs%in%invnams
  if(sum(iind)>0) plabs[iind]<-paste(plabs[iind],"(i)")
  inverty<-function(x,maxx)maxx-x

  if(sum(iind)>0){
    piind<-(1:length(plabs))[iind]
    for(i in piind){
      perf1[,i]<-inverty(perf1[,i],max(perf1[,i],perf2[,i]))
      perf2[,i]<-inverty(perf2[,i],max(perf1[,i],perf2[,i]))
    }
  }

  # metric reciprocal
  invnams<-c("POS")
  iind<-plabs%in%invnams
  if(sum(iind)>0) plabs[iind]<-paste(plabs[iind],"(r)")

  if(sum(iind)>0){
    piind<-(1:length(plabs))[iind]
    for(i in piind){
      perf1[,i]<-100-perf1[,i]
      perf2[,i]<-100-perf2[,i]
    }
  }

  if(Ino==1)perf<-perf1
  if(Ino==2)perf<-perf2


  #maxes<-apply(rbind(perf1,perf2),2,max) # need to do ylims for each radar plot.
  maxes<-apply(perf,2,max)

  perf<-rbind(maxes,rep(0,ncol(perf)),perf)
  perf<-data.frame(perf)

  names(perf)<-plabs
  row.names(perf)<-c("min","max",MPnames[MPind])


  makeTrans<-function(someColor, alpha=50){
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }

  cols<-makeTrans(MPcols[2:4])
  par(mar=rep(0.1,4))
  radarchart(perf,pcol=MPcols[2:4],plty=1,plwd=1,pfcol=cols[1:3],seg=4,centerzero=T)
  legend('topright',legend=MPnames[MPind],text.col=MPcols[2:4],bty='n')

}



radarPEW<-function(Ino,leg=F){

  #datatable MP x metric
  temp<-changed()

  if(Ino==1)Iind<-getind1()
  if(Ino==2)Iind<-getind2()

  MPind<-match(c(input$REW_MP1,input$REW_MP2,input$REW_MP3),MPnames)
  pindE<-pnames%in%input$RE_PMs
  pindW<-pnames%in%input$RW_PMs

  plabs<-c(paste0("W-",pnames[pindW]),paste0("E-",pnames[pindE]))

  # ind1<-1:4; ind2<-100:105; ind<-ind1; MPind<-1:3; pind=1:4; pp=1; Ino=1
  perf1<-MET[,,1,,]
  perf1<-perf1[,Iind,MPind,pindE,drop=F]
  perf1<-apply(perf1,3:4,mean,na.rm=T)

  perf2<-MET[,,2,,]
  perf2<-perf2[,Iind,MPind,pindW,drop=F]
  perf2<-apply(perf2,3:4,mean,na.rm=T)

  # metric inversions
  invnams<-c("AAVC")

  iind<-plabs%in%invnams
  if(sum(iind)>0) plabs[iind]<-paste(plabs[iind],"(i)")
  inverty<-function(x,maxx)maxx-x

  if(sum(iind)>0){
    piind<-(1:length(plabs))[iind]
    for(i in piind){
      perf1[,i]<-inverty(perf1[,i],max(perf1[,i],perf2[,i]))
      perf2[,i]<-inverty(perf2[,i],max(perf1[,i],perf2[,i]))
    }
  }

  # metric reciprocal
  invnams<-c("POS")
  iind<-plabs%in%invnams
  if(sum(iind)>0) plabs[iind]<-paste(plabs[iind],"(r)")

  if(sum(iind)>0){
    piind<-(1:length(plabs))[iind]
    for(i in piind){
      perf1[,i]<-100-perf1[,i]
      perf2[,i]<-100-perf2[,i]
    }
  }

  perf<-cbind(perf2,perf1)
  maxes<-apply(perf,2,max) # need to do ylims for each radar plot.

  perf<-rbind(maxes,rep(0,ncol(perf)),perf)
  perf<-data.frame(perf)

  names(perf)<-plabs
  row.names(perf)<-c("min","max",MPnames[MPind])

  makeTrans<-function(someColor, alpha=50){
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }

  cols<-makeTrans(MPcols[2:4])
  par(mar=rep(0.1,4))
  radarchart(perf,pcol=MPcols[2:4],plty=1,plwd=1,pfcol=cols[1:3],seg=4,centerzero=T)
  legend('topright',legend=MPnames[MPind],text.col=MPcols[2:4],bty='n')

}

