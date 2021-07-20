
# MSEcomp plots

Explot<-function(MSEobj,sim=1,plotnam=NA,MPnams=c("ZeroC","CurC100"),bystock=T){

  nMPs<-MSEobj@nMPs
  par(mfcol=c(3,MSEobj@npop),mai=c(0.3,0.5,0.01,0.01),omi=c(0.4,0.2,0.4,0.1))
  allyears<-MSEobj@nyears+MSEobj@proyears
  totyears<-allyears+MSEobj@nHyears
  yrs<-1863+1:totyears
  cols<-c('red','blue')
  scale<-1E-6
  labline=2.5
  ttplot<-function(yrs,x,cols,ltys,verts=c(1965,2016,2020)){
    matplot(yrs,x,type='l',xlab="",ylab="",col="white",yaxs='i',ylim=c(0,max(x,na.rm=T)*1.05))
    filly<-"#99999930"
    hseq<-pretty(seq(0,max(x,na.rm=T)*1.3,length.out=8))
    abline(h=hseq,col=filly,lwd=1)
    abline(v=verts[1],col=filly,lwd=2)
    big<-1E10
    polygon(c(verts[2],verts[2],verts[3],verts[3]),c(-big,big,big,-big),col=filly,border=filly)
    matplot(yrs,x,type='l',xlab="",ylab="",col=cols,lty=ltys,lwd=2,add=T)
  }
  cols<-c("#ff000090","#0000ff90")
  ltys=rep(1,2)

  for(p in MSEobj@npop:1){
    Cmat<-array(NA,c(nMPs,totyears))
    if(bystock)Cmat[,MSEobj@nHyears+1:allyears]<-MSEobj@CW[,sim,p,]
    if(!bystock)Cmat[,MSEobj@nHyears+1:allyears]<-MSEobj@CWa[,sim,p,]
    if(bystock)Bmat<-MSEobj@BB[,sim,p,]
    if(!bystock)Bmat<-MSEobj@BBa[,sim,p,]
    Bmat[,1]<-NA
    ttplot(yrs,t(Bmat)*scale,cols,ltys)
    if(p==2)mtext("Biomass",2,line=labline)
    if(bystock)mtext(paste(MSEobj@Snames[p]," Stock"),3,line=0.7)
    if(!bystock)mtext(paste(MSEobj@Snames[p]," Area"),3,line=0.7)
    if(p==2)mtext(plotnam,3,adj=0,font=2,line=1.3,cex=1.2)
    ttplot(yrs,t(Cmat)*scale,cols,ltys)
    if(p==2)mtext("Catch",2,line=labline)
    ttplot(yrs,t(Cmat/(Cmat+Bmat)),cols,ltys)
    if(p==2)mtext("Harvest rate",2,line=labline)
    if(p==2)legend('topleft',legend=MPnams,text.font=2,cex=1.2,text.col=c('red','blue'),bty='n')
  }
  mtext("Year",1,line=1,outer=T)

}


Explot_SS<-function(MSEobj,sim=1,plotnam=NA,MPnams=c("ZeroC","CurC100"),bystock=T,S_A=2,legendy=F,snam=NA,labs=F){

  nMPs<-MSEobj@nMPs
  #par(mfcol=c(3,1),mai=c(0.3,0.5,0.01,0.01),omi=c(0.4,0.2,0.4,0.1))
  allyears<-MSEobj@nyears+MSEobj@proyears
  totyears<-allyears+MSEobj@nHyears
  yrs<-1863+1:totyears
  cols<-c('red','blue')
  scale<-1E-6
  labline=2.5

  ttplot<-function(yrs,x,cols,ltys,verts=c(1965,2016,2020)){
    matplot(yrs,x,type='l',xlab="",ylab="",col="white",yaxs='i',ylim=c(0,max(x,na.rm=T)*1.05))
    filly<-"#99999930"
    hseq<-pretty(seq(0,max(x,na.rm=T)*1.3,length.out=8))
    abline(h=hseq,col=filly,lwd=1)
    abline(v=verts[1],col=filly,lwd=2)
    big<-1E10
    polygon(c(verts[2],verts[2],verts[3],verts[3]),c(-big,big,big,-big),col=filly,border=filly)
    matplot(yrs,x,type='l',xlab="",ylab="",col=cols,lty=ltys,lwd=2,add=T)
  }
  cols<-c("#ff000090","#0000ff90")
  ltys=rep(1,2)

  p<-S_A
  if(bystock)Bmat<-MSEobj@BB[,sim,p,]
  if(!bystock)Bmat<-MSEobj@BBa[,sim,p,]
  Bmat[,1]<-NA
  ttplot(yrs,t(Bmat)*scale,cols,ltys)
  if(labs)mtext("Biomass",2,line=labline)
  if(!is.na(snam))mtext(snam,3,line=0.7,adj=0.7)
  if(!bystock)mtext(paste(MSEobj@Snames[p]," Area"),3,line=0.7,adj=0.7)
  if(p==2)mtext(plotnam,3,adj=0,font=2,line=1.3,cex=1.2)
  if(legendy)legend('topleft',legend=MPnams,text.font=2,cex=1.2,text.col=c('red','blue'),bty='n')

  #mtext("Year",1,line=1,outer=T)

}


Explot_SS_multi<-function(MSEobj1,MSEobj2,sim=1,plotnam=NA,MPnams=c("ZeroC","CurC100"),bystock=T,S_A=2,legendy=T,MSEnams=c("","")){

  MSEs<-list(MSEobj1,MSEobj2)

  nMPs<-MSEobj1@nMPs
  par(mfcol=c(3,MSEobj1@npop),mai=c(0.3,0.5,0.01,0.01),omi=c(0.4,0.2,0.4,0.1))
  allyears<-MSEobj1@nyears+MSEobj1@proyears
  totyears<-allyears+MSEobj1@nHyears
  yrs<-1863+1:totyears
  tcols<-c('red','blue')
  scale<-1E-6
  labline=2.5

  ttplot<-function(yrs,x1,x2,cols,ltys,verts=c(1965,2016,2020)){
    matplot(yrs,x1,type='l',xlab="",ylab="",col="white",yaxs='i',ylim=c(0,max(c(x1,x2),na.rm=T)*1.05))
    filly<-"#99999930"
    hseq<-pretty(seq(0,max(c(x1,x2),na.rm=T)*1.3,length.out=8))
    abline(h=hseq,col=filly,lwd=1)
    abline(v=verts[1],col=filly,lwd=2)
    big<-1E10
    polygon(c(verts[2],verts[2],verts[3],verts[3]),c(-big,big,big,-big),col=filly,border=filly)
    matplot(yrs,x1,type='l',xlab="",ylab="",col=cols[1],lty=c(3,1),lwd=2,add=T)
    matplot(yrs,x2,type='l',xlab="",ylab="",col=cols[2],lty=c(3,1),lwd=2,add=T)
  }
  cols<-c("#ff000090","#0000ff90")
  ltys=rep(1,2)

  for(p in MSEobj1@npop:1){
    Cmat1<-Cmat2<-array(NA,c(nMPs,totyears))

    if(bystock){
      Cmat1[,MSEobj1@nHyears+1:allyears]<-MSEobj1@CW[,sim,p,]
      Cmat2[,MSEobj2@nHyears+1:allyears]<-MSEobj2@CW[,sim,p,]
    }
    if(!bystock){
      Cmat1[,MSEobj1@nHyears+1:allyears]<-MSEobj@CWa[,sim,p,]
      Cmat2[,MSEobj2@nHyears+1:allyears]<-MSEobj@CWa[,sim,p,]
    }

    if(bystock){
      Bmat1<-MSEobj1@BB[,sim,p,]
      Bmat2<-MSEobj2@BB[,sim,p,]
    }
    if(!bystock){
      Bmat1<-MSEobj1@BBa[,sim,p,]
      Bmat2<-MSEobj2@BBa[,sim,p,]
    }

    Bmat1[,1]<- Bmat2[,1]<-NA

    ttplot(yrs,x1=t(Bmat1)*scale,x2=t(Bmat2)*scale,cols,ltys)
    if(p==2)mtext("Biomass",2,line=labline)
    if(bystock)mtext(paste(MSEobj1@Snames[p]," Stock"),3,line=0.7)
    if(!bystock)mtext(paste(MSEobj1@Snames[p]," Area"),3,line=0.7)
    if(p==2)mtext(plotnam,3,adj=0,font=2,line=1.3,cex=1.2)
    ttplot(yrs,t(Cmat1)*scale,t(Cmat2)*scale,cols,ltys)
    if(p==2)mtext("Catch",2,line=labline)
    ttplot(yrs,t(Cmat1/(Cmat1+Bmat1)),t(Cmat2/(Cmat2+Bmat2)),cols,ltys)
    if(p==2)mtext("Harvest rate",2,line=labline)
    #if(p==2)legend('topleft',legend=MPnams,text.font=2,cex=1.2,text.col=c('red','blue'),bty='n')
    if(p==1)legend('topleft',legend=MSEnams,text.font=2,text.col=tcols,bty='n',cex=1.2)
    if(p==1)legend('bottomleft',legend=MPnams,lty=c(3,1),text.font=2,bty='n',cex=1.2)

  }

  mtext("Year",1,line=1,outer=T)
  #mtext(MSEnams[2],3,adj=0.01,font=2,outer=T,line=0.8)

}

Explot_SS_multi_bio<-function(MSEobj1,MSEobj2,sim=1,plotnam=NA,
                              MPnams=c("ZeroC","CurC100"),bystock=T,
                              S_A=2,legendy=T,MSEnams=c("",""),panel=F,labo=T,zoom=F,
                              relMSY=F){

  MSEs<-list(MSEobj1,MSEobj2)

  nMPs<-MSEobj1@nMPs
  if(!panel)par(mfcol=c(1,MSEobj1@npop),mai=c(0.3,0.5,0.01,0.01),omi=c(0.4,0.2,0.4,0.1))
  allyears<-MSEobj1@nyears+MSEobj1@proyears
  totyears<-allyears+MSEobj1@nHyears
  yrs<-1863+1:totyears
  tcols<-c('red','blue')
  scale<-1E-6
  labline=2.5

  ttplot<-function(yrs,x1,x2,cols,ltys,verts=c(1965,2016,2020),zoom=F){
    if(zoom)ylim=range(x2,na.rm=T)
    if(!zoom)ylim=c(0,max(c(x1,x2),na.rm=T)*1.05)
    if(zoom)xlim=c(2016,max(yrs))
    if(!zoom)xlim=range(yrs)
    matplot(yrs,x1,type='l',xlab="",ylab="",col="white",yaxs='i',ylim=ylim,xlim=xlim)
    filly<-"#99999930"
    hseq<-pretty(seq(0,max(c(x1,x2),na.rm=T)*1.3,length.out=8))
    abline(h=hseq,col=filly,lwd=1)
    abline(v=verts[1],col=filly,lwd=2)
    big<-1E10
    polygon(c(verts[2],verts[2],verts[3],verts[3]),c(-big,big,big,-big),col=filly,border=filly)
    matplot(yrs,x1,type='l',xlab="",ylab="",col=cols[1],lty=c(3,1),lwd=2,add=T)
    matplot(yrs,x2,type='l',xlab="",ylab="",col=cols[2],lty=c(3,1),lwd=2,add=T)
  }
  cols<-c("#ff000090","#0000ff90")
  ltys=rep(1,2)

  for(p in MSEobj1@npop:1){
    Cmat1<-Cmat2<-array(NA,c(nMPs,totyears))

    if(!relMSY){
      Bmat1<-MSEobj1@BB[,sim,p,]
      Bmat2<-MSEobj2@BB[,sim,p,]
    }else{
      Bmat1<-MSEobj1@B_BMSY[,sim,p,]
      Bmat2<-MSEobj2@B_BMSY[,sim,p,]
      yrs<-1965+1:allyears
    }


    Bmat1[,1]<- Bmat2[,1]<-NA

    ttplot(yrs,x1=t(Bmat1)*scale,x2=t(Bmat2)*scale,cols,ltys,zoom=zoom)
    if(p==2&!relMSY)mtext("Biomass",2,line=labline)
    if(p==2&relMSY)mtext("Biomass / BMSY",2,line=labline)
    if(bystock&labo)mtext(paste(MSEobj1@Snames[p]," Stock"),3,line=0.7)
    if(!bystock&labo)mtext(paste(MSEobj1@Snames[p]," Area"),3,line=0.7)
    if(p==2)mtext(plotnam,3,adj=0,font=2,line=0.5,cex=1.2)
    if(p==1&legendy)legend('topleft',legend=MSEnams,text.font=2,text.col=tcols,bty='n',cex=1.2)
    if(p==1&legendy)legend('bottomleft',legend=MPnams,lty=c(3,1),text.font=2,bty='n',cex=1.2)

  }

  mtext("Year",1,line=1,outer=T)
  #mtext(MSEnams[2],3,adj=0.01,font=2,outer=T,line=0.8)

}


