# ======================================================================================================================
# ==== ABT MSE Diagnostics =============================================================================================
# ======================================================================================================================

getperf<-function(object){            
          
  MSEobj<-object      
  nsim<-MSEobj@nsim
  proyears<-MSEobj@proyears
  nMPs<-MSEobj@nMPs
  ntargpop<-length(MSEobj@targpop)
  
  prjd<-(MSEobj@nyears+1):(MSEobj@nyears+MSEobj@proyears)
  CIind<-prjd-MSEobj@nyears-1
  
  GK<-round(apply(MSEobj@F_FMSY[,,prjd]<1 & MSEobj@B_BMSY[,,prjd]>1,1:2,sum)/(MSEobj@proyears)*100,3)
  Y<-round(apply(MSEobj@C[,,MSEobj@targpop,prjd],1:2,mean),0)
  Y5<-round(apply(array(MSEobj@C[,,MSEobj@targpop,prjd],c(nMPs,nsim,ntargpop,proyears))*array(rep(0.95^CIind,each=nMPs*nsim*ntargpop),c(nMPs,nsim,ntargpop,proyears)),1:2,mean),0)
  Y10<-round(apply(array(MSEobj@C[,,MSEobj@targpop,prjd],c(nMPs,nsim,ntargpop,proyears))*array(rep(0.9^CIind,each=nMPs*nsim*ntargpop),c(nMPs,nsim,ntargpop,proyears)),1:2,mean),0)
  Y_5<-round(apply(array(MSEobj@C[,,MSEobj@targpop,prjd],c(nMPs,nsim,ntargpop,proyears))*array(rep(1.05^CIind,each=nMPs*nsim*ntargpop),c(nMPs,nsim,ntargpop,proyears)),1:2,mean),0)
  ind<-prjd[1:(length(prjd)-1)]
  ind2<-prjd[2:length(prjd)]
  AAVY<-apply(((MSEobj@C[,,MSEobj@targpop,ind2]-MSEobj@C[,,MSEobj@targpop,ind])^2)^0.5,1:2,mean)
  list(GK,Y,AAVY,Y5,Y10,Y_5)
}

setMethod("plot",
  signature(x = "MSE"),
  function(x,bysim=F){            
            
  MSEobj<-x      
  nsim<-MSEobj@nsim
  proyears<-MSEobj@proyears
  allyears<-MSEobj@proyears+MSEobj@nyears
  nMPs<-MSEobj@nMPs
  ntargpop<-length(MSEobj@targpop)
            
  prjd<-(MSEobj@nyears+1):(MSEobj@nyears+MSEobj@proyears)
  CIind<-prjd-MSEobj@nyears-1
            
  GK<-round(apply(MSEobj@F_FMSY[,,prjd]<1 & MSEobj@B_BMSY[,,prjd]>1,1:2,sum)/(MSEobj@proyears)*100,3)
  Y<-round(apply(MSEobj@C[,,MSEobj@targpop,prjd],1:2,mean),0)
  Dend<-MSEobj@B_BMSY[,,allyears]
  Y5<-round(apply(array(MSEobj@C[,,MSEobj@targpop,prjd],c(nMPs,nsim,ntargpop,proyears))*array(rep(0.95^CIind,each=nMPs*nsim*ntargpop),c(nMPs,nsim,ntargpop,proyears)),1:2,mean),0)
  Y10<-round(apply(array(MSEobj@C[,,MSEobj@targpop,prjd],c(nMPs,nsim,ntargpop,proyears))*array(rep(0.9^CIind,each=nMPs*nsim*ntargpop),c(nMPs,nsim,ntargpop,proyears)),1:2,mean),0)
  Y_5<-round(apply(array(MSEobj@C[,,MSEobj@targpop,prjd],c(nMPs,nsim,ntargpop,proyears))*array(rep(1.05^CIind,each=nMPs*nsim*ntargpop),c(nMPs,nsim,ntargpop,proyears)),1:2,mean),0)
  ind<-prjd[1:(length(prjd)-1)]
  ind2<-prjd[2:length(prjd)]
  AAVY<-apply(((MSEobj@C[,,MSEobj@targpop,ind2]-MSEobj@C[,,MSEobj@targpop,ind])^2)^0.5,1:2,mean)
  
  if(bysim){
    list(GK,Dend,AAVY,Y,Y5,Y10,Y_5)
  }else{
    temp<-data.frame(cbind(apply(GK,1,mean),
                              apply(Dend,1,mean),
                              apply(AAVY,1,mean),
                              apply(Y,1,mean),
                              apply(Y5,1,mean),
                              apply(Y10,1,mean),
                              apply(Y_5,1,mean)),                              
                              row.names=MSEobj@MPs)
    names(temp)=c("Green Kobe","Final depletion","AAV Yield","Yield","Yield 5% DR", "Yield 10% DR", "Yield -5% DR")
    temp
  }
})

sumplot<-function(dat,field,adjv=c(1,1,1),pm=2,UB=10){
  
  perc=c(0.02,0.02,0.02)
  perc[3]<-perc[3]*pm
  col<-c("black","red","green","blue","orange","grey","purple","pink","brown")
  coln<-match(field,names(dat))
  levs<-unique(dat[,coln])
  
  mnam<-c("Yield (% Disc. Rate)","Prob. Green Kobe","Av Ann. Var. Yield")
  mind<-c(13,11,10)
  for(met in 1:length(mnam)){
    ymax<--1000
    xlim<-c(10000,-10000)
    for(i in 1:length(levs)) {
      tdat<-dat[dat[,coln]==levs[i],mind[met]]
      tdat<-tdat[tdat<UB&tdat>-0.001]
      dd<-density(tdat,adj=adjv[met],from=0)
      xt<-quantile(tdat,c(perc[met]/2,1-perc[met]))
      xlim[1]<-min(xlim[1],xt[1])
      xlim[2]<-max(xlim[2],xt[2])
      ymax<-max(ymax, max(dd$y))
    }  
    for(i in 1:length(levs)){ 
      tdat<-as.numeric(dat[dat[,coln]==levs[i],mind[met]])
      tdat<-tdat[tdat<UB&tdat>-0.001]
      if(i==1)plot(density(tdat,adj=adjv[met],from=0),ylim=c(0,ymax),xlim=xlim,col=col[1],type='l',main=mnam[met])
      if(i>1)lines(density(tdat,adj=adjv[met],from=0),col=col[i])
    }
  }
  
  legend('topright',legend=levs,text.col=col[1:length(levs)],bty='n')
  
}
sumplot2<-function(dat,fieldv,adjv=c(1,1,1),pm=2,UB=10,refMP="UMSY_PI"){
  
  dat<-dat[dat$MP!=refMP,]
  perc=c(0.02,0.02,0.02)
  perc[3]<-perc[3]*pm
  col<-c("black","red","green","blue","orange","grey","purple","pink","brown")
  
  for(ff in 1:length(fieldv)){
  field<-fieldv[ff]
  coln<-match(field,names(dat))
  levs<-unique(dat[,coln])
    
  mnam<-c("Yield (5% Disc. Rate)","Av Ann. Var. Yield","Prob. Green Kobe")
  mind<-c(13,10,11)
  for(met in 1:length(mnam)){
    ymax<--1000
    xlim<-c(10000,-10000)
    for(i in 1:length(levs)) {
      tdat<-dat[dat[,coln]==levs[i],mind[met]]
      tdat<-tdat[tdat<UB&tdat>-0.001]
      dd<-density(tdat,adj=adjv[met],from=0)
      xt<-quantile(tdat,c(perc[met]/2,1-perc[met]))
      xlim[1]<-min(xlim[1],xt[1])
      xlim[2]<-max(xlim[2],xt[2])
      ymax<-max(ymax, max(dd$y))
    }  
    for(i in 1:length(levs)){ 
      tdat<-as.numeric(dat[dat[,coln]==levs[i],mind[met]])
      tdat<-tdat[tdat<UB&tdat>-0.001]
      if(i==1)plot(density(tdat,adj=adjv[met],from=0),ylim=c(0,ymax),xlim=xlim,xlab="",ylab="",col=col[1],type='l',main="")
      if(i>1)lines(density(tdat,adj=adjv[met],from=0),col=col[i])
    }
    if(ff==1)mtext(mnam[met],side=3,line=0.3)
  }
  
  legend('topright',legend=levs,text.col=col[1:length(levs)],bty='n')
  }
  mtext("Relative frequency",side=2,line=0.5,outer=T)
  
}

Tplot<-function(dat,field,refMP="UMSY_PI",legy=F,legpos='top'){
  
  dat<-dat[dat$MP!=refMP,]  
  col<-c("black","orange","blue","green")
  mnam<-c("Yield (no Disc. rate)","Yield (5% Disc. rate)",
          "Yield (10% Disc. rate)","Prob. Green Kobe",
          "Av Ann. Var. Yield")
  
  MPs<-unique(dat$MP)
  nMP<-length(MPs)
  
  coln<-match(field,names(dat))
  levs<-unique(dat[,coln])
  nlev<-length(levs)
   
  Y<-aggregate(dat$Y,by=list(dat$MP,dat[,coln]),FUN="mean")$x
  Y5<-aggregate(dat$Y5,by=list(dat$MP,dat[,coln]),FUN="mean")$x
  Y10<-aggregate(dat$Y10,by=list(dat$MP,dat[,coln]),FUN="mean")$x
  PGK<-aggregate(dat$PGK,by=list(dat$MP,dat[,coln]),FUN="mean")$x
  AAV<-aggregate(dat$AAV,by=list(dat$MP,dat[,coln]),FUN="mean")$x
  
  Yinc<-(max(Y)-min(Y))/25
  Y5inc<-(max(Y5)-min(Y5))/25
  Y10inc<-(max(Y10)-min(Y10))/25
  PGKinc<-(max(PGK)-min(PGK))/25
  AAVinc<-(max(AAV)-min(AAV))/25
    
  cols<-rep(col,each=nMP)
  
  plot(range(PGK)+c(-PGKinc,PGKinc),range(Y5)+c(-Y5inc,Y5inc),col='white',xlab="",ylab="")
  addgg(PGK,Y5)
  textplot(PGK,Y5,rep(MPs,nlev),col=cols,new=F)
      
  plot(range(AAV)+c(-AAVinc,AAVinc),range(Y5)+c(-Y5inc,Y5inc),col='white',xlab="",ylab="")
  addgg(AAV,Y5)
  textplot(AAV,Y5,rep(MPs,nlev),col=cols,new=F)
  
  legend(legpos,legend=levs,text.col=col[1:length(levs)],bg=makeTrans('white',97),box.col='white')
    
}


Tplot2<-function(dat,fieldv,legpos='top'){
  
  for(ll in 1:length(fieldv))Tplot(dat,fieldv[ll],legpos)
    
  mtext("Yield relative to MSY (5% Disc. rate)",side=2,line=0.5,outer=T)
  mtext(c("Prob. green Kobe","AAVY"),side=1,at=c(0.25,0.75),line=0.8,outer=T)
   
} 


addgg<-function(x,y,pcol='azure2'){
 
  resx<-(max(x)-min(x))/10
  resy<-(max(y)-min(y))/10
  xlim<-c(min(x)-(2*resx),max(x)+(2*resx))
  ylim<-c(min(y)-(2*resy),max(y)+(2*resy))
  
  divx<-pretty(seq(xlim[1],xlim[2],length.out=20))
  divy<-pretty(seq(ylim[1],ylim[2],length.out=20))
  
  polygon(c(xlim,xlim[2:1]),rep(ylim,each=2),col=pcol)
  abline(v=divx,col='white')
  abline(h=divy,col='white')
  
  
}  
  
makeTrans<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

  
# Plot performance summary of the mse object
#setMethod("summary",
#  signature(object = "MSE"),
stats<-function(object){
  nsim<-object@nsim
  nyears<-object@nyears
  proyears<-object@proyears
  nMPs<-object@nMPs
  targpop<-object@targpop
  
  C<-apply(array(object@C[,,targpop,(nyears+1):(nyears+proyears)],
                 c(nMPs,nsim,length(targpop),proyears)),c(1,2,4),sum)
  AAVY<-array(NA,c(nMPs,nsim,proyears-1))
  ind1<-as.matrix(expand.grid(1:nMPs,1:nsim,1:(proyears-1)))    
  ind2<-as.matrix(expand.grid(1:nMPs,1:nsim,2:proyears))
  AAVY[ind1]<-((C[ind1]-C[ind2])^2)^0.5
  AAVY<-apply(AAVY,1:2,mean)
  
  Y<-apply(C[,,(proyears-4):proyears],1:2,mean)
  
  F_FMSY<-object@F_FMSY[,,(nyears+1):(nyears+proyears)]
  B_BMSY<-object@B_BMSY[,,(nyears+1):(nyears+proyears)]
  Pgreen<-apply(array(as.integer(F_FMSY<1&B_BMSY>1),dim(B_BMSY)),1:2,mean)
  
  list("Y"=Y,"AAVY"=AAVY,"Pgreen"=Pgreen,"Dep"=B_BMSY[,,proyears],"C"=C,"F_FMSY"=F_FMSY,"B_BMSY"=B_BMSY)
  
  
}#) 

anim8mov<-function(OM,outfile='bftanimov'){
  
  
  
  #animov<-function(stat){
    
    fac<-4
    stat<-tinter(t(stat),fac)
    UB<-1
    LB<-0
    nsim<-nrow(stat)
    Drive<-"D"
    setwd(paste(Drive,':/HerringMSE/Data/',sep=""))
    
    Lat<-c(48,55)
    Lon<-c(-133,-122)
    
    gridcol<-'grey'
    axiscol<-'black'
    landcol<-'grey'
    
    Regions<-c("Straight of Georgia","W. Coast Van. Island","Central Coast", "Haida Gwaii", "Prince Rupert District")
    RegCodes<-c("SOG","WCVI","CC","HG","PRD")
    MAs<-importShapefile('Assessment_Regions_2W_27_March_2009',readDBF=F)
    MAnam<-c("PRD","CC","SOG","WCVI","A27","2W","HG")
    MAs<-subset(MAs,MAs$PID%in%match(RegCodes,MAnam))
    MAs$PID<-match(MAnam[MAs$PID],RegCodes)
    
    dcolgrad<-500
    dcols<-rainbow(dcolgrad+2,start=0.05,end=0.4)
    statcol<-1+ceiling((stat^0.5-LB^0.5)/(UB^0.5-LB^0.5)*dcolgrad)
    paste("#ff0000",floor(stat*0.99*100),sep="") 
    statcol<-array(sprintf("#ff0000%02d", floor(stat*0.99*100)),dim(stat))
    for(j in 1:nsim){
      
      plot(Lon,Lat,col="white",xlab="",ylab="",main="",axes=F)
      xlimz<-c(-1000,1000)
      ylimz<-xlimz
      polygon(rep(xlimz,each=2),c(ylimz,ylimz[2:1]),col='azure',border='azure')
      
      # x<-(-122:-135)     # the global 1 deg longitudes
      #y<-47:56       # the global 1 deg latitudes
      #abline(h=y,col=gridcol)   # add the 1 degree latitude lines
      #abline(v=x,col=gridcol)   # add the 1 degree longitude lines
      #axis(1,at=x,labels=as.character(x),col=gridcol,col.axis=axiscol,cex.axis=1)
      #axis(2,at=y,labels=as.character(y),col=gridcol,col.axis=axiscol,cex.axis=1)
      #mtext(expression(paste("Longitude ",~degree~W,sep="")),side=1,line=2.5,outer=F,font=2,cex=14/12)
      #mtext(expression(paste("Latitude ",~degree~N,sep="")),side=2,line=2.5,outer=F,font=2,cex=14/12)
      
      tompoly(MAs,acol=statcol[j,],lcol=statcol[j,])
      map(database = "worldHires", xlim=Lon, ylim=Lat,resolution = 0,add=T,fill=T,col=landcol)
      map(database = "worldHires", xlim=Lon, ylim=Lat,resolution = 0,add=T,col=landcol)
      legend('topright',legend=ceiling(j/5),text.col='white',cex=1.8,bty='n')
      
      ani.pause()
    }
    
  #}
  
  
  
  
}  
  
  
  


