# ======================================================================================================================
# ==== ABT MSE Diagnostics =============================================================================================
# ======================================================================================================================

Y10<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+1:10],1:2,mean)
class(Y10)<-"ABT_PM"

Y20<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+11:20],1:2,mean)
class(Y20)<-"ABT_PM"

Y30<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+21:30],1:2,mean)
class(Y30)<-"ABT_PM"

PGK<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+1:MSE@proyears]<1 & MSE@B_BMSY[,,pp,MSE@nyears+1:MSE@proyears]>1,1:2,sum)/MSE@proyears*100
class(PGK)<-"ABT_PM"

POF<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+1:MSE@proyears]>1,1:2,sum)/MSE@proyears*100
class(POF)<-"ABT_PM"

POFed<-function(MSE,pp=1) apply(MSE@B_BMSY[,,pp,MSE@nyears+1:MSE@proyears]<1,1:2,sum)/MSE@proyears*100
class(POFed)<-"ABT_PM"

D10<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+1:10]/array(rep(MSE@SSB0proj[,pp,1:10],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,10))
  apply(D,1:2,mean)
}  
class(D10)<-"ABT_PM"

D20<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+11:20]/array(rep(MSE@SSB0proj[,pp,11:20],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,10))
  apply(D,1:2,mean)
}  
class(D20)<-"ABT_PM"

D30<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+21:30]/array(rep(MSE@SSB0proj[,pp,21:30],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,10))
  apply(D,1:2,mean)
}  
class(D30)<-"ABT_PM"

LD<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+1:30]/array(rep(MSE@SSB0proj[,pp,1:30],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,30))
  apply(D,1:2,min)
}
class(LD)<-"ABT_PM"

RSSB<-function(MSE,pp=1)  MSE@SSB[,,pp,MSE@nyears+30]/array(rep(MSE@SSB[1,,pp,MSE@nyears+30],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim))
class(RSSB)<-"ABT_PM"

LRSSB<-function(MSE,pp=1) apply(MSE@SSB[,,pp,MSE@nyears+1:30]/array(rep(MSE@SSB[1,,pp,MSE@nyears+1:30],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim,30)),1:2,min)
class(LRSSB)<-"ABT_PM"

AAVY<-function(MSE,pp=1){
  ind1<-MSE@nyears+0:29
  ind<-MSE@nyears+1:30
  apply(((MSE@C[,,pp,ind]-MSE@C[,,pp,ind1])^2)^0.5/MSE@C[,,pp,ind1],1:2,mean)
}
class(AAVY)<-"ABT_PM"

getperf<-function(object,bysim=F){            
          
  MSE<-object      
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
  
  
  


