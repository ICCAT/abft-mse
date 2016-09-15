# ===========================================================================================================================================================================
# ==== ABT MSE Methods ======================================================================================================================================================
# ===========================================================================================================================================================================

# Plot spatial distribution implied by the OM model 
setMethod("plot", signature(x = "OM"),function(x,outfile=NA){
            
    .Object<-x
    Istore<-array(NA,c(.Object@npop,.Object@nma,.Object@nsubyears,.Object@nareas))
    Idist<-array(1/.Object@nareas,c(.Object@npop,.Object@nages,.Object@nareas))
    ind<-as.matrix(expand.grid(1:.Object@npop,1:.Object@nages,1:.Object@nareas))  
    mref<-c(2:.Object@nsubyears,1)
    # OM@mov s p a m r r
    
    for(i in 1:100){for(m in 1:.Object@nsubyears){
      Idist<-domov2(Idist,.Object@mov[1,,,m,,])
      if(i==100){
        for(p in 1:.Object@npop){
          for(j in 1:.Object@nma){
            ref<-match(j,.Object@ma[,p])
            Istore[p,j,mref[m],]<-Idist[p,ref,]
          }
        }
      }
    }}
    
    #pid<-3
    #nplots<-ceiling(.Object@npop/2)
   # if(nplots>2)pid<-(((1:nplots)*2)-1)[2:nplots]
    if(!is.na(outfile))jpeg(paste(getwd(),"/Images/",outfile,".jpg",sep=""),res=300,units="in",width=8,height=8)
    par(mfrow=c(.Object@nma,.Object@npop*.Object@nsubyears),mai=c(0.1,0.1,0.01,0.01),omi=c(0.4,0.4,0.6,0.05))
    
    for(ma in 1:.Object@nma){
       
      for(p in 1:.Object@npop){
        for(m in 1:.Object@nsubyears){
        
          sdensplot(Istore[p,ma,m,],.Object@Area_defs)
         
          if(p==1&m==1)mtext(paste("Age class",ma),2,line=0.6)
          if(ma==.Object@nma)mtext(paste("Subyear",m),1,line=0.6)
          #if(m==1&mat==1)mtext("Juvenile",3,line=0.3)
          #if(m==1&mat==2)mtext("Mature",3,line=0.3)
          #if(m==1&mat==1)mtext(paste("Population",p),3,adj=1.5,line=1.6)
          
    }}}
    
    mtext(paste("Stock",1:.Object@npop),3,at=seq(0,1,length.out=.Object@npop*2+1)[(1:.Object@npop)*2],outer=T,line=0.5)  
    if(!is.na(outfile))dev.off()
})

# Plot spatial definitions of the OMd object
setMethod("plot", signature(x = "OMd"),function(x){
  
  OMd<-x
  cols<-rep(c("#ff000040","#00ff0040","#0000ff40","#00000040","#ff00ff40"),4)
  res<-0.03
  map(database = "worldHires",xlim=c(-105,50),ylim=c(-55,85),mar=rep(0,4),resolution=res)
  
  abline(v=(-20:20)*10,col='light grey')
  abline(h=(-20:20)*10,col='light grey')
  abline(v=0,col="green")
  abline(h=0,col="green")
  map(database = "worldHires",mar=rep(0,4),border=0,xlim=c(-105,50), ylim=c(-55,85),add=T,fill=T,col="light grey",resolution=res)
  
  
  for(i in 1:length(OMd@Area_names)){
    polygon(OMd@Area_defs[[i]],border='blue',lwd=2,col=NA)#cols[i])
    text(mean(OMd@Area_defs[[i]]$x),2.5+mean(OMd@Area_defs[[i]]$y),OMd@Area_names[i],col='red',font=2,cex=0.6)         
  }
})

setMethod("plot", signature(x = "MSE"),function(x){
  
  tmse<-x
  cols<-c("black","orange","blue","red","dark green","grey","purple","brown","pink")
  mnam<-c("Yield (no Disc. rate)","Yield (5% Disc. rate)",
          "Yield (10% Disc. rate)","Prob. Green Kobe",
          "Av Ann. Var. Yield")
  
  MPs<-tmse@MPs
  nMP<-length(MPs)
  yind<-(tmse@nyears+1):(tmse@nyears+tmse@proyears)    
  Y<-apply(tmse@C[,,,yind],1,mean)
  PGK<-apply(tmse@B_BMSY[,,yind]>1&tmse@F_FMSY[,,yind]>1,1,mean)
  y1<-yind[1:(tmse@proyears-1)]
  y2<-yind[2:tmse@proyears]
  AAV<-apply(apply(((tmse@C[,,,y1]-tmse@C[,,,y2])^2)^0.5,1:2,mean) / apply(tmse@C[,,,yind],1:2,mean),1,mean)
  
  Yinc<-(max(Y)-min(Y))/15
  #Y5inc<-(max(Y5)-min(Y5))/15
  #Y10inc<-(max(Y10)-min(Y10))/15
  PGKinc<-(max(PGK)-min(PGK))/15
  AAVinc<-(max(AAV)-min(AAV))/15
  
  par(mfrow=c(1,2),mai=c(1.1,1.1,0.05,0.05),omi=c(0.01,0.01,0.7,0.01))
  
  plot(range(PGK)+c(-PGKinc,PGKinc),range(Y)+c(-Yinc,Yinc),col='white',xlab="PGK",ylab="Y")
  addgg(PGK,Y)
  textplot(PGK,Y,MPs,col=cols,new=F,font=2)
  
  plot(range(AAV)+c(-AAVinc,AAVinc),range(Y)+c(-Yinc,Yinc),col='white',xlab="AAVY",ylab="Y")
  addgg(AAV,Y)
  textplot(AAV,Y,MPs,col=cols,new=F,font=2)
  mtext(paste(tmse@Name," (n =",tmse@nsim,")",sep=""),3,line=0.3,outer=T)
  
})

