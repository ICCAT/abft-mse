
retseq<-function(res){

  nres<-length(res)
  str<-rep(1,nres)
  i<-1
  going<-TRUE
  while(going){

    same=TRUE
    j<-0
    while(same){

      if(res[i]!=res[i+j+1]|(j+i+1)==(nres+1)){

        same=FALSE
        str[i:(i+j)]<-j+1

      }else{
        j<-j+1
      }
    }

    if(i+j+1>nres)going=FALSE
    i<-i+j+1

  }

  str
}



lcs<-function(x){
  x1<-x/mean(x) # rescale to mean 1
  x2<-log(x1)     # log it
  x3<-x2-mean(x2) # mean 0
  x3
}

resplot<-function(yrs,res){ # freecomp.rmd

  col<-rep('blue',length(res))
  col[res<0]<-'dark grey'
  ACcol<-c("white","white","#ff000010","#ff000030","#ff000080",rep('red',200))
  ACval<-retseq(res<0)
  bcol=ACcol[ACval]
  par(lwd = 2)
  barplot(res,names.arg=yrs,border=bcol,col="white")
  abline(h=c(-0.25,0.25),lty=2,col="grey",lwd=2)
  abline(h=c(-0.5,0.5),col="grey",lwd=2)
  barplot(res,names.arg=yrs,border=NA,col=col,add=T)
  ac<-acf(res,plot=F)$acf[2,1,1]
  leg<-c(paste("StDev =",round(sd(res),2)),paste("AC =", round(ac,2)))
  legend('topleft',legend=leg,box.col="#ffffff99",bg="#ffffff99")
  legend('topleft',legend=leg, box.col="#ffffff99",bg="#ffffff99")

}


resplot2<-function(yrs,res,horiz=T){ # Summary.rmd

  resind<-length(res):1
  col<-rep('blue',length(res))
  col[res<0]<-'dark grey'
  ACcol<-c("white","white","#ff000010","#ff000030","#ff000080",rep('red',200))
  ACval<-retseq(res<0)
  bcol=ACcol[ACval]
  par(lwd = 2)
  nspace<-ceiling(length(res)*0.2)
  barplot(c(res[resind],rep(NA,nspace)),names.arg=c(yrs[resind],rep(NA,nspace)),border="white",col="white",horiz=horiz)
  abline(v=c(-0.25,0.25),lty=2,col="grey",lwd=1)
  abline(v=c(-0.5,0.5),col="grey",lwd=1)
  barplot(res[resind],names.arg=yrs[resind],border=bcol[resind],col=col[resind],add=T,horiz=horiz)
  ac<-stats::acf(res,plot=F)$acf[2,1,1]
  leg<-c(paste("SD =",round(sd(res),2)),paste("AC =", round(ac,2)))
  legend('topleft',legend=leg,box.col="#ffffff99",bg="#ffffff99",cex=0.8)
  #legend('topleft',legend=leg, box.col="#ffffff99",bg="#ffffff99")

}
