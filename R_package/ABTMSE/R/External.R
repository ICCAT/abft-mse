
tiny=1E-10

#' Generic class finder
#'
#' @param classy An character string representing an R class
#' @return a vector of objects of class\code{classy}
#' @examples
#' avail('data.frame')
#' avail('matrix')
#' avail('ABT_PM')
avail<-function(classy){

  return(unique(c(ls('package:ABTMSE')[unlist(lapply(ls('package:ABTMSE'),
                                                      getclass,classy=classy))], ls(envir=.GlobalEnv)[unlist(
                                                        lapply(ls(envir=.GlobalEnv),getclass,classy=classy))])))
  #return(c(ls(envir=.GlobalEnv)[unlist(lapply(ls(envir=.GlobalEnv),getclass,classy=classy))]))

}

getclass <- function(x,classy) inherits(get(x),classy)

#' Load all ABT-MSE objects and functions
#'
#' @param dir (optional) An character string representing a directory from which objects can be loaded
#' @return all package data objects are placed in the global namespace \code{dir}
#' @examples
#' loadABT()
loadABT<-function(dir=NA,quiet=F){

  chk <- "package:ABTMSE" %in% search()
  if(chk){
    datadir<- paste(searchpaths()[match("package:ABTMSE",search())],"/data/",sep="")
    fnams<-list.files(datadir)

    files<-paste0(datadir,fnams)
    for(i in 1:length(files))load(files[i],envir=globalenv())

    MPs<-avail('MP')
    for(i in 1:length(MPs))assign(MPs[i],get(MPs[i]),envir=globalenv())

    # other
    oth<-c("DD_R","tiny")
    for(i in 1:length(oth))assign(oth[i],get(oth[i]),envir=globalenv())


    if(!quiet)cat("ABT-MSE objects loaded")
    cat("\n")
  }else{
    stop("Can't find the ABTMSE in this session: you may have to load the library: library(ABTMSE)")
  }

}

#' Load all ABT-MSE R code including interal functions
#'
#' @return all functions are available in the global namespace \code{}
#' @examples
#' sourceall("C:/ABT-MSE/")
sourceall<-function(ABTdir="C:/ABT-MSE/"){


  if(dir.exists(ABTdir)){

    #sourcedir<- paste(searchpaths()[match("package:ABTMSE",search())],"/R/",sep="")
    sourcedir<- paste0(ABTdir,"R_package/ABTMSE/R/")

    fnams<-list.files(sourcedir)

    files<-paste0(sourcedir,fnams)
    for(i in 1:length(files))source(files[i])

    cat(paste0("ABT-MSE source code loaded from ",ABTdir))
    cat("\n")

  }else{

    stop(paste0("The specified directory",ABTdir,"doesn't appear to exist, please specify the correct directory e.g. D:/myfiles/ABT-MSE"))

  }

}

indfit<-function(SSB,ind,Year,sim=F,plot=T,lcex=0.8){

  if(sim){
    AC<-0.6
    beta<-0.7
    n<-30
    SSB<-exp(runif(1)*2+sin(runif(1)*5+seq((1:n))/3)*rnorm(n,1,0.3))
    ind<-SSB^beta
    Res<-rnorm(n,1,0.2)
    for(y in 2:n)Res[y]<-AC*Res[y-1]+Res[y]*(1-AC*AC)^0.5
    ind<-ind*Res
    Year<-1981:(1981+n-1)
  }

  SSB<-SSB/mean(SSB)
  ind<-ind/mean(ind)

  if(plot){
    par(mfrow=c(1,2),mai=c(0.7,0.5,0.05,0.01),omi=c(0.01,0.2,0.01,0.01))
    plot(SSB,ind,xlab="",ylab="",pch=19,col=rgb(0,0,0,0.5))
    mtext("Model estimate",1,line=2.2)
    mtext("Index",2,outer=T,line=0)
  }

  getbeta<-function(beta,x,y)sum((y-x^beta)^2)
  opt<-optimize(getbeta,x=SSB,y=ind,interval=c(0.1,10))
  res<-ind-(SSB^opt$minimum)
  ac<-acf(res,plot=F)$acf[2,1,1] # lag-1 autocorrelation


  if(plot){
    SSBseq<-seq(min(SSB),max(SSB),length.out=1000)
    lines(SSBseq,SSBseq^opt$minimum,col='#0000ff90',pch=19)
    legend('bottomright',legend=round(c(sum((ind-SSB)^2),opt$objective),3),text.col=c("black","blue"),bty='n',title="SSQ",cex=lcex)
    legend('topleft',legend=round(opt$minimum,3),text.col="blue",bty='n',title='Hyper-stability, beta',cex=lcex)
    legend('left',legend=round(cor(SSB,ind),3),bty='n',title='Correlation',cex=lcex)

    plot(Year,SSB,ylab="",xlab="",ylim=range(c(ind,SSB)),type="l")
    mtext("Year",1,line=2.2)
    points(Year,ind,col='#ff000090',pch=19)
    legend('topleft',legend=round(ac,3),text.col="red",bty='n',title="Lag 1 autocorrelation",cex=lcex)
    legend('bottomleft',legend=round(sd(res),3),text.col="red",bty='n',title="Residual StDev",cex=lcex)
    legend('topright',legend=c("Model estimate","Index"),text.col=c("black","red"),bty='n',cex=lcex)
  }

  list(stats=data.frame(beta=opt$minimum,AC=ac,sd=sd(ind/(SSB^opt$minimum)),cor=cor(SSB,ind)),mult=ind/(SSB^opt$minimum))

}


