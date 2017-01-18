#' Transpose an array
#'
#' @param arr An array
#' @return the transposition of\code{arr}
#' @examples
#' tomt(array(1:120,c(4,5,6)))
tomt<-function(arr){
  dim<-new('list')
  dims<-dim(arr)
  for(i in 1:ndims)dim[[i]]<-1:dims[i]
  ind<-as.matrix(expand.grid(dim))
  out<-array(NA,dims[ndims:1])
  out[ind[,ndims:1]]<-arr[ind]
  out
}

#' Calculate coefficient of variance
#'
#' @param x a vector of length 2 or more
#' @return the coefficient of variation of \code{arr}
#' @examples
#' cv(runif(10))
cv<-function(x)  sd(x)/mean(x)

#' Calculate the lognormal standard deviation from logspace mean and standard deviations
#'
#' @param m a positive real number
#' @param sd a positive real number
#' @return the lognormal standard deviation corresponding to \code{m, sd}
#' @examples
#' sdconv(0.5,0.2)
sdconv<-function(m,sd)(log(1+((sd^2)/(m^2))))^0.5        # get log normal standard deviation from transformed space mean and standard deviation

#' Calculate the lognormal mean from logspace mean and standard deviations
#'
#' @param m a positive real number
#' @param sd a positive real number
#' @return the lognormal mean corresponding to \code{m, sd}
#' @examples
#' mconv(0.5,0.2)
mconv<-function(m,sd)log(m)-0.5*log(1+((sd^2)/(m^2)))    # get log normal mean from transformed space mean and standard deviation

#' Calculate the beta distribution parameter alpha from transformed space mean and standard deviations
#'
#' @param m a fraction
#' @param sd a positive real number
#' @return the beta distribution parameter alpha corresponding to \code{m, sd}
#' @examples
#' alphaconv(0.25,0.1)
alphaconv<-function(m,sd)m*(((m*(1-m))/(sd^2))-1)

#' Calculate the beta distribution parameter beta from transformed space mean and standard deviations
#'
#' @param m a fraction
#' @param sd a positive real number
#' @return the beta distribution parameter alpha corresponding to \code{m, sd}
#' @examples
#' betaconv(0.25,0.1)
betaconv<-function(m,sd)(1-m)*(((m*(1-m))/(sd^2))-1)

#' A log-normal probabilty density function specified according to the transformed mean and standar deviation
#' @param reps an integer value representing the number of samples
#' @param mu the desired mean of the random variable
#' @param cv the desired coefficient of variation (StDev/mean) of the random variable
#' @return a random sample produced by \code{reps, mu, cv}
#' @examples
#' x<-trlnorm(1000,2,0.1)
#' mean(x)
#' sd(x)/mean(x)
trlnorm<-function(reps,mu,cv)return(rlnorm(reps,mconv(mu,mu*cv),sdconv(mu,mu*cv)))


sampCatch<-function(Csamp,nSamp){
  out<-array(NA,dim(Csamp))
  nsim<-dim(Csamp)[1]
  nages<-dim(Csamp)[2]
  nyears<-dim(Csamp)[3]
  for(ss in 1:nsim){
    for(yy in 1:nyears){

      Csampo<-Csamp[ss,,yy]
      #assign("Csampot",Csampo,envir=globalenv()) # debugging
      #assign("nsampt",nSamp[ss],envir=globalenv()) # debugging
      if(sum(Csampo)==0)Csampo<-rep(1/nages,nages)
      out[ss,,yy]<-ceiling(rmultinom(1,size=nSamp[ss],Csampo)*sum(Csampo)/nSamp[ss])

    }}
  out
}

makeCAL<-function(CAA,Linf,K,t0,CAL_bins,CALsd=0.05){
  ny<-dim(CAA)[3]
  na<-dim(CAA)[2]
  ns<-dim(CAA)[1]
  CALmu<--0.5*CALsd^2
  nCALbins<-length(CAL_bins)-1
  CAL<-array(NA,dim=c(ns,nCALbins,ny))
  for(i in 1:ns){
    for(j in 1:ny){
      ages<-rep(1:na,CAA[i,,j])+runif(sum(CAA[i,,j]),-0.5,0.5)
      lengths<-Linf[i,j]*(1-exp(-K[i,j]*(ages-t0)))*exp(rnorm(sum(CAA[i,,j]),CALmu,CALsd))
      CAL[i,,j]<-hist(lengths,CAL_bins,plot=F)$counts
    }
  }
  CAL
}


makeTrans<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

