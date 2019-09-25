# ===================================================================
# === Harvest control rules =========================================
# ===================================================================

#' A 40-10 harvest control rule based on B/BMSY
#'
#' @param Brel Improper fraction: an estimate of Biomass relative to BMSY
#' @return a TAC or TAE adjustement factor.
#' @author T. Carruthers
#' @references Pretty generic fishery control rule
#' @export
#' @examples
#' Brel<-seq(0,1.5,length.out=200)
#' plot(Brel,HCR40_10(Brel),xlab="B/BMSY",ylab="HCR adjustement factor",main="A 40-10 harvest control rule",type='l',col='blue')
#' abline(v=c(0.1,0.4),col='red',lty=2)
HCR40_10=function(Brel)HCRlin(Brel,0.1,0.4)

#' A 60-20 harvest control rule based on B/BMSY
#'
#' @param Brel Improper fraction: an estimate of Biomass relative to BMSY
#' @return a TAC or TAE adjustement factor.
#' @author T. Carruthers
#' @references Pretty generic fishery control rule
#' @export
#' @examples
#' Brel<-seq(0,1.5,length.out=200)
#' plot(Brel,HCR60_20(Brel),xlab="B/BMSY",ylab="HCR adjustement factor",main="A 60-20 harvest control rule",type='l',col='blue')
#' abline(v=c(0.2,0.6),col='red',lty=2)
HCR60_20=function(Brel)HCRlin(Brel,0.2,0.6)

#' Generic linear harvest control rule based on B/BMSY
#'
#' @param Brel Improper fraction: an estimate of Biomass relative to BMSY
#' @param LRP Improper fraction: the Limit Reference Point, a biomass level below which the adjustment is zero (no fishing)
#' @param TRP Improper fraction: the Target Reference Point, a biomass level above which the adjustment is 1 (no adjustment)
#' @return a TAC or TAE adjustement factor.
#' @author T. Carruthers
#' @references Pretty generic fishery control rule
#' @export
#' @examples
#' Brel<-seq(0,1.5,length.out=200)
#' plot(Brel,HCRlin(Brel,0.1,0.4),xlab="B/BMSY",ylab="HCR adjustement factor",main="Basically a 40-10 harvest control rule",type='l',col='blue')
#' abline(v=c(0.1,0.4),col='red',lty=2)
HCRlin=function(Brel,LRP,TRP){
  adj <- rep(1,length(Brel))
  adj[Brel<LRP] <- 0
  cond <- Brel>LRP&Brel<TRP
  adj[cond] <- (Brel[cond]-LRP)/(TRP-LRP)
  adj
}

#' A Harvest Control Rule using B/BMSY and F/FMSY to adjust TAC or TAE.
#'
#' @param Brel improper fraction: an estimate of Biomass relative to BMSY
#' @param Frel improper fraction: an estimate of Fishing mortality rate relative to FMSY
#' @param Bpow non-negative real number: controls the shape of the biomass adjustment, when zero there is no adjustment
#' @param Bgrad non-negative real number: controls the gradient of the biomass adjustment
#' @param Fpow non-negative real number: controls the adjustment speed relative to F/FMSY. When set to 1, next recommendation is FMSY. When less than 1 next recommendation is between current F and FMSY.
#' @param Fgrad improper fraction: target Fishing rate relative to FMSY
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC or TAE adjustement factor.
#' @author T. Carruthers
#' @references Made up for this package
#' @export
#' @examples
#' res<-100
#' Frel<-seq(1/2,2,length.out=res)
#' Brel<-seq(0.05,2,length.out=res)
#' adj<-array(HCR_FB(Brel[rep(1:res,res)],Frel[rep(1:res,each=res)],Bpow=2,Bgrad=1,Fpow=1,Fgrad=0.75),c(res,res))
#' contour(Brel,Frel,adj,nlevels=20,xlab="B/BMSY",ylab="F/FMSY",main="FBsurface HCR adjustment factor")
#' abline(h=1,col='red',lty=2)
#' abline(v=1,col='red',lty=2)
#' legend('topright',c("Bpow = 2", "Bgrad=1","Fpow=1","Fgrad=0.75"),text.col='blue')
HCR_FB<-function(Brel,Frel,Bpow=2,Bgrad=1,Fpow=1,Fgrad=1){
  Fresp <- exp(log(1/Frel)*Fpow)
  Bresp <- exp(powdif(Brel-1,Bpow,Bgrad))
  Fgrad*Bresp*Fresp
}

#' Power difference function.
#'
#' @param x Real number: the absolute difference between two numbers
#' @param z Real number: the exponent the difference will be raised to
#' @param g Real number: the gradient in the exponential difference
#' @return a positive real number
#' @export
#' @examples
#' powdif(-2,3,1)
powdif<-function(x,z,g){
  x2<-(g*abs(x))^z
  x2[x<0]<-(-x2[x<0])
  x2
}


# ====================================================================
