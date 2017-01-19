# ======================================================================================================================
# ==== ABT MSE Diagnostics =============================================================================================
# ======================================================================================================================

#' Yield over the first 10 projected years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' Y10(MSE_example)
Y10<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+1:10],1:2,mean)
class(Y10)<-"ABT_PM"

#' Yield over projected years 11-20 (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' Y20(MSE_example)
Y20<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+11:20],1:2,mean)
class(Y20)<-"ABT_PM"

#' Yield over projected years 21-30 (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' Y30(MSE_example)
Y30<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+21:30],1:2,mean)
class(Y30)<-"ABT_PM"

#' Probability of being in the Green Kobe region (F<FMSY AND B>BMSY) over all projected years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' PGK(MSE_example)
PGK<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+1:MSE@proyears]<1 & MSE@B_BMSY[,,pp,MSE@nyears+1:MSE@proyears]>1,1:2,sum)/MSE@proyears*100
class(PGK)<-"ABT_PM"

#' Probability of Over-Fishing (F>FMSY) over all projected years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' POF(MSE_example)
POF<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+1:MSE@proyears]>1,1:2,sum)/MSE@proyears*100
class(POF)<-"ABT_PM"

#' Probability of Over-Fished status (B<BMSY) over all projected years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' POFed(MSE_example)
POFed<-function(MSE,pp=1) apply(MSE@B_BMSY[,,pp,MSE@nyears+1:MSE@proyears]<1,1:2,sum)/MSE@proyears*100
class(POFed)<-"ABT_PM"

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) for the first 10 projected years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' D10(MSE_example)
D10<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+1:10]/array(rep(MSE@SSB0proj[,pp,1:10],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,10))
  apply(D,1:2,mean)
}
class(D10)<-"ABT_PM"

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) for projected years 11-20 (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' D20(MSE_example)
D20<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+11:20]/array(rep(MSE@SSB0proj[,pp,11:20],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,10))
  apply(D,1:2,mean)
}
class(D20)<-"ABT_PM"

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) for projected years 21-30 (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' D30(MSE_example)
D30<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+21:30]/array(rep(MSE@SSB0proj[,pp,21:30],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,10))
  apply(D,1:2,mean)
}
class(D30)<-"ABT_PM"

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) all projected years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' LD(MSE_example)
LD<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+1:MSE@proyears]/array(rep(MSE@SSB0proj[,pp,1:MSE@proyears],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,MSE@proyears))
  apply(D,1:2,min)
}
class(LD)<-"ABT_PM"

#' Relative SSB (SSB relative to zero fishing) in final projection year (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' RSSB(MSE_example)
RSSB<-function(MSE,pp=1)  MSE@SSB[,,pp,MSE@nyears+MSE@proyears]/array(rep(MSE@SSB[1,,pp,MSE@nyears+MSE@proyears],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim))
class(RSSB)<-"ABT_PM"

#' Relative SSB (SSB relative to zero fishing) over all projection years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' LRSSB(MSE_example)
LRSSB<-function(MSE,pp=1) apply(MSE@SSB[,,pp,MSE@nyears+1:MSE@proyears]/array(rep(MSE@SSB[1,,pp,MSE@nyears+1:MSE@proyears],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim,MSE@proyears)),1:2,min)
class(LRSSB)<-"ABT_PM"

#' Average Annual Variability in Yield over the first 30 projection years (a performance metrics function of class ABT_PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' AAVY(MSE_example)
AAVY<-function(MSE,pp=1){
  ind1<-MSE@nyears+1:MSE@proyears-1
  ind<-MSE@nyears+1:MSE@proyears
  apply(((MSE@C[,,pp,ind]-MSE@C[,,pp,ind1])^2)^0.5/MSE@C[,,pp,ind1],1:2,mean)
}
class(AAVY)<-"ABT_PM"
