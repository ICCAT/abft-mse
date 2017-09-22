# ======================================================================================================================
# ==== ABT MSE Diagnostics =============================================================================================
# ======================================================================================================================


# === (a) Annual average catch ============================================

#' Mean  catches over the first 10 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C10(MSE_example)
C10<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+1:10],1:2,mean)
class(C10)<-"PM"

#' Mean catches over projected years 11-20 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C20(MSE_example)
C20<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+11:20],1:2,mean)
class(C20)<-"PM"

#' Mean catches over projected years 21-30 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C30(MSE_example)
C30<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+21:30],1:2,mean)
class(C30)<-"PM"


# === (b) Spawning biomass depletion (means) ==================================

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) for the first 10 projected years (a performance metrics function of class PM)
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
class(D10)<-"PM"

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) for projected years 11-20 (a performance metrics function of class PM)
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
class(D20)<-"PM"

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) for projected years 21-30 (a performance metrics function of class PM)
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
class(D30)<-"PM"


# === (c) Lowest depletion =================================================

#' Depletion (SSB relative to unfished SSB given current recruitment dynamics) all projected years (a performance metrics function of class PM)
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
class(LD)<-"PM"


# === (d) Spawning biomass after 30 years relative to zero catches ========

#' Relative SSB (SSB relative to zero fishing) in final projection year (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' DNC(MSE_example)
DNC<-function(MSE,pp=1)  MSE@SSB[,,pp,MSE@nyears+MSE@proyears]/array(rep(MSE@SSB[1,,pp,MSE@nyears+MSE@proyears],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim))
class(DNC)<-"PM"


# === (e) Lowest spawning biomass depletion over 30 years calculated relative to zero catch =========

#' Relative SSB (SSB relative to zero fishing) over all projection years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' LDNC(MSE_example)
LDNC<-function(MSE,pp=1) apply(MSE@SSB[,,pp,MSE@nyears+1:MSE@proyears]/array(rep(MSE@SSB[1,,pp,MSE@nyears+1:MSE@proyears],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim,MSE@proyears)),1:2,min)
class(LDNC)<-"PM"


# === (f) Kobe metrics ==============================================

#' Probability of being in the Green Kobe region (F<FMSY AND B>BMSY) over 30 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' PGK(MSE_example)
PGK<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+1:30]<1 & MSE@B_BMSY[,,pp,MSE@nyears+1:30]>1,1:2,mean)*100
class(PGK)<-"PM"

#' Probability of Over-Fishing (F>FMSY) over 30 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' POF(MSE_example)
POF<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+1:30]>1,1:2,mean)*100
class(POF)<-"PM"

#' Probability of Over-Fished status (B<BMSY) after 30 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' POS(MSE_example)
POS<-function(MSE,pp=1) apply(MSE@B_BMSY[,,pp,MSE@nyears+1:30]<1,1:2,mean)/MSE@proyears*100
class(POS)<-"PM"


# === (g) Variation in catches ======================================

#' Average Annual Variability in Yield over the first 30 projection years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' AAVC(MSE_example)
AAVC<-function(MSE,pp=1){
  ind1<-MSE@nyears+1:MSE@proyears-1
  ind<-MSE@nyears+1:MSE@proyears
  C<-MSE@C[,,pp,]
  C[C==0]<-tiny
  apply(((C[,,ind]-C[,,ind1])^2)^0.5/C[,,ind1],1:2,mean,na.rm=T)*100
}
class(AAVC)<-"PM"
