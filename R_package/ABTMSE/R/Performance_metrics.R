# ======================================================================================================================
# ==== ABT MSE Diagnostics =============================================================================================
# ======================================================================================================================


# === Average catches over all years =======================

#' Mean  catches over first 30 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' AvC30(MSE_example)
AvC30<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+1:30],1:2,mean,na.rm=T)
class(AvC30)<-"PM"

# === Average catches over all years =======================

#' Relative depletion (spawning biomass relative to dynamic B0) after 30 years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' RD30(MSE_example)
RD30<-function(MSE,pp=1){
  nMP<-dim(MSE@SSB)[1]
  nsim<-dim(MSE@SSB)[2]
  yr<-MSE@nyears+MPlag+30
  MSE@SSB[,,pp,yr]/array(rep(MSE@dynB0[,pp,yr-MSE@nyears],each=nMP),c(nMP,nsim))
}
class(RD30)<-"PM"


#' Relative depletion (spawning biomass relative to dynamic B0) after 15 years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' RD15(MSE_example)
RD15<-function(MSE,pp=1){
  nMP<-dim(MSE@SSB)[1]
  nsim<-dim(MSE@SSB)[2]
  yr<-MSE@nyears+MPlag+15
  MSE@SSB[,,pp,yr]/array(rep(MSE@dynB0[,pp,yr-MSE@nyears],each=nMP),c(nMP,nsim))
}
class(RD15)<-"PM"



# === (a) Annual average catch ============================================

#' Mean  catches over the first 10 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C10(MSE_example)
C10<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+1:10],1:2,mean,na.rm=T)
class(C10)<-"PM"

#' Mean catches over projected years 11-20 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C20(MSE_example)
C20<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+11:20],1:2,mean,na.rm=T)
class(C20)<-"PM"

#' Mean catches over projected years 21-30 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C30(MSE_example)
C30<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+21:30],1:2,mean,na.rm=T)
class(C30)<-"PM"


# === (b) Spawning biomass depletion (means) ==================================

#' Depletion (spawning biomass relative to dynamic B0) after the first 10 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' D10(MSE_example)
D10<-function(MSE,pp=1){
  MSE@SSB[,,pp,MSE@nyears+MPlag+10]/array(rep(MSE@dynB0[,pp,MPlag+10],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim))
  #apply(D,1:2,mean)
}
class(D10)<-"PM"

#' Depletion (spawning biomass relative to dynamic B0) after projection year 20 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' D20(MSE_example)
D20<-function(MSE,pp=1){
  MSE@SSB[,,pp,MSE@nyears+MPlag+20]/array(rep(MSE@dynB0[,pp,MPlag+20],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim))
  #apply(D,1:2,mean)
}
class(D20)<-"PM"

#' Depletion (spawning biomass relative to dynamic B0) after projection yeare 30 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' D30(MSE_example)
D30<-function(MSE,pp=1){
  MSE@SSB[,,pp,MSE@nyears+MPlag+30]/array(rep(MSE@dynB0[,pp,MPlag+30],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim))
}
class(D30)<-"PM"

#' Depletion (spawning biomass relative to dynamic BMSY) after projection yeare 30 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' Br30(MSE_example)
Br30<-function(MSE,pp=1){
  MSE@SSB[,,pp,MSE@nyears+MPlag+30]/array(rep(MSE@dynB0[,pp,MPlag+30]*MSE@SSBMSY_SSB0[,pp],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim))
}
class(Br30)<-"PM"


#' Depletion (spawning biomass relative to dynamic B0) all projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' LD(MSE_example)
LD<-function(MSE,pp=1){
  D<-MSE@SSB[,,pp,MSE@nyears+MPlag+1:30]/array(rep(MSE@dynB0[,pp,MPlag+1:30],each=MSE@nMPs),c(MSE@nMPs,MSE@nsim,30))
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
DNC<-function(MSE,pp=1)  MSE@SSB[,,pp,MSE@nyears+MPlag+30]/array(rep(MSE@SSB[1,,pp,MSE@nyears+MPlag+30],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim))
class(DNC)<-"PM"


# === (e) Lowest spawning biomass depletion over 30 years calculated relative to zero catch =========

#' Relative SSB (SSB relative to zero fishing) over all projection years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' LDNC(MSE_example)
LDNC<-function(MSE,pp=1) apply(MSE@SSB[,,pp,MSE@nyears+MPlag+1:30]/array(rep(MSE@SSB[1,,pp,MSE@nyears+MPlag+1:30],each=MSE@nMPs),dim=c(MSE@nMPs,MSE@nsim,30)),1:2,min)
class(LDNC)<-"PM"


# === (f) Kobe metrics ==============================================

#' Probability of being in the Green Kobe region (F<FMSY AND B>BMSY) over 30 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' PGK(MSE_example)
PGK<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+MPlag+1:30]<1 & MSE@B_BMSY[,,pp,MSE@nyears+MPlag+1:30]>1,1:2,mean,na.rm=T)*100
class(PGK)<-"PM"

#' Probability of Over-Fishing (F>FMSY) over 30 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' POF(MSE_example)
POF<-function(MSE,pp=1) apply(MSE@F_FMSY[,,pp,MSE@nyears+MPlag+1:30]>1,1:2,mean,na.rm=T)*100
class(POF)<-"PM"

#' Probability of Over-Fished status (B<BMSY) after 30 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' POS(MSE_example)
POS<-function(MSE,pp=1) apply(MSE@B_BMSY[,,pp,MSE@nyears+MPlag+1:30]<1,1:2,mean,na.rm=T)*100
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
  ind<-MSE@nyears+MPlag+(((1:floor(30/3))*3)-2)
  ind1<-ind[1:(length(ind)-1)] # first update is after three years
  ind<-ind[2:length(ind)]
  C<-MSE@C[,,pp,]
  C[C==0]<-tiny

  apply(abs(C[,,ind]-C[,,ind1])/C[,,ind1],1:2,quantile,p=0.5,na.rm=T)*100
}
class(AAVC)<-"PM"



