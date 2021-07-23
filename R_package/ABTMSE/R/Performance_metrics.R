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
AvC30<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+1:30],1:2,mean,na.rm=T)/1E6
class(AvC30)<-"PM"


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
C10<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+1:10],1:2,mean,na.rm=T)/1E6
class(C10)<-"PM"

#' Mean catches over projected years 11-20 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C20(MSE_example)
C20<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+11:20],1:2,mean,na.rm=T)/1E6
class(C20)<-"PM"

#' Mean catches over projected years 21-30 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' C30(MSE_example)
C30<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+21:30],1:2,mean,na.rm=T)/1E6
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
  interval=2
  ind<-MSE@nyears+MPlag+(((1:floor(30/2))*2)-1)
  ind1<-ind[1:(length(ind)-1)] # first update is after three years
  ind<-ind[2:length(ind)]
  C<-MSE@C[,,pp,]
  C[C==0]<-tiny

  apply(abs(C[,,ind]-C[,,ind1])/C[,,ind1],1:2,mean,na.rm=T)*100
}
class(AAVC)<-"PM"


# ================new metrics  =========================


#' Probability Good trend: 1 minus probability of negative trend (Br31 – Br35) and Br30 is less than 1 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix of 1's and zeros, n Management procedures (MSE@nMP) rows and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' PGT(myMSE2)
PGT<-function(MSE,pp=1){


  slp3<-function(y){

    y<-y[!is.na(y)]
    y<-log(y)
    x1<-1:length(y)
    mux<-mean(x1)
    muy<-mean(y,na.rm=T)
    SS<-sum((x1-mux)^2,na.rm=T)
    (1/SS)*sum((x1-mux)*(y-muy),na.rm=T)

  }

  SSB<-MSE@SSB[,,pp,MSE@nyears+MPlag+(30:35)]
  dynB0<-MSE@dynB0[,pp,MPlag+30:35]*MSE@SSBMSY_SSB0[,pp]
  Brs<-array(NA,dim(SSB))
  ind<-TEG(dim(SSB))
  Brs[ind]<-SSB[ind]/dynB0[ind[,c(2,3)]]

  array(as.numeric(!(apply(Brs[,,2:6],1:2,slp3)<0 & Brs[,,1]<1)),dim(SSB)[1:2]) # 1 minus probability of negative trend (Br31 – Br35) and Br30 is less than 1.

}
class(PGT)<-"PM"


#' Mean  catches over first 10 projected years (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix with n Management procedures (MSE@nMP) and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' AvC10(MSE_example)
AvC10<-function(MSE,pp=1) apply(MSE@C[,,pp,MSE@nyears+MPlag+1:10],1:2,mean,na.rm=T)/1E6
class(AvC10)<-"PM"


#' Average Br (spawning biomass relative to dynamic SSBMSY) over projection years 11-30 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix n Management procedures (MSE@nMP) rows and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' AvgBr(myMSE2)
AvgBr<-function(MSE,pp=1){

  SSB<-MSE@SSB[,,pp,MSE@nyears+MPlag+(11:30)]
  dynB0<-MSE@dynB0[,pp,MPlag+11:30]*MSE@SSBMSY_SSB0[,pp]
  Brs<-array(NA,dim(SSB))
  ind<-TEG(dim(SSB))
  Brs[ind]<-SSB[ind]/dynB0[ind[,c(2,3)]]
  apply(Brs,1:2,mean)

}
class(AvgBr)<-"PM"



# ================newest metrics  =========================


#' Over-fished Trend, the slope in SSB at the origin Good trend: 1 minus probability of negative trend (Br31 – Br35) and Br30 is less than 1 (a performance metrics function of class PM)
#'
#' @param MSE An object of class MSE
#' @return a matrix of 1's and zeros, n Management procedures (MSE@nMP) rows and nsim (MSE@nsim) columns from \code{MSE}
#' @examples
#' loadABT()
#' OFT(myMSE2)
OFT<-function(MSE,pp=1){

  slp3<-function(y){

    y<-y[!is.na(y)]
    y<-log(y)
    x1<-1:length(y)
    mux<-mean(x1)
    muy<-mean(y,na.rm=T)
    SS<-sum((x1-mux)^2,na.rm=T)
    (1/SS)*sum((x1-mux)*(y-muy),na.rm=T)

  }

  SSB<-MSE@SSB[,,pp,MSE@nyears+MPlag+(30:35)]
  dynB0<-MSE@dynB0[,pp,MPlag+30:35]*MSE@SSBMSY_SSB0[,pp]
  Brs<-array(NA,dim(SSB))
  ind<-TEG(dim(SSB))
  Brs[ind]<-SSB[ind]/dynB0[ind[,c(2,3)]]
  out<-apply(Brs[,,2:6],1:2,slp3)
  out[Brs[,,1]>1]<-0.1
  out[out>0]<-0.1
  out

}
class(OFT)<-"PM"


