
# ===================================================
# = multistock MPs ==================================
# ===================================================

#setwd("C:/Users/tcar_/Dropbox/abft-mse/RScripts/Debugging")
#myMSE<-new('MSE',OM_1)
#save(dset,file="dset.rda")
#load("dset.rda")

#' Combined Stock Management Procedure.
#'
#' @param x a simulation number.
#' @param dset a list with two positions (1:East  2:West) of simulated data for use by management procedures.
#' @param AS the management area for which advice is being provided (1:East 2:West)
#' @param Balpha degree of TAC throttling with declining biomass
#' @param Bbeta non-linearity in TAC throttling with declining biomass
#' @param Falpha target fraction of F relative to FMSY
#' @param Fbeta degree of TAC throttling with increasing F
#' @param i1CR Catch rate index for stock 1 (East)
#' @param i1SUV Survey index for stock 1 (East)
#' @param i2CR Catch rate index for stock 2 (West)
#' @param i2SUV Survey index for stock 2 (West)
#' @param theta Assumed mixing matrix from Stock (row, E/W) to Area (column, E/W)
#' @return a TAC recommendation arising from \code{x, dset}.
#' @export
#' @examples
#' ZeroC(1,dset_example_West)
#' sapply(1:10,ZeroC,dset_example_West)
CSMP<-function(x,dset,AS,Balpha=1,Bbeta=2,Falpha=0.8,Fbeta=0.5,
               i1CR=1,i1SUV=3,i2CR=6,i2SUV=7,theta=matrix(c(0.8,0.3,0.2,0.7),nrow=2)){
#              JPN_LL_NEATl2, MED_LAR_SUV, US_RR_66_144, GOM_LAR_SUV


  BMSYE<-308470*1000*2
  BMSYW<-20008 *1000*2
  FMSY=0.03
  SSBE<-0.507/0.376*(BMSYE/2)
  SSBW<-0.081/0.442*(BMSYW/2)
  SSBEa<-300000*1000
  SSBWa<-50000*1000

  nyears<-51

  q1SUV=SSBE/dset[[1]]$Iobs[x,i1SUV,nyears]
  q2SUV=SSBW/dset[[2]]$Iobs[x,i2SUV,nyears]
  q1CR=SSBEa/dset[[1]]$Iobs[x,i1CR,nyears]
  q2CR=SSBWa/dset[[2]]$Iobs[x,i2CR,nyears]

  thisyr<-length(dset[[1]]$Iobs[x,1,])

  BSUVE<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,1]+
         dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,1]

  BSUVW<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,2]+
         dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,2]

  BCRE<-dset[[1]]$Iobs[x,i1CR,thisyr]*q1CR
  BCRW<-dset[[2]]$Iobs[x,i2CR,thisyr]*q2CR

  lastTACyr<-length(dset[[1]]$TAC[x,])
  ETAC<-dset[[1]]$TAC[x,lastTACyr]
  WTAC<-dset[[2]]$TAC[x,lastTACyr]

  FSUVE<--log(1-ETAC/(ETAC+BSUVE))
  FSUVW<--log(1-WTAC/(WTAC+BSUVW))
  FCRE<--log(1-ETAC/(ETAC+BCRE))
  FCRW<--log(1-WTAC/(WTAC+BCRW))

  powdif<-function(x,z,g){
    x2<-(g*(((x)^2)^0.5))^z
    x2[x<0]<-(-x2[x<0])
    x2
  }

  dBE=exp(1/2*(log(BSUVE/BMSYE)+log(BCRE/BMSYE)))
  dBW=exp(1/2*(log(BSUVW/BMSYW)+log(BCRW/BMSYW)))

  dFE=exp(1/2*(log(FSUVE/FMSY)+log(FCRE/FMSY)))
  dFW=exp(1/2*(log(FSUVW/FMSY)+log(FCRW/FMSY)))

  if(AS==1){
    Frel<-dFE
    Brel<-dBE
    TAC<-ETAC
  }else{
    Frel=dFW
    Brel=dBW
    TAC=WTAC
  }

  Fresp<-exp(log(1/Frel)*Fbeta)
  Bresp<-exp(powdif(Brel-1,Bbeta,Balpha))
  TACadj<-Falpha*Bresp*Fresp
  TACadj*TAC


}
class(CSMP)<-"MSMP"



#' Combined Stock Management Procedure no mix.
#'
#' @param x a simulation number.
#' @param dset a list with two positions (1:East  2:West) of simulated data for use by management procedures.
#' @param AS the management area for which advice is being provided (1:East 2:West)
#' @param Balpha degree of TAC throttling with declining biomass
#' @param Bbeta non-linearity in TAC throttling with declining biomass
#' @param Falpha target fraction of F relative to FMSY
#' @param Fbeta degree of TAC throttling with increasing F
#' @param i1CR Catch rate index for stock 1 (East)
#' @param i1SUV Survey index for stock 1 (East)
#' @param i2CR Catch rate index for stock 2 (West)
#' @param i2SUV Survey index for stock 2 (West)
#' @param theta Assumed mixing matrix from Stock (row, E/W) to Area (column, E/W)
#' @return a TAC recommendation arising from \code{x, dset}.
#' @export
#' @examples
#' ZeroC(1,dset_example_West)
#' sapply(1:10,ZeroC,dset_example_West)
CSMP_N<-function(x,dset,AS,Balpha=1,Bbeta=2,Falpha=0.8,Fbeta=0.5,
               i1CR=1,i1SUV=3,i2CR=6,i2SUV=7,theta=matrix(c(1,0,0,1),nrow=2)){
  #              JPN_LL_NEATl2, MED_LAR_SUV, US_RR_66_144, GOM_LAR_SUV

  BMSYE<-308470*1000
  BMSYW<-20008 *1000
  FMSY=0.07
  SSBE<-0.507/0.376*BMSYE
  SSBW<-0.081/0.442*BMSYW
  SSBEa<-300000*1000
  SSBWa<-50000*1000

  nyears<-51

  q1SUV=SSBE/dset[[1]]$Iobs[x,i1SUV,nyears]
  q2SUV=SSBW/dset[[2]]$Iobs[x,i2SUV,nyears]
  q1CR=SSBEa/dset[[1]]$Iobs[x,i1CR,nyears]
  q2CR=SSBWa/dset[[2]]$Iobs[x,i2CR,nyears]

  thisyr<-length(dset[[1]]$Iobs[x,1,])

  BSUVE<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,1]+
    dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,1]

  BSUVW<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,2]+
    dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,2]

  BCRE<-dset[[1]]$Iobs[x,i1CR,thisyr]*q1CR
  BCRW<-dset[[2]]$Iobs[x,i2CR,thisyr]*q2CR

  lastTACyr<-length(dset[[1]]$TAC[x,])
  ETAC<-dset[[1]]$TAC[x,lastTACyr]
  WTAC<-dset[[2]]$TAC[x,lastTACyr]

  FSUVE<--log(1-ETAC/(ETAC+BSUVE))
  FSUVW<--log(1-WTAC/(WTAC+BSUVW))
  FCRE<--log(1-ETAC/(ETAC+BCRE))
  FCRW<--log(1-WTAC/(WTAC+BCRW))

  powdif<-function(x,z,g){
    x2<-(g*(((x)^2)^0.5))^z
    x2[x<0]<-(-x2[x<0])
    x2
  }

  dBE=exp(1/2*(log(BSUVE/BMSYE)+log(BCRE/BMSYE)))
  dBW=exp(1/2*(log(BSUVW/BMSYW)+log(BCRW/BMSYW)))

  dFE=exp(1/2*(log(FSUVE/FMSY)+log(FCRE/FMSY)))
  dFW=exp(1/2*(log(FSUVW/FMSY)+log(FCRW/FMSY)))

  if(AS==1){
    Frel<-dFE
    Brel<-dBE
    TAC<-ETAC
  }else{
    Frel=dFW
    Brel=dBW
    TAC=WTAC
  }

  Fresp<-exp(log(1/Frel)*Fbeta)
  Bresp<-exp(powdif(Brel-1,Bbeta,Balpha))
  TACadj<-Falpha*Bresp*Fresp
  TACadj*TAC

}
class(CSMP_N)<-"MSMP"


CSMP_F<-function(x,dset,AS,Balpha=1,Bbeta=2,Falpha=0.8,Fbeta=0.5,
               i1CR=1,i1SUV=3,i2CR=6,i2SUV=7,theta=matrix(c(0.6,0.4,0.4,0.6),nrow=2)){
  #              JPN_LL_NEATl2, MED_LAR_SUV, US_RR_66_144, GOM_LAR_SUV

  BMSYE<-308470*1000
  BMSYW<-20008 *1000
  FMSY=0.07
  SSBE<-0.507/0.376*BMSYE
  SSBW<-0.081/0.442*BMSYW
  SSBEa<-300000*1000
  SSBWa<-50000*1000

  nyears<-51

  q1SUV=SSBE/dset[[1]]$Iobs[x,i1SUV,nyears]
  q2SUV=SSBW/dset[[2]]$Iobs[x,i2SUV,nyears]
  q1CR=SSBEa/dset[[1]]$Iobs[x,i1CR,nyears]
  q2CR=SSBWa/dset[[2]]$Iobs[x,i2CR,nyears]

  thisyr<-length(dset[[1]]$Iobs[x,1,])

  BSUVE<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,1]+
    dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,1]

  BSUVW<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,2]+
    dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,2]

  BCRE<-dset[[1]]$Iobs[x,i1CR,thisyr]*q1CR
  BCRW<-dset[[2]]$Iobs[x,i2CR,thisyr]*q2CR

  lastTACyr<-length(dset[[1]]$TAC[x,])
  ETAC<-dset[[1]]$TAC[x,lastTACyr]
  WTAC<-dset[[2]]$TAC[x,lastTACyr]

  FSUVE<--log(1-ETAC/(ETAC+BSUVE))
  FSUVW<--log(1-WTAC/(WTAC+BSUVW))
  FCRE<--log(1-ETAC/(ETAC+BCRE))
  FCRW<--log(1-WTAC/(WTAC+BCRW))

  powdif<-function(x,z,g){
    x2<-(g*(((x)^2)^0.5))^z
    x2[x<0]<-(-x2[x<0])
    x2
  }

  dBE=exp(1/2*(log(BSUVE/BMSYE)+log(BCRE/BMSYE)))
  dBW=exp(1/2*(log(BSUVW/BMSYW)+log(BCRW/BMSYW)))

  dFE=exp(1/2*(log(FSUVE/FMSY)+log(FCRE/FMSY)))
  dFW=exp(1/2*(log(FSUVW/FMSY)+log(FCRW/FMSY)))

  if(AS==1){
    Frel<-dFE
    Brel<-dBE
    TAC<-ETAC
  }else{
    Frel=dFW
    Brel=dBW
    TAC=WTAC
  }

  Fresp<-exp(log(1/Frel)*Fbeta)
  Bresp<-exp(powdif(Brel-1,Bbeta,Balpha))
  TACadj<-Falpha*Bresp*Fresp
  TACadj*TAC

}
class(CSMP_F)<-"MSMP"




xMPi<-function(x,dset,AS,Balpha,Bbeta,Falpha,Fbeta,
               i1CR,i1SUV,i2CR,i2SUV,theta,
               BMSYE, BMSYW,
               FMSYE, FMSYW,
               SSBE , SSBW ,
               SSBEa, SSBWa ){
  #              JPN_LL_NEATl2, MED_LAR_SUV, US_RR_115_144, GOM_LAR_SUV
  nyears<-33
  q1SUV=SSBE/dset[[1]]$Iobs[x,i1SUV,nyears]
  q2SUV=SSBW/dset[[2]]$Iobs[x,i2SUV,nyears]
  q1CR=SSBEa/dset[[1]]$Iobs[x,i1CR,nyears]
  q2CR=SSBWa/dset[[2]]$Iobs[x,i2CR,nyears]

  thisyr<-length(dset[[1]]$Iobs[x,1,])

  BSUVE<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,1]+
    dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,1]

  BSUVW<-dset[[1]]$Iobs[x,i1SUV,thisyr]*q1SUV*theta[1,2]+
    dset[[2]]$Iobs[x,i2SUV,thisyr]*q2SUV*theta[2,2]

  BCRE<-dset[[1]]$Iobs[x,i1CR,thisyr]*q1CR
  BCRW<-dset[[2]]$Iobs[x,i2CR,thisyr]*q2CR

  lastTACyr<-length(dset[[1]]$TAC[x,])
  ETAC<-dset[[1]]$TAC[x,lastTACyr]
  WTAC<-dset[[2]]$TAC[x,lastTACyr]

  FSUVE<--log(1-ETAC/(ETAC+BSUVE))
  FSUVW<--log(1-WTAC/(WTAC+BSUVW))
  FCRE<--log(1-ETAC/(ETAC+BCRE))
  FCRW<--log(1-WTAC/(WTAC+BCRW))

  powdif<-function(x,z,g){
    x2<-(g*(((x)^2)^0.5))^z
    x2[x<0]<-(-x2[x<0])
    x2
  }

  dBE=exp(1/2*(log(BSUVE/BMSYE)+log(BCRE/BMSYE)))
  dBW=exp(1/2*(log(BSUVW/BMSYW)+log(BCRW/BMSYW)))

  dFE=exp(1/2*(log(FSUVE/FMSYE)+log(FCRE/FMSYE)))
  dFW=exp(1/2*(log(FSUVW/FMSYW)+log(FCRW/FMSYW)))

  if(AS==1){
    Frel<-dFE
    Brel<-dBE
    TAC<-ETAC
  }else{
    Frel=dFW
    Brel=dBW
    TAC=WTAC
  }

  Fresp<-exp(log(1/Frel)*Fbeta)
  Bresp<-exp(powdif(Brel-1,Bbeta,Balpha))
  TACadj<-Falpha*Bresp*Fresp
  TACadj*TAC

}



