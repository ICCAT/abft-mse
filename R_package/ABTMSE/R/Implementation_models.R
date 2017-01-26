
# ===============================================================================
# Implementation error models ===================================================
# ===============================================================================

#' TAC has a cap of 30per cent harvest rate (an implementation error function of class IE)
#'
#' @param U A numeric value, vector or array of harvest rates
#' @return an object of dimensions U with a harvest rate cap \code{U, maxU}
#' @examples
#' Umax_30((1:10)/10)
Umax_30<-function(U,maxU=0.3){
  U[U>maxU]<-maxU
  U
}
class(Umax_30)<-"IE"

#' TAC has a cap of 50per cent harvest rate (an implementation error function of class IE)
#'
#' @param U A numeric value, vector or array of harvest rates
#' @return an object of dimensions U with a harvest rate cap \code{U, maxU}
#' @examples
#' Umax_50((1:10)/10)
Umax_50<-function(U,maxU=0.5){
  U[U>maxU]<-maxU
  U
}
class(Umax_50)<-"IE"

#' TAC has a cap of 90per cent harvest rate (an implementation error function of class IE)
#'
#' @param U A numeric value, vector or array of harvest rates
#' @return an object of dimensions U with a harvest rate cap \code{U, maxU}
#' @examples
#' Umax_90((1:10)/10)
Umax_90<-function(U,maxU=0.9){
  U[U>maxU]<-maxU
  U
}
class(Umax_90)<-"IE"

#' TAC overages of 10 per cent are simulated up to a maximum harvest rate of 90per cent (an implementation error function of class IE)
#'
#' @param U A numeric value, vector or array of harvest rates
#' @return an object of dimensions U with a harvest rate cap \code{U, over, maxU}
#' @examples
#' Overage_10((1:10)/10)
Overage_10<-function(U,over=0.1,maxU=0.9){
  U<-U*(1+over)
  U[U>maxU]<-maxU
  U
}
class(Overage_10)<-"IE"

#' TAC overages of 20 per cent are simulated up to a maximum harvest rate of 90per cent (an implementation error function of class IE)
#'
#' @param U A numeric value, vector or array of harvest rates
#' @return an object of dimensions U with a harvest rate cap \code{U, over, maxU}
#' @examples
#' Overage_20((1:10)/10)
Overage_20<-function(U,over=0.2,maxU=0.9){
  U<-U*(1+over)
  U[U>maxU]<-maxU
  U
}
class(Overage_20)<-"IE"

#' TAC overages of 30 per cent are simulated up to a maximum harvest rate of 90per cent (an implementation error function of class IE)
#'
#' @param U A numeric value, vector or array of harvest rates
#' @return an object of dimensions U with a harvest rate cap \code{U, over, maxU}
#' @examples
#' Overage_30((1:10)/10)
Overage_30<-function(U,over=0.3,maxU=0.9){
  U<-U*(1+over)
  U[U>maxU]<-maxU
  U
}
class(Overage_30)<-"IE"
