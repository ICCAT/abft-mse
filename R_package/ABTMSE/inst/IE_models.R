
# ===============================================================================
# Implementation error models ===================================================
# ===============================================================================

# A simple maximum F implementation error
Umax<-function(U,maxU=0.3){
  U[U>maxU]<-maxU
  U
}
class(Umax)<-"ABT_IE"

Overage<-function(U,over=0.2,maxU=0.3){
  U<-U*(1+over)
  U[U>maxU]<-maxU
  U
}
class(Overage)<-"ABT_IE"