# ==============================================================================
# === Functions for iterative reweighting of M3 models =========================
# ==============================================================================

# May 12th 2018

CalcCVfac<-function(out,OMI){

  Ccv<-sd(log(out$Cobs[,5])-log(out$Cpred[out$Cobs[,1:4]]))
  Cspec<-OMI@CobsCV[1]
  Cfac<-Ccv/Cspec

  CPUEres<-log(OMI@CPUEobs[,6])-log(out$CPUEpred_vec)
  CPUEcv<-aggregate(CPUEres,list(OMI@CPUEobs[,4]),sd)$x
  CPUEspec<-OMI@CPUEobsCV
  CPUEfac<-mean(CPUEcv/CPUEspec)

  Ires<-log(OMI@Iobs[,7])-log(out$Ipred_vec)
  Icv<-aggregate(Ires,list(OMI@Iobs[,5]),sd)$x
  Ispec<-OMI@IobsCV
  Ifac<-mean(Icv/Ispec)

  #log(CLprop(i)+tiny),log(CLobs(i,6)),sqrt(0.01/CLobs(i,6))

  CLres<-log(OMI@CLobs[,6])-log(out$CLprop)
  CLspec_byobs<-(0.01/OMI@CLobs[,6])^0.5
  CLfac<-sd(CLres/CLspec_byobs)

  #propbins<-floor(OMI@CLobs[,6]*5)/5
  #CLcv<-aggregate(CLres,list(propbins),sd)$x
  #CLspec<-aggregate(CLspec_byobs,list(propbins),mean)$x
  #CLfac<-mean(CLcv/CLspec)

  SOOres<-OMI@SOOobs[,6]-out$SOOpred
  SOOfac<-sd(SOOres/OMI@SOOobs[,7])

  PSATres<-log(OMI@PSAT[,8]/out$PSATpred)
  PSATfac<-sd(PSATres)/1

  ind<- rep(c(T,F),100)[1:OMI@ny]
  RDcv<-sd(out$lnRD[,ind])
  RDspec<-OMI@RDCV
  RDfac<-RDcv/RDspec

  Fmodfac<-sd(out$Fmod)/OMI@FCV

  FDYfac<-sd(out$FDYt)/OMI@MICV

  list(Cfac=Cfac, CPUEfac=CPUEfac, Ifac=Ifac, CLfac=CLfac, SOOfac=SOOfac,
       PSATfac=PSATfac, RDfac=RDfac, Fmodfac=Fmodfac, FDYfac=FDYfac)

}

IRW_bd<-function(out,OMIt){ # reweights CPUE, FI, CL, SOO

  OMI<-OMIt
  #Ccv<-sd(log(out$Cobs[,5])-log(out$Cpred[out$Cobs[,1:4]]))
  #Cspec<-OMI@CobsCV[1]
  #Cfac<-Ccv/Cspec

  CPUEres<-log(OMI@CPUEobs[,6])-log(out$CPUEpred_vec)
  CPUEcv<-aggregate(CPUEres,list(OMI@CPUEobs[,4]),sd)$x
  CPUEspec<-aggregate(OMI@CPUEobs[,8],list(OMI@CPUEobs[,4]),mean)$x
  CPUEfac<-CPUEcv/CPUEspec
  CPUEfac[CPUEfac<1]<-1
  qno<-OMI@CPUEobs[,4]
  nuwt<-1/(OMI@CPUEobs[,8]*CPUEfac[qno])^2
  #cbind(OMI@CPUEobs,nuwt)
  OMI@CPUEobs[,9]<-nuwt

  Ires<-log(OMI@Iobs[,7])-log(out$Ipred_vec)
  Icv<-aggregate(Ires,list(OMI@Iobs[,5]),sd)$x
  Ispec<-aggregate(OMI@Iobs[,8],list(OMI@Iobs[,5]),mean)$x
  Ifac<-Icv/Ispec
  Ifac[Ifac<1]<-1
  Ino<-OMI@Iobs[,5]
  nuwt<-1/(OMI@Iobs[,8]*Ifac[Ino])^2
  #cbind(OMI@Iobs,nuwt)
  OMI@Iobs[,9]<-nuwt

  #log(CLprop(i)+tiny),log(CLobs(i,6)),sqrt(0.01/CLobs(i,6))

  CLres<-log(OMI@CLobs[,6])-log(out$CLprop)
  CLcv<-aggregate(CLres,list(OMI@CLobs[,4]),sd)$x
  speced<-(0.01/OMI@CLobs[,6])^0.5
  CLspec<-aggregate(speced,list(OMI@CLobs[,4]),mean)$x
  CLfac<-CLcv/CLspec
  CLfac[CLfac<1]<-1
  Fno<-OMI@CLobs[,4]
  nuwt<-1/(speced*CLfac[Fno])^2
  #cbind(OMI@CLobs,nuwt)
  OMI@CLobs[,7]<-nuwt

  #propbins<-floor(OMI@CLobs[,6]*5)/5
  #CLcv<-aggregate(CLres,list(propbins),sd)$x
  #CLspec<-aggregate(CLspec_byobs,list(propbins),mean)$x
  #CLfac<-mean(CLcv/CLspec)

  SOOres<-OMI@SOOobs[,6]-out$SOOpred
  SOOcv<-aggregate(SOOres,list(OMI@SOOobs[,8]),sd)$x
  SOOspec<-aggregate(OMI@SOOobs[,7],list(OMI@SOOobs[,8]),mean)$x
  SOOfac<-SOOcv/SOOspec
  SOOfac[SOOfac<1]<-1
  tno<-OMI@SOOobs[,8]
  nuwt<-1/(OMI@SOOobs[,7]*SOOfac[tno])^2
  #cbind(OMI@SOOobs,nuwt)
  OMI@SOOobs[,9]<-nuwt

  OMI

}


CalcCVs<-function(out,OMI){

  Ccv<-sd(log(out$Cobs[,5])-log(out$Cpred[out$Cobs[,1:4]]))

  CPUEres<-log(OMI@CPUEobs[,6])-log(out$CPUEpred_vec)
  CPUEcv<-aggregate(CPUEres,list(OMI@CPUEobs[,4]),sd)$x

  Ires<-log(OMI@Iobs[,7])-log(out$Ipred_vec)
  Icv<-aggregate(Ires,list(OMI@Iobs[,5]),sd)$x

  CLres<-log(OMI@CLobs[,6])-log(out$CLprop)
  CLres[CLres==Inf]<-NA
  #CLspec_byobs<-(0.01/OMI@CLobs[,6])^0.5
  CLcv<-sd(CLres,na.rm=T)

  SOOres<-OMI@SOOobs[,6]-out$SOOpred
  SOOcv<-sd(SOOres)   #/OMI@SOOobs[,7])

  PSATres<-log(OMI@PSAT[,8]/out$PSATpred)
  PSATcv<-sd(PSATres)

  ind<- rep(c(T,F),100)[1:OMI@ny]
  RDcv<-sd(out$lnRD[,ind])

  Fmodcv<-sd(out$Fmod)
  FDYcv<-sd(out$FDYt)

  list(Ccv=Ccv, CPUEcv=CPUEcv, Icv=Icv, CLcv=CLcv, SOOcv=SOOcv,
       PSATcv=PSATcv, RDcv=RDcv, Fmodcv=Fmodcv, FDYcv=FDYcv)

}


