
# ===============================================================================================================
# === Management Procedures for ABT MSE==========================================================================
# ===============================================================================================================

#' No catches (actually very small catches) (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' ZeroC(1,dset_example_West)
#' sapply(1:10,ZeroC,dset_example_West)
ZeroC <- function(x,dset)mean(dset$Cobs[x,],na.rm=T)*1E-10
class(ZeroC)<-"MP"



#' Example Management Procedure 1 west using the GOM_LAR_SUV
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' EMP1w(1,dset_example_West)
#' sapply(1:10,EMP1w,dset_example_West)
EMP1w <- function(x,dset,Jtarg=0.66,TACadj=0.1,thresh=0.4){

  ny<-dim(dset$Iobs)[3]                          # Last year of index observations
  cury<-dim(dset$TAC)[2]                         # Last year of past TAC recommendations
  Jratio<-mean(dset$Iobs[x,7,(-4:0)+ny])/Jtarg  # Index 7 is the GOM_LAR_SUV

  if(Jratio>(1-thresh) & Jratio<(1+thresh)){
    rec=dset$TAC[x,cury]
  }else if(Jratio<(1-thresh)){
    rec=dset$TAC[x,cury]*(1-TACadj)
  }else{
    rec=dset$TAC[x,cury]*(1+TACadj)
  }

  rec
}
class(EMP1w)<-"MP"



#' Example Management Procedure 1 east using the JP_LL_NE
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' EMP1e(1,dset_example_West)
#' sapply(1:10,EMP1e,dset_example_West)
EMP1e <- function(x,dset,Jtarg=4.8,TACadj=0.1,thresh=0.4){
  ny<-dim(dset$Iobs)[3]
  cury<-dim(dset$TAC)[2]
  Jratio<-mean(dset$Iobs[x,1,(-4:0)+ny])/Jtarg  # Index 1 is the JPN_LL_NEAtl
  if(Jratio>(1-thresh) & Jratio<(1+thresh)){
    rec=dset$TAC[x,cury]
  }else if(Jratio<(1-thresh)){
    rec=dset$TAC[x,cury]*(1-TACadj)
  }else{
    rec=dset$TAC[x,cury]*(1+TACadj)
  }
  rec
}
class(EMP1e)<-"MP"


#' Example Management Procedure 2 west.
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' EMP2w(1,dset_example_West)
#' sapply(1:10,EMP2w,dset_example_West)
EMP2w <- function(x,dset,Jtarg=0.66,lup=0.05,ldown=0.15,pup=0.05,pdown=0.15){

  ny<-dim(dset$Iobs)[3]
  cury<-dim(dset$TAC)[2]
  Ind<-dset$Iobs[x,7,(-5:0)+ny]
  slp<-lm(y~x,data=data.frame(y=log(Ind),x=1:6))$coefficients[2]
  Jratio<-mean(dset$Iobs[x,7,(-4:0)+ny])/Jtarg  # Index 7 is the GOM_LAR_SUV
  oldTAC<-dset$TAC[x,cury]

  if(slp>0){
    smod<-lup*slp
  }else{
    smod<-ldown*slp
  }

  if(Jratio>1){
    Jmod<-pup*(Jratio-1)
  }else{
    Jmod<-pdown*(Jratio-1)
  }

  Tmod<-Jmod+smod

  if(Tmod>0.15)Tmod=0.15
  if(Tmod<(-0.15))Tmod=-0.15

  oldTAC*(1+Tmod)

}
class(EMP2w)<-"MP"

#' Example Management Procedure 2 east.
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' EMP2e(1,dset_example_West)
#' sapply(1:10,EMP2e,dset_example_West)
EMP2e <- function(x,dset,Jtarg=4.8,lup=0.05,ldown=0.15,pup=0.05,pdown=0.15){

  ny<-dim(dset$Iobs)[3]
  cury<-dim(dset$TAC)[2]
  Ind<-dset$Iobs[x,1,(-5:0)+ny]
  slp<-lm(y~x,data=data.frame(y=log(Ind),x=1:6))$coefficients[2]
  Jratio<-mean(dset$Iobs[x,1,(-4:0)+ny])/Jtarg  # Index 1 is the JPN_LL_NEAtl2
  oldTAC<-dset$TAC[x,cury]

  if(slp>0){
    smod<-lup*slp
  }else{
    smod<-ldown*slp
  }

  if(Jratio>1){
    Jmod<-pup*(Jratio-1)
  }else{
    Jmod<-pdown*(Jratio-1)
  }

  Tmod<-Jmod+smod

  if(Tmod>0.15)Tmod=0.15
  if(Tmod<(-0.15))Tmod=-0.15

  oldTAC*(1+Tmod)

}
class(EMP2e)<-"MP"

#' Current catches x 1.5 (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' CurC150(1,dset_example_West)
#' sapply(1:10,CurC150,dset_example_West)
CurC150 <- function(x,dset)dset$TAC[x,1]*1.5
class(CurC150)<-"MP"


#' Current catches (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' CurC100(1,dset_example_West)
#' sapply(1:10,CurC100,dset_example_West)
CurC100 <- function(x,dset)dset$TAC[x,1]
class(CurC100)<-"MP"



#' Half current catches (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' CurC50(1,dset_example_West)
#' sapply(1:10,CurC50,dset_example_West)
CurC50 <- function(x,dset)dset$TAC[x,1]*0.5
class(CurC50)<-"MP"



#' Fish at MSY harvest rate with imperfect information regarding UMSY and current biomass (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' UMSY(1,dset_example_West)
#' sapply(1:10,UMSY,dset_example_West)
UMSY<-function(x,dset,Urat=1) dset$UMSY[x]*dset$Bt[x]*Urat
class(UMSY)<-"MP"

#' Fish at MSY harvest rate with perfect information of UMSY and current biomass (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' UMSY_PI(1,dset_example_West)
#' sapply(1:10,UMSY_PI,dset_example_West)
UMSY_PI<-function(x,dset) dset$UMSY_PI[x]*dset$Bt_PI[x]
class(UMSY_PI)<-"MP"

#' A rapid 3 parameter observation error only delay difference model FOR INDEX 4 conditioned on effort and parameterized with UMSY and MSY leading (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param checkfit logical: should fitting diagnostics be plotted
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' DD_i4(1,dset_example_West)
#' sapply(1:10,DD_i4,dset_example_West)
DD_i4<-function(x,dset,checkfit=F) DD(x,dset,startD=0.2,ii=4,checkfit=checkfit)
class(DD_i4)<-"MP"

#' A rapid 3 parameter observation error only delay difference model FOR INDEX 2 conditioned on effort and parameterized with UMSY and MSY leading (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param checkfit logical: should fitting diagnostics be plotted
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' DD_i2(1,dset_example_West)
#' sapply(1:10,DD_i2,dset_example_West)
DD_i2<-function(x,dset,checkfit=F) DD(x,dset,startD=0.2,ii=2,checkfit=checkfit)
class(DD_i2)<-"MP"


#' A rapid 3 parameter observation error only delay difference model linked to 40-10 harvest control rule FOR INDEX 4 conditioned on effort and parameterized with UMSY and MSY leading (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param checkfit logical: should fitting diagnostics be plotted
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' DD_i4_4010(1,dset_example_West)
#' sapply(1:10,DD_i4_4010,dset_example_East)
DD_i4_4010<-function(x,dset,checkfit=F) DD(x,dset,startD=0.2,ii=4,checkfit=checkfit,fortyten=T)
class(DD_i4_4010)<-"MP"


#' A rapid 3 parameter observation error only delay difference model linked to 40-10 harvest control rule FOR INDEX 2 conditioned on effort and parameterized with UMSY and MSY leading (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param checkfit logical: should fitting diagnostics be plotted
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' DD_i2_4010(1,dset_example_East)
#' sapply(1:10,DD_i2_4010,dset_example_East)
DD_i2_4010<-function(x,dset,checkfit=F) DD(x,dset,startD=0.2,ii=2,checkfit=checkfit,fortyten=T)
class(DD_i2_4010)<-"MP"


DD<-function(x,dset,startD,ii,checkfit,fortyten=F){

  Linfc<-dset$Linf[x]
  Kc<-dset$K[x]
  t0c<-dset$t0[x]
  Mc<-sum((dset$M[x,]*exp(-cumsum(dset$M[x,])))/(sum(exp(-cumsum(dset$M[x,]))))) # M average weighted by survival
  ac<-dset$a[x]
  bc<-dset$b[x]
  nages<-dset$nages
  Winf=ac*Linfc^bc
  age<-1:nages
  la<-Linfc*(1-exp(-Kc*((age-t0c))))
  wa<-ac*la^bc
  a50V<-dset$ageM[x]

  ny<-dim(dset$Iobs)[3]
  I_hist<-dset$Iobs[x,ii,]
  years<-(1:ny)[!is.na(I_hist)]
  C_hist<-dset$Cobs[x,years]

  E_hist<-C_hist/I_hist[years]
  E_hist<-E_hist/mean(E_hist)
  ny_DD<-length(C_hist)
  params<-log(c(Mc,mean(C_hist,na.rm=T),Mc/2))
  k_DD<-ceiling(a50V)   # get age nearest to 50% vulnerability (ascending limb)  -------------
  k_DD[k_DD>nages/2]<-ceiling(nages/2)  # to stop stupidly high estimates of age at 50% vulnerability
  Rho_DD<-(wa[k_DD+2]-Winf)/(wa[k_DD+1]-Winf)
  Alpha_DD<-Winf*(1-Rho_DD)
  So_DD<-exp(-Mc) # get So survival rate
  wa_DD<-wa[k_DD]
  UMSYprior<-c(1-exp(-Mc*0.5),0.5)
  opt<-optim(params,DD_R,opty=1,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,
             ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,
             C_hist=C_hist,UMSYprior=UMSYprior,startD=startD,
             method="L-BFGS-B",
             lower=log(exp(params)/20),upper=log(exp(params)*20),
             hessian=TRUE)

  posdef<-sum(eigen(solve(opt$hessian))$values>0)==3 # is the hessian positive-definite, ie has convergence been achieved?

  if(checkfit){                            # Plot fit to catches for model testing
    fit<-DD_R(opt$par,opty=99,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior,startD=startD)

    plot(fit[,1],xlab='Model year',ylab="Catches",col='blue')
    lines(fit[,2],col='red')
    legend('topright',legend=c("Observed","Predicted"),text.col=c("blue","red"),bty='n')
    legend('topleft',legend=paste0("Converged: ",posdef),bty='n')
  }

  if(posdef){   # if model converged return new TAC
    if(fortyten){
      DD_R(opt$par,opty=2,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior,startD=startD)
    }else{
      DD_R(opt$par,opty=4,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior,startD=startD)
    }
  }else{         # otherwise return previous TAC subject to a 5 percent downward adjustment
    dset$MPrec[x]*0.95
  }

}


#' A delay difference model fitted to INDEX 2 linked to Justin Cooke's harvest control rule (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' CDD_i2(1,dset_example_West)
#' sapply(1:10,CDD_i2,dset_example_West)
CDD_i2<-function(x,dset)CDD(x,dset,ii=2)
class(CDD_i2)<-"MP"

#' A delay difference model fitted to INDEX 4 linked to Justin Cooke's harvest control rule (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' CDD_i4(1,dset_example_West)
#' sapply(1:10,CDD_i4,dset_example_West)
CDD_i4<-function(x,dset)CDD(x,dset,ii=4)
class(CDD_i4)<-"MP"

CDD<-function(x,dset,startD=0.2,ii){

  Linfc<-dset$Linf[x]
  Kc<-dset$K[x]
  t0c<-dset$t0[x]
  Mc<-sum((dset$M[x,]*exp(-cumsum(dset$M[x,])))/(sum(exp(-cumsum(dset$M[x,]))))) # M average weighted by survival
  ac<-dset$a[x]
  bc<-dset$b[x]
  nages<-dset$nages
  Winf=ac*Linfc^bc
  age<-1:nages
  la<-Linfc*(1-exp(-Kc*((age-t0c))))
  wa<-ac*la^bc
  a50V<-dset$ageM[x]

  ny<-dim(dset$Iobs)[3]
  I_hist<-dset$Iobs[x,ii,]  # !!!!!!!!!!!!!!!! INDEX
  years<-(1:ny)[!is.na(I_hist)]
  C_hist<-dset$Cobs[x,years]

  E_hist<-C_hist/I_hist[years]
  E_hist<-E_hist/mean(E_hist)
  ny_DD<-length(C_hist)
  params<-log(c(Mc,mean(C_hist,na.rm=T),Mc/2))
  k_DD<-ceiling(a50V)   # get age nearest to 50% vulnerability (ascending limb)  -------------
  k_DD[k_DD>nages/2]<-ceiling(nages/2)  # to stop stupidly high estimates of age at 50% vulnerability
  Rho_DD<-(wa[k_DD+2]-Winf)/(wa[k_DD+1]-Winf)
  Alpha_DD<-Winf*(1-Rho_DD)
  So_DD<-exp(-Mc) # get So survival rate
  wa_DD<-wa[k_DD]
  UMSYprior<-c(1-exp(-Mc*0.5),0.5)
  opt<-optim(params,DD_R,opty=1,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,
             ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,
             C_hist=C_hist,UMSYprior=UMSYprior,startD=startD,
             method="L-BFGS-B",
             lower=log(exp(params)/20),upper=log(exp(params)*20),
             hessian=TRUE)

  posdef<-sum(eigen(solve(opt$hessian))$values>0)==3 # is the hessian positive-definite, ie has convergence been achieved?

  if(posdef){
    UMSY<-exp(opt$par[1])
    MSY<-exp(opt$par[2])
    BMSY<-MSY/UMSY

    B09<-BMSY*1.25
    F09<-0.72*UMSY

    Bcur<-DD_R(opt$par,opty=5,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior,startD=startD)

    # Harvest control rule of Cooke
    if((Bcur/B09)<0.1){
      mult<-0
    }else if((Bcur/B09)>1){
      mult<-1
    }else{
      mult<-((Bcur/B09)-0.1)/0.9
    }

    mult*F09*Bcur

  }else{

    dset$MPrec[x]*0.95

  }

}


# Internal function for delay-difference management procedure
DD_R<-function(params,opty,So_DD,Alpha_DD,Rho_DD,ny_DD,k_DD,wa_DD,E_hist,C_hist,UMSYprior,startD){
  UMSY_DD=exp(params[1])
  MSY_DD=exp(params[2])
  q_DD=exp(params[3])
  SS_DD=So_DD*(1-UMSY_DD)    # Initialise for UMSY, MSY and q leading.
  Spr_DD=(SS_DD*Alpha_DD/(1-SS_DD)+wa_DD)/(1-Rho_DD*SS_DD)
  DsprDu_DD=-So_DD*(Rho_DD/(1-Rho_DD*SS_DD)*Spr_DD+1/(1-Rho_DD*SS_DD)*(Alpha_DD/(1-SS_DD)+SS_DD*Alpha_DD/(1-SS_DD)^2))
  Arec_DD=1/(((1-UMSY_DD)^2)*(Spr_DD+UMSY_DD*DsprDu_DD))
  Brec_DD=UMSY_DD*(Arec_DD*Spr_DD-1/(1-UMSY_DD))/MSY_DD
  Spr0_DD=(So_DD*Alpha_DD/(1-So_DD)+wa_DD)/(1-Rho_DD*So_DD)
  Ro_DD=(Arec_DD*Spr0_DD-1)/(Brec_DD*Spr0_DD)
  Bo_DD=Ro_DD*Spr0_DD
  No_DD=Ro_DD/(1-So_DD)

  B_DD<-rep(NA,ny_DD+1)
  N_DD<-rep(NA,ny_DD+1)
  R_DD<-rep(NA,ny_DD+k_DD)
  Cpred_DD<-rep(NA,ny_DD)

  B_DD[1]=Bo_DD*startD
  N_DD[1]=No_DD*startD
  R_DD[1:k_DD]=Ro_DD # bit of a fudge to assume R0 when stock depleted

  for(tt in 1:ny_DD){

    Surv_DD=So_DD*exp(-q_DD*E_hist[tt])
    Cpred_DD[tt]=B_DD[tt]*(1-exp(-q_DD*E_hist[tt]))
    Sp_DD=B_DD[tt]-Cpred_DD[tt]
    R_DD[tt+k_DD]=Arec_DD*Sp_DD/(1+Brec_DD*Sp_DD);
    B_DD[tt+1]=Surv_DD*(Alpha_DD*N_DD[tt]+Rho_DD*B_DD[tt])+wa_DD*R_DD[tt+1]
    N_DD[tt+1]=Surv_DD*N_DD[tt]+R_DD[tt+1]

  }
  Cpred_DD[Cpred_DD<tiny]<-tiny

  if(opty==1){
    test<-dnorm(log(Cpred_DD),log(C_hist),0.25,log=T)
    test2<-dlnorm(UMSY_DD,log(UMSYprior[1]),UMSYprior[2],log=T)
    test[is.na(test)]<--1000
    test[test==(-Inf)]<--1000
    if(is.na(test2)|test2==-Inf|test2==Inf)test2<-1000
    return(-sum(test,test2))      # return objective function
  }else if(opty==2){                                  # return MLE OFL estimate
    UMSY_DD*B_DD[ny_DD]
  }else if(opty==3){
    B_DD[tt+1]/Bo_DD
  }else if(opty==4){
    if((B_DD[tt+1]/Bo_DD)<0.1)mult<-0
    if((B_DD[tt+1]/Bo_DD)>0.4)mult<-1
    if((B_DD[tt+1]/Bo_DD)>0.1&(B_DD[tt+1]/Bo_DD)<0.4)mult<-((B_DD[tt+1]/Bo_DD)-0.1)/0.3
    mult*UMSY_DD*B_DD[ny_DD]
  }else if(opty==5){
    B_DD[tt+1]
  }else{
    cbind(C_hist,Cpred_DD)                           # return observations vs predictions
  }
}

XSA<-function(x,dset){
  #for(x in 1:8){
  maxage<-dset$nages
  nyears<-dim(dset$Iobs)[2]
  Lage<-dset$Linf[x]*(1-exp(-dset$K[x]*((1:maxage)-dset$t0[x])))
  Wage<-dset$a[x]*Lage^dset$b[x]
  plusg<-20
  vpaCAA<-array(NA,c(plusg,nyears))
  vpaCAA[1:(plusg-1),]<-dset$CAA[x,1:(plusg-1),1:nyears]
  vpaCAA[plusg,]<-apply(dset$CAA[x,plusg:maxage,1:nyears],2,sum)

  iage<-7
  Idx<-FLIndex()
  Idx@index<-FLQuant(dim=c(iage,nyears,1,1,1),
                   dimnames = list(age=as.character(1:iage),year = as.character(1:nyears)),quant="age",units="tonnes")
  Idx@index.var<-FLQuant(dim=c(iage,nyears,1,1,1),
                       dimnames = list(age=as.character(1:iage),year = as.character(1:nyears)),quant="age")
  Idx@catch.n<-FLQuant(dim=c(iage,nyears,1,1,1),
                     dimnames = list(age=as.character(1:iage),year = as.character(1:nyears)),quant="age")
  Idx@catch.wt<-FLQuant(dim=c(iage,nyears,1,1,1),
                      dimnames = list(age=as.character(1:iage),year = as.character(1:nyears)),quant="age")
  Idx@sel.pattern<-FLQuant(dim=c(iage,nyears,1,1,1),
                         dimnames = list(age=as.character(1:iage),year = as.character(1:nyears)),quant="age")
  Idx@effort<-FLQuant(dim=c(1,nyears,1,1,1),
                    dimnames = list(age='all',year = as.character(1:nyears)),quant="age")
  Idx@index.q<-FLQuant(dim=c(iage,nyears,1,1,1),
                     dimnames = list(age=as.character(1:iage),year = as.character(1:nyears)),quant="age")

  Idx@index.var[]<-0.2
  Idx@index[]<-rep(dset$Iobs[x,],each=iage)
  Idx@range[1:7]<-c(1,iage,iage,1,nyears,0,1)
  Idx@name<-"simulated"


  Stk<-FLStock(FLQuant(dim=c(plusg,nyears,1,1,1),dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age"))        # need to edit years
  Stk@name<-"temp"
  Stk@catch.n<-FLQuant(vpaCAA,dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age",units="thousands")
  Stk@catch.n[Stk@catch.n==0] <- 0.0000001
  #Idx@catch.n<-Stk@catch.n
  Stk@m<-FLQuant(array(dset$M[x,1:plusg],dim=c(plusg,nyears)),dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age")
  Stk@mat<-FLQuant(dset$Mat[x,1:plusg,],dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age")
  Stk@range[1:7]<-c(1,plusg,plusg,1,nyears,0,1)

  #xsa.control  <- FLXSA.control(tol = 1e-10, maxit = 500,  min.nse = 0.3,  fse  = 2.5,
  #                            rage = 1,   qage  = 4,   shk.n   = T, shk.f = F,shk.yrs = 5,
  #                            shk.ages= 3, window  = 100,  tsrange =100,tspower = 0,vpa=FALSE)
  #xsa.control<- FLXSA.control(tol=1e-10, maxit=500, min.nse=0.3,   	fse=2.5, rage=8, qage=12, shk.n=FALSE, 			shk.f=FALSE,shk.yrs=5, shk.ages=3, window=100, tsrange=100,tspower = 0)

 # xsaCtrl <-FLXSA.control(shk.n  = TRUE, shk.yrs  = 5,  rage  = 1, shk.f  = TRUE, shk.ages = 5, fse    = 0.3,  qage   = 6,   window = 100,  tsrange = 20,   tspower = 3,        vpa    = TRUE)
  xsa.control<- FLXSA.control( tol = 1e-09, maxit = 50, min.nse = 0.5, fse = 0.5,
    rage = 1, qage = 5, shk.n = TRUE, shk.f = TRUE, shk.yrs = 5,
    shk.ages = 5, window =100, tsrange = 30, tspower = 3, vpa = TRUE)

  Idl<-FLIndices()
  Idl[[1]]<-Idx
  xsa<-FLXSA(Stk,Idl[[1]],xsa.control)                  # Run XSA VPA

  #save(Idl,file="Debugging/Idl")
  #save(Stk,file="Debugging/Stk")
  #save(xsa.control,file="Debugging/xsa.control")

  # xsa<-FLXSA_tom(Stk,Idl,xsa.control)                  # Run XSA VPA

  # Calculate some reference points
  Wta<-Wage[1:plusg]
  Wta[plusg]<-sum(exp(cumsum(-dset$M[x,plusg:maxage]))*Wage[plusg:maxage])/sum(exp(cumsum(-dset$M[x,plusg:maxage])))
  SSBnow<-sum(as.vector(xsa@stock.n[,ncol(xsa@stock.n)])*Wta*dset$Mat[x,1:plusg,nyears])
  SSB0<-sum(as.vector(xsa@stock.n[,1])*Wta*dset$Mat[x,1:plusg,nyears])
  B0<-sum(as.vector(xsa@stock.n[,1])*Wta)

  refy<-getrefs(xsa,Stk,Wta=Wta,plusg,nyears)                  # Optimize for FMSY reference points

  nyr<-xsa@range[5]-xsa@range[4]+1
  Fnow<-as.vector(apply(xsa@harvest[,(nyr-8):(nyr-1)],1,mean))    # get selectivity according to mean of last four years
  Fapical<-max(Fnow)                                              # Maximum F by age
  sel<-Fnow/Fapical                                               # Derive selectivity

  Bnow<-sum(as.vector(xsa@stock.n[,ncol(xsa@stock.n)])*Wta)
  VBnow<-sum(as.vector(xsa@stock.n[,ncol(xsa@stock.n)])*Wta*sel)
  Cnow<-sum(as.vector(Stk@catch.n[,ncol(Stk@catch.n)])*Wta)
  Unow<-Cnow/(VBnow+Cnow)                                        # Current harvest rate
  Fnow<--log(1-Unow)                                             # Current fishing mortality rate

  # Refs indexing is:  (1) FMSY  (2) BMSY  (3) MSY   (4) depletion   (5) F/FMSY    (6) B/BMSY
  refs<-c(round(refy[1],4),round(refy[2]/1000,0),round(refy[3]/1000,0),Bnow/B0,round(Fnow/refy[1],8),round(Bnow/refy[2],8))
  return(refy[1]*Bnow)
  #print(c(refy[1],dset$UMSY_PI[x]))
  #print(c(Bnow,dset$Bt_PI[x]))
  #}
}
class(XSA)<-"MP"

# internal function for XSA
getrefs<-function(xsa,Stk,Wta,maxage,nyears){

  nage<-xsa@range["max"]
  nyr<-xsa@range[5]-xsa@range[4]+1

  N<-xsa@stock.n
  U<-xsa@harvest
  C<-Stk@catch.n
  Wt<-FLQuant(array(Wta,dim=c(nage,nyr)),dimnames = list(age=as.character(1:nage),year = as.character(1:nyr)),quant="age")
  Mat<-Stk@mat
  SSB<-as.vector(apply(N*Mat*Wt,2,sum,na.rm=T)[,1:(ncol(N)-1)] )
  SSB[SSB==Inf]<-NA
  rec<-as.vector(N[1,2:ncol(N)])
  opt<-optim(c(0,log(rec[1])),getBHmp,lower=c(-15, log(rec[1]/50)), upper=c(15, log(rec[1]*10)), method = "L-BFGS-B",SSB=SSB,rec=rec)
  #getBH(opt$par,SSB,rec,opty=F,namey=Stk@name)
  hh<-0.2+(exp(opt$par[1])/(1+exp(opt$par[1])))*0.8
  R0<-exp(opt$par[2])

  mfrac<-cumsum(as.vector(Stk@m[,nyr]))
  SSB0<-sum(R0*exp(-mfrac)*Wta*Mat[,nyr])

  SSBpR<-SSB0/R0
  Nna<-as.vector(N[,1]/2)
  Fnow<-as.vector(apply(xsa@harvest[,(nyr-8):(nyr-1)],1,mean))
  Fapical<-max(Fnow)
  sel<-Fnow/Fapical
  Wt<-Wta
  M<-as.vector(Stk@m[,1])
  mature<-as.vector(Mat[,1])

  #plot(Nna,type='l',lwd=2,col="#ff000040")

  opt2<-optimize(getMSYrefs2,log(c(0.00001,100000)),Nna,Wt,sel,Fapical,M,mature,hh,R0,SSBpR,tol=1E-2)

  getMSYrefs2(opt2$minimum,Nna,Wt,sel,Fapical,M,mature,hh,R0,SSBpR,opty=FALSE)

  #plot(0:11,sel,col="blue",type="l",xlab="Age class",ylab="Vuln / M / Mat / Weight",ylim=c(0,1))
  #lines(0:11,M,col="red")
  #lines(0:11,mature,col="green")
  #lines(0:11,Wt/max(Wt),col="grey")
  #legend("top",legend=paste("steepness = 0.245"),bty='n')
  #legend("right",legend=c('Vulnerability','M',"% Mature","Weight"),text.col=c('blue','red','green','grey'),bty='n')

}

# internal function for XSA
getMSYrefs2<-function(Fmult,Nna,Wt,sel,Fapical,M,mature,hh,R0,SSBpR,nprojy=50,opty=T){
  nage<-length(Nna)
  Ctot<-0
  Nnow<-Nna

  for(y in 1:nprojy){

    Nstore<-Nnow
    FF<-sel*exp(Fmult)*Fapical
    Z<-M+FF
    C<-sum(Wt*Nnow*(1-exp(-Z))*(FF/Z))
    Ctot<-Ctot+C
    SSB<-sum(Nnow*mature*Wt)
    Nnow[2:nage]<-Nnow[1:(nage-1)]*exp(-Z[1:(nage-1)])
    Nnow[1]<-(0.8*R0*hh*SSB)/(0.2*SSBpR*R0*(1-hh)+(hh-0.2)*SSB)

  }

  #if(opty){
  # Bstore<-sum(Nstore*Wt)
  # lines(Nnow,type='l',lwd=2,col="#ff000040")
  # legend('topright',legend=round(C/1000,0),bg='white')
  # legend('right',legend=round(-log(1-(C/Bstore)),4),bg='white')
  # Sys.sleep(0.35)
  #}

  if(opty){return(-C)
  }else{
    Bstore<-sum(Nstore*Wt*sel)
    return(c(-log(1-(C/(Bstore+C))),Bstore,C))
  }
}

# internal function for XSA
getBHmp<-function(parm,SSB,rec,opty=T,namey=""){
  hh<-0.2+(exp(parm[1])/(1+exp(parm[1])))*0.8
  R0<-exp(parm[2])
  SSBpR<-SSB[1]/R0
  SSBt<-SSB
  SSBt[SSBt==Inf]<-NA
  SSBp<-seq(0,max(SSBt,na.rm=T),length.out=100)
  pred<-(0.8*R0*hh*SSB)/(0.2*SSBpR*R0*(1-hh)+(hh-0.2)*SSB)
  predp<-(0.8*R0*hh*SSBp)/(0.2*SSBpR*R0*(1-hh)+(hh-0.2)*SSBp)
  if(opty){

    #return(((log(pred)-log(rec))^2))
    return(-sum(dnorm(log(pred),log(rec),0.6,log=T),na.rm=T)-dnorm(log(hh),log(0.3),1,log=T))   # weak lognormal prior for h with mean = 0.4  cv = 20%
  }
  if(!opty){
    plot(SSB,rec,xlim=c(0,max(SSB)),ylim=c(0,max(c(rec,pred))),main="",axes=F,pch=19,cex=0.8,col="#0000ff60")
    axis(1,c(-1E100,1E100),c(-1E100,1E100))
    axis(2,c(-1E100,1E100),c(-1E100,1E100))
    legend('topleft',legend=namey,bty='n')
    legend('bottomright',legend=round(hh,2),bty='n')
    lines(SSBp,predp,col="#ff000070")
    abline(v=0.2*max(SSB),col="green")
  }
}

#' A length based management procedure that incrementally changes TAC based on recent and long-term length observations (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param yrsmth the number of years over which to compare length observations
#' @param xx alters target catch as a fraction of historical catch TACstar = (1-xx)muC
#' @param stepsz the maximum fractional change in TAC between MP updates
#' @param llim a vector of three ratios that determine whether the TAC should be reduced by 2 steps, 1 steps and increased by 1 step
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' LstepCC4(1,dset_example_East)
#' sapply(1:10,LstepCC4,dset_example_East)
LstepCC4<-function(x,dset,yrsmth=5,xx=0.3,stepsz=0.05,llim=c(0.96,0.98,1.05)){
  ny<-length(dset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  C_dat<-dset$Cobs[x,]
  if(is.na(dset$MPrec[x])){TACstar<-(1-xx)*mean(C_dat)
  }else{TACstar<-dset$MPrec[x]}
  step<-stepsz*TACstar
  binval<-dset$CAL_bins[1:(length(dset$CAL_bins)-1)]+(dset$CAL_bins[2]-dset$CAL_bins[1])/2
  CALdat<-dset$CAL[x,,]*rep(binval,dim(dset$CAL)[3])
  avCAL<-apply(CALdat,2,sum)/apply(dset$CAL[x,,],2,sum)
  Lrecent<-mean(avCAL[ind])
  Lave<-mean(avCAL[(ny-(yrsmth*2-1)):ny])
  rat<-Lrecent/Lave
  if(rat<llim[1]){OFL<-TACstar-2*step
  }else if(rat<llim[2]){OFL<-TACstar-step
  }else if(rat>llim[3]){OFL<-TACstar+step
  }else{OFL<-TACstar
  }
  OFL
}
class(LstepCC4)<-"MP"

#' An index-based (INDEX 2) management procedure that incrementally changes TAC to achieve a stable index(a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param yrsmth the number of years over which to compare index observations
#' @param xx alters target catch as a fraction of historical catch TACstar = (1-xx)muC
#' @param lambda a scalar which determines the responsiveness of the adjustment
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' Islope1_i2(1,dset_example_East)
#' sapply(1:10,Islope1_i2,dset_example_East)
Islope1_i2<-function(x,dset)Islope1(x,dset,ii=2)
class(Islope1_i2)<-"MP"


#' An index-based (INDEX 4) management procedure that incrementally changes TAC to achieve a stable index(a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param yrsmth the number of years over which to compare index observations
#' @param xx alters target catch as a fraction of historical catch TACstar = (1-xx)muC
#' @param lambda a scalar which determines the responsiveness of the adjustment
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' Islope1_i4(1,dset_example_East)
#' sapply(1:10,Islope1_i4,dset_example_East)
Islope1_i4<-function(x,dset)Islope1(x,dset,ii=4)
class(Islope1_i4)<-"MP"


Islope1<-function(x,dset,yrsmth=5,lambda=0.4,xx=0.2,ii){
  ny<-length(dset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  C_dat<-dset$Cobs[x,]
  if(is.na(dset$MPrec[x])){TACstar<-(1-xx)*mean(C_dat)
  }else{TACstar<-dset$MPrec[x]}
  I_hist<-dset$Iobs[x,ii,ind]
  yind<-1:yrsmth
  slppar<-summary(lm(I_hist~yind))$coefficients[2,1:2]
  Islp <-slppar[1]
  TACstar*(1+lambda*Islp)
}

#' A surplus production management procedure using INDEX 2 that attempts to identify the slope of changes in surplus production and biomass to locate a gradient of zero (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param yrsmth the number of years over which to quantify slope in surplus production with biomass
#' @param alp the SP gradient limits that lead to TAC changes
#' @param bet the degree of responsiveness of the MP
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' SPslope_i2(1,dset_example_East)
#' sapply(1:10,SPslope_i2,dset_example_East)
SPslope_i2<-function(x,dset)SPslope(x,dset,ii=2)
class(SPslope_i2)<-"MP"


#' A surplus production management procedure using INDEX 2 that attempts to identify the slope of changes in surplus production and biomass to locate a gradient of zero (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param yrsmth the number of years over which to quantify slope in surplus production with biomass
#' @param alp the SP gradient limits that lead to TAC changes
#' @param bet the degree of responsiveness of the MP
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' SPslope_i4(1,dset_example_East)
#' sapply(1:10,SPslope_i4,dset_example_East)
SPslope_i4<-function(x,dset)SPslope(x,dset,ii=4)
class(SPslope_i4)<-"MP"


SPslope<-function(x,dset,yrsmth=4,alp=c(0.9,1.1),bet=c(1.5,0.9),ii){
  ny<-length(dset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  yind<-1:yrsmth
  C_dat<-dset$Cobs[x,ind]
  B_dat<-dset$Iobs[x,ii,ind]/dset$Iobs[x,ii,ind[yrsmth]]*dset$Bt[x] #!!! INDEX
  Pt_mu<-B_dat[yrsmth]-B_dat[yrsmth-1]+C_dat[yrsmth-1]
  It<-exp(predict(lm(log(B_dat)~yind),newdat=list(yind=yrsmth+1)))
  Ilast<-B_dat[1]
  Ct_1<-mean(C_dat)
  rat<-It/Ilast
  if(rat<alp[1])OFL<-(1-bet[1]*(Ilast-It)/Ilast)*Ct_1
  if(rat>alp[1]&rat<alp[2])OFL<-Ct_1
  if(rat>alp[2])OFL<-bet[2]*Pt_mu
  if(OFL<0)OFL<-dset$Cobs[x,ny]/2
  OFL
}


#' An adaptive surplus production management procedure using INDEX 2 that attempts to identify the slope of changes in surplus production and biomass to locate a gradient of zero (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param yrsmth the number of years over which to quantify slope in surplus production with biomass
#' @param gg the responsiveness of the MP with respect to the slope
#' @param FMSY_M the assumed ratio of FMSY to M
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' Fadapt_i2(1,dset_example_East)
#' sapply(1:10,Fadapt_i2,dset_example_East)
Fadapt_i2<-function(x,dset)Fadapt(x,dset,ii=2)
class(Fadapt_i2)<-"MP"

#' An adaptive surplus production management procedure using INDEX 4 that attempts to identify the slope of changes in surplus production and biomass to locate a gradient of zero (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param yrsmth the number of years over which to quantify slope in surplus production with biomass
#' @param gg the responsiveness of the MP with respect to the slope
#' @param FMSY_M the assumed ratio of FMSY to M
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' Fadapt_i4(1,dset_example_East)
#' sapply(1:10,Fadapt_i4,dset_example_East)
Fadapt_i4<-function(x,dset)Fadapt(x,dset,ii=4)
class(Fadapt_i4)<-"MP"


Fadapt<-function(x,dset,yrsmth=7,gg=1,FMSY_M=0.5,ii){
  ny<-length(dset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  C_dat<-log(dset$Cobs[x,ind])
  B_dat<-log(dset$Iobs[x,ii,ind]/dset$Iobs[x,ii,ind[yrsmth]]*dset$Bt[x])
  C_hist<-exp(predict(loess(C_dat~ind,degree=1)))
  B_hist<-exp(predict(loess(B_dat~ind,degree=1)))

  ind<-2:yrsmth
  ind1<-1:(yrsmth-1)
  SP_hist<-B_hist[ind]-B_hist[ind1]+C_hist[ind1]
  Mc<-sum((dset$M[x,]*exp(-cumsum(dset$M[x,])))/(sum(exp(-cumsum(dset$M[x,]))))) # M average weighted by survival

  Frat<-Mc*FMSY_M
  Flim<-Frat*c(0.5,2)
  Flimr<-Flim[2]-Flim[1]

  yind<-1:length(SP_hist)
  SP_new<-predict(lm(SP_hist~yind),newdat=list(yind=length(SP_hist)+1))
  #SP_se<-predict(lm(SP_hist~yind),newdat=list(yind=length(SP_hist)+1),se=T)$se.fit
  #SP_new<-rnorm(reps,SP_mu,SP_se/2)
  Glm<-summary(lm(SP_hist~B_hist[ind1]))$coefficients[2,1:2] # plot(B_hist[ind1],SP_hist) # points(B_hist[ind1],SP_hist,col='green')
  G_new<-Glm[1]#rnorm(reps,Glm[1],Glm[2])
  Fold<-mean(C_hist/B_hist)

  if(Fold<Flim[1])Fmod1<-(-2)
  if(Fold>Flim[2])Fmod1<-2
  if(Fold>Flim[1]&Fold<Flim[2]){
    Ffrac<-(Fold-Flim[1])/Flimr
    Fmod1<-log(Ffrac/(1-Ffrac))
  }
  Fmod2<-Fmod1+gg*-G_new
  newF<-Flim[1]+(exp(Fmod2)/(1+exp(Fmod2)))*Flimr
  newF*B_hist[yrsmth]
}


#' One of two Southern Bluefin Tuna management procedures that uses recruitment trajectory to adjust TAC (a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @param epsB currently unused
#' @param epsR resposiveness of the MP to recruitment gradient
#' @param tauR the time lag (years) for evaluating trend in recruitment
#' @param tauB currently unused
#' @param gamma currently unused
#' @return a TAC recommendation arising from \code{x, dset, epsB, epsR, tauR, tauB, gamma}.
#' @examples
#' SBT2(1,dset_example_East)
#' sapply(1:10,SBT2,dset_example_East)
SBT2<-function(x,dset,epsB=0.25,epsR=0.75,tauR=5,tauB=7,gamma=1){
  ny<-length(dset$Cobs[x,])
  Ctarg<-dset$MSY[x]
  Rec<-dset$CAA[x,3,]/mean(dset$CAA[x,3,])
  muR<-mean(Rec[(ny-tauR+1):ny])
  phi<-mean(Rec[(ny-9):ny])
  Rrat<-muR/phi

  if(Rrat>1)deltaR<-Rrat^(1-epsR)
  if(Rrat<1|Rrat==1)deltaR<-Rrat^(1+epsR)
  0.5*(dset$Cobs[x,ny]+Ctarg*deltaR)
}
class(SBT2)<-"MP"



#' A 3 parameter surplus production assessmetn using INDEX 2 (observation error only)(a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' SP_i2(1,dset_example_East)
#' sapply(1:10,SP_i2,dset_example_East)
SP_i2<-function(x,dset)SP(x,dset,ii=2)
class(SP_i2)<-"MP"

#' A 3 parameter surplus production assessmetn using INDEX 4 (observation error only)(a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' SP_i4(1,dset_example_East)
#' sapply(1:10,SP_i4,dset_example_East)
SP_i4<-function(x,dset)SP(x,dset,ii=4)
class(SP_i4)<-"MP"


SP<-function(x,dset,startD=0.5,checkfit=F,ii){                  # a very simple surplus production model, r, K and q leading

  nyears<-dim(dset$Iobs)[3]               # get the number of years of data (the 3rd dimension of Iobs)
  Ind<-dset$Iobs[x,ii,1:nyears]            # get the index
  yind<-(1:nyears)[!is.na(Ind)]           # find the years where the index is available (ie not NA)

  C_hist <- dset$Cobs[x,yind]             # store catches for this time period
  E_hist <- C_hist/dset$Iobs[x,ii,yind]    # store standardized effort (partial F)
  E_hist <- E_hist/mean(E_hist)           # normalize the effort to mean 1

  surv<-exp(-cumsum(dset$M[x,]))          # survival at age
  muM<-sum(dset$M[x,]*surv)/sum(surv)     # mean natural mortality rate accounting for survival
  ny <- length(C_hist)                    # record the total number of years of data
  params<-c(muM,sum(C_hist),0.05)         # initial values for r, K and q (Effort covariate has been standardized to mean 1)
  rprior<-c(muM,0.5)                      # a vague prior on r based on the assumption that FMSY ~ 0.5 x M and FMSY = r/2

  opt<-optim(log(params),SP_R,opty=1,
             C_hist=C_hist,E_hist=E_hist,
             rprior=rprior,ny=ny, # run an optimization to fit the data
             startD=startD,                # starting depletion according to function argument above
             method = "L-BFGS-B",
             lower=log(params/c(3,20,20)), # the first parameter, r, is bounded more tightly as K and q
             upper=log(params*c(3,20,20)), # the greater constraint on r is to prevent chaotic model behavior above 1.3
             hessian = TRUE,               # return a hessian matrix for simple testing of convergence
             control=list(maxit=100))      # optimization can't run for more than 100 iterations

  posdef<-sum(eigen(solve(opt$hessian))$values>0)==3 # is the hessian positive-definite, ie has convergence been achieved?

  if(checkfit){                            # Plot fit to catches for model testing
    fit<-SP_R(opt$par,opty=4,C_hist=C_hist,E_hist=E_hist,rprior=rprior,ny=ny,startD=startD);
    plot(fit[,1],xlab='Model year',ylab="Catches",col='blue')
    lines(fit[,2],col='red')
    legend('topright',legend=c("Observed","Predicted"),text.col=c("blue","red"),bty='n')
    legend('topleft',legend=paste0("Converged: ",posdef),bty='n')
  }

  if(posdef){   # if model converged return new TAC
    SP_R(opt$par,opty=2,C_hist=C_hist,E_hist=E_hist,rprior=rprior,ny=ny,startD=startD)  # opty = 2 returns FMSY x cur biomass
  }else{         # otherwise return previous TAC subject to a 5 percent downward adjustment
    dset$MPrec[x]*0.95
  }
}


SP_R<-function(logparams, opty, C_hist, E_hist, rprior,ny,startD){   # simple surplus production model r, K and q leading

  r<-exp(logparams[1])                                # Intrinsic rate of increase
  K<-exp(logparams[2])                                # Carrying capacity
  qq<-exp(logparams[3])                               # Catchability (F=qE)
  B<-K*startD                                         # Starting biomass level

  Cpred<-rep(NA,ny)                                   # A predicted catch vector
  Bpred<-rep(NA,ny)                                   # A predicted biomass vector

  for(i in 1:ny){                                     # loop over years
    Cpred[i]<-B*(1-exp(-qq*E_hist[i]))                # Predicted catches
    B<-B+r*B*(1-B/K)-Cpred[i]                         # update biomass according to SP dynamics
    Bpred[i]<-B                                       # Record biomass
  }

  if(opty==1){                                         # return objective function

    test<-dnorm(log(Cpred),log(C_hist),0.2,log=T)      # observed versus predicted log catches
    #test<-dnorm(Cpred,C_hist,0.2*Cpred,log=T)         # observed versus predicted catches
    test2<-dlnorm(r,log(rprior[1]),rprior[2],log=T)    # a weak  lognormal prior on r
    test[is.na(test)|test==(-Inf)]<--1000              # some robustification
    if(is.na(test2)|test2==-Inf|test2==Inf)test2<-1000 # some more robustification
    return(-sum(test,test2))                           # objective function

  }else if(opty==2){                                   # return MLE FMSY * current biomass estimate

    r/2*Bpred[ny]

  }else if(opty==3){

    B[ny]/K                                            # return depletion

  }else{

    cbind(C_hist,Cpred)                                # return observations vs predictions

  }

}


#' A simple MP that uses an index (INDEX 2) to derive depletion then fishing at average catch x depletion x 2(a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' MCD_i2(1,dset_example_East)
#' sapply(1:10,MCD_i2,dset_example_East)
MCD_i2<-function(x,dset)MCD(x,dset,ii=2)
class(MCD_i2)<-"MP"


#' A simple MP that uses an index (INDEX 2) to derive depletion then fishing at average catch x depletion x 2(a management procedure of class MP).
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' MCD_i2(1,dset_example_East)
#' sapply(1:10,MCD_i2,dset_example_East)
MCD_i4<-function(x,dset)MCD(x,dset,ii=4)
class(MCD_i4)<-"MP"


MCD<-function(x,dset,startD=0.1,ii){

  nyears<-dim(dset$Iobs)[3]                 # Most recent year
  mean(dset$Cobs[x,],na.rm=T)*              # Average historical catches
    mean(dset$Iobs[,ii,(nyears-2):nyears])* # Mean index over last three years
    2*startD                                # Adjusted for starting depletion and MSY production at depletion = 0.5

}

#' A simple average catch MP.
#'
#' @param x a simulation number.
#' @param dset a list of simulated data for use by management procedures.
#' @return a TAC recommendation arising from \code{x, dset}.
#' @examples
#' MeanC(1,dset_example_East)
MeanC<-function(x,dset) mean(dset$Cobs[x,],na.rm=T)
class(MeanC)<-"MP"





