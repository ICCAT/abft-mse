
# ===============================================================================================================
# === Management Procedures for ABT MSE==========================================================================
# ===============================================================================================================

UMSY<-function(x,pset,Urat=0.75){
  pset$UMSY[x]*pset$Bt[x]*Urat
}
class(UMSY)<-"ABT_MP"

UMSY_PI<-function(x,pset){
  pset$UMSY_PI[x]*pset$Bt_PI[x] 
}  
class(UMSY_PI)<-"ABT_MP"

DD<-function(x,pset){
  Linfc<-pset$Linf[x]
  Kc<-pset$K[x]
  t0c<-pset$t0[x]
  Mc<-sum((pset$M[x,]*exp(-cumsum(pset$M[x,])))/(sum(exp(-cumsum(pset$M[x,]))))) # M average weighted by survival
  ac<-pset$a[x]
  bc<-pset$b[x]
  nages<-pset$nages
  Winf=ac*Linfc^bc
  age<-1:nages
  la<-Linfc*(1-exp(-Kc*((age-t0c))))
  wa<-ac*la^bc
  a50V<-pset$ageM[x]
  
  
  C_hist<-pset$Cobs[x,]
  I_hist<-pset$Iobs[x,]
  
  E_hist<-C_hist/I_hist
  E_hist<-E_hist/mean(E_hist)
  ny_DD<-length(C_hist)
  params<-log(c(Mc,mean(C_hist,na.rm=T),Mc))
  k_DD<-ceiling(a50V)   # get age nearest to 50% vulnerability (ascending limb)  -------------
  k_DD[k_DD>nages/2]<-ceiling(nages/2)  # to stop stupidly high estimates of age at 50% vulnerability
  Rho_DD<-(wa[k_DD+2]-Winf)/(wa[k_DD+1]-Winf)
  Alpha_DD<-Winf*(1-Rho_DD)
  So_DD<-exp(-Mc) # get So survival rate
  wa_DD<-wa[k_DD]
  UMSYprior<-c(1-exp(-Mc*0.5),0.3)
  opt<-optim(params,DD_R,opty=1,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,
             ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,
             C_hist=C_hist,UMSYprior=UMSYprior,
             method="L-BFGS-B",
             lower=log(exp(params)/20),upper=log(exp(params)*20),
             hessian=TRUE)
  
  #Catfit<-DD_R(opt$par,opty=4,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior)
  #plot(Catfit[,1],ylim=c(0,max(Catfit)))
  #lines(Catfit[,2],col="red")
 DD_R(opt$par,opty=2,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior)

}
class(DD)<-"ABT_MP"

DD4010<-function(x,pset){
  #for(x in 1:8){
  Linfc<-pset$Linf[x]
  Kc<-pset$K[x]
  t0c<-pset$t0[x]
  Mc<-sum((pset$M[x,]*exp(-cumsum(pset$M[x,])))/(sum(exp(-cumsum(pset$M[x,]))))) # M average weighted by survival
  ac<-pset$a[x]
  bc<-pset$b[x]
  nages<-pset$nages
  Winf=ac*Linfc^bc
  age<-1:nages
  la<-Linfc*(1-exp(-Kc*((age-t0c))))
  wa<-ac*la^bc
  a50V<-pset$ageM[x]
  
  
  C_hist<-pset$Cobs[x,]
  I_hist<-pset$Iobs[x,]
  
  E_hist<-C_hist/I_hist
  E_hist<-E_hist/mean(E_hist)
  ny_DD<-length(C_hist)
  params<-log(c(Mc,mean(C_hist,na.rm=T),Mc))
  k_DD<-ceiling(a50V)   # get age nearest to 50% vulnerability (ascending limb)  -------------
  k_DD[k_DD>nages/2]<-ceiling(nages/2)  # to stop stupidly high estimates of age at 50% vulnerability
  Rho_DD<-(wa[k_DD+2]-Winf)/(wa[k_DD+1]-Winf)
  Alpha_DD<-Winf*(1-Rho_DD)
  So_DD<-exp(-Mc) # get So survival rate
  wa_DD<-wa[k_DD]
  UMSYprior<-c(1-exp(-Mc*0.5),0.3)
  opt<-optim(params,DD_R,opty=1,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,
             ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,
             C_hist=C_hist,UMSYprior=UMSYprior,
             method="L-BFGS-B",
             lower=log(exp(params)/20),upper=log(exp(params)*20),
             hessian=TRUE)
  
  #Catfit<-DD_R(opt$par,opty=4,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior)
  #plot(Catfit[,1],ylim=c(0,max(Catfit)))
  #lines(Catfit[,2],col="red")
  DD_R(opt$par,opty=4,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior)
  #}
}
class(DD4010)<-"ABT_MP"

Cooke_DD<-function(x,pset){
  #for(x in 1:8){
  Linfc<-pset$Linf[x]
  Kc<-pset$K[x]
  t0c<-pset$t0[x]
  Mc<-sum((pset$M[x,]*exp(-cumsum(pset$M[x,])))/(sum(exp(-cumsum(pset$M[x,]))))) # M average weighted by survival
  ac<-pset$a[x]
  bc<-pset$b[x]
  nages<-pset$nages
  Winf=ac*Linfc^bc
  age<-1:nages
  la<-Linfc*(1-exp(-Kc*((age-t0c))))
  wa<-ac*la^bc
  a50V<-pset$ageM[x]
  
  
  C_hist<-pset$Cobs[x,]
  I_hist<-pset$Iobs[x,]
  
  E_hist<-C_hist/I_hist
  E_hist<-E_hist/mean(E_hist)
  ny_DD<-length(C_hist)
  params<-log(c(Mc,mean(C_hist,na.rm=T),Mc))
  k_DD<-ceiling(a50V)   # get age nearest to 50% vulnerability (ascending limb)  -------------
  k_DD[k_DD>nages/2]<-ceiling(nages/2)  # to stop stupidly high estimates of age at 50% vulnerability
  Rho_DD<-(wa[k_DD+2]-Winf)/(wa[k_DD+1]-Winf)
  Alpha_DD<-Winf*(1-Rho_DD)
  So_DD<-exp(-Mc) # get So survival rate
  wa_DD<-wa[k_DD]
  UMSYprior<-c(1-exp(-Mc*0.5),0.3)
  opt<-optim(params,DD_R,opty=1,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,
             ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,
             C_hist=C_hist,UMSYprior=UMSYprior,
             method="L-BFGS-B",
             lower=log(exp(params)/20),upper=log(exp(params)*20),
             hessian=TRUE)
  
  #Catfit<-DD_R(opt$par,opty=4,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior)
  #plot(Catfit[,1],ylim=c(0,max(Catfit)))
  #lines(Catfit[,2],col="red")
  UMSY<-exp(opt$par[1])
  MSY<-exp(opt$par[2])
  BMSY<-MSY/UMSY
  
  B09<-BMSY*1.25
  F09<-0.72*UMSY
  
  Bcur<-DD_R(opt$par,opty=5,So_DD=So_DD,Alpha_DD=Alpha_DD,Rho_DD=Rho_DD,ny_DD=ny_DD,k_DD=k_DD,wa_DD=wa_DD,E_hist=E_hist,C_hist=C_hist,UMSYprior=UMSYprior)
  
  # Harvest control rule of Cooke
  if((Bcur/B09)<0.1){
    mult<-0
  }else if((Bcur/B09)>1){
    mult<-1
  }else{
    mult<-((Bcur/B09)-0.1)/0.9
  }
  
  mult*F09*Bcur

}
class(Cooke_DD)<-"ABT_MP"





DD_R<-function(params,opty,So_DD,Alpha_DD,Rho_DD,ny_DD,k_DD,wa_DD,E_hist,C_hist,UMSYprior){
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
  
  B_DD[1]=Bo_DD
  N_DD[1]=No_DD
  R_DD[1:k_DD]=Ro_DD
  
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

XSA<-function(x,pset){
  #for(x in 1:8){
  maxage<-pset$nages
  nyears<-dim(pset$Iobs)[2]
  Lage<-pset$Linf[x]*(1-exp(-pset$K[x]*((1:maxage)-pset$t0[x])))
  Wage<-pset$a[x]*Lage^pset$b[x]
  plusg<-20  
  vpaCAA<-array(NA,c(plusg,nyears))
  vpaCAA[1:(plusg-1),]<-pset$CAA[x,1:(plusg-1),1:nyears]
  vpaCAA[plusg,]<-apply(pset$CAA[x,plusg:maxage,1:nyears],2,sum)
  
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
  Idx@index[]<-rep(pset$Iobs[x,],each=iage)
  Idx@range[1:7]<-c(1,iage,iage,1,nyears,0,1)
  Idx@name<-"simulated"
  
  
  Stk<-FLStock(FLQuant(dim=c(plusg,nyears,1,1,1),dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age"))        # need to edit years
  Stk@name<-"temp"
  Stk@catch.n<-FLQuant(vpaCAA,dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age",units="thousands")
  Stk@catch.n[Stk@catch.n==0] <- 0.0000001
  #Idx@catch.n<-Stk@catch.n
  Stk@m<-FLQuant(array(pset$M[x,1:plusg],dim=c(plusg,nyears)),dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age")
  Stk@mat<-FLQuant(pset$Mat[x,1:plusg,],dimnames = list(age=as.character(1:plusg),year = as.character(1:nyears)),quant="age")
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
  Wta[plusg]<-sum(exp(cumsum(-pset$M[x,plusg:maxage]))*Wage[plusg:maxage])/sum(exp(cumsum(-pset$M[x,plusg:maxage])))
  SSBnow<-sum(as.vector(xsa@stock.n[,ncol(xsa@stock.n)])*Wta*pset$Mat[x,1:plusg,nyears])
  SSB0<-sum(as.vector(xsa@stock.n[,1])*Wta*pset$Mat[x,1:plusg,nyears])
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
  #print(c(refy[1],pset$UMSY_PI[x]))
  #print(c(Bnow,pset$Bt_PI[x]))
  #}
}
class(XSA)<-"ABT_MP"

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
  opt<-optim(c(0,log(rec[1])),getBH,lower=c(-15, log(rec[1]/50)), upper=c(15, log(rec[1]*10)), method = "L-BFGS-B",SSB=SSB,rec=rec)
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

getBH<-function(parm,SSB,rec,opty=T,namey=""){
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

LstepCC4<-function(x,pset,yrsmth=5,xx=0.3,stepsz=0.05,llim=c(0.96,0.98,1.05)){
  ny<-length(pset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  C_dat<-pset$Cobs[x,]
  if(is.na(pset$MPrec[x])){TACstar<-(1-xx)*mean(C_dat)
  }else{TACstar<-pset$MPrec[x]}
  step<-stepsz*TACstar
  binval<-pset$CAL_bins[1:(length(pset$CAL_bins)-1)]+(pset$CAL_bins[2]-pset$CAL_bins[1])/2
  CALdat<-pset$CAL[x,,]*rep(binval,dim(pset$CAL)[3]) 
  avCAL<-apply(CALdat,2,sum)/apply(pset$CAL[x,,],2,sum)
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
class(LstepCC4)<-"ABT_MP"

Islope1<-function(x,pset,yrsmth=5,lambda=0.4,xx=0.2){
  ny<-length(pset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  C_dat<-pset$Cobs[x,]
  if(is.na(pset$MPrec[x])){TACstar<-(1-xx)*mean(C_dat)
  }else{TACstar<-pset$MPrec[x]}
  I_hist<-pset$Iobs[x,ind]
  yind<-1:yrsmth
  slppar<-summary(lm(I_hist~yind))$coefficients[2,1:2]
  Islp <-slppar[1]
  TACstar*(1+lambda*Islp)
}
class(Islope1)<-"ABT_MP"

SPslope<-function(x,pset,yrsmth=4,alp=c(0.9,1.1),bet=c(1.5,0.9)){
  ny<-length(pset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  yind<-1:yrsmth
  C_dat<-pset$Cobs[x,ind]
  B_dat<-pset$Iobs[x,ind]/pset$Iobs[x,ind[yrsmth]]*pset$Bt[x]
  Pt_mu<-B_dat[yrsmth]-B_dat[yrsmth-1]+C_dat[yrsmth-1]
  It<-exp(predict(lm(log(B_dat)~yind),newdat=list(yind=yrsmth+1)))
  Ilast<-B_dat[1]
  Ct_1<-mean(C_dat)
  rat<-It/Ilast
  if(rat<alp[1])OFL<-(1-bet[1]*(Ilast-It)/Ilast)*Ct_1
  if(rat>alp[1]&rat<alp[2])OFL<-Ct_1
  if(rat>alp[2])OFL<-bet[2]*Pt_mu
  if(OFL<0)OFL<-pset$Cobs[x,ny]/2
  OFL
}
class(SPslope)<-"ABT_MP"

Fadapt<-function(x,pset,yrsmth=7,gg=1,FMSY_M=0.5){
  ny<-length(pset$Cobs[x,])
  ind<-(ny-(yrsmth-1)):ny
  C_dat<-log(pset$Cobs[x,ind])
  B_dat<-log(pset$Iobs[x,ind]/pset$Iobs[x,ind[yrsmth]]*pset$Bt[x])
  C_hist<-exp(predict(loess(C_dat~ind,degree=1)))
  B_hist<-exp(predict(loess(B_dat~ind,degree=1)))
  
  ind<-2:yrsmth
  ind1<-1:(yrsmth-1)
  SP_hist<-B_hist[ind]-B_hist[ind1]+C_hist[ind1]
  Mc<-sum((pset$M[x,]*exp(-cumsum(pset$M[x,])))/(sum(exp(-cumsum(pset$M[x,]))))) # M average weighted by survival
  
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
class(Fadapt)<-"ABT_MP"

SBT2<-function(x,pset,epsB=0.25,epsR=0.75,tauR=5,tauB=7,gamma=1){
  ny<-length(pset$Cobs[x,])
  Ctarg<-pset$MSY[x]
  Rec<-pset$CAA[x,3,]/mean(pset$CAA[x,3,])
  muR<-mean(Rec[(ny-tauR+1):ny])
  phi<-mean(Rec[(ny-9):ny])
  Rrat<-muR/phi
 
  if(Rrat>1)deltaR<-Rrat^(1-epsR)
  if(Rrat<1|Rrat==1)deltaR<-Rrat^(1+epsR)
  0.5*(pset$Cobs[x,ny]+Ctarg*deltaR)
}
class(SBT2)<-"ABT_MP"
