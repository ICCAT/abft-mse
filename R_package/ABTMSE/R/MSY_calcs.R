# MSY calculations

get_dynB0<-function(OMI,out){

  surv<-exp(-t(apply(cbind(c(0,0),OMI@Ma[,1:(OMI@na-1)]),1,cumsum)))
  surv[,OMI@na]<-surv[,OMI@na]+surv[,OMI@na]*exp(-OMI@Ma[,OMI@na])/(1-exp(-OMI@Ma[,OMI@na]))
  matwt<-t(out$wt_age[out$ny,,])*out$mat_age*surv
  SSBpR<-OMI@SSBpR

  dynB0<-array(NA,c(OMI@np,OMI@nHy+OMI@ny))

  for(pp in 1:OMI@np){

    SSB=out$R0[pp,1]*SSBpR[pp]#OMI@SSBpR[pp]
    dynB0[pp,1:OMI@nHy]<-rep(SSB,OMI@nHy)
    N<-out$R0[pp,1]*surv[pp,]

    for(yy in 1:OMI@ny){
      Nplus<-N[OMI@na]*exp(-OMI@Ma[pp,OMI@na])
      for(aa in OMI@na:2)N[aa]<-N[aa-1]*exp(-OMI@Ma[pp,aa-1])
      N[1]<-out$R0[pp,yy]
      N[OMI@na]<-N[OMI@na]+Nplus
      dynB0[pp,OMI@nHy+yy]<-sum(out$wt_age[out$ny,,pp]*out$mat_age[pp,]*N)
    }

  }

  dynB0

}

doMSY<-function(out,OMI,dynB0,refyr){

  res<-MSYMLE(out,OMI)


  Fap<-meanFs(FML=out$FL[refyr,,,,], iALK=out$iALK[,refyr,,], N=out$N[,refyr,,,],
              wt_age=t(out$wt_age[refyr,,]))

  #Fap<-timeFs(FML=out$FL, iALK=out$iALK, N=out$N,
  #         wt_age=t(out$wt_age[out$ny,,]))

  F_FMSY<-apply(Fap,1,max)/res[,2]
  SSB_SSB0<-apply(out$SSB[,refyr,],1,mean)/out$SSB0#res$SSBMSY
  res2<-cbind(res,F_FMSY,SSB_SSB0)
  res2[,c(2,3,6,7,8,9,10)]<-round(res2[,c(2,3,6,7,8,9,10)],3)
  res2[,c(1,4,5)]<-round(res2[,c(1,4,5)]/1000,0)
  #res2[res2[,2]==5,2]<-"5 (hit max bound)"

  BMSY<-round(res2[,7]*dynB0[,out$nHy+refyr]/1000,0)
  B_BMSY<-round(apply(out$SSB,1:2,mean)[,refyr]/(BMSY*1000),3)
  Dep_dyn<-round(apply(out$SSB,1:2,mean)[,refyr]/dynB0[,out$nHy+refyr],3)
  res2<-cbind(res2[,c(1,2)],BMSY,res2[,c(7,9)],B_BMSY,Dep_dyn)
  names(res2)<-c("MSY","FMSY","BMSY","BMSY_B0","F_FMSY","B_BMSY","Dep")

  res2

}


optF01 <- function(M_age, Wt_age, Mat_age, V, maxage,R0,SSBpR,hs,SRrel,plot=F) {


  Fslope<-F1calc(M_age, Wt_age, V, maxage)
  bounds <- c(1e-5, 5)
  doopt <- optimise(F01Calcs, log(bounds), M_age=M_age, Wt_age=Wt_age,
                    V=V, maxage=maxage, Fslope=Fslope, R0=R0, SSBpR=SSBpR,hs=hs,SRrel=SRrel,opt=1)
  F01<-exp(doopt$minimum)

  if(plot){
    nplot<-30
    Fs<-YPRs<-seq(0,F01*2.5,length.out=nplot)
    for(i in 1:30)YPRs[i]<-YPR(Fs[i],M_age,Wt_age,V,maxage)
    plot(Fs,YPRs,type='l')
    abline(v=F01,col="red")
    YPR01<-YPR(F01,M_age,Wt_age,V,maxage)
    lines(c(0,2.5*F01),c(YPR01-(Fslope*0.1*F01),YPR01+Fslope*0.1*(F01*2.5-F01)),col='red',lty=2)
    lines(c(0,100),c(0,Fslope*100),col='blue',lty=2)
  }

  MSYs <- F01Calcs(log(F01), M_age, Wt_age, Mat_age, V, maxage, Fslope, R0,SSBpR, hs,SRrel,opt=2)

  return(MSYs)

}

YPR<-function(FF,M_age, Wt_age, V, maxage){
  F_age<-V*FF
  Z_age<-F_age+M_age
  surv<-exp(-cumsum(c(0,Z_age[1:(maxage-1)])))
  sum((1-exp(-F_age))*surv*Wt_age)
}

F1calc<-function(M_age,Wt_age,V,maxage){

  Flow<-1E-5
  Flower<-1E-6
  YPRlow  <-YPR(Flow,M_age,Wt_age,V,maxage)
  YPRlower<-YPR(Flower,M_age,Wt_age,V,maxage)
  (YPRlow-YPRlower)/(Flow-Flower)

}

F01Calcs <- function(logapicF, M_age, Wt_age, Mat_age, V, maxage, Fslope, R0, SSBpR,hs, SRrel=1,opt=1) {
  # Box 3.1 Walters & Martell 2004

  if (opt == 1){  # minimize to get F01 (F where d(YPR)/d(F) is 10% that at origin)
    Flower <- exp(logapicF)
    Flow<-Flower+(1E-5)

    YPRlow<-YPR(Flow,M_age,Wt_age,V,maxage)
    YPRlower<-YPR(Flower,M_age,Wt_age,V,maxage)
    slope<-(YPRlow-YPRlower)/(Flow-Flower)
    return((slope-(Fslope/10))^2)

  }else if (opt == 2) {

    apicF <- exp(logapicF)
    lx <- l0 <- rep(1, maxage)
    for (a in 2:maxage) {
      l0[a] <- l0[a-1] * exp(-M_age[a-1])
      lx[a] <- lx[a-1] * exp(-(M_age[a-1] + apicF*V[a-1]))
    }
    Egg0 <- sum(l0 * Wt_age * Mat_age) # unfished egg production (assuming fecundity proportional to weight)
    EggF <- sum(lx * Wt_age * Mat_age) # fished egg production (assuming fecundity proportional to weight)

    vB0 <- sum(l0 * Wt_age * V)
    vBF <- sum(lx * Wt_age * V)

    SB0 <- sum(l0 * Wt_age * Mat_age) # same as eggs atm
    SBF <- sum(lx * Wt_age * Mat_age)

    B0 <- sum(l0 * Wt_age) # same as eggs atm
    BF <- sum(lx * Wt_age)

    hs[hs>0.999] <- 0.999
    recK <- (4*hs)/(1-hs) # Goodyear compensation ratio
    reca <- recK/Egg0
    if (SRrel ==1) {
      recb <- (reca * Egg0 - 1)/(R0*Egg0) # BH SRR
      RelRec <- (reca * EggF-1)/(recb*EggF)
    }
    if (SRrel ==2) {
      recb <- log(reca*Egg0)/(R0*Egg0) # Ricker SRR
      RelRec <- (log(reca*EggF))/(recb*EggF)
    }


    RelRec[RelRec<0] <- 0

    Fa <- apicF*V
    Za <- Fa + M_age
    relyield <- Fa/Za * lx * (1-exp(-Za)) * Wt_age
    YPR <- sum(relyield)
    Yield <- YPR * RelRec

    out <- c(Yield=Yield,
             F=-log(1 - (Yield/(vBF*RelRec+Yield))),
             SB = SBF * RelRec,
             SB_SB0 = (SBF * RelRec)/(SB0 * R0),
             B_B0 = (BF * RelRec + Yield)/(B0 * R0),
             B = BF * RelRec + Yield,
             VB = vBF * RelRec + Yield,
             VB_VB0 = (vBF * RelRec + Yield)/(vB0 * R0),
             RelRec=RelRec,
             SB0 = SB0 * R0,
             B0=B0 * R0,
             apicF=apicF)

    return(out)
  }


}



MSYMLE<-function(out,OMI,yr=NA,useF01=F){

  if(is.na(yr))yr=out$ny
  Fprof<-meanFs(FML=out$FL[out$ny,,,,], iALK=out$iALK[,out$ny,,], N=out$N[,out$ny,,,],
                wt_age=t(out$wt_age[out$ny,,]))
  V=Fprof/apply(Fprof,1,max)

  nt<-rep(1,2)
  np<-out$np
  yswitch<-rep(NA,2)
  type=rep(NA,out$np)
  for(pp in 1:out$np) {
    recs<-unique(out$R0[pp,])
    nt[pp]<-length(recs)
  }

  typey<-array("BH",c(out$np,out$ny))
  for(sr in 1:OMI@nSR){
    typey[OMI@SRp[sr],OMI@SRminyr[sr]:OMI@SRmaxyr[sr]]<-OMI@SRtype[sr]
  }

  type<-typey[,yr]


  # get steepness of most recent SR relationship (even if HS)
  hs<-rep(NA,out$np)
  SSB0<-rep(NA,out$np)
  R0<-rep(NA,out$np)
  for(pp in 1:out$np){

    R0[pp]<-out$R0[pp,yr]
    SSB0[pp]<-R0[pp]*OMI@SSBpR[pp]

    if(type[pp]=="BH"){
      hs[pp]<-out$h[pp,yr]
    }else if(type[pp]=="HS"){

      ind<-(1:out$ny)[unique(out$R0[pp,])[nt[pp]] == out$R0[pp,]]
      yr_nam<-OMI@years[1]+ind-1
      ind_inflect<-yr_nam<1996&yr_nam>1989
      SSB=out$SSB[pp,ind,out$spawns[pp]]
      SSB_inflect<-mean(SSB[ind_inflect])

      if(SSB_inflect<(0.2*SSB0[pp])){
        hs[pp]=0.98
      }else{
        frac<-SSB_inflect/SSB0[pp]
        hs[pp]=0.2/frac
      }
      if(hs[pp]>0.98)hs[pp]=0.98
      if(hs[pp]<0.3)hs[pp]=0.3

    }
  }

 # MSYs<-array(NA,c(out$np,8))

  for(pp in 1:out$np){
   if(!useF01){
     res<-optMSY_eq(M_age=out$M_age[pp,],
                          Wt_age=out$wt_age[yr,,pp],
                          Mat_age= out$mat_age[pp,],
                          V=V[pp,],
                          maxage=out$na,
                          R0=out$R0[pp,yr],
                          SRrel=1,
                          hs=hs[pp])
   }else{
     res<-optF01(M_age=out$M_age[pp,],
               Wt_age=out$wt_age[yr,,pp],
               Mat_age= out$mat_age[pp,],
               V=V[pp,],
               maxage=out$na,
               R0=out$R0[pp,yr],
               SSBpR=OMI@SSBpR[pp],
               hs=hs,SRrel=1)
   }


   # 1      2  3   4       5     6  7   8       9       10   11
   # Yield, F, SB, SB/SB0, B/B0, B, VB, VB_VB0, RelRec, SB0, B0
   MSY<-res[1]
   FMSYap<-res[2]
   UMSY<-res[1]/(res[7]+res[1]) # may be necessary to do this in terms of Biomass (usually vulnerable biomass) due to the difficulty in characterizing vulnerable biomass over seasons and areas
   BMSY<-res[7]
   SSBMSY<-res[3]
   BMSY_B0<-res[5]
   SSBMSY_SSB0<-res[4]
   RMSY_R0<-res[9]/R0[pp]
   all<-data.frame(MSY,FMSYap,UMSY,BMSY,SSBMSY,BMSY_B0,SSBMSY_SSB0,RMSY_R0)
   if(pp==1){
     MSYs<-all
   }else{
     MSYs<-rbind(MSYs,all)
   }

  }
  rownames(MSYs)<-c("East stock","West stock")

  MSYs
}



MSYMLE_parallel<-function(i,FMLs,iALK,N,wt_age,M_age,mat_age,R0_arr,fixpar_arr,SSBpR,SRtypes,maxage){

  FML<-FMLs[i,,,,]
  R0=R0_arr[i,]
  hs=fixpar_arr[i,]
  #Fprof<-meanFs2(FML, iALK, N, wt_age, M_age)
  Fprof<-meanFs(FML, iALK, N, wt_age)

  V=Fprof/apply(Fprof,1,max)
  # V2 = Fprof2/apply(Fprof2,1,max)
  # get steepness of most recent SR relationship (even if HS)
  SSB0<-rep(NA,2)
  for(pp in 1:2){

    SSB0[pp]<-R0[pp]*SSBpR[pp]

    if(SRtypes[pp]=="HS"){

      frac<-hs[pp]

      hs[pp]=0.2/frac

      if(hs[pp]>0.98)hs[pp]=0.98
      if(hs[pp]<0.3)hs[pp]=0.3

    }

  }
  # MSYs<-array(NA,c(out$np,8))

  for(pp in 1:2){

      res<-optMSY_eq(M_age=M_age[pp,],
                     Wt_age=wt_age[pp,],
                     Mat_age=mat_age[pp,],
                     V=V[pp,],
                     maxage=maxage,
                     R0=R0[pp],
                     SRrel=1,
                     hs=hs[pp])

    # 1      2  3   4       5     6  7   8       9       10   11  12
    # Yield, F, SB, SB/SB0, B/B0, B, VB, VB_VB0, RelRec, SB0, B0, ApicalF
    # from func        2        3                 4                                   5
    #Yield=Yield,     F= FF, SB = SBF * RelRec, SB_SB0 = (SBF * RelRec)/(SB0 * R0x), B_B0 = (BF * RelRec)/(B0 * R0x),
    #      6                  7                      8                              9              10                 11
    #B = BF * RelRec,  VB = vBF * RelRec, VB_VB0 = (vBF * RelRec)/(vB0 * R0x),  RelRec=RelRec,  SB0 = SB0 * R0x,    B0=B0 * R0x
    MSY<-res[1]
    FMSYap<-res[2]
    UMSY<-res[1]/(res[7]+res[1])
    BMSY<-res[7]
    SSBMSY<-res[3]
    BMSY_B0<-res[5]
    SSBMSY_SSB0<-res[4]
    RMSY_R0<-res[9]/R0[pp]
    #                1    2      3   4     5       6       7          8
    all<-data.frame(MSY,FMSYap,UMSY,BMSY,SSBMSY,BMSY_B0,SSBMSY_SSB0,RMSY_R0)
    if(pp==1){
      MSYs<-all
    }else{
      MSYs<-rbind(MSYs,all)
    }

  }
  rownames(MSYs)<-c("East stock","West stock")

  MSYs
}



optMSY_eq <- function(M_age, Wt_age, Mat_age, V, maxage, R0, SRrel, hs) {
  bounds <- c(0.0000001, 5)
  doopt <- optimise(MSYCalcs_plus, log(bounds), M_at_Age=M_age, Wt_at_Age=Wt_age,
                    Mat_at_Age=Mat_age, V_at_Age=V, maxage, R0x=R0, SRrelx=SRrel, hx=hs, opt=1)

  apicFMSY <- exp(doopt$minimum)
  apicFMSY2 <- apicFMSY

  MSYs <- MSYCalcs_plus(log(apicFMSY), M_at_Age=M_age, Wt_at_Age=Wt_age,
                        Mat_at_Age=Mat_age, V_at_Age=V, maxage, R0x=R0, SRrelx=SRrel, hx=hs, opt=2)

  return(MSYs)

}


MSYCalcs_plus <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age,
                     maxage, R0x, SRrelx, hx, opt=1, plusgroup=1) {
  # Box 3.1 Walters & Martell 2004
  FF <- exp(logF)
  lx <- rep(1, maxage)
  l0 <- c(1, exp(cumsum(-M_at_Age[1:(maxage-1)]))) # unfished survival

  surv <- exp(-M_at_Age - FF * V_at_Age)
  for (a in 2:maxage) {
    lx[a] <- lx[a-1] * surv[a-1] # fished survival
  }

  if (plusgroup == 1) {
    l0[length(l0)] <- l0[length(l0)]/(1-exp(-M_at_Age[length(l0)]))
    lx[length(lx)] <- lx[length(lx)]/(1-surv[length(lx)])
  }

  Egg0 <- sum(l0 * Wt_at_Age * Mat_at_Age) # unfished egg-per-recruit (assuming fecundity proportional to weight)
  EggF <- sum(lx * Wt_at_Age * Mat_at_Age) # fished egg-per-recruit (assuming fecundity proportional to weight)

  vB0 <- sum(l0 * Wt_at_Age * V_at_Age) # unfished and fished vuln. biomass per-recruit
  vBF <- sum(lx * Wt_at_Age * V_at_Age)

  SB0 <- sum(l0 * Wt_at_Age * Mat_at_Age) # spawning biomas per-recruit - same as eggs atm
  SBF <- sum(lx * Wt_at_Age * Mat_at_Age)

  B0 <- sum(l0 * Wt_at_Age) # biomass-per-recruit
  BF <- sum(lx * Wt_at_Age)

  hx[hx>0.999] <- 0.999
  recK <- (4*hx)/(1-hx) # Goodyear compensation ratio
  reca <- recK/Egg0

  SPR <- EggF/Egg0
  # Calculate equilibrium recruitment at this SPR
  if (SRrelx ==1) { # BH SRR
    recb <- (reca * Egg0 - 1)/(R0x*Egg0)
    RelRec <- (reca * EggF-1)/(recb*EggF)
  }
  if (SRrelx ==2) { # Ricker
    bR <- (log(5*hx)/(0.8*SB0))
    aR <- exp(bR*SB0)/(SB0/R0x)
    RelRec <- (log(aR*EggF/R0x))/(bR*EggF/R0x)
  }

  RelRec[RelRec<0] <- 0

  Z_at_Age <- FF * V_at_Age + M_at_Age
  YPR <- sum(lx * Wt_at_Age * FF * V_at_Age * (1 - exp(-Z_at_Age))/Z_at_Age)
  Yield <- YPR * RelRec

  if (opt == 1)  return(-Yield)
  if (opt == 2) {
    out <- c(Yield=Yield,
             F= FF,
             SB = SBF * RelRec,
             SB_SB0 = (SBF * RelRec)/(SB0 * R0x),
             B_B0 = (BF * RelRec)/(B0 * R0x),
             B = BF * RelRec,
             VB = vBF * RelRec,
             VB_VB0 = (vBF * RelRec)/(vB0 * R0x),
             RelRec=RelRec,
             SB0 = SB0 * R0x,
             B0=B0 * R0x)
    return(out)
  }
}

MSYCalcs <- function(logapicF, MatAge, WtAge, MatureAge, VAge, maxage, R0, SRrel, hs, opt=1) {
  # Box 3.1 Walters & Martell 2004
  apicF <- exp(logapicF)
  lx <- l0 <- rep(1, maxage)
  for (a in 2:maxage) {
    l0[a] <- l0[a-1] * exp(-MatAge[a-1])
    lx[a] <- lx[a-1] * exp(-(MatAge[a-1] + apicF*VAge[a-1]))
  }
  Egg0 <- sum(l0 * WtAge * MatureAge) # unfished egg production (assuming fecundity proportional to weight)
  EggF <- sum(lx * WtAge * MatureAge) # fished egg production (assuming fecundity proportional to weight)

  vB0 <- sum(l0 * WtAge * VAge)
  vBF <- sum(lx * WtAge * VAge)

  SB0 <- sum(l0 * WtAge * MatureAge) # same as eggs atm
  SBF <- sum(lx * WtAge * MatureAge)

  B0 <- sum(l0 * WtAge) # same as eggs atm
  BF <- sum(lx * WtAge)

  hs[hs>0.999] <- 0.999
  recK <- (4*hs)/(1-hs) # Goodyear compensation ratio
  reca <- recK/Egg0
  if (SRrel ==1) {
    recb <- (reca * Egg0 - 1)/(R0*Egg0) # BH SRR
    RelRec <- (reca * EggF-1)/(recb*EggF)
  }
  if (SRrel ==2) {
    recb <- log(reca*Egg0)/(R0*Egg0) # Ricker SRR
    RelRec <- (log(reca*EggF))/(recb*EggF)
  }


  RelRec[RelRec<0] <- 0

  Fa <- apicF*VAge
  Za <- Fa + MatAge
  relyield <- Fa/Za * lx * (1-exp(-Za)) * WtAge
  YPR <- sum(relyield)
  Yield <- YPR * RelRec

  if (opt == 1)  return(-Yield)
  if (opt == 2) {
    out <- c(Yield=Yield,
             F=-log(1 - (Yield/(vBF*RelRec+Yield))),
             SB = SBF * RelRec,
             SB_SB0 = (SBF * RelRec)/(SB0 * R0),
             B_B0 = (BF * RelRec + Yield)/(B0 * R0),
             B = BF * RelRec + Yield,
             VB = vBF * RelRec + Yield,
             VB_VB0 = (vBF * RelRec + Yield)/(vB0 * R0),
             RelRec=RelRec,
             SB0 = SB0 * R0,
             B0=B0 * R0,
             apicF=apicF)

    return(out)
  }


}

