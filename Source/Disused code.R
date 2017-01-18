
# Disused code 

setClass("MSE",representation(
  
  # Description
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",                           
  
  # Dimensions
  nsim="integer",npop="integer",nages="integer",             # MSE dimensions
  nyears="integer",nHyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
  proyears="integer", nMPs="integer", targpop="integer",     # Proyears, number of management procedures
  
  # Observation model 
  Cimp="numeric",Cb="numeric",Cerr="array",
  Iimp="numeric",Ibeta="numeric",Ierr="array",
  nCAAobs="numeric",nCALobs="numeric",Lcv="numeric",
  Mb="numeric",Kb="numeric",t0b="numeric",Linfb="numeric",
  LFCb="numeric",LFSb="numeric",
  FMSYb="numeric",FMSY_Mb="numeric",BMSY_B0b="numeric",
  ageMb="numeric", 
  Dimp="numeric", Db="numeric",Derr="array",
  Btimp="numeric", Btb="numeric",Bterr="array",
  Ftimp="numeric", Ftb="numeric",Fterr="array",
  
  hb="numeric",
  Recbcv="numeric",
  IMSYb="numeric", MSYb="numeric", BMSYb="numeric",
  
  # Management quantities
  C="array",
  D="array",
  B_BMSY="array",
  F_FMSY="array",
  B="array",
  SSB="array",
  TAC="array",
  
  # Performance metrics
  Perf="data.frame",
  POF="array",
  Y="array",
  AAVY="array",
  PB10="array",
  PB50="array",
  PB100="array",
  
  MPs="character"
  
)) 

setMethod("initialize", "MSE", function(.Object,OM,Obs,MPs="UMSY",interval=3,IE="Umax"){
  
  # .Object})
  # .Object<-new('MSE',OM,Obs)
  
  # Auto-correlation in recrutiment deviations is currently disabled
  set.seed(OM@seed)
  if(class(OM)!='OM'){
    print(paste('Could not run MSE:',deparse(substitute(OMd)),'not of class OM'))
    stop()
  }
  if(class(Obs)!='Obs'){
    print(paste('Could not run MSE:',deparse(substitute(Obs)),'not of class Obs'))
    stop()
  }
  if(class(get(MPs[1]))!='ABT_MP'){
    print(paste('Could not run MSE:',deparse(substitute(MPs[1])),'not of class ABT_MP'))
    stop()
  }
  if(class(get(IE))!='ABT_IE'){
    print(paste('Could not run MSE:',deparse(substitute(IE)),'not of class ABT_IE'))
    stop()
  }
  
  
  # copy over dimensions ------
  dimslots<-slotNames(OM)[1:13]
  for(i in 1:13)slot(.Object,dimslots[i])<-slot(OM,dimslots[i])
  
  # -------------------------------------------------------------------------
  
  cat("Constructing arrays")
  cat("\n")
  flush.console()  
  
  # Dimensions  S P A Y M R
  nsim<-OM@nsim
  npop<-OM@npop
  nyears<-OM@nyears
  proyears<-OM@proyears
  nages<-OM@nages
  nsubyears<-OM@nsubyears
  nareas<-OM@nareas
  nfleets<-OM@nfleets
  targpop<-as.integer(OM@targpop)
  .Object@targpop<-targpop
  allyears<-nyears+proyears
  nMPs<-length(MPs)
  .Object@nMPs<-nMPs
  nlen<-OM@nlen
  lenbins<-OM@lenbins
  mulen<-OM@mulen
  
  
  # Define arrays -----------------------------------------------------------
  
  # Management variables
  .Object@C<-.Object@D<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@TAC<-.Object@F_FMSY<-.Object@B_BMSY<-array(NA,c(nMPs,nsim,allyears))
  
  # Run historical simulation ----------------------------------------------
  
  surv=tomt(exp(-apply(OM@M[,,1:nages,1],2:1,cumsum)))
  agearray<-array(rep(1:nages,each=npop),c(npop,nages))
  
  Len_age<-Wt_age<-array(NA,c(nsim,npop,nages,allyears))
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:allyears))
  Len_age[ind]<-OM@Linf[ind[,c(1,2,4)]]*(1-exp(-OM@K[ind[,c(1,2,4)]]*(agearray[ind[,2:3]]-OM@t0[ind[,2]])))
  
  Wt_age[ind]<-OM@a[ind[,2]]*Len_age^OM@b[ind[,2]]
  SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,allyears+1,nsubyears,nareas)) # only need aggregated catch for these purposes
  SSBA<-array(NA,c(nsim,npop,allyears))
  FD<-array(NA,c(nsim,nfleets,allyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  FM<-VB<-C<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  CA<-array(NA,c(nsim,npop,allyears,nsubyears,nareas))
  
  mref<-c(2:nsubyears,1)
  y<-1
  m<-1
  
  RFL<-array(NA,c(nsim,nfleets,nlen,nyears))
  indL<-as.matrix(expand.grid(1:nsim,1:nfleets,1:nlen,1:nyears))
  RFL[indL]<-OM@q[indL[,c(1,2)]]*OM@sel[indL[,1:3]]*OM@E[indL[,c(1,2,4)]]
  
  #got to here! translate RFL (fishing mort by length to fishing mort by age)
  Ftrans<-array(0,c(nsim,nfleets,nyears,nages,nlen))
  Find<-as.matrix(expand.grid(1:nsim,1:nfleets,1:nyears,1:nages,1:nlen))
  
  Lind<-Find[,c(1,2,5,3)]
  Ftrans[Find]<-OM@iALK[Find]*RFL[Lind]
  RF<-apply(Ftrans,c(1,2,4,3),sum)
  
  SFAYMR<-as.matrix(expand.grid(1:nsim, 1:nfleets,1:nages,y,m,1:nareas)) # Set up some array indexes
  SFAY<-SFAYMR[,1:4]
  
  SPAYMR<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas))     # Set up some array indexes
  SPA<-SPAYMR[,1:3]
  SP<-SPAYMR[,1:2]
  SA<-SPAYMR[,c(1,3)]
  SPAR<-SPAYMR[,c(1:3,6)]
  SPAY<-SPAYMR[,1:4]
  SPAM<-SPAYMR[,c(1:3,5)]
  
  SSN[SPAYMR]<-surv[SPA]*OM@R0[SP]*OM@MIdist[SPAR]                                 # Calculate initial spawning stock numbers
  NSN[SPAYMR]<-surv[SPA]*OM@R0[SP]*OM@Idist[SPAR]                                  # Calculate initial non spawning numbers
  SSB[SPAYMR]<-SSN[SPAYMR]*Wt_age[SPAY]                                     # Calculate spawning stock biomass
  SSB0<-apply(SSB[,,,y,m,],1:2,sum)
  SSBpR<-SSB0/OM@R0                            # Calculate spawning stock biomass per recruit
  
  bR<-log(5*OM@h)/(0.8*SSB0)                                     # Ricker SR params
  aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params
  
  SPAYMRF2<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas,1:nfleets))
  SF2<-SPAYMRF2[,c(1,7)]
  SFA2<-SPAYMRF2[,c(1,7,3)]
  SPRFA2<-SPAYMRF2[,c(1,2,6,7,3)]
  SPFR2<-SPAYMRF2[,c(1,2,7,6)]
  
  cat("Re-running historical simulations")
  cat("\n")
  for(y in 1:nyears){
    cat(".")
    sof<-apply(array(OM@E[,,y]*OM@q,c(nsim,nfleets,nages))*OM@sel,c(1,3),sum)
    sof<-sof/apply(sof,1,max)
    SPAYMR[,4]<-y
    SPAY<-SPAYMR[,1:4]
    SPAYMRF2[,4]<-y
    SPAY2<-SPAYMRF2[,1:4]
    SFAY2<-SPAYMRF2[,c(1,7,3,4)]
    
    for(m in 1:nsubyears){
      
      SPAYMR[,5]<-m
      SPAM<-SPAYMR[,c(1:3,5)]
      SPAYMRF2[,5]<-m
      SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
      SPAYMR2<-SPAYMRF2[,1:6]
      
      # VB[SPAYMRF2]<-(NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2]*RF[SFAY2]                    # Calculate prop to vunerable biomass
      
      VB[SPAYMRF2]<-(NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2]*OM@sel[SFA2]                    # Calculate prop to vunerable biomass
      VBA[SPAYMR]<-(NSN[SPAYMR]+SSN[SPAYMR])*Wt_age[SPAY]*sof[SA]                          # Calculate actual vunerable biomass
      
      FD[,,y,m,]<-(apply(VB[,,,y,m,,],c(1,5,4),sum)^array(OM@Spat_targ,c(nsim,nfleets,nareas)))/
        array(apply(apply(VB[,,,y,m,,],c(1,5,4),sum)^array(OM@Spat_targ,c(nsim,nfleets,nareas)),1:2,mean),c(nsim,nfleets,nareas)) # distribute E x qs x sel by area according to spatial targetting parameters
      FM[SPAYMRF2]<-RF[SFAY2]*FD[SFYMR2]
      Ftot<-apply(FM[,,,y,m,,],1:4,sum)
      Z[SPAYMR]<-Ftot[SPAR]+OM@M[SPAY]/nsubyears
      
      # harvest fish
      #C[,,,y,m,]<-(SSN[,,y,m,]+NSN[,,y,m,])*(1-exp(-Z[,,y,m,]))*(FM[,,,y,m,]/Z[,,y,m,])
      C[SPAYMRF2]<-(SSN[SPAYMR2]+NSN[SPAYMR2])*(1-exp(-Z[SPAYMR2]))*(FM[SPAYMRF2]/Z[SPAYMR2])
      
      SSN[,,,y,m,]<-SSN[,,,y,m,]*exp(-Z[,,,y,m,])
      NSN[,,,y,m,]<-NSN[,,,y,m,]*exp(-Z[,,,y,m,])
      
      # move fish
      SSN[,,,y,m,]<-domov(SSN[,,,y,m,],OM@Mmov[,,,m,,])
      NSN[,,,y,m,]<-domov(NSN[,,,y,m,],OM@mov[,,,m,,])
      
      #  age individuals
      for(pp in 1:npop){
        if(OM@Recsubyr[pp]==m){
          # age fish
          SSBA[,pp,y]<-apply(SSN[,pp,,y,m,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),1,sum)
          SSBdist<-apply(SSN[,pp,,y,m,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]
          TN<-NSN[,pp,1:(nages-1),y,m,]+SSN[,pp,1:(nages-1),y,m,]
          
          # Maturity is refreshed which is dumb: kind of removes the point in modelling movement of modelling mature and non mature fish separtely
          SSN[,pp,2:nages,y,m,]<-TN*array(OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
          NSN[,pp,2:nages,y,m,]<-TN*array(1-OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
          
          # recruit fish
          if(OM@SRrel[pp]==1){    # Beverton-Holt recruitment
            rec<-OM@Recdevs[,pp,y]*((0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)*SSBA[,pp,y])) 
          }else{              # Most transparent form of the Ricker uses alpha and beta params
            rec<-OM@Recdevs[,pp,y]*aR[,pp]*SSBA[,y]*exp(-bR[,pp]*SSBA[,pp,y])
          }            
          SSN[,pp,1,y,m,]<-rec*OM@mat[,pp,1,y]*SSBdist
          NSN[,pp,1,y,m,]<-rec*(1-OM@mat[,pp,1,y])*SSBdist
        } # if its the right subyear
      } # end of pop
      
      # Send to the next year      
      if(m==nsubyears){
        SSN[,,,y+1,1,]<-SSN[,,,y,nsubyears,]
        NSN[,,,y+1,1,]<-NSN[,,,y,nsubyears,]
      }else{
        SSN[,,,y,m+1,]<-SSN[,,,y,m,]
        NSN[,,,y,m+1,]<-NSN[,,,y,m,]
      }
    } # end of subyear  
  } # end of year  
  
  Bcur<-apply(SSN[,,,nyears,4,]*
                array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)
  D<-Bcur/SSB0
  
  # Generate observation errors ---------------------------------------------
  
  .Object@Cimp<-runif(nsim,Obs@Ccv[1],Obs@Ccv[2])
  .Object@Cb<-trlnorm(nsim,1,Obs@Cbcv)
  .Object@Cerr<-array(trlnorm(nsim*allyears,rep(.Object@Cb,allyears),rep(.Object@Cimp,allyears)),c(nsim,allyears)) 
  
  .Object@Iimp<-runif(nsim,Obs@Icv[1],Obs@Icv[2])
  .Object@Ierr<-array(trlnorm(nsim*allyears,1,rep(.Object@Iimp,allyears)),c(nsim,allyears)) 
  .Object@Ibeta<-exp(runif(nsim,log(Obs@Ibeta[1]),log(Obs@Ibeta[2])))
  
  .Object@Btimp<-runif(nsim,Obs@Btcv[1],Obs@Btcv[2])
  .Object@Btb<-trlnorm(nsim,1,Obs@Btbcv)
  .Object@Bterr<-array(trlnorm(nsim*allyears,rep(.Object@Btb,allyears),rep(.Object@Btimp,allyears)),c(nsim,allyears)) 
  
  .Object@Mb<-trlnorm(nsim,1,Obs@Mbcv)
  .Object@Kb<-trlnorm(nsim,1,Obs@Kbcv)
  .Object@Linfb<-trlnorm(nsim,1,Obs@Linfbcv)
  .Object@t0b<-rep(1,nsim)
  
  .Object@MSYb<-trlnorm(nsim,1,Obs@MSYbcv)
  .Object@BMSYb<-trlnorm(nsim,1,Obs@BMSYbcv)
  .Object@IMSYb<-trlnorm(nsim,1,Obs@IMSYbcv)
  .Object@FMSYb<-trlnorm(nsim,1,Obs@FMSYbcv)
  .Object@FMSY_Mb<-trlnorm(nsim,1,Obs@FMSY_Mbcv)
  
  .Object@nCAAobs<-ceiling(runif(nsim,Obs@nCAAobs[1],Obs@nCAAobs[2]))
  
  .Object@ageMb<-trlnorm(nsim,1,Obs@ageMbcv)
  
  # Run projections ------------------------------------------------
  cat("\n")
  cat("Running projections")
  cat("\n")
  
  sfExport(list=c("XSA","DD","DD_R","UMSY","tiny"))
  upyrs<-nyears+(0:(floor(OM@proyears/interval)-1))*interval  # the years in which there are updates (every three years)
  
  sof<-apply(array(OM@E[,,nyears]*OM@q,c(nsim,nfleets,nages))*OM@sel,c(1,3),sum)
  sof<-sof/apply(sof,1,max)
  SFAY1<-SFAY2
  Find<-as.matrix(expand.grid(1:nsim,1:npop,1:nareas,1:nfleets))[,c(1,2,4,3)]
  FindSF<-Find[,c(1,3)]
  FindSPR<-Find[,c(1,2,4)]
  SPFR3<-as.matrix(expand.grid(1:nsim,1:npop,1:nfleets,1:nareas))
  SPR3<-SPFR3[,c(1,2,4)]
  testC<-array(NA,c(nsim,npop,nfleets,nareas))
  CAdist<-array(NA,c(nsim,npop,nareas,nfleets,nages))
  CAA<-sampCatch(apply(C[,,,1:(nyears-1),,,],c(1,3,4),sum),.Object@nCAAobs)
  nCALbins<-30
  CAL_bins<-seq(0,max(OM@Linf),length.out=nCALbins)
  CAL_bins<-c(CAL_bins,CAL_bins[nCALbins]*2)
  CAL<-makeCAL(CAA,Linf=OM@Linf[,1,1:nyears],K=OM@K[,1,1:nyears],t0=OM@t0[1],CAL_bins)
  
  for(MP in 1:nMPs){
    
    cat(paste(MP,"/",nMPs," Running MSE for ",MPs[MP],sep=""))  # print a progress report
    cat("\n")
    flush.console()                                                  # update the console
    TAC<-rep(NA,nsim)  # refresh the MP store of TAC among simulations
    
    for(y in nyears:(nyears+proyears)){
      cat(".")
      if(y%in%upyrs){# Operate MP S P A Y M R
        
        # Simulate imperfect information ----------------------------------------------
        Iobs<-apply(SSBA[,,1:(y-1)],c(1,3),sum)^.Object@Ibeta
        Iobs<-Iobs*.Object@Ierr[,1:(y-1)]
        Iobs<-Iobs/apply(Iobs,1,mean)
        
        if(y!=nyears){
          nuy<-(upyrs[match(y,upyrs)-1]):(y-1)
          nCAA<-sampCatch(apply(C[,,,nuy,,,],c(1,3,4),sum),.Object@nCAAobs)
          CAA<-abind(CAA,nCAA,along=3)
          CAL<-abind(CAL,makeCAL(nCAA,Linf=OM@Linf[,1,nuy],K=OM@K[,1,nuy],t0=OM@t0[1],CAL_bins),along=3)
        }
        
        #SPAYMRF
        pset<-list("Cobs"=apply(C[,,,1:(y-1),,,]*array(Wt_age[,,,1:(y-1)],c(nsim,npop,nages,y-1,nsubyears,nareas,nfleets)),c(1,4),sum)*.Object@Cerr[,1:(y-1)],
                   "Iobs"=Iobs,
                   "K"=OM@K[,1,y-1]*.Object@Kb,
                   "Linf"=OM@Linf[,1,y-1]*.Object@Kb,
                   "t0"=rep(OM@t0[1],nsim),
                   "M"=OM@M[,1,,(y-1)]*.Object@Mb,
                   "Bt"=apply(VBA[,,,(y-1),4,],1,sum)*.Object@Bterr[,(y-1)],
                   "MSY"=OM@MSY*.Object@MSYb,
                   "BMSY"=OM@BMSY*.Object@BMSYb,
                   "UMSY"=OM@UMSY*.Object@FMSYb,
                   "a"=rep(OM@a,nsim),
                   "b"=rep(OM@b,nsim),
                   "nages"=OM@nages,
                   "ageM"=OM@ageM[,1,(y-1)]*.Object@ageMb,
                   "Mat"=OM@mat[,1,,1:(y-1)],
                   "Bt_PI"=apply(VBA[,,,(y-1),4,],1,sum),
                   "UMSY_PI"=OM@UMSY,
                   "CAA"=CAA,
                   "CAL"=CAL,
                   "CAL_bins"=CAL_bins,
                   "MPrec"=TAC
        )
        assign("pset",pset,envir=globalenv()) # debugging
        sfExport("pset")
        if(MPs[MP]=="XSA")TAC<-sapply(1:nsim,get(MPs[MP]),pset)
        if(MPs[MP]!="XSA")TAC<-sfSapply(1:nsim,get(MPs[MP]),pset)
        #print(TAC)
      }
      .Object@TAC[MP,,y]<-TAC
      for(mm in 1:nsubyears){
        
        SPAYMR[,4]<-y
        SPAYMR[,5]<-mm
        SPAY<-SPAYMR[,1:4]
        SPAM<-SPAYMR[,c(1:3,5)]
        
        SPAYMRF2[,4]<-y
        SPAYMRF2[,5]<-mm
        SPAY2<-SPAYMRF2[,1:4]
        SFAY2<-SPAYMRF2[,c(1,7,3,4)]
        SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
        SPAYMR2<-SPAYMRF2[,1:6]
        
        VBA[SPAYMR]<-(NSN[SPAYMR]+SSN[SPAYMR])*Wt_age[SPAY]*sof[SA]                          # Calculate actual vulnerable biomass
        
        #Ftot<-TAC/apply(VBA[,,,y,m,],1,sum)
        
        #Fdist<-(apply(VB[,,,y,m,,],c(1,4),sum)^array(OM@Spat_targ,c(nsim,npop,nages,nareas,nfleets)))/
        # array(apply(apply(VB[,,,y,m,,],c(1,4),sum)^array(OM@Spat_targ,c(nsim,npop,nages,nareas,nfleets)),1,mean),c(nsim,nareas)) # distribute E x qs x sel by area according to spatial targetting parameters
        #Fdist<-Fdist/(max(Fdist)/OM@UMSY)
        
        VBs<-apply(VBA[,,,y,mm,],c(1,2,4),sum)
        Fdist[Find]<-VBs[FindSPR]^OM@Spat_targ[FindSF]
        Fdist<-OM@UMSY*Fdist/array(apply(Fdist,1:3,sum),dim(Fdist))
        Btemp<-apply((NSN[,,,y,mm,]+SSN[,,,y,mm,])*array(Wt_age[,,,y],c(nsim,npop,nages,nareas)),c(1,2,4),sum)
        testC[SPFR3]<-(1-exp(-Fdist[SPFR3]))*Btemp[SPR3]
        
        Up<-1-exp(-Fdist)
        Crat<-(TAC/4)/apply(testC,1,sum)
        testU<-Crat*Up
        Fp<-(-log(1-(do.call(IE,list(testU)))))
        testC[SPFR3]<-(1-exp(-Fp[SPFR3]))*Btemp[SPR3]
        
        CAdist[SPRFA2]<-(NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2]*OM@sel[SFA2]
        CAdist<-CAdist/array(apply(CAdist,1:4,sum,na.rm=T),dim(CAdist))
        
        C[SPAYMRF2]<-testC[SPFR2]*CAdist[SPRFA2]
        C[SPAYMRF2][is.na(C[SPAYMRF2])]<-0
        C[SPAYMRF2]<-C[SPAYMRF2]/Wt_age[SPAY2]
        # test: cbind(apply(C[,,,y,m,,],1,sum), TAC/4)
        #FM[SPAYMRF2]<--log(1-(C[SPAYMRF2]/((NSN[SPAYMR2]+SSN[SPAYMR2])*Wt_age[SPAY2])))
        Up<-array(C[SPAYMRF2]/(NSN[SPAYMR2]+SSN[SPAYMR2]),c(nsim,npop,nages,nareas,nfleets))
        Up[is.na(Up)|Up<0.00001]<-0.00001
        Up[Up>0.9]<-0.9
        FM[SPAYMRF2]<--log(1-Up)
        #FM[SPAYMRF2][is.na(FM[SPAYMRF2])]<-0
        Ftot<-apply(FM[,,,y,mm,,],1:4,sum)
        Z[SPAYMR]<-Ftot[SPAR]+OM@M[SPAY]/nsubyears
        
        # harvest fish
        SSN[,,,y,mm,]<-SSN[,,,y,mm,]*exp(-Z[,,,y,mm,])
        NSN[,,,y,mm,]<-NSN[,,,y,mm,]*exp(-Z[,,,y,mm,])
        
        # move fish
        SSN[,,,y,mm,]<-domov(SSN[,,,y,mm,],OM@Mmov[,,,mm,,])
        NSN[,,,y,mm,]<-domov(NSN[,,,y,mm,],OM@mov[,,,mm,,])
        
        #  age individuals
        for(pp in 1:npop){
          if(OM@Recsubyr[pp]==mm){
            # age fish
            SSBA[,pp,y]<-apply(SSN[,pp,,y,mm,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),1,sum)
            SSBdist<-apply(SSN[,pp,,y,mm,]*array(Wt_age[,pp,,y],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]
            TN<-NSN[,pp,1:(nages-1),y,mm,]+SSN[,pp,1:(nages-1),y,mm,]
            
            # Maturity is refreshed which is dumb: kind of removes the point in modelling movement of modelling mature and non mature fish separtely
            SSN[,pp,2:nages,y,mm,]<-TN*array(OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
            NSN[,pp,2:nages,y,mm,]<-TN*array(1-OM@mat[,pp,2:nages,y],c(nsim,nages-1,nareas))
            
            # recruit fish
            if(OM@SRrel[pp]==1){    # Beverton-Holt recruitment
              rec<-OM@Recdevs[,pp,y]*((0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)*SSBA[,pp,y])) 
            }else{              # Most transparent form of the Ricker uses alpha and beta params
              rec<-OM@Recdevs[,pp,y]*aR[,pp]*SSBA[,pp,y]*exp(-bR[,pp]*SSBA[,pp,y])
            }            
            SSN[,pp,1,y,mm,]<-rec*OM@mat[,pp,1,y]*SSBdist
            NSN[,pp,1,y,mm,]<-rec*(1-OM@mat[,pp,1,y])*SSBdist
          } # if its the right subyear
        } # end of pop
        
        
        # Send to the next year      
        if(mm==nsubyears){
          SSN[,,,y+1,1,]<-SSN[,,,y,nsubyears,]
          NSN[,,,y+1,1,]<-NSN[,,,y,nsubyears,]
        }else{
          SSN[,,,y,mm+1,]<-SSN[,,,y,mm,]
          NSN[,,,y,mm+1,]<-NSN[,,,y,mm,]
        }
      } # end of subyear  
      
      
    } # end of year  
    
    # Store results
    
    .Object@C[MP,,,]<-apply(C[,,,1:allyears,,,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets)),c(1,2,4),sum)
    SSB<-apply(SSN[,,,1:allyears,4,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nareas)),c(1,2,4),sum)
    .Object@D[MP,,,]<-SSB/apply(SSB0,1,sum)
    B<-apply((SSN[,,,1:allyears,4,]+NSN[,,,1:allyears,4,])*array(Wt_age,c(nsim,npop,nages,allyears,nareas)),c(1:2,4),sum)
    #Bthen<-apply((SSN[,,,1,4,]+NSN[,,,1,4,])*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)
    .Object@B_BMSY[MP,,]<-apply(array(B[,targpop,],dim=c(nsim,length(targpop),allyears)),c(1,3),sum)/OM@BMSY
    U<-apply(array(.Object@C[MP,,targpop,],c(nsim,length(targpop),allyears)),c(1,3),sum)/
      apply(array(VBA[,targpop,,1:allyears,4,],c(nsim,length(targpop),nages,allyears,nareas)),c(1,4),sum)
    .Object@F_FMSY[MP,,]<-U/OM@UMSY
    
    cat("\n")
  } # end of MP
  
  .Object@MPs<-MPs
  
  .Object
  
})


