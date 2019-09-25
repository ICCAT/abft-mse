

setClass("SimSam",representation(

  # Description
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",

  # Dimensions
  nsim="integer",npop="integer",nages="integer",             # MSE dimensions
  nyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
  proyears="integer", targpop="integer", nfleets="integer",    # Proyears, number of management procedures
  interval="integer",nma="integer",ma="array",                                  # Number of movement age classes, age class definitions
  nlen="integer",lenbins="numeric",       # Proyears
  mulen="numeric",

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


  simlist="list",

  # Performance metrics
  Perf="data.frame",
  POF="array",
  Y="array",
  AAVY="array",
  PB10="array",
  PB50="array",
  PB100="array"

))


setMethod("initialize", "SimSam", function(.Object,OM,Obs,movtype=2,OMDir="G:/M3",verbose=0,
                                           complexF=0,complexRD=0,M3temp="C:/M3temp/"){
  #.Object})
  #.Object<-new('SimSam')
  # Bias in fraction in spawning area (unfished)


  # Auto-correlation in recrutiment deviations is currently disabled
  set.seed(OM@seed)
  if(class(OM)!='OM'){
    print(paste('Could not run SimSam:',deparse(substitute(OMd)),'not of class OM'))
    stop()
  }
  if(class(Obs)!='Obs'){
    print(paste('Could not run SimSam:',deparse(substitute(Obs)),'not of class Obs'))
    stop()
  }


  # copy over dimensions ------
  dimslots<-slotNames(OM)[1:17]
  for(i in 1:17)slot(.Object,dimslots[i])<-slot(OM,dimslots[i])

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
  .Object@nfleets<-nfleets
  targpop<-as.integer(OM@targpop)
  .Object@targpop<-targpop
  allyears<-nyears+proyears
  nlen<-OM@nlen
  lenbins<-OM@lenbins
  mulen<-OM@mulen
  Wt_age<-OM@Wt_age
  nZeq<-OM@nZeq
  nydist<-OM@nydist
  nyeq<-OM@nyeq


  # Define arrays -----------------------------------------------------------

  # Management variables
  # !!!! This a temporary fix for simulation testing- keep maturity constant
  ind2<-ind<-TEG(dim(OM@mat))
  ind2[,4]<-1
  OM@mat[ind]<-OM@mat[ind2]
  OM@Wt_age[ind]<-OM@Wt_age[ind]
  OM@Mmov<-OM@mov
  OM@Recdevs[,,1]<-1

  # Run historical simulation ----------------------------------------------
  M<-OM@M
  Mtemp<-array(0,dim(OM@M))
  Mtemp[,,2:nages,]<-OM@M[,,1:(nages-1),]

  surv=tomt(exp(-apply(Mtemp[,,,1],2:1,cumsum)))
  surv[,,nages]<-surv[,,nages]*exp(-M[,,nages,1])/(1-exp(-M[,,nages,1]))

  N<-SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  SSBA<-array(NA,c(nsim,npop,allyears))
  FD<-array(NA,c(nsim,nfleets,allyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  FM<-VB<-C<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  CA<-array(NA,c(nsim,npop,allyears,nsubyears,nareas))

  mref<-c(2:nsubyears,1)  # movement reference
  y<-1
  m<-1

  # need to remake all these for OM renewal
  RFL<-array(NA,c(nsim,nfleets,nlen,nyears,nsubyears,nareas))
  indL<-TEG(dim(RFL))
  indE<-indL[,c(1,2,4,5,6)]
  RFL[indL]<-OM@q[indL[,c(1,2)]]*OM@sel[indL[,1:3]]*OM@E[indE]

  #got to here! translate RFL (fishing mort by length to fishing mort by age)
  Ftrans<-array(0,c(nsim,nfleets,nyears,nsubyears,nages,nlen,nareas,npop))
  Find<-TEG(dim(Ftrans))

  Lind<-Find[,c(1,2,6,3,4,7)] # s f l y m r
  Ftrans[Find]<-OM@iALK[Find[,c(1,8,3,5,6)]]*RFL[Lind]
  RF<-apply(Ftrans,c(1,8,5,3,4,7,2),sum) # s p a y m r f

  maxRF<-apply(RF,c(1,2,4,5,6,7),max) # s p y r f

  Rind<-TEG(c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))#as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nareas,1:nfleets))
  sel<-RF
  sel[Rind]<-sel[Rind]/maxRF[Rind[,c(1,2,4,5,6,7)]]
  sel<-sel[,,,nyears,nsubyears,,] # s p a r f # Take this from last year, in future simulations this may be by year so leave this code!

  SFAYMR<-as.matrix(expand.grid(1:nsim, 1:nfleets,1:nages,y,m,1:nareas)) # Set up some array indexes
  SFAY<-SFAYMR[,1:4]

  SPAYMR<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas))     # Set up some array indexes
  SARP<-SPAYMR[,c(1,3,6,2)]
  SPA<-SPAYMR[,1:3]
  SPR<-SPAYMR[,c(1,2,6)]
  SPMR<-SPAYMR[,c(1,2,5,6)]
  SP<-SPAYMR[,1:2]
  SA<-SPAYMR[,c(1,3)]
  SAR<-SPAYMR[,c(1,3,6)]
  SPAR<-SPAYMR[,c(1:3,6)]
  SPAY<-SPAYMR[,1:4]
  SPAM<-SPAYMR[,c(1:3,5)]

  # New model initialization ------------ pay         paymrf
  R0<- OM@R0
  h<-OM@h
  mat<-OM@mat
  mov<-OM@mov
  Zeq<-array(apply(M[,,,1:nZeq],1:3,mean),c(nsim,npop,nages,nsubyears,nareas))/nsubyears+apply(apply(RF[,,,1:nZeq,,,],1:6,sum),c(1,2,3,5,6),mean)
  SSB0<-apply(surv*array(R0,dim(surv))*Wt_age[,,,1]*mat[,,,1],1:2,sum)
  SSBpR<-SSB0/R0

  stemp<-array(1/nareas,dim=c(nsim,npop,nsubyears,nareas))
  movi<-mov[,,nages,,,]

  for(y in 1:nydist){

    for(m in 1:nsubyears){

      if(m==1){

        stemp[,,m,]<-apply(array(rep(stemp[,,nsubyears,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)

      }else{

        stemp[,,m,]<-apply(array(rep(stemp[,,m-1,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)

      }

    }

  }

  indN<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1,nsubyears,1:nareas))#
  N[indN]=R0[indN[,1:2]]*surv[indN[,1:3]]*stemp[indN[,c(1,2,5,6)]]

  SSB[,,,1,nsubyears,]<-N[,,,nyears,nsubyears,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)

  for(y in 1:nyeq){

    for(m in 1:nsubyears){

      if(m==1){ # first subyear

        N[,,,1,m,]<-exp(-Zeq[,,,nsubyears,])*N[,,,1,nsubyears,]
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)


      }else if(m==2){ # spawning subyear

        N[,,,1,m,]<-exp(-Zeq[,,,m-1,])*N[,,,1,m-1,]
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)
        spawnr<-apply(SSB[,,,1,m,],c(1,2,4),sum)/array(apply(SSB[,,,1,m,],1:2,sum),dim(SSB)[c(1,2,6)])
        SSBt<-apply(SSB[,,,1,m,],1:2,sum)
        N[,,nages,1,m,]<-N[,,nages,1,m,]+N[,,nages-1,1,m,] # plus group
        N[,,2:(nages-1),1,m,]<-N[,,1:(nages-2),1,m,]
        N[,,1,1,m,]<-spawnr*array(((0.8*R0*h*SSBt)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSBt)),dim(spawnr))
        #print(sum(N[1,1,1,1,m,]))
        #SSBA[,,1]<-apply(N[,,,1,m,]*array(Wt_age[,,,1]*OM@mat[,,,nyears],dim=c(nsim,npop,nages,nareas)),1:2,sum)


      }else{   # after spawning subyear

        N[,,,1,m,]<-exp(-Zeq[,,,m-1,])*N[,,,1,m-1,]
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)

      } # End of if subyear

    }   # end of subyear
  }     # end of equlibrium calculation year nyeq


  bR<-log(5*h)/(0.8*SSB0)                                      # Ricker SR params
  aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params


  y<-1
  m<-1

  SPAYMRF2<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas,1:nfleets))
  SF2<-SPAYMRF2[,c(1,7)]
  SFA2<-SPAYMRF2[,c(1,7,3)]
  SFAR2<-SPAYMRF2[,c(1,7,3,6)]
  SPRFA2<-SPAYMRF2[,c(1,2,6,7,3)]
  SPFR2<-SPAYMRF2[,c(1,2,7,6)]
  SPAY2<-SPAYMRF2[,1:4]
  SFAR2<-SPAYMRF2[,c(1,7,3,6)]
  SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
  SPAYRF2<-SPAYMRF2[,c(1,2,3,4,6,7)]
  SPARF2<-SPAYMRF2[,c(1,2,3,6,7)]

  for(m in 1:nsubyears){

    SPAYMRF2[,5]<-m
    SPAYMR2<-SPAYMRF2[,1:6]
    SPAYMR[,5]<-m
    VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                    # Calculate vunerable biomass
    #FM[SPAYMRF2]<-RF[SPAYRF2]#*FD[FYMR2]
    Ftot<-apply(RF[,,,y,m,,],1:4,sum)
    Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears
    C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*RF[SPAYMRF2]/Z[SPAYMR2]  # need to add back in mortality rate before C calculation
    #C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*RF[SPAYMRF2]/Z[SPAYMR2]

  }

  SPAYMR[,5]<-1
  SPAYMRF2[,5]<-1
  SPAYMR2<-SPAYMRF2[,1:6]

  cat("Re-running historical simulations")
  cat("\n")

  for(y in 2:nyears){

    SPAYMR[,4]<-y
    SPAY<-SPAYMR[,1:4]
    SPAYMRF2[,4]<-y
    SPAYRF2[,4]<-y
    SPAY2<-SPAYMRF2[,1:4]
    SFAY2<-SPAYMRF2[,c(1,7,3,4)]
    SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
    SFAR2<-SPAYMRF2[,c(1,7,3,6)]

    for(m in 1:nsubyears){

      SPAYMR[,5]<-m
      SPAM<-SPAYMR[,c(1:3,5)]
      SPAYMRF2[,5]<-m
      SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
      SPAYMR2<-SPAYMRF2[,1:6]

      if(m==1){
        N[,,,y,m,]<-N[,,,y-1,nsubyears,]*exp(-Z[,,,y-1,nsubyears,])
      }else{
        N[,,,y,m,]<-N[,,,y,m-1,]*exp(-Z[,,,y,m-1,])
      }

      # move fish
      N[,,,y,m,]<-domov(N[,,,y,m,],OM@mov[,,,m,,])


      VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                    # Calculate prop to vunerable biomass
      Ftot<-apply(RF[,,,y,m,,],1:4,sum)
      Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears

      # harvest fish
      #C[SPAYMRF2]<-N[SPAYMR2]*(exp(Z[SPAYMR2])-1)*RF[SPAYMRF2]/Z[SPAYMR2]
      C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*RF[SPAYMRF2]/Z[SPAYMR2]
      #  age individuals
      for(pp in 1:npop){
        if(OM@Recsubyr[pp]==m){

          # age fish
          SSBA[,pp,y]<-apply(N[,pp,,y-1,m,]*array(Wt_age[,pp,,nyears]*OM@mat[,pp,,nyears],dim=c(nsim,nages,nareas)),1,sum)
          SSBdist<-apply(N[,pp,,y-1,m,]*array(Wt_age[,pp,,nyears]*OM@mat[,pp,,nyears],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]

          N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,]
          N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]

          # recruit fish
          if(OM@SRrel[pp]==1){    # Beverton-Holt recruitment
            rec<-OM@Recdevs[,pp,y]*(0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)*SSBA[,pp,y])
          }else{              # Most transparent form of the Ricker uses alpha and beta params
            rec<-OM@Recdevs[,pp,y]*aR[,pp]*SSBA[,pp,y]*exp(-bR[,pp]*SSBA[,pp,y])
          }

          N[,pp,1,y,m,]<-rec*SSBdist


        } # if its the right subyear

      } # end of pop
      SSB[,,,y,m,]<-N[,,,y,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)

    } # end of subyear

  } # end of year

  Bcur<-apply(N[,,,nyears,nsubyears,]*
                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)

  #Bcur<-apply(N[ss,1,,nyears,nsubyears,]*
  #             array(Wt_age[ss,1,,nyears]*OM@mat[ss,1,,nyears],c(nages,nareas)),1:2,sum)

  #Bcur<-sum(array(N[targpop,,nyears,nsubyears,],c(length(targpop),nages,nareas))*
  #            array(Wt_age[targpop,,nyears]*mat[targpop,,nyears],c(length(targpop),nages,nareas)))

  SSBall<-N*array(Wt_age,dim(N))*array(OM@mat,dim(N))
  RAI<-apply(SSBall,c(1,4,5,6),sum)
  RAI<-RAI[,1:nyears,,]
  RAI<-RAI/array(apply(RAI,1,mean),dim(RAI))

  D<-Bcur/SSB0 # Check against OM@D (remember only targetpop is matched)

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

  # Generate data ------------------------------------------------
  datfile<-paste(OMDir,"/M3.dat",sep="")
  cat("\n")
  cat("Generating data")
  cat("\n")

  #sof<-apply(array(OM@E[,,nyears]*OM@q,c(nsim,nfleets,nages))*sel,c(1,3),sum)
  #sof<-sof/apply(sof,1,max)
  SFAY1<-SFAY2
  Find<-as.matrix(expand.grid(1:nsim,1:npop,1:nareas,1:nfleets))[,c(1,2,4,3)]
  FindSF<-Find[,c(1,3)]
  FindSPR<-Find[,c(1,2,4)]
  SPFR3<-as.matrix(expand.grid(1:nsim,1:npop,1:nfleets,1:nareas))
  SPR3<-SPFR3[,c(1,2,4)]

  # Age-length key --
  #contour(OM@iALK[1,1,1,,])

  # Spawning --
  spawnr<-array(NA,c(nsim,npop,nareas))

  for(pp in 1:npop){
    m<-OM@Recsubyr[pp]
    spawnr[,pp,]<-apply(SSN[,pp,,1,m,]*array(Wt_age[,pp,,1],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]
  }

  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nareas))
  sums<-apply(spawnr,1:2,sum)
  sind<-ind[,1:2]
  spawnr[ind]<-spawnr[ind]/sums[sind]

  # Fishery data -------------


  # Catch
  mult<-nyears*nsubyears*nareas*nfleets
  #Cerr<-array(trlnorm(nsim*mult,rep(.Object@Cb,mult),rep(.Object@Cimp,mult)),c(nsim,nyears,nsubyears,nareas,nfleets))
  Cerr<-array(trlnorm(nsim*mult,rep(1,mult),rep(.Object@Cimp,mult)),c(nsim,nyears,nsubyears,nareas,nfleets))
  Cobsta<-array(NA,c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nfleets))
  Cobsta[ind]<-C[ind]*Wt_age[ind[,1:4]]
  Cobst<-apply(Cobsta,c(1,4:7),sum,na.rm=T)*Cerr
  Cobsta<-apply(Cobsta,c(1,3:7),sum,na.rm=T)
  # CPUE
  Ierr<-array(trlnorm(nsim*mult,1,rep(.Object@Iimp,mult)),c(nsim,nyears,nsubyears,nareas,nfleets))
  Ibeta<-exp(runif(nsim,log(Obs@Ibeta[1]),log(Obs@Ibeta[2])))

  if(complexF==1){
    # SYMRF                             SPAY M R F2
    Iobst<-apply(VB[,,,1:nyears,,,],c(1, 4:7),sum)#^Ibeta
    Isum<-apply(Iobst,c(1,5),mean)
    ind<-as.matrix(expand.grid(1:nsim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
    #Iobst[ind]<-Ierr*(Iobst[ind]/Isum[ind[,c(1,5)]])
    Iobst[ind]<-Iobst[ind]/Isum[ind[,c(1,5)]]
  }else{
    #Iobst<-Ierr*apply(VB[,,,1:nyears,,,],c(1,4,5,6,7),sum)#^Ibeta
    apicalFage<-apply(OM@sel,1:2,which.max)
    Iobst<-array(NA,dim=c(nsim,nyears,nsubyears,nareas,nfleets))
    ind<-as.matrix(expand.grid(1:nsim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
    VBsum<-apply(VB[,,,1:nyears,,,],c(1,3:7),sum) # sum over pops
    VBind<-cbind(ind[,1],apicalFage[ind[,c(1,5)]],ind[,2:5]) # add apical age to VBindex
    #Iobst[ind]<--log(1-(Cobsta[VBind]/VBsum[VBind]))
    Iobst[ind]<-OM@E[ind[,c(1,5,2,3,4)]]#(OMd@nsim,OMd@nfleets,OMd@nyears,OMd@nsubyears,OMd@nareas))
    #Isum<-apply(Iobst,c(1,5),mean)
    #Iobst[ind]<-(Iobst[ind]/Isum[ind[,c(1,5)]])
  }

  debugR<-F
  if(debugR){
    simo<-1
    p<-1
    age<-13
    m<-1
    f<-1
    r<-1
    ys<-1:25

    test<-as.data.frame(cbind(Cobst[simo,ys,m,r,f],apply(VB[simo,,,ys,m,r,f],3,sum),Iobst[simo,ys,m,r,f],Cobst[simo,ys,m,r,f]/Iobst[simo,ys,m,r,f],FM[ss,p,8,ys,m,r,f],OM@E[ss,f,ys]))
    test<-test/rep(apply(test,2,mean),each=nrow(test))
    names(test)<-c("Cobs","VulnB","vBindex","Cobs/vBindex","FM","effort")
    test
  }


  # Length composition
  CALm<-array(NA,c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets,nlen))
  ind<-TEG(dim(CALm))
  ALKind<-ind[,c(1,2,4,3,8)]
  Cind<-ind[,1:7]
  CALm[ind]<-C[Cind]*OM@iALK[ALKind]


  # You were here simulating fishery independent SSB in the spawning area
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,OM@Recsubyr,1:nareas))
  SSBtemp<-array(NA,c(nsim,npop,nages,nyears,nareas))
  SSBtemp[ind[,c(1,2,3,4,6)]]<-N[ind]*Wt_age[ind[,1:4]]*OM@mat[ind[,1:4]]
  SSBtemp<-apply(SSBtemp,c(1,2,4,5),sum)
  SpawnA<-apply(SSBtemp,1:3,which.max)
  FIobst<-array(NA,c(nsim,npop,nyears))
  ind<-TEG(c(nsim,npop,nyears))
  FIobst[ind]<-SSBtemp[cbind(ind,SpawnA[ind])]
  meanFI<-apply(FIobst,1:2,mean)
  FIobst[ind]<-FIobst[ind]/meanFI[ind[,1:2]]
  FIerr<-array(trlnorm(nsim*mult,1,rep(.Object@Iimp,mult)),c(nsim,npop,nyears))
  FIobst<-FIobst*FIerr

  # Tagging data ---
  nRPT<-2 # maximum number of timesteps that a tag may be recaptured in (n subyears)
  temp<-rep(1:nsubyears,ceiling(nRPT/nsubyears)+nsubyears)
  RPTind<-array(NA,c(nsubyears,nRPT))
  for(ss in 1:nsubyears)RPTind[ss,]<-temp[ss:(ss+nRPT-1)]


  for(sim in 1:nsim){ # Now loop over simulations, create data and write M3 files for parallel processing

    simfolder<-paste(M3temp,sim,sep="")
    if(!file.exists(simfolder))dir.create(simfolder)

    file.copy(paste(OMDir,"/M3.exe",sep=""),simfolder,overwrite=T)
    file.copy(paste(OMDir,"/M3.pin",sep=""),simfolder,overwrite=T)
    datfile<-paste(simfolder,"/M3.dat",sep="")
    #datfile<-"G:/M3/M3.dat"
    # }

    #sim<-1
    print(sim)
    print(Sys.time())
    #datfile<-paste(OMDir,"/M3.dat",sep="")
    cat("\n")
    cat("Write data")
    cat("\n")


    # -- Model Dimensions --

    write("# ny number of years",datfile,1,append=F)
    write(nyears,datfile,1,append=T)

    write("# ns number of subyears",datfile,1,append=T)
    write(nsubyears,datfile,1,append=T)

    write("# np number of populations/stocks",datfile,1,append=T)
    write(npop,datfile,1,append=T)

    write("# na number of age classes",datfile,1,append=T)
    write(nages,datfile,1,append=T)

    write("# nr number of regions/areas",datfile,1,append=T)
    write(nareas,datfile,1,append=T)

    write("# nf number of fleets",datfile,1,append=T)
    write(nfleets,datfile,1,append=T)

    write("# nl number of length classes",datfile,1,append=T)
    write(nlen,datfile,1,append=T)

    write("# nRPT maximum number of time steps that a PSAT can be recaptured",datfile,1,append=T)
    write(nRPT,datfile,1,append=T)

    write("# RPtind correct subyear recapture index",datfile,1,append=T)
    write(t(RPTind),datfile,nRPT,append=T)

    write("# sdur the duration of the various subyears (sums to 1)",datfile,1,append=T)
    write(rep(1/nsubyears,nsubyears),datfile,nsubyears,append=T)

    write("# nZeq: number of years at the start of the model to calculate equilibrium Z from",datfile,1,append=T)
    write(nZeq,datfile,1,append=T)

    write("# nydist: number of years over which initial stock distribution is calculated (prior to spool up)",datfile,1,append=T)
    write(nydist,datfile,1,append=T)

    write("# nyeq: number of spool-up years over which the stock is subject to nZeq, used to define equilibrium conditions",datfile,1,append=T)
    write(nyeq,datfile,1,append=T)

    write("# ml the mean length of the length categories",datfile,1,append=T)
    write(mulen,datfile,nlen,append=T)

    yblock<-5
    RDblock<-rep(1:100,each=yblock)[1:nyears]

    write("# RDblock the RD parameter for each year",datfile,1,append=T)
    write(RDblock,datfile,nyears,append=T)

    write("# nRD the number of estimated recruitment strengths",datfile,1,append=T)
    if(complexRD==0)write(max(RDblock),datfile,nyears,append=T)
    if(complexRD==1)write(nyears,datfile,nyears,append=T)

    # -- Growth --

    write("# iALK the age-length key by population and year p y a l",datfile,1,append=T)
    write(tomt(OM@iALK[sim,,,,]),datfile,nlen,append=T)

    write("# lwa weight-length parameter a w=al^ b",datfile,1,append=T)
    write(OM@a,datfile,npop,append=T)

    write("# lwa weight-length parameter b w=al^ b",datfile,1,append=T)
    write(OM@b,datfile,npop,append=T)

    write("# len_age (pay)",datfile,1,append=T)
    write(OM@Len_age[sim,,,1:nyears],datfile,nyears,append=T)

    write("# wt_age (pay)",datfile,1,append=T)
    write(OM@Wt_age[sim,,,1:nyears],datfile,nyears,append=T)



    # -- Maturity --

    write("# Fec, fecundity at age, SSB at age",datfile,1,append=T)
    write(t(Wt_age[sim,,,nyears]*OM@mat[sim,,,nyears]),datfile,nages,append=T)

    write("# steep, steepness of the Bev-Holt SR relationship",datfile,1,append=T)
    write(OM@h[sim,],datfile,npop,append=T)


    # -- Spawning --

    write("# spawns, the subyear in which the stock spawns",datfile,1,append=T)
    write(OM@Recsubyr,datfile,npop,append=T)

    #write("# spawnr, the fracton of recruits in each area",datfile,1,append=T)
    #write(t(spawnr[sim,,]),datfile,nareas,append=T)


    # -- Natural Mortality rate --

    write("# Ma, instantaneous natural mortality rate at age",datfile,1,append=T)
    write(t(M[sim,,,1]),datfile,nages,append=T)


    # -- Fishery data --

    # Catches / F init
    if(complexF==1){

      allobsbelow<-0.02 # level of catches at cumulative 2%
      Cobs_cutoff<- min(Cobst[order(as.vector(Cobst))][cumsum(Cobst[order(as.vector(Cobst))])/sum(Cobst)>allobsbelow])

      ind<-as.matrix(expand.grid(sim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
      cond<-Cobst[ind]>Cobs_cutoff
      nind<-ind[cond,]
      rat<-sum(Cobst[ind])/sum(Cobst[nind])
      Cobs<-cbind(nind[,2:5],Cobst[nind])
      Cobs[,5]<-Cobs[,5]*rat

    }else{

      ind<-as.matrix(expand.grid(sim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
      Cobs<-cbind(ind[,2:5],Cobst[ind])

    }

    #plot(density(Cobst[nind]))
    #lines(density(Cobst[ind]),col='red')
    #legend('topright',legend=c(round(rat,4),nrow(Cobs)))

    write("# nCobs, the number of catch weight observations y s r f CW",datfile,1,append=T)
    write(nrow(Cobs),datfile,1,append=T)

    write("# Cobs, catch weight observations y s r f C(weight)",datfile,1,append=T)
    write(t(Cobs),datfile,5,append=T)

    # CPUE
    ind<-as.matrix(expand.grid(sim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
    CPUEobs<-cbind(ind[,c(2:5,5)],Iobst[sim,,,,]) # fleet is index number

    write("# nCPUE, the number of CPUE series",datfile,1,append=T)
    write(nfleets,datfile,1,append=T) # in this simulation this is the same as the number of fleets

    write("# nCPUEobs, the number of CPUE observations y s r f CPUE(weight)",datfile,1,append=T)
    write(nrow(CPUEobs),datfile,1,append=T)

    write("# CPUEobs, CPUE observations y s r f CPUE(weight)",datfile,1,append=T)
    write(t(CPUEobs),datfile,6,append=T)

    # Length composition

    CALt<-CALm[sim,,,,,,,]  # p a y s m f l
    CALsum<-ceiling(apply(CALt,3:7,sum,na.rm=T))  # y s m f l
    #CALtot<-apply(CALsum)


    #par(mfrow=c(1,2))
    #plot(CALsum[2,1,2,1,]/max(CALsum[2,1,2,1,]))
    #lines(CALsum[2,1,2,2,]/max(CALsum[2,1,2,2,]),col='red')
    #plot(OM@sel[sim,1,])
    #lines(OM@sel[sim,2,],col='red')

    ind<-as.matrix(expand.grid(1:nyears,1:nsubyears,1:nareas,1:nfleets,1:nlen))
    cond<-CALsum>0
    CLobs<-cbind(ind[cond,],CALsum[cond])
    #CLobs<-cbind(ind,CALsum[ind])
    sum(is.na(CLobs))


    write("# nCLobs, the number of catch-at-length observations y s r f l N",datfile,1,append=T)
    write(nrow(CLobs),datfile,1,append=T)

    write("# CLobs, catch-at-length observations y s r f l N",datfile,1,append=T)
    write(t(CLobs),datfile,6,append=T)

    # The real relative abundance index RAI (y, s, r) !!! need to change this to real values
    write("# RAI, Relative Abundance index r x s x y",datfile,1,append=T)
    write(RAI[sim,,,],datfile,nyears,append=T)

    # Fishery-independent indices y s r pp i type(biomass/ssb) index
    ind<-as.matrix(expand.grid(sim,1:npop,1:nyears))
    Iobs<-as.matrix(cbind(ind[,3],OM@Recsubyr[ind[,2]],SpawnA[ind],ind[,2],ind[,2],rep(2,nrow(ind)),FIobst[ind])) # type SSB

    write("# nI, the number of fishery independent indices series",datfile,1,append=T)
    write(npop,datfile,1,append=T) # in this simulation this is the same as the number of populations

    write("# nIobs, the number of fishery independent observations y s r i type(biomass/ssb) index",datfile,1,append=T)
    write(nrow(Iobs),datfile,1,append=T)

    write("# Iobs, fishery independent observations y s r i type(biomass/ssb) index",datfile,1,append=T)
    write(t(Iobs),datfile,7,append=T)


    # PSAT tagging --
    nPSATs<-10000
    PSATdist<-apply(C[sim,,,,,,],c(1,2,4,5),sum,na.rm=T)^0.01
    PSATdist<-PSATdist/apply(PSATdist,1,sum)
    PSATdist<-ceiling(PSATdist/sum(PSATdist)*nPSATs)
    nPSATs<-sum(PSATdist)
    track<-array(NA,c(nPSATs,nRPT))
    sy<-rep(NA,nPSATs)
    SOO<-array(NA,c(nPSATs,npop))
    nT<-1+ceiling(runif(nPSATs)*(nRPT-1)) # nT is the number of timesteps for recapture, this is set to 2 here,nRPT is the maximum number of timesteps that a tag may be recaptured

    PSAT<-c(1,1,3,1,9,9)
    PSAT2<-c(1,1,1,1,1,0.05,0.95)

    j<-0
    mov<-OM@mov[sim,,,,,]

    for(pp in 1:npop){
      for(aa in 1:nages){
        for(ss in 1:nsubyears){
          for(rr in 1:nareas){
            if(PSATdist[pp,aa,ss,rr]>0){
              for(i in 1:PSATdist[pp,aa,ss,rr]){
                j<-j+1
                SOO[j,]<-apply(C[sim,,aa,ceiling(nyears*0.7),ss,rr,],1,sum)/sum(C[sim,,aa,ceiling(nyears*0.7),ss,rr,]) #SPAYMRF
                track[j,1]<-rr
                sy[j]<-ss

                #for(rpt in 2:nT[j]){
                rpt<-2
                m<-RPTind[ss,rpt]

                track[j,rpt]<-(1:nareas)[rmultinom(1,1,mov[pp,aa,mref[m],track[j,rpt-1],])==1] #
                SOO[j,]<-SOO[j,]*apply(C[sim,,aa,ceiling(nyears*0.7),m,track[j,rpt],],1,sum)/sum(C[sim,,aa,ceiling(nyears*0.7),m,track[j,rpt],])
                #} # track length
                SOO[j,]<-SOO[j,]/sum(SOO[j,])
                #if(1%in%SOO[j,]){
                # for(rpt in 2:nT[j]){
                #m<-RPTind[ss,rpt]
                PSAT<-rbind(PSAT,c(pp,OM@ma[aa,pp],ss,2,track[j,(rpt-1):rpt]))
                #}
                #}else{
                # for(rpt in 2:nT[j]){
                #  #m<-RPTind[ss,rpt]
                #  PSAT2<-rbind(PSAT2,c(ss,2,track[j,(rpt-1):rpt],SOO[j,]))
                #}
                #}

              } # tags
            }
          } # areas pp
        } # ages
      } # subyears
    } # pops

    PSAT<-PSAT[2:nrow(PSAT),]
    PSAT<-aggregate(rep(1,nrow(PSAT)),by=list(PSAT[,1],PSAT[,2],PSAT[,3],PSAT[,4],PSAT[,5],PSAT[,6]),sum)
    #testPSAT<-array(0,c(npop,OM@nma,nsubyears,nareas,nareas))
    #testPSAT[as.matrix(PSAT[,c(1,2,3,5,6)])]<-PSAT[,7]

    #PSAT2<-PSAT2[2:nrow(PSAT2),]

    write("# nPSAT, PSATs data of known stock of origin p a s t fr tr N",datfile,1,append=T)
    write(nrow(PSAT),datfile,1,append=T)

    write("# PSAT data of known stock of origin p a s t fr tr N",datfile,1,append=T)
    write(t(PSAT),datfile,7,append=T)

    write("# nPSAT2, PSATs data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
    write(1,datfile,1,append=T)
    #write(nrow(PSAT2),datfile,1,append=T)

    write("# PSAT2 data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
    write(t(PSAT2),datfile,5+npop,append=T)


    # Placeholder for conventional tags
    Tag<-array(c(2,1,1,1,2,2,1,1,1,1),c(1,10))

    write("# nTag, number of conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
    write(nrow(Tag),datfile,1,append=T)

    write("# Tag, conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
    write(t(Tag),datfile,10,append=T)


    # Stock of origin
    NSOO<-min(ceiling(nyears*nsubyears*nareas/2),500) # number of data points in time and space
    muSOO<-10  # mean number of observations at those points
    SOO<-apply(C[sim,,,1:nyears,,,],1:5,sum,na.rm=T)
    #Csum<-apply(SOO,2:4,sum)
    rat<-mean(SOO,na.rm=T)/muSOO
    SOO<-SOO/rat
    ind<-expand.grid(1:nages,1:nyears,1:nsubyears,1:nareas)[sample(1:(nages*nyears*nsubyears*nareas),NSOO),]
    ind<-as.matrix(cbind(rep(1:npop,rep=NSOO),ind[rep(1:NSOO,each=npop),]))
    SOOobs<-as.matrix(cbind(ind,SOO[ind]))
    SOOobs<-SOOobs[SOOobs[,6]>0,] # remove zeros

    write("# nSOOobs, number of stock of origin observations p aa y s r N",datfile,1,append=T)
    write(nrow(SOOobs),datfile,1,append=T)

    write("# SOOobs, stock of origin observations p aa y s r N",datfile,1,append=T)
    write(t(SOOobs),datfile,6,append=T)


    # -- Selectivity controls
    write("# nsel, number of estimated selectivities",datfile,1,append=T)
    write(nfleets,datfile,1,append=T) # same as number of fleets

    write("# seltype, 2:logistic, 3:Thompson",datfile,1,append=T)
    write(c(2,3),datfile,nfleets,append=T) # first fleet is logistic

    write("# selind, which selectivity is assigned to each fleet",datfile,1,append=T)
    write(c(1,2),datfile,nfleets,append=T) # same as fleets

    write("# ratiolim, limits on logistic slope parameter relative to inflection point",datfile,1,append=T)
    write(c(0.1,1),datfile,nfleets,append=T) # same as fleets

    write("# infleclim, limits on model selectivity",datfile,1,append=T)
    write(c(4,15),datfile,nfleets,append=T) # same as fleets


    # -- Movement estimation

    mov<-array(NA,c(npop,OM@nma,nsubyears,nareas,nareas))
    #mov[as.matrix(PSAT[,c(1,2,4,5)])]<-1

    movind<-mov1<-c(1,1,1,1,1)
    maclassfind<-match(1:OM@nma,OM@ma[,1])
    mov<-OM@mov[sim,,maclassfind,,,] # p ma s fr tr
    mov[mov>0]<-1
    notanarea<-apply(mov,c(1,2,3,5),sum) # p ma s tr
    notanarea<-array(as.integer(notanarea>0),dim(notanarea))
    can<-apply(mov,c(1,5),sum)  # can a movement happen from this area for this stock?
    can<-array(as.integer(can>0),dim(can))

    ind<-TEG(dim(mov))
    mov[ind]<-mov[ind]*notanarea[ind[,c(1,2,3,4)]]

    for(pp in 1:npop){
      for(ma in 1:OM@nma){
        for(ss in 1:nsubyears){
          for(rr in 1:nareas){
            np<-sum(mov[pp,ma,ss,rr,],na.rm=T)
            if(np>0){
              fR<-match(1,mov[pp,ma,ss,rr,])
              mov1<-rbind(mov1,c(pp,ma,ss,rr,fR))
              if(np>1){
                oR<-grep(1,mov[pp,ma,ss,rr,])
                oR<-oR[oR!=fR]
                for(i in 1:length(oR)){
                  movind<-rbind(movind,c(pp,ma,ss,rr,oR[i]))
                }
              }
            }
          }
        }
      }
    }

    movind<-movind[2:nrow(movind),]
    mov1<-mov1[2:nrow(mov1),]


    if(movtype==1){ # if a gravity formulation these indices are for the to area that should be estimated by season

      firstr<-apply(can,1,which.max)
      mov1<-TEG(c(npop,OM@nma,nsubyears))
      mov1<-cbind(mov1,firstr[mov1[,1]],rep(999,nrow(mov1)))
      #mov1<-cbind(rep(1:npop,each=nsubyears),rep(1:nsubyears,npop),firstr[rep(1:npop,each=nsubyears)],rep(999,nsubyears*npop))
      can2<-can
      can2[cbind(1:npop,firstr)]<-0
      can2<-t(can2)
      nrest<-apply(can2,1,sum)
      indr<-array(1:nareas,c(nareas,npop))
      indp<-array(rep(1:npop,each=nareas),c(nareas,npop))
      rs<-indr[can2==1]
      ps<-indp[can2==1]
      movindo<-cbind(rep(ps,each=nsubyears),rep(1:nsubyears,length(rs)),rep(rs,each=nsubyears),rep(999,length(rs)*nsubyears))

      movind<-array(rep(movindo,each=OM@nma),dim=c(nrow(movindo)*OM@nma,4))
      movind<-cbind(movind[,1],rep(1:OM@nma,nrow(movindo)),movind[,2:4])

    }

    write("# nMP, number of estimated movement parameters",datfile,1,append=T)
    if(movtype==1)write(nrow(movind)+nsubyears*OM@nma*npop,datfile,1,append=T)
    if(movtype==2)write(nrow(movind),datfile,1,append=T)

    write("# nma, number of estimated movement age classes",datfile,1,append=T)
    write(OM@nma,datfile,1,append=T)

    write("# ma, assignment of age classes to age",datfile,1,append=T)
    write(OM@ma,datfile,nages,append=T)

    write("# nmovind, number of estimated movement parameters minus viscosity",datfile,1,append=T)
    write(nrow(movind),datfile,1,append=T)

    write("# movind, the location of estimated movement parameters p s r r",datfile,1,append=T)
    write(t(movind),datfile,5,append=T)

    write("# nmov1, number of initial non-estimated movement parameters",datfile,1,append=T)
    write(nrow(mov1),datfile,1,append=T)

    write("# mov1, the location of initial non-estimated movement parameters p s r r",datfile,1,append=T)
    write(t(mov1),datfile,5,append=T)

    write("# movtype, the type of movement parameterization 1: gravity 2:markov matrix",datfile,1,append=T)
    write(movtype,datfile,1,append=T)


    # -- Observation errors

    write("# CobsCV, lognormal CV of the observed catches",datfile,1,append=T)
    write(rep(0.2,nfleets),datfile,nfleets,append=T)

    write("# CPUEobsCV, lognormal CV of the CPUE indices",datfile,1,append=T)
    write(rep(0.2,nfleets),datfile,nfleets,append=T) # CPUE index for each fleet

    write("# IobsCV, lognormal CV of the fishery independent indices",datfile,1,append=T)
    write(rep(0.2,npop),datfile,npop,append=T) # SSB index for each population


    # -- Priors

    write("# RDCV, lognormal penalty on recruitment deviations",datfile,1,append=T)
    write(2,datfile,1,append=T) # SSB index for each population

    write("# nLHw, number of likelihood weights",datfile,1,append=T)
    write(10,datfile,1,append=T) # SSB index for each population

    write("# LHw, likelihood weights (1 catch, 2 cpue, 3 FIindex,   4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel)",datfile,1,append=T)
    write(c(                          10,       1,     1/100000000, 1/1000,  1/10,  1/100,  1,       1,        1,      2),datfile,10,append=T) # SSB index for each population

    # -- Initial values
    write("# R0_ini, initial values for log R0",datfile,1,append=T)
    write(OM@R0[sim,],datfile,npop,append=T) # Simulated R0 for each population

    write("# sel_ini, initial values for selectivity",datfile,1,append=T)
    write(t(OM@sel[sim,,]),datfile,nlen,append=T) # Actual selectivity

    write("# selpar_ini, initial values for selectivity parameters",datfile,1,append=T)
    write(t(OM@selpars[sim,,]),datfile,3,append=T) # Actual selectivity

    #RFL<-array(NA,c(nsim,nfleets,nlen,nyears,nsubyears,nareas))
    Fsub<-array(NA,c(nyears,nsubyears,nareas,nfleets))
    indt<-as.matrix(expand.grid(sim,1:nfleets,1:nyears,1:nsubyears,1:nareas))
    Fsub[indt[,c(3,4,5,2)]]<-OM@E[indt]*OM@q[indt[,1:2]]

    # old-----
    #Fsum<-RF[sim,1,,,,,]
    #ind<-TEG(c(nyears,nsubyears,nareas,nfleets))
    #ind<-as.matrix(cbind(maxv[ind[,4]],ind[]))
    #Fsub<-array(Fsum[ind],c(nyears,nsubyears,nareas,nfleets))
    #-----

    if(complexF==0)lnFini<-log(as.vector(Fsub))
    if(complexF==1)lnFini<-log(Fsub[nind[,2:5]])

    write("# lnF_ini, initial values for log F",datfile,1,append=T)
    write(lnFini,datfile,nrow(Cobs),append=T) # log apical F

    write("# ilnRD_ini, initial recruitment deviations y=1 a=2:nages",datfile,1,append=T)
    write(array(0,c(nages-1,npop)),datfile,nages-1,append=T) # Initial recruitment deviations

    write("# lnRD_ini, initial recruitment deviations y=1:nyears",datfile,1,append=T)
    write(log(t(OM@Recdevs[sim,,1:nyears])),datfile,nyears,append=T) # Recruitment deviations

    write("# mov_ini, simulated movement p s a r r",datfile,1,append=T) # this is a pain: M3 is p s a r r, OM@mov is p a s r r (oh well)
    movt<-OM@mov[sim,,,,,] # p a s r r
    mov<-array(NA,dim(movt)[c(1,3,2,4,5)]) # p s a r r
    ind<-TEG(dim(movt))
    mov[ind[,c(1,3,2,4,5)]]<-movt[ind]
    write(tomt(mov),datfile,nareas,append=T) # Movement probabilities

    write("# qCPUE_ini, initial values for CPUE catchability nCPUE",datfile,1,append=T)

    if(complexF==1)write(log(1/Isum[sim,]),datfile,nfleets,append=T) # CPUE catchabilities I=qVB
    if(complexF==0){
      #apicalF<-apply(FM[sim,,,1:nyears,,,],c(1,3,4,5,6),max,na.rm=T)
      write(log(OM@q[sim,]),datfile,nfleets,append=T) # CPUE catchabilities I=qVB
    }

    write("# qI_ini, initial values for fishery independent catchability nI",datfile,1,append=T)
    write(log(rep(1,nfleets)),datfile,nfleets,append=T) # Catchabilities I=qSSB or I=qB

    write("# D_ini, simulated depletion SSB/SSB0",datfile,1,append=T)
    write(D[sim,],datfile,nfleets,append=T) # Catchabilities I=qSSB or I=qB

    write("# complexRD 1= run with full estimation of all recruitment deviations by year",datfile,1,append=T)
    write(complexRD,datfile,1,append=T) # debug switch

    write("# complexF 1= run with full estimation of all F's by year, subyear, fleet, region",datfile,1,append=T)
    write(complexF,datfile,1,append=T) # debug switch

    write("# nF either nCobs or 1 if complexF=0",datfile,1,append=T)
    if(complexF==0)write(1,datfile,1,append=T)
    if(complexF==1)write(nrow(Cobs),datfile,1,append=T)

    write("# debug 1= run with initial values",datfile,1,append=T)
    write(0,datfile,1,append=T) # debug switch

    write("# verbose 1= run with printouts",datfile,1,append=T)
    write(verbose,datfile,1,append=T) # debug switch

    write("# datacheck",datfile,1,append=T)
    write(991199,datfile,1,append=T) # datacheck

    #system(paste(OMDir,"M3.exe -est",sep="/"),wait=T,show.output.on.console = F) # run the exe

    #if(sim==1)pin_from_par(file=paste(OMDir,"/M3",sep=""))
    # Store results

    #out[[sim]]<-M3read(OMDir)
  }



  spawnr=c(4,1)
  B0<-apply(N[,,,1,1,]*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),c(1:2,4),sum)
  B0<-B0/array(apply(B0,1:2,sum),dim(B0))

  Bind<-expand.grid(1:nsim,1:npop)
  Bfrac<-matrix(B0[as.matrix(cbind(Bind,spawnr[Bind[,2]]))],ncol=npop)

  SSB1<-apply(N[,,,1,1,]*
                array(Wt_age[,,,1]*OM@mat[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)

  SSBcur<-apply(N[,,,nyears,nsubyears,]*
                  array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)

  Bcur<-apply(N[,,,nyears,nsubyears,]*
                array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)

  Cobsta<-array(NA,c(nsim,npop,nages,nsubyears,nareas,nfleets))
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,nyears,1:nsubyears,1:nareas,1:nfleets))
  Cobsta[ind[,c(1:3,5:7)]]<-C[ind]*Wt_age[ind[,1:4]]
  Cobsta<-apply(Cobsta,c(1,2,6),sum)
  Ct<-apply(Cobsta,1:2,sum)
  Urat<-Cobsta/array(Ct,dim(Cobsta))

  U<-Ct/(apply(Bcur,1,sum)+Ct)
  B0t<-apply(N[,,,1,1,]*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),c(1:2),sum)

  ratB0<-B0t/apply(B0t,1,sum)
  ratBcur<-Bcur/apply(Bcur,1,sum)



  .Object@simlist<-list(SSB0=SSB0,D=SSBcur/SSB0,D1=SSBcur/SSB1,B0=B0t,Bfrac=Bfrac,Bcur=Bcur,Urat=Urat,U=U,ratB0=ratB0,ratBcur=ratBcur)


  #Bfracp<-t(sapply(1:nsim,getBfrac,out,spawnr=spawnr))
  #Bfracbias<-(Bfracp-Bfrac)/Bfrac

  # Bias in current depletion (SSB)
  #Dp<-t(sapply(1:nsim,getdep,out))
  #Dbias<-(Dp-D)/D

  # Bias in current SSB (absolute)
  #SSBp<-t(sapply(1:nsim,getSSBnow,out))
  #SSBbias<-(SSBp-Bcur)/Bcur

  #Perf<-data.frame(Dbias,SSBbias,Bfracbias)
  #names(Perf)<-c(paste("Dbias",1:npop,sep="_"),paste("SSBtbias",1:npop,sep="_"),paste("Bfracbias",1:npop,sep="_"))
  #.Object@Perf<-Perf

  #.Object@C[MP,,,]<-apply(C[,,,1:allyears,,,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets)),c(1,2,4),sum)
  #SSB<-apply(SSN[,,,1:allyears,4,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nareas)),c(1,2,4),sum)
  #.Object@D[MP,,,]<-SSB/apply(SSB0,1,sum)
  #B<-apply((SSN[,,,1:allyears,4,]+NSN[,,,1:allyears,4,])*array(Wt_age,c(nsim,npop,nages,allyears,nareas)),c(1:2,4),sum)
  #Bthen<-apply((SSN[,,,1,4,]+NSN[,,,1,4,])*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)
  #.Object@B_BMSY[MP,,]<-apply(array(B[,targpop,],dim=c(nsim,length(targpop),allyears)),c(1,3),sum)/OM@BMSY
  #U<-apply(array(.Object@C[MP,,targpop,],c(nsim,length(targpop),allyears)),c(1,3),sum)/
  #apply(array(VBA[,targpop,,1:allyears,4,],c(nsim,length(targpop),nages,allyears,nareas)),c(1,4),sum)
  #.Object@F_FMSY[MP,,]<-U/OM@UMSY

  #cat("\n")
  #  #.Object@MPs<-MPs

  .Object

})






# Operating model definition object ---------------------------------------------------------------------------------------------------------------------
setClass("OMd",representation(

  # Description
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",

  # Dimensions
  nsim="integer",npop="integer",nages="integer",             # MSE dimensions
  nyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
  proyears="integer",nlen="integer",lenbins="numeric",       # Projected years
  interval="integer",                                        # Update interval
  nma="integer",ma="array",                                  # Number of movement age classes, age class definitions

  # Parameter ranges / simulation sample distributions
  Magemu="array",Mrange="array",Msd="array",Mgrad="array",   # Mean natural mortality rate at age, sample range, interannual variability and gradient % yr-1
  SRrel="integer",h="array",recgrad="array",                 # Stock-recruitment relationship type, steepness, underlying gradient % yr-1
  Reccv="array",AC="array", Recsubyr="integer",              # CV of recruitment deviations and recruitment auto-correlation
  Linf="array",K="array",t0="numeric",                       # Mean growth parameters
  Ksd="array",Kgrad="array",Linfsd="array",Linfgrad="array", # Interannual variability in growth and mean trajectory % yr-1
  a="numeric",b="numeric",                                   # Weight - Length conversion W=aL^b
  ageM="array",ageMsd="array",ageMgrad="array",              # Age-at-maturity, interannual variability and gradient % yr-1
  D="array",R0="array",                                      # Current stock depletion, abundance
  Size_area="array",mov="array",                             # Size of regions, Markov movement matrix for all fish and mature fish
  movvar="numeric",movsd="array",movgrad="array",              # Inter-simulation variability in movement, interannual-variability in movement, gradient changes in area gravity weights
  excl="array",                                              # Exclusion matrix [0,1] depending on whether the stock can go there

  # Fleet specifications
  nfleets="integer",                                         # Number of fleets,
  L05="array",VmaxL="array", LFS="array",                    # Length at 5% vulnerability, vulnerability of largest fish, length at full selection
  Fsd="array",Fgrad="array", Frat="numeric",                 # Interannual variability in F, Final gradient in F yr-1
  Spat_targ="array",                                         # Spatial targetting parameter F =prop= V^Spat_targ
  Area_names="character", Area_defs="list",                  # Area definitions (polygons)
  targpop="numeric",                                         # The target population for calculation of MSY and depletion reference points
  #nZeq="integer",                                            # The number of initial years to calculation equilibrium F
  nydist="integer",                                          # The number of years (iterations) taken to find equilibrium spatial distribution
  #nyeq="integer",                                            # The number of years (iterations) taken to find equilibrium F

  # Observation properties relevant to trial specifications
  Cbias="numeric",

  # Misc
  seed="numeric"                                             # Random seed for the generation of the OM

))

# Plot spatial definitions of the OMd object
setMethod("plot", signature(x = "OMd"),function(x){

  OMd<-x
  cols<-rep(c("#ff000040","#00ff0040","#0000ff40","#00000040","#ff00ff40"),4)
  res<-0.03
  map(database = "worldHires",xlim=c(-105,50),ylim=c(-55,85),mar=rep(0,4),resolution=res)

  abline(v=(-20:20)*10,col='light grey')
  abline(h=(-20:20)*10,col='light grey')
  abline(v=0,col="green")
  abline(h=0,col="green")
  map(database = "worldHires",mar=rep(0,4),border=0,xlim=c(-105,50), ylim=c(-55,85),add=T,fill=T,col="light grey",resolution=res)


  for(i in 1:length(OMd@Area_names)){
    polygon(OMd@Area_defs[[i]],border='blue',lwd=2,col=NA)#cols[i])
    text(mean(OMd@Area_defs[[i]]$x),2.5+mean(OMd@Area_defs[[i]]$y),OMd@Area_names[i],col='red',font=2,cex=0.6)
  }
})

#setMethod("plot", signature(x = "MSE"),function(x){


