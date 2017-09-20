#' An S4 class object that contains all MSE outputs
#'
#'\describe{
#' \item{Name}{The name of the MSE}
#' \item{Date}{The date and time the object was created}
#' \item{Author}{Who made the object}
#' \item{Notes}{Anything noteworthy}
#' \item{PrimarySource}{The primary origins of these data}
#' \item{nsim}{The number of simulations}
#' \item{npop}{number of stocks (e.g. 2, East-West)}
#' \item{nages}{number of ages (e.g. 18: 1-17 and 18+)}
#' \item{nyears}{number of years of simulation by statistical catch at length}
#' \item{nHyears}{number of historical years for stock reduction analysis prior ot statistical catch at length}
#' \item{nsubyears}{number of subyears (e.g. 4 quarters)}
#' \item{nareas}{number of areas}
#' \item{proyears}{number of projection years for closed-loop MSE analysis}
#' \item{nlen}{number of length classes}
#' \item{lenbins}{vector of length bins that define length classes}
#' \item{interval}{how often (number of years) assessments/MPs are implemented (e.g. interval=3 means 2015 assessment, 2016-2017 no assessment, 2018 assessment etc)}
#' \item{nma}{number of movement age classes}
#' \item{ma}{an array of movement age classes [npop x nage]}
#' \item{mulen}{a vector of mean length in each length bin [nage]}
#' \item{Cimp}{a vector of imprecision in catch observations (lognormal sd) by simulation [nsim]}
#' \item{Cb}{a vector representing the bias in catch observations}
#' \item{Cerr}{a matrix of the catch mulitpliers applied to true simulated catch the product of Cimp and Cb [nsim x nyears]}
#' \item{Iimp}{currently unused}
#' \item{Ib}{currently unused}
#' \item{Ierr}{currently unused}
#' \item{nCAAobs}{a vector of the number of annual catch-at-age observations by simulation [nsim]}
#' \item{nCALobs}{a vector of the number of annual catch-at-length observations by simulation [nsim]}
#' \item{Lcv}{currently unused}
#' \item{Mb}{a vector of biases in natural mortality rate by simulation [nsim]}
#' \item{Kb}{a vector of biases in von-Bert growth parameter K by simulation [nsim]}
#' \item{t0b}{a vector of biases in von-Bert growth parameter t0 by simulation [nsim]}
#' \item{Mb}{a vector of biases in von-Bert maximum length parameter by simulation [nsim]}
#' \item{LFCb}{currently unused}
#' \item{LFSb}{currently unused}
#' \item{FMSYb}{a vector of biases in fishing mortality rate at MSY by simulation [nsim]}
#' \item{FMSY_Mb}{a vector of biases in the ratio of fishing mortality rate at MSY to natural mortality rate by simulation [nsim]}
#' \item{BMSY_B0b}{currently unused}
#' \item{ageMb}{a vector of biases in the age at 50 per cent maturity [nsim]}
#' \item{Dimp}{currently unused}
#' \item{Db}{currently unused}
#' \item{Derr}{currently unused}
#' \item{Btimp}{a vector of imprecision in current biomass observations (lognormal sd) by simulation [nsim]}
#' \item{Btb}{a vector representing the bias in current biomass observations}
#' \item{Bterr}{a matrix of the mulitpliers applied to true simulated biomass the product of Btimp and Btb [nsim x nyears]}
#' \item{Ftimp}{currently unused}
#' \item{Ftb}{currently unused}
#' \item{Fterr}{currently unused}
#' \item{hb}{currently unused}
#' \item{Reccbcv}{currently unused}
#' \item{IMSYb}{a vector of biases in the index at MSY by simulation [nsim]}
#' \item{IMSYb}{a vector of biases in MSY (a reference catch level) by simulation [nsim]}
#' \item{IMSYb}{a vector of biases in BMSY (a reference biomass level) by simulation [nsim]}
#' \item{C}{a 4D array containing true simulated catches [MP x nsim x nstocks x nyears]}
#' \item{D}{a 4D array containing true simulated stock depletion (SSB/SSB0) [MP x nsim x nstocks x nyears]}
#' \item{B_BMSY}{a 4D array containing true simulated biomass relative to BMSY [MP x nsim x nstocks x nyears]}
#' \item{F_FMSY}{a 4D array containing true simulated fishing mortality rate relative to FMSY [MP x nsim x nstocks x nyears]}
#' \item{B}{currently unused}
#' \item{SSB}{a 4D array containing true simulated spawning biomass [MP x nsim x nstocks x nyears]}
#' \item{SSB0}{a 2D array containing unfished spawnign biomass by simulation and stock [nsim x nstocks]}
#' \item{SSB0proj}{a 3D array containing unfished spawning biomass by simulation, stock and projection year [nsim x nstocks x proyears]}
#' \item{TAC}{a 4D array containing the TAC recommendations [nsim x MP x stock x proyear]}
#' \item{nMPs}{an integer number representing the number of MPs in the MSE}
#' \item{Snames}{a character vector naming the stocks [nstocks]}
#' \item{area_defs}{a list of area definitions for graphing (lons and lats describing the polygon)}
#' \item{areanams}{a character vector of area names}
#' \item{MPs}{a list object containing the names of the MPs [nMPs]}
#' }
setClass("MSE",representation(

  # Description
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",

  # Dimensions
  nsim="integer",npop="integer",nages="integer",             # MSE dimensions
  nyears="integer",nHyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
  proyears="integer", nlen="integer",lenbins="numeric",       # Proyears
  interval="integer",                                        # Update interval
  nma="integer",ma="array",                                  # Number of movement age classes, age class definitions
  mulen="numeric",                                           # Mean length of each length bin

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
  SSB0="array",
  SSB0proj="array",
  TAC="array",

  nMPs="integer",
  Snames="character",
  area_defs="list",
  areanams="character",

  MPs="list"

))

setMethod("initialize", "MSE", function(.Object,OM,Obs,MPs=list(c("UMSY","UMSY")),interval=3,IE="Umax_90",
                                        curTAC=c(13500000,2000000),Allocation=NA,MPareas=NA,Fdistyrs=3){
  #.Object})
  #.Object<-new('MSE')

  .Object@Snames<-OM@Snames

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
  if(class(get(MPs[[1]][1]))!='MP'){
    print(paste('Could not run MSE:',deparse(substitute(MPs[[1]][1])),'not of class MP'))
    stop()
  }
  if(class(get(IE))!='IE'){
    print(paste('Could not run MSE:',deparse(substitute(IE)),'not of class IE'))
    stop()
  }

  MPs<-append(list(c("ZeroC","ZeroC")),MPs) # make sure a zeroC reference MP is included

  # copy over dimensions ------
  dimslots<-slotNames(OM)[1:18]
  for(i in 1:18)slot(.Object,dimslots[i])<-slot(OM,dimslots[i])

  cat("Constructing arrays")
  cat("\n")
  flush.console()

  # Dimensions  S P A Y M R
  nsim<-OM@nsim
  npop<-OM@npop
  nyears<-OM@nyears
  nHyears<-OM@nHyears
  proyears<-OM@proyears
  nages<-OM@nages
  nsubyears<-OM@nsubyears
  nareas<-OM@nareas
  nfleets<-OM@nfleets
  allyears<-nyears+proyears
  nMPs<-length(MPs)
  .Object@nMPs<-nMPs
  nlen<-OM@nlen
  lenbins<-OM@lenbins
  mulen<-OM@mulen
  Wt_age<-OM@Wt_age
  nydist<-OM@nydist
  #R0<-OM@Recpars[,,1,2]
  mat<-OM@mat
  mov<-OM@mov
  h<-OM@h
  Recsubyr<-OM@Recsubyr
  Recdevs<-OM@Recdevs

  HCobs<-OM@HCobs

  M<-OM@M
  Mtemp<-array(0,dim(OM@M))
  Mtemp[,,2:nages,]<-OM@M[,,1:(nages-1),]

  surv=tomt(exp(-apply(Mtemp[,,,1],2:1,cumsum)))
  surv[,,nages]<-surv[,,nages]*exp(-Mtemp[,,nages,1])/(1-exp(-Mtemp[,,nages,1]))

  N<-SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  FD<-array(NA,c(nsim,nfleets,nyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  VB<-C<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  CA<-array(NA,c(nsim,npop,allyears,nsubyears,nareas))

  hFAT<-array(NA,c(nsim,nHyears,nsubyears,nages,nareas))
  hZ<-array(NA,c(nsim,npop,nages,nHyears,nsubyears,nareas))
  hZind<-TEG(dim(hZ))
  hFATind<-hZind[,c(1,3,4,5,6)]

  mref<-c(2:nsubyears,1)  # movement reference
  y<-1
  m<-1


  # Calculating F arrays ----------------------------------------------
  cat("Calculating historical fishing mortality rate at length (computationally intensive)")
  cat("\n")

  RFL<-array(NA,c(nsim,nfleets,nlen,nyears,nsubyears,nareas))
  indL<-TEG(dim(RFL))
  RFL[indL]<-OM@qE[indL[,c(1,2)]]*OM@sel[indL[,1:3]]*OM@E[indL[,c(1,2,4,5,6)]]
  iALK<-OM@iALK

  aseltemp<-array(NA,c(nsim,npop,nfleets,nages,nlen))
  aselind<-TEG(dim(aseltemp))
  iALKs<-OM@iALK[,,1,,] # time invariant
  aseltemp[aselind]<-OM@sel[aselind[,c(1,3,5)]]*iALKs[aselind[,c(1,2,4,5)]]
  asel<-apply(aseltemp,1:4,sum)

  FM<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  Find<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nfleets))
  FM[Find]<-OM@qE[Find[,c(1,7)]]*asel[Find[,c(1,2,7,3)]]*OM@E[Find[,c(1,7,4,5,6)]]

  apply(FM[1,1,,nyears,3,,],1:2,sum)

  maxRF<-apply(FM,c(1,2,4,5,6,7),max)
  Rind<-TEG(c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))
  sel<-FM
  sel[Rind]<-sel[Rind]/maxRF[Rind[,c(1,2,4,5,6,7)]]
  sel<-sel[,,,nyears,nsubyears,,] # Take this from last year, in future simulations this may be by year so leave this code!
  sel[is.na(sel)]<-0

  # Initializing the simulation ----------------------------------------------
  cat("Initializing simulations")
  cat("\n")

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

  # New model initialization -----------------------

  Rec<-array(array(OM@muR,c(nsim,npop,nyears))*OM@Recdevs,c(nsim,npop,nyears))
  Rec1<-Rec[,,1]

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


  indN<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1,1:nsubyears,1:nareas))
  N[indN]=OM@muR[indN[,1:2]]*exp(OM@lnHR1[indN[,1:2]])*surv[indN[,1:3]]*stemp[indN[,c(1,2,5,6)]]
  #N[,,nages,1,,]<-N[,,nages,1,,]+N[,,nages,1,,]*array(exp(-M[,,nages,1])/(1-exp(-M[,,nages,1])),c(nsim,npop,nsubyears,nareas)) # plus group

  SSB[,,,1,,]<-N[,,,1,,]*rep(Wt_age[,,,1],nareas*nsubyears)*rep(mat[,,,1],nareas*nsubyears)

  #SSB[sim,pp,age,y,m,rr]

  #apply(SSB[1,    ,   ,1,,],c(1,3),sum)
  sdur<-1/nsubyears
  canspawn<-array(rep(c(0,1,0,0,0,0,1,0),each=nsim),c(nsim,npop,nareas))
  hM<-array(M,c(nsim,npop,nages,nareas))

  for(y in 2:nHyears){

    for(m in 1:nsubyears){

      if(m==1){ # first subyear

        Ntemp<-apply(N[,,,1,nsubyears,]*exp(-hM*sdur/2),c(1,3,4),sum,na.rm=T)
        Ntemp[Ntemp==0]<-tiny # to avoid zero catches divided by zero numbers
        Htemp<-array(rep(HCobs[y-1,nsubyears,,],each=nsim),c(nsim,nages,nareas))/Ntemp
        Htemp[Htemp>0.9]<-0.9
        hFAT[,y-1,nsubyears,,]<--log(1-Htemp)
        hZind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y-1,nsubyears,1:nareas))
        hFATind<-hZind[,c(1,4,5,3,6)]
        hMind<-hZind[,c(1,2,3,6)]
        hZ[hZind]<-hFAT[hFATind]+hM[hMind]*sdur
        N[,,,1,m,]<-N[,,,1,nsubyears,]*exp(-hZ[,,,y-1,nsubyears,])
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)

      }else if(m==2){ # spawning subyear

        Ntemp<-apply(N[,,,1,m-1,]*exp(-hM*sdur/2),c(1,3,4),sum,na.rm=T)
        Ntemp[Ntemp==0]<-tiny
        Htemp<-array(rep(HCobs[y,m-1,,],each=nsim),c(nsim,nages,nareas))/Ntemp
        Htemp[Htemp>0.9]<-0.9
        hFAT[,y,m-1,,]<--log(1-Htemp)
        hZind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m-1,1:nareas))
        hFATind<-hZind[,c(1,4,5,3,6)]
        hMind<-hZind[,c(1,2,3,6)]
        hZ[hZind]<-hFAT[hFATind]+hM[hMind]*sdur
        N[,,,1,m,]<-N[,,,1,m-1,]*exp(-hZ[,,,y,m-1,])
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)

        SSBtemp<-apply(SSB[,,,1,m,],c(1,2,4),sum)*canspawn # viable spawning biomass
        spawnr<-SSBtemp/array(apply(SSBtemp,1:2,sum),dim(SSBtemp))
        SSBt<-apply(SSB[,,,1,m,],1:2,sum)
        N[,,nages,1,m,]<-N[,,nages,1,m,]+N[,,nages-1,1,m,] # plus group
        N[,,2:(nages-1),1,m,]<-N[,,1:(nages-2),1,m,]
        #N[,,1,1,m,]<-spawnr*array(Rec1,dim(spawnr)) # Initial recruitment distributed by viable spawning biomass

        if(y<(nHyears-10)){
          N[,,1,1,m,]<-spawnr*array(OM@muR[indN[,1:2]]*exp(OM@lnHR1[indN[,1:2]]),dim(spawnr))
        }else{
          N[,,1,1,m,]<-spawnr*array(OM@muR[indN[,1:2]]*exp(OM@lnHR2[indN[,1:2]]),dim(spawnr))
        }


      }else{   # after spawning subyear

        Ntemp<-apply(N[,,,1,m-1,]*exp(-hM*sdur/2),c(1,3,4),sum,na.rm=T)
        Ntemp[Ntemp==0]<-tiny
        Htemp<-array(rep(HCobs[y,m-1,,],each=nsim),c(nsim,nages,nareas))/Ntemp
        Htemp[Htemp>0.9]<-0.9
        hFAT[,y,m-1,,]<--log(1-Htemp)
        hZind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m-1,1:nareas))
        hFATind<-hZind[,c(1,4,5,3,6)]
        hMind<-hZind[,c(1,2,3,6)]
        hZ[hZind]<-hFAT[hFATind]+hM[hMind]*sdur
        N[,,,1,m,]<-N[,,,1,m-1,]*exp(-hZ[,,,y,m-1,])
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)

      }# End of if subyear
    }  # end of subyear
  }    # end of SRA year

  # Run historical simulation ----------------------------------------------
  cat("Running historical simulations")
  cat("\n")

  y<-1
  m<-1

  SPAYMRF2<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas,1:nfleets))
  SPAYRF2<-SPAYMRF2[,c(1:4,6,7)]
  SF2<-SPAYMRF2[,c(1,7)]
  SPAY2<-SPAYMRF2[,1:4]
  SFAY2<-SPAYMRF2[,c(1,7,3,4)]
  SFYMR2<-SPAYMRF2[,c(1,7,4,5,6)]
  SPAYMR2<-SPAYMRF2[,1:6]
  SFA2<-SPAYMRF2[,c(1,7,3)]
  SFAR2<-SPAYMRF2[,c(1,7,3,6)]
  SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
  SPARF2<-SPAYMRF2[,c(1:3,6,7)]
  SPRFA2<-SPAYMRF2[,c(1,2,6,7,3)] #
  SPR2<-SPAYMRF2[,c(1,2,6)] # for projections only
  SRF2<-SPAYMRF2[,c(1,6,7)]
  SPAL<-cbind(SPAYMRF2[,c(1:3)],rep(nyears,nrow(SPAYMRF2)))

  for(m in 1:nsubyears){

    SPAYMRF2[,5]<-m
    SPAYMR2<-SPAYMRF2[,1:6]
    SPAYMR[,5]<-m
    VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                    # Calculate vunerable biomassp a y m r f
    Ftot<-apply(FM[,,,y,m,,],1:4,sum)
    Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears

    C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*(FM[SPAYMRF2]/Z[SPAYMR2]) # Calculate catches

  }

  SPAYMR[,5]<-1
  SPAYMRF2[,5]<-1
  SPAYMR2<-SPAYMRF2[,1:6]

  for(y in 2:nyears){

    SPAYMR[,4]<-y
    SPAY<-SPAYMR[,1:4]
    SPAYMRF2[,4]<-y
    SPAY2<-SPAYMRF2[,1:4]
    SFAY2<-SPAYMRF2[,c(1,7,3,4)]
    SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
    SPAYRF2<-SPAYMRF2[,c(1:4,6,7)]

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

      N[,,,y,m,]<-domov(N[,,,y,m,],mov[,,,m,,])

      VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                     # Calculate vunerable biomass
      Ftot<-apply(FM[,,,y,m,,],1:4,sum)
      Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears

      C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*(FM[SPAYMRF2]/Z[SPAYMR2]) # Calculate catches

      for(pp in 1:npop){

        SSB[,pp,,y,m,]<-N[,pp,,y,m,]*array(Wt_age[,pp,,y]*mat[,pp,,y],dim=c(nsim,nages,nareas))

        if(Recsubyr[pp]==m){

          SSBtemp<-apply(SSB[,pp,,y,m,],c(1,3),sum)*canspawn[,pp,] # viable spawning biomass
          spawnr<-SSBtemp/array(apply(SSBtemp,1,sum),dim(SSBtemp))
          N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,] # plus group
          N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]
          N[,pp,1,y,m,]<-spawnr*array(Rec[,pp,y],dim(spawnr)) # Initial recruitment distributed by viable spawning biomass

        } # if its the right subyear
      } # end of pop
    } # end of subyear
  } # end of year

  #apply(SSB[1,    ,   ,nyears,,],c(1,3),sum)

  SSB0=OM@muR[1,]*exp(OM@lnHR1[1,])*apply(surv[1,,]*Wt_age[1,,,1]*mat[1,,,1],1,sum);     #// Unfished Spawning Stock Biomass
  #SSB0<-SSB0+OM@muR[1,]*exp(OM@lnHR1[1,])*surv[1,,nages]*Wt_age[1,,nages,1]*mat[1,,nages,1]*exp(-M[1,,nages,1])/(1-exp(-M[1,,nages,1])) #// indefinite integral of surv added to get plus group SSB0

  SSBcur<-apply(N[,,,nyears,nsubyears,]*
                  array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)

  Bcur<-apply(N[,,,nyears,nsubyears,]*
                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)

  Biomass<-array(0,c(nsim,npop,nages,allyears,nsubyears,nareas))

  Biomass[,,,1:nyears,,]<-N[,,,1:nyears,,]*array(Wt_age,c(nsim,npop,nages,nyears,nsubyears,nareas))


  SSBall<-N*array(Wt_age,dim(N))*array(OM@mat,dim(N))
  RAI<-apply(SSBall,c(1,4,5,6),sum)
  RAI<-RAI[,1:nyears,,]
  RAI<-RAI/array(apply(RAI,1,mean),dim(RAI))

  #D<-Bcur/SSB0 # sim sam Check against OM@D (remember only targetpop is matched)

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

  .Object@C<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@B_BMSY<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@F_FMSY<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@D<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@ageMb<-trlnorm(nsim,1,Obs@ageMbcv)
  .Object@SSB<-array(NA,c(nMPs,nsim,npop,allyears))

  # Run projections ------------------------------------------------
  cat("Running projections")
  cat("\n")
  sfExport(list=c("XSA","DD_i4","DD_i2","DD_i4_4010","Islope1",
                  "DD_i2_4010","CDD_i4","CDD_i2","SPslope","DD",
                  "DD_R","UMSY","CDD","Fadapt","MeanC","tiny"),  namespace="ABTMSE")
  upyrs<-nyears+(0:(floor(OM@proyears/interval)-1))*interval  # the years in which there are updates (every three years)

  testC<-array(NA,c(nsim,npop,nfleets,nareas))
  CAdist<-array(NA,c(nsim,npop,nareas,nfleets,nages))
  CAA<-sampCatch(apply(C[,,,1:(nyears-1),,,],c(1,3,4),sum,na.rm=T),.Object@nCAAobs)
  inc<-OM@mulen[2]-OM@mulen[1]
  CAL<-makeCAL2(CAA,OM@iALK)
  CAL_bins<-c(OM@mulen,OM@mulen[OM@nlen]+inc)-0.5*inc

  # Allocation / assessment vector -------------------------
  if(is.na(MPareas[1]))MPareas<-c(rep(2,4),rep(1,nareas-4))
  nAss<-max(MPareas)

  if(is.na(Allocation)[1]){
    Allocation<-array(0,c(nAss,nfleets))
    Cdist<-apply(OM@Cobs[(nyears-2):nyears,,,],3:4,sum)
    for(a in 1:nAss)Allocation[a,]<-apply(Cdist[MPareas==a,],2,sum)/sum(Cdist[MPareas==a,])
  }

  if(ncol(Allocation)!=nfleets)stop("You need to specify an allocation array with  (OM@nfleets) columns")
  if(length(MPareas)!=nareas)stop("You need to specify an MPareas array with  (OM@nareas) columns")

  Assess_data<-array(rep(MPareas,each=nAss)==rep(1:nAss,nareas),c(nAss,nareas)) # logical array for later calculations
  .Object@TAC<-array(NA,c(nsim,nMPs,nAss,proyears))

  # MP index properties -------------------------------------

  nind<-max(Obs@MPind$No)
  Index_areas<-array(FALSE,c(nind,nareas))
  Istats<-array(NA,c(nsim,nind,4)) # beta, AC, sd, cor
  Itype<-rep(1,nind)
  Ilev<-array(NA,c(nsim,nyears+proyears,nareas,2))
  Ilev[,1:nyears,,1]<-apply(Biomass[,,,1:nyears,,],c(1,4,6),sum)
  Ilev[,1:nyears,,2]<-apply(SSB[,,,1:nyears,,],c(1,4,6),sum)
  Isim<-array(NA,c(nsim,nind,nyears+proyears,2))


  for(i in 1:nind){

    id<-subset(Obs@MPind,Obs@MPind$No==i & Obs@MPind$Year<(OM@nyears+1))
    yrs<-yrs2<-id$Year
    if(!nyears%in%yrs)yrs2<-c(yrs,nyears)
    Itype[i]<-id$Type[1]  # Biomass (1) or spawning biomass (2)
    aind<-as.numeric(strsplit(as.character(id$Areas[1]),"-")[[1]])
    Index_areas[i,aind]<-TRUE

    for(s in 1:nsim){

      IB<-Ilev[s,yrs,Index_areas[i,],Itype[i]]
      if(class(IB)=="matrix")IB<-apply(IB,1,sum)

      IB2<-Ilev[s,yrs2,Index_areas[i,],Itype[i]]
      if(class(IB2)=="matrix")IB2<-apply(IB2,1,sum)

      Isim[s,i,yrs2,1]<-IB2 # these also have to include the latest model year nyears yrs2

      fitout<-indfit(SSB=IB,ind=id$Index,Year=yrs,sim=F,plot=F)

      Istats[s,i,]<-as.numeric(fitout[[1]])
      Isim[s,i,yrs,2]<-fitout$mult
      procmu <- -0.5*(Istats[s,i,3])^2 # adjusted log normal mean

      Perr<-rnorm(proyears+1,procmu, Istats[s,i,3])
      Perr<-Istats[s,i,2]*Perr+Perr*(1-Istats[s,i,2]*Istats[s,i,2])^0.5#2#AC*Perr[,y-1]+(1-AC)*Perr[,y] # apply a pseudo AR1 autocorrelation to rec devs (log space)
      Isim[s,i,nyears:(proyears+nyears),2] <-exp(Perr*Istats[s,i,3]/sd(Perr)) # normal
      #Isim[s,i,nyears,1]<-sum(Ilev[s,nyears,Index_areas[i,],Itype[i]])
    }

  }


  Itemp<-Isim[,,1:nyears,1]^array(Istats[,,1],c(nsim,nind,nyears))                 # add hyperstability / hyper depletion
  Itemp2<-Itemp[,,1:nyears]/array(apply(Itemp[,,1:nyears],1:2,mean,na.rm=T),dim(Itemp))*Isim[,,1:nyears,2] # normalize to mean 1 pre autocorrelated residuals
  Iobs<-Itemp2/array(apply(Itemp2,1:2,mean,na.rm=T),dim(Itemp2))                # normalize to mean 1 post residual error
  # !! CHECK !! These historical indices should be identical among simulations (they are the backward recreation of the statistical fits)

  Fdist<-apply(FM[,,,(nyears-Fdistyrs+1):nyears,,,],c(1,5,6,7),sum) # F is the same for both stocks so summing makes no difference
  Fdistsum<-apply(Fdist,c(1,4),sum)
  Fdistind<-TEG(dim(Fdist))
  Fdist[Fdistind]<-Fdist[Fdistind]/Fdistsum[Fdistind[,c(1,4)]]
  Fdist[is.na(Fdist)]<-0   # gets rid of zero divided by zero NaN values

  testC<-array(NA,c(nsim,nsubyears,nareas,nfleets,nAss))
  testCind<-TEG(dim(testC))

  #include check for Fdist and Allocation

  Regime<-OM@Recind[,y-nyears]
  Rectype<-OM@Rectype[Regime,pp] # rec changes at the same time for both stocks
  proccv<-rep(OM@Reccv,allyears)
  proccv[proccv>0.8]<-0.8 # max proc error ### for testing purposes
  procmu <- -0.5*(proccv)^2
  Pe<-array(exp(rnorm(nsim*npop*allyears,procmu,proccv)),c(nsim,npop,allyears))

  Pe<-Pe/array(apply(Pe,1:2,mean),dim(Pe))

  dset<-new('list')

  for(MP in 1:nMPs){

    cat(paste0(paste0(MP,"/",nMPs," Running MSE for: "),paste0(MPs[[MP]]," (",.Object@Snames,")",collapse="  ")))  # print a progress report
    cat("\n")
    flush.console()                                                  # update the console

    TAC<-array(rep(curTAC,each=nsim),c(nsim,nAss))

    for(y in nyears:(nyears+proyears)){

      cat(".")

      if(y%in%upyrs){# Operate MP S P A Y M R

        # Simulate indices ----------------------------------------------
        # if new data are required
        if(y!=nyears){

          Ilev[,(y-interval+1):y,,1]<-apply(Biomass[,,,(y-interval+1):y,,],c(1,4,6),sum)
          Ilev[,(y-interval+1):y,,2]<-apply(SSB[,,,(y-interval+1):y,,],c(1,4,6),sum)

          Itemp<-array(NA,c(nsim,nind,y,nareas))
          Itemp_ind<-TEG(dim(Itemp))
          Ilev_ind<-cbind(Itemp_ind[,c(1,3,4)],Itype[Itemp_ind[,2]])
          Itemp[Itemp_ind]<-Ilev[Ilev_ind]*Index_areas[Itemp_ind[,c(2,4)]]
          Isim[,,1:y,1]<-apply(Itemp[,,1:y,],1:3,sum)

          Itemp<-Isim[,,1:y,1]^array(Istats[,,1],c(nsim,nind,y))                 # add hyperstability / hyper depletion
          Itemp2<-Itemp/array(apply(Itemp,1:2,mean,na.rm=T),dim(Itemp))*Isim[,,1:y,2] # normalize to mean 1 pre autocorrelated residuals
          Iobs<-Itemp2/array(apply(Itemp2,1:2,mean,na.rm=T),dim(Itemp2))                # normalize to mean 1 post residual error

          # if additional data are required
          nuy<-(upyrs[match(y,upyrs)-1]):(y-1)
          nCAA<-sampCatch(apply(C[,,,nuy,,,],c(1,3,4),sum),.Object@nCAAobs)
          CAA<-abind(CAA,nCAA,along=3)
          CAL<-abind(CAL,makeCAL3(nCAA,OM@iALK[,,nyears,,]),along=3)
        }
        for(AS in 1:nAss){
          #SPAYMRF
          AA<-Assess_data[AS,]
          nA<-sum(AA)
          dset[[AS]]<-list("Cobs"=apply(C[,,,1:(y-1),,AA,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,y-1,nsubyears,nA,nfleets)),c(1,4),sum)*.Object@Cerr[,1:(y-1)],
                           "Iobs"=Iobs[,,1:y-1],
                           "K"=OM@Kmu[,AS]*.Object@Kb,        # for now these assume same growth by stock
                           "Linf"=OM@Linfmu[,AS]*.Object@Kb,  # for now these assume same growth by stock
                           "t0"=OM@t0[,AS],                   # no error in t0
                           "M"=OM@M[,AS,,(y-1)]*.Object@Mb,  # assume AS is same as stock
                           #"Bt"=apply(N[,,,y-1,nsubyears,AA]*
                            #            array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nA)),1,sum)*.Object@Bterr[,(y-1)],#apply(VBA[,,,(y-1),4,],1,sum)*.Object@Bterr[,(y-1)], # you were here
                           "Bt"=apply(N[,,,y-1,nsubyears,AA]*
                                        array(Wt_age[,,,nyears]*exp(-Ftot[,,,AA]),c(nsim,npop,nages,nA)),1,sum)*.Object@Bterr[,(y-1)],#
                           "MSY"=OM@MSY[,AS]*.Object@MSYb,
                           "BMSY"=OM@BMSY[,AS]*.Object@BMSYb,
                           "UMSY"=OM@UMSY[,AS]*.Object@FMSYb,
                           "a"=rep(OM@a,nsim),
                           "b"=rep(OM@b,nsim),
                           "nages"=OM@nages,
                           "ageM"=OM@ageM[,AS]*.Object@ageMb,
                           "Mat"=OM@mat[,AS,,nyears],
                           "Bt_PI"=apply(N[,,,y-1,nsubyears,AA]*
                                        array(Wt_age[,,,nyears]*exp(-Ftot[,,,AA]),c(nsim,npop,nages,nA)),1,sum),
                           #"Bt_PI"=apply(N[,,,y-1,nsubyears,]*
                            #               array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),1,sum),
                           "UMSY_PI"=OM@UMSY[,AS],
                           "CAA"=CAA,
                           "CAL"=CAL,
                           "CAL_bins"=CAL_bins,
                           "MPrec"=TAC[,AS])

          assign("dset",dset,envir=globalenv()) # debugging
          sfExport("dset")
          if(MPs[[MP]][AS]=="XSA") TAC[,AS]<-sapply(1:nsim,get(MPs[[MP]][AS]),dset[[AS]])
          if(MPs[[MP]][AS]!="XSA")TAC[,AS]<-sfSapply(1:nsim,get(MPs[[MP]][AS]),dset[[AS]])
          if(y<allyears).Object@TAC[,MP,AS,y-nyears+1]<-TAC[,AS]
        }

        testC[testCind]<-TAC[testCind[,c(1,5)]]*Fdist[testCind[,c(1:4)]]*Allocation[testCind[,c(5,4)]] # predicted catch by TAC

        #nsim, nsubyears, nareas, nfleets
        aggC<-apply(testC,1:4,sum)

      } # end of upyrs

      SPAYMR[,4]<-y
      SPAYMRF2[,4]<-y
      SPAY<-SPAYMR[,1:4]
      SPAY2<-SPAYMRF2[,1:4]
      SFAY2<-SPAYMRF2[,c(1,7,3,4)]

      # need to aggregate and allocate TACs here.
      # Fdist [sim, subyear, area, fleet]

      # testC [nsim,nsubyears,nareas,nfleets,nAss]

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

        # move fish spaymrr

        N[,,,y,m,]<-domov(N[,,,y,m,],mov[,,,m,,])

        Biomass[,,,y,m,]<-N[,,,y,m,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas))

        VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAL]*sel[SPARF2]                    # Calculate vunerable biomass

        Btemp<-apply(Biomass[,,,y,m,],c(1,4),sum)

        testU<-aggC[,m,,]/array(Btemp,dim(aggC[,m,,])) # implied harvest rate
        #Fp<-(-log(1-(do.call(IE,list(testU))))) # subject to implementation error
        #testC2<-(1-exp(-Fp))*array(Btemp,dim(aggC[,m,,]))
        testC2<-do.call(IE,list(testU))*array(Btemp,dim(aggC[,m,,]))

        CAdist[SPRFA2]<-N[SPAYMR2]*Wt_age[SPAL]*sel[SPARF2] # predicted magnitude of catch in each strata
        CAdistsum<-apply(CAdist,c(1,3,4),sum)                # total in each sim, region and fleet
        CAdist[SPRFA2]<-CAdist[SPRFA2]/CAdistsum[SPRFA2[,c(1,3,4)]] # fraction in each stock and age class per sim region and fleet
        CAdist[is.na(CAdist)]<-0

        C[SPAYMRF2]<-testC2[SRF2]*CAdist[SPRFA2]
        C[SPAYMRF2][is.na(C[SPAYMRF2])]<-0
        C[SPAYMRF2]<-C[SPAYMRF2]/Wt_age[SPAL] # divide by weight to get numbers
        Up<-array(C[SPAYMRF2]/N[SPAYMR2],c(nsim,npop,nages,nareas,nfleets)) # additional check on maximum / minimum U
        Up[is.na(Up)|Up<0.000001]<-0.000001   # otherwise you can't generate some of the automatic fishery data
        Up[Up>0.9]<-0.9
        FM[SPAYMRF2]<--log(1-Up[SPARF2])

        Ftot<-apply(FM[,,,y,m,,],1:4,sum)
        Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears

        for(pp in 1:npop){

          SSB[,pp,,y,m,]<-N[,pp,,y,m,]*array(Wt_age[,pp,,nyears]*mat[,pp,,nyears],dim=c(nsim,nages,nareas))

          if(Recsubyr[pp]==m){

            SSBtemp<-apply(SSB[,pp,,y,m,],c(1,3),sum)*canspawn[,pp,] # viable spawning biomass
            spawnr<-SSBtemp/array(apply(SSBtemp,1,sum),dim(SSBtemp))
            N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,] # plus group
            N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]

            if(y==nyears){

              N[,pp,1,y,m,]<-spawnr*array(Rec[,pp,y],dim(spawnr)) # Initial recruitment distributed by viable spawning biomass

            }else{

              SSBt<-apply(SSB[,pp,,y,m,],1,sum)
              Regime<-OM@Recind[,y-nyears]
              Rectype<-OM@Rectype[Regime,pp] # rec changes at the same time for both stocks
              #procmu <- -0.5*(OM@Reccv[,pp])^2
              #Pe<-exp(rnorm(nsim,procmu,OM@Reccv[,pp]))
              # OM@Recpars [nsim, pp, regime,par]
              R0<-OM@Recpars[cbind(1:nsim,rep(pp,nsim),Regime,rep(2,nsim))]
              SSBpR<-apply(surv[,pp,]*OM@mat[,pp,,nyears]*OM@Wt_age[,pp,,nyears],1,sum) # This R0 dependent so needs updating for varying future R0s
              SSB0<-R0*SSBpR

              if(Rectype[1]=="BH"){ # currently rectypes change together

                h<-OM@Recpars[cbind(1:nsim,rep(pp,nsim),Regime,rep(1,nsim))]
                N[,pp,1,y,m,]<-Pe[,pp,y]*spawnr*(    (0.8*R0*h*SSBt) /
                                                       (0.2*SSBpR*R0*(1-h) + (h-0.2)*SSBt))

              }else if(Rectype[1]=="HS"){

                inflect<-OM@Recpars[cbind(1:nsim,rep(pp,nsim),Regime,rep(1,nsim))]
                N[,pp,1,y,m,]<-Pe[,pp,y]*spawnr*R0
                cond<-SSBt<(SSB0*inflect)
                N[cond,pp,1,y,m,]<-N[cond,pp,1,y,m,]*SSBt[cond]/(SSB0[cond]*inflect[cond])

              }
            }
          } # if its the right subyear
        } # end of pop
      } # end of subyear
    } # end of year

    # Store results

    SSBmu<-apply(SSB,c(1:4,6),mean)
    .Object@SSB[MP,,,]<-apply(SSBmu,c(1:2,4),sum)

    .Object@C[MP,,,]<-apply(C[,,,1:allyears,,,]*array(Wt_age[,,,nyears],dim(C[,,,1:allyears,,,])),c(1,2,4),sum)

    SSB2<-apply(N[,,,1:allyears,4,]*array(mat[,,,nyears]*Wt_age[,,,nyears],c(nsim,npop,nages,allyears,nareas)),c(1,2,4),sum)
    .Object@D[MP,,,]<-SSB2/array(SSB2[,,1],dim(SSB2))

    B<-apply(N[,,,1:allyears,4,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,allyears,nareas)),c(1:2,4),sum)
    #Bthen<-apply((SSN[,,,1,4,]+NSN[,,,1,4,])*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)
    .Object@B_BMSY[MP,,,]<-B/array(OM@BMSY,dim(B))

    U<-.Object@C[MP,,,]/(.Object@C[MP,,,]+B)
    .Object@F_FMSY[MP,,,]<-U/apply(array(OM@UMSY[,],c(nsim,npop)),1,mean)

    cat("\n")
  } # end of MP

  .Object@SSB0<-apply(array(OM@muR,dim(surv))*surv*Wt_age[,,,nyears]*mat[,,,nyears],1:2,sum)

  # This is all about calculating the equilibrium unfished SSB0 for the various recruitment types in the future
  SSB0proj<-array(NA,c(nsim,npop,proyears))
  SSB0ind<-TEG(dim(SSB0proj))
  parind<-cbind(SSB0ind[,1:2],OM@Recind[SSB0ind[,c(1,3)]],rep(2,nrow(SSB0ind)))
  SSB0proj[SSB0ind]<-OM@Recpars[parind]*apply(surv*Wt_age[,,,nyears]*mat[,,,nyears],1:2,sum)[SSB0ind[,1:2]]
  .Object@SSB0proj<-SSB0proj

  .Object@MPs<-MPs
  .Object@area_defs<-OM@area_defs
  .Object@areanams<-OM@areanams

  .Object

})

