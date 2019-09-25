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
#' \item{CW}{a 4D array containing true simulated catches by east-west area [MP x nsim x 2 x nyears]}
#' \item{CWa}{a 4D array containing true simulated catches by east-west area [MP x nsim x 2 x nyears]}
#' \item{D}{a 4D array containing true simulated stock depletion (SSB/SSB0) [MP x nsim x nstocks x nyears]}
#' \item{B_BMSY}{a 4D array containing true simulated biomass relative to BMSY [MP x nsim x nstocks x nyears]}
#' \item{F_FMSY}{a 4D array containing true simulated fishing mortality rate relative to FMSY [MP x nsim x nstocks x nyears]}
#' \item{BB}{currently unused}
#' \item{BBa}{currently unused}
#' \item{SSB}{a 4D array containing true simulated spawning biomass [MP x nsim x nstocks x nyears]}
#' \item{SSB0}{a 2D array containing unfished spawnign biomass by simulation and stock [nsim x nstocks]}
#' \item{dynB0}{a 3D array containing dynamic unfished spawning biomass by simulation, stock and projection year [nsim x nstocks x proyears]}
#' \item{dynB0h}{a 3D array containing dynamic unfished spawning biomass by simulation, stock and historical year [nsim x nstocks x nyears]}
#' \item{MSY}{a 2D array containing MSY estimates (2018 parameters) [sim x nstocks]}
#' \item{BMSY}{a 2D array containing BMSY estimates (2018 parameters) [sim x nstocks]}
#' \item{SSBMSY}{a 2D array containing SSBMSY estimates (2018 parameters) [sim x nstocks]}
#' \item{UMSY}{a 2D array containing UMSY estimates (2018 parameters) [sim x nstocks]}
#' \item{FMSYa}{a 2D array containing apical FMSY estimates (2018 parameters) [sim x nstocks]}
#' \item{SSBMSY_SSB0}{a 2D array containing MSY estimates (2018 parameters) [sim x nstocks]}
#' \item{TAC}{a 4D array containing the TAC recommendations [nsim x MP x stock x proyear]}
#' \item{nMPs}{an integer number representing the number of MPs in the MSE}
#' \item{Snames}{a character vector naming the stocks [nstocks]}
#' \item{area_defs}{a list of area definitions for graphing (lons and lats describing the polygon)}
#' \item{areanams}{a character vector of area names}
#' \item{Istats}{an array of index fit statistics nsim x nind x 4 (beta, AC, sd, cor)}
#' \item{Fleet_comp}{an array of Fleet composition data nsim x MP x nfleet x proyear x length class}
#' \item{TACtaken}{an array of TAC actually taken nsim x MP x NAss x proyear}
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
  Ftimp="numeric", Ftb="array",Fterr="array",

  hb="numeric",
  Recbcv="numeric",
  IMSYb="numeric", MSYb="numeric", BMSYb="numeric",

  # Management quantities
  C="array",
  CW="array",
  CWa="array",
  BB="array",
  BBa="array",
  D="array",
  B_BMSY="array",
  F_FMSY="array",
  B="array",
  SSB="array",
  SSBa="array",
  SSB0="array",
  dynB0="array",
  dynB0h="array",
  TAC="array",
  SSBpR="array",

  # operating model MSY quantities
  MSY="array",
  BMSY="array",
  SSBMSY="array",
  UMSY="array",
  FMSYa="array",
  SSBMSY_SSB0="array",

  nMPs="integer",
  Snames="character",
  area_defs="list",
  areanams="character",
  Istats="array",

  Fleet_comp="array",
  TACtaken="array",

  MPs="list"

))

setMethod("initialize", "MSE", function(.Object,OM=OM_example,Obs=Good_Obs,MPs=list(c("UMSY","UMSY")),interval=2,IE="Umax_90",
                                        TAC2015=c(16142000,2000000),TAC2016=c(19296000,1912000),TAC2017=c(23155000,2000000),TAC2018=c(28200000,2350000),TAC2019=c(32240000,2350000),TAC2020=c(36000000,2350000),
                                        Allocation=ABTMSE:::Allocation,MPareas=NA,Fdistyrs=3,maxTAC=c(10,10),MSEparallel=F,check=FALSE){

  # .Object}); .Object<-new('MSE')
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
  if(class(get(MPs[[1]][1]))!='MP'& class(get(MPs[[1]][1]))!='MSMP'){
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

  # All MSY operating model quantities
  .Object@MSY<-OM@MSY
  .Object@BMSY<-OM@BMSY
  .Object@SSBMSY<-OM@SSBMSY
  .Object@UMSY<-OM@UMSY
  .Object@FMSYa<-OM@FMSYa
  .Object@SSBMSY_SSB0<-OM@SSBMSY_SSB0
  .Object@SSBpR<-OM@SSBpR

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
  movIndex<-OM@movIndex
  h<-OM@h
  Recsubyr<-OM@Recsubyr
  Recdevs<-OM@Recdevs
  SSBpR<-OM@SSBpR
  HCobs<-OM@HCobs

  M<-OM@M
  Mtemp<-array(0,dim(OM@M))
  Mtemp[,,2:nages,]<-OM@M[,,1:(nages-1),]

  surv=tomt(exp(-apply(Mtemp[,,,1],2:1,cumsum)))
  surv[,,nages]<-surv[,,nages]+surv[,,nages]*exp(-Mtemp[,,nages,1])/(1-exp(-Mtemp[,,nages,1]))

  rm(Mtemp); invisible(gc())

  BB<-BBa<-CW<-CWa<-array(NA,c(nMPs,nsim,npop,allyears+nHyears))
  #HC<-array(NA,c(nsim,npop,nHyears,nsubyears))

  N<-SSB<-BBd<-Z<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  FD<-array(NA,c(nsim,nfleets,nyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  C<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets)) # VB removed
  CA<-array(NA,c(nsim,npop,allyears,nsubyears,nareas))

  hFAT<-array(NA,c(nsim,nHyears,nsubyears,nages,nareas))
  hZ<-array(NA,c(nsim,npop,nages,nHyears,nsubyears,nareas))
  #hZind<-TEG(dim(hZ))
  #hFATind<-hZind[,c(1,3,4,5,6)]

  mref<-c(2:nsubyears,1)  # movement reference
  y<-1
  m<-1

  # Calculating F arrays ----------------------------------------------
  cat("Calculating historical fishing mortality rate at length (computationally intensive)")
  cat("\n")
  iALK<-OM@iALK

  FML<-array(NA,c(nsim,npop,nlen,allyears,nsubyears,nareas,nfleets))
  FM<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  Ftemp<-array(NA,c(nsubyears,nfleets,nareas,nages,nlen))
  Ftind<-TEG(dim(Ftemp))
  nFt<-nrow(Ftind)

  for(y in 1:nyears){

    FLind<-as.matrix(expand.grid(1:nsim,1:npop,1:nlen,y,1:nsubyears,1:nareas,1:nfleets))
    FML[FLind]<-OM@qE[FLind[,c(1,7)]]*OM@sel[FLind[,c(1,7,3)]]*OM@E[FLind[,c(1,7,4,5,6)]]*OM@Fmod[FLind[,c(1,6,5)]]*OM@FDY[FLind[,c(1,4,5,6)]]
    rm(FLind); invisible(gc()) # delete F index

    for(s in 1:nsim){
      for(p in 1:npop){

        FMLind<-cbind(rep(s,nFt),rep(p,nFt),Ftind[,5],rep(y,nFt),Ftind[,c(1,3,2)]) # s p l m r f
        iALKind<-cbind(rep(s,nFt),rep(p,nFt),rep(y,nFt),Ftind[,4:5]) # s p y a l
        Ftemp[Ftind]<-FML[FMLind]*iALK[iALKind]
        FM[s,p,,y,,,]<-aperm(apply(Ftemp,1:4,sum),c(4,1,3,2)) #[m f r a]  to [a m r f]

      }
    }

  }
  FML[is.na(FML)]<-0
  FM[is.na(FM)]<-0

  maxRF<-apply(FM[,,,nyears,nsubyears,,],c(1,2,4,5),max)
  Rind<-TEG(c(nsim,npop,nages,nareas,nfleets))
  sel<-FM[,,,nyears,nsubyears,,]
  sel[Rind]<-sel[Rind]/maxRF[Rind[,c(1,2,4,5)]]
  sel[is.na(sel)]<-0

  rm(Rind); invisible(gc())

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

  # Recruitment calculation ---

  LTyrs<-4 # Lower triangle (recent recruitment years)

  #proccv<-rep(OM@Reccv,allyears)
  recconv<-sigR_AC_conv(OM@Reccv,OM@AC) # sim by
  Reccv<-recconv$sig1
  Recmu <- -0.5*(Reccv)^2
  AC<-recconv$AC1

  nSR<-length(OM@Rectype)
  Pe<-procmu<-proccv<-array(NA,c(nsim,nSR,LTyrs+allyears))
  Pind<-TEG(dim(Pe))
  proccv[Pind]<-Reccv[Pind[,1:2]]
  procmu[Pind]<-Recmu[Pind[,1:2]]

  Pe[]<-rnorm(nsim*nSR*(LTyrs+allyears),procmu,proccv) # all years are populated but Pe is currently only used in projection
  for (y in 2:(LTyrs+allyears)) Pe[,, y] <- AC * Pe[,, y - 1] +   Pe[,, y] * (1 - AC * AC)^0.5
  Pe<-exp(Pe)
  Pe<-Pe/array(apply(Pe,1:2,mean),dim(Pe)) # renormalize deviations (that have been subject to AC) to be mean 1

  OM@Recdevs[,,nyears+(-(LTyrs-1):0)]<-Pe[,OM@Recind[,1],1:LTyrs] # lower triangle are predicted as mean historical recruitment

  Rec<-array(NA,c(nsim,npop,nyears))  #

  stemp<-array(1/nareas,dim=c(nsim,npop,nsubyears,nareas))
  movi<-mov[,,nages,1,,,] # sim, pop, season, from, to

  for(y in 1:20){
    for(m in 1:nsubyears){
      if(m==1){
        stemp[,,m,]<-apply(array(rep(stemp[,,nsubyears,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)
      }else{
        stemp[,,m,]<-apply(array(rep(stemp[,,m-1,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)
      }
    }
  }

  indN<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1,1:nsubyears,1:nareas))
  R0ind<-cbind(indN[,1:2],rep(1,nrow(indN)))
  N[indN]=OM@hR0[R0ind]*surv[indN[,1:3]]*stemp[indN[,c(1,2,5,6)]]

  SSB[,,,1,,]<-N[,,,1,,]*rep(Wt_age[,,,1],nareas*nsubyears)*rep(mat[,,,1],nareas*nsubyears)
  BBd<-N[,,,1,,]*rep(Wt_age[,,,1],nareas*nsubyears)
  BBa[,,,1]<-rep(apply(BBd[,,,2,],1:2,sum),each=nMPs)
  BB[,,1,1]<-rep(apply(BBd[,,,2,4:7],1,sum),each=nMPs)
  BB[,,2,1]<-rep(apply(BBd[,,,2,1:3],1,sum),each=nMPs)

  sdur<-1/nsubyears
  canspawn<-array(rep(c(0,1,0,1,0,0,0,0,0,0,0,0,1,0),each=nsim),c(nsim,npop,nareas))
  hM<-array(M[,,,1],c(nsim,npop,nages,nareas))
  spawnr<-array(NA,c(nsim,npop,nareas))

  for(y in 2:nHyears){
    mi<-movIndex[1]
    for(m in 1:nsubyears){

      if(m==1){ # first subyear

        Ntemp<-apply(N[,,,1,nsubyears,]*exp(-hM*sdur/2),c(1,3,4),sum,na.rm=T)

        Ntemp[Ntemp==0]<-tiny # to avoid zero catches divided by zero numbers
        Htemp<-array(rep(HCobs[y-1,nsubyears,,],each=nsim),c(nsim,nages,nareas))/Ntemp
        Htemp[Htemp>0.9]<-0.9
        #for(pp in 1:np)HC[,pp,y,m]<-Htemp[pp,,]*N[,pp,,1,m,]
        hFAT[,y-1,nsubyears,,]<--log(1-Htemp)
        hZind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y-1,nsubyears,1:nareas))
        hFATind<-hZind[,c(1,4,5,3,6)]
        hMind<-hZind[,c(1,2,3,6)]
        hZ[hZind]<-hFAT[hFATind]+hM[hMind]*sdur
        N[,,,1,m,]<-N[,,,1,nsubyears,]*exp(-hZ[,,,y-1,nsubyears,])
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,mi,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)

      }else if(m==2){ # spawning subyear

        #SSBt<-apply(SSB[,,,1,m,],1:2,sum) # s p
        SSBs<-apply(N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas),c(1:2,4),sum)
        SSBt<-apply(SSBs,1:2,sum)

        for(pp in 1:npop){
          SSBtemp<-SSBs[,pp,]*canspawn[,pp,] # viable spawning biomass
          spawnr[,pp,]<-SSBtemp/array(apply(SSBtemp,1,sum),dim(SSBtemp)) # s p r
        }

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
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,mi,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)

        N[,,nages,1,m,]<-N[,,nages,1,m,]+N[,,nages-1,1,m,] # plus group
        N[,,2:(nages-1),1,m,]<-N[,,1:(nages-2),1,m,]
        #N[,,1,1,m,]<-spawnr*array(Rec1,dim(spawnr)) # Initial recruitment distributed by viable spawning biomass

        for(pp in 1:npop){

          SSB0=OM@hR0[,pp,1]*SSBpR[,pp]    #// Unfished Spawning Stock Biomass
          R0=OM@hR0[,pp,1]


          if(OM@hRectype[pp,1]=="BH"){

            h<-OM@hRecpar[,pp,1]
            N[,pp,1,1,m,]<-spawnr[,pp,]*(    (0.8*R0*h*SSBt[,pp]) / (0.2*SSBpR[,pp]*R0*(1-h) + (h-0.2)*SSBt[,pp]))

          }else{ # hockey stick

            inflect<-OM@hRecpar[,pp,1]
            N[,pp,1,1,m,]<-spawnr*R0
            cond<-SSBt[,pp]<(SSB0*inflect)
            N[cond,pp,1,1,m,]<-N[cond,pp,1,1,m,]*SSBt[cond,pp]/(SSB0[cond]*inflect[cond])

          }

        }

        BBd<-N[,,,1,,]*rep(Wt_age[,,,1],nareas*nsubyears)
        BBa[,,,y]<-rep(apply(BBd[,,,2,],1:2,sum),each=nMPs)
        BB[,,1,y]<-rep(apply(BBd[,,,2,4:7],1,sum),each=nMPs)
        BB[,,2,y]<-rep(apply(BBd[,,,2,1:3],1,sum),each=nMPs)

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
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,mi,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)

      }# End of if subyear
    }  # end of subyear

  }    # end of SRA year


  rm(hFAT); rm(hZ); rm(hFATind); rm(hZind); invisible(gc())

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
    Ftot<-apply(FM[,,,y,m,,],1:4,sum)
    Z[SPAYMR2]<-Ftot[SPAR]+M[SPAY]/nsubyears
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
      mi<-movIndex[y]
      N[,,,y,m,]<-domov(N[,,,y,m,],mov[,,,mi,m,,])

      #VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                     # Calculate vunerable biomass
      Ftot<-apply(FM[,,,y,m,,],1:4,sum)
      Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears

      C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*(FM[SPAYMRF2]/Z[SPAYMR2]) # Calculate catches

      for(pp in 1:npop){

        SSB[,pp,,y,m,]<-N[,pp,,y,m,]*array(Wt_age[,pp,,y]*mat[,pp,,y],dim=c(nsim,nages,nareas))

        if(Recsubyr[pp]==m){

          SSBtemp<-apply(SSB[,pp,,y,m,],c(1,3),sum)*canspawn[,pp,] # viable spawning biomass
          SSBt<-apply(SSB[,pp,,y,m,],1,sum)
          spawnr<-SSBtemp/array(apply(SSBtemp,1,sum),dim(SSBtemp))
          N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,] # plus group
          N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]

          #SSBpR=apply(surv[,pp,]*Wt_age[,pp,,y]*mat[,pp,,y],1,sum)  # SSBpR both this and SSB0 are now dynamic
          SSB0=OM@hR0[,pp,y]*SSBpR[,pp]    #// Unfished Spawning Stock Biomass
          R0=OM@hR0[,pp,y]

          if(OM@hRectype[pp,y]=="BH"){

              h<-OM@hRecpar[,pp,y]
              N[,pp,1,y,m,]<-OM@Recdevs[,pp,y]*spawnr*(    (0.8*R0*h*SSBt) / (0.2*SSBpR[,pp]*R0*(1-h) + (h-0.2)*SSBt))

          }else{ # hockey stick

              inflect<-OM@hRecpar[,pp,y]
              N[,pp,1,y,m,]<-spawnr*OM@Recdevs[,pp,y]*R0
              cond<-SSBt<(SSB0*inflect)
              N[cond,pp,1,y,m,]<-N[cond,pp,1,y,m,]*SSBt[cond]/(SSB0[cond]*inflect[cond])
          }

          #N[,pp,1,y,m,]<-spawnr*array(Rec[,pp,y],dim(spawnr)) # Initial recruitment distributed by viable spawning biomass

        } # if its the right subyear

        BBd<-N[,,,y,,]*rep(Wt_age[,,,1],nareas*nsubyears)
        BBa[,,,nHyears+y]<-rep(apply(BBd[,,,2,],1:2,sum),each=nMPs)
        BB[,,1,nHyears+y]<-rep(apply(BBd[,,,2,4:7],1,sum),each=nMPs)
        BB[,,2,nHyears+y]<-rep(apply(BBd[,,,2,1:3],1,sum),each=nMPs)

      } # end of pop
    } # end of subyear
  } # end of year


  if(check){
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    passmark=0.05 # maximum % deviation of 5%
    par(mfrow=c(3,2),mai=c(0.4,0.4,0.4,0.01),omi=c(0.4,0.4,0.4,0.01))

    #     p y s a r
    ageclasses<-c(1,1)
    muN<-apply(OM@checks$N[,,,ageclasses,],c(1,2,4,5),mean) # mean over seasons p y a r
    N_M3<-apply(muN,1:2,sum)
    N_M3[,ncol(N_M3)+0:-4]<-NA
    #      sim p a y s a
    muN<-apply(N[,,ageclasses,,,],c(1,2,3,4,6),mean) # s, p a y a
    N_R<-apply(muN,c(1,2,4),sum)[,,1:nyears]
    N_R[,,dim(N_R)[3]+0:-4]<-NA

    yrs<-1964+(1:nyears)
    for(pp in npop:1){
      plot(yrs,N_M3[pp,],col='red',lwd=2,type="l",ylim=c(0,max(N_M3[pp,],N_R[,pp,],na.rm=T)))
      col<-"blue"
      if(nsim>2)col<-'#0000ff50'
      matplot(yrs,t(N_R[,pp,]),col=col,lwd=1,add=T,type='l',lty=1)
      mtext(paste(OM@Snames[pp],"stock"),3,line=0.4)
      if(pp==npop){
        legend('topleft',legend=c("M3 v5.0",paste0("ABT-MSE v",packageVersion('ABTMSE'))),text.col=c("red","blue"),bty='n',cex=1.2,text.font=2)
        mtext("Age 1 recruits",2,line=2.5)
      }
      if(nsim<3){
        pass=mean(abs(1-abs(N_M3[pp,]/N_R[1,pp,])),na.rm=T)<passmark
        legend('bottomright',legend=paste("Pass:",as.character(pass)),text.col=c('red','green')[pass+1],bty='n')
      }
    }

    #     p y s a r
    ageclasses<-3:9
    muN<-apply(OM@checks$N[,,,ageclasses,],c(1,2,4,5),mean) # mean over seasons p y a r
    N_M3<-apply(muN,1:2,sum)
    N_M3[,ncol(N_M3)+0:-4]<-NA
    #      sim p a y s a
    muN<-apply(N[,,ageclasses,,,],c(1,2,3,4,6),mean) # p a y a
    N_R<-apply(muN,c(1,2,4),sum)[,,1:nyears]
    N_R[,,dim(N_R)[3]+0:-4]<-NA

    yrs<-1964+(1:nyears)
    for(pp in npop:1){
      plot(yrs,N_M3[pp,],col='red',lwd=2,type="l",ylim=c(0,max(N_M3[pp,],N_R[,pp,],na.rm=T)))
      matplot(yrs,t(N_R[,pp,]),col=col,lwd=1,add=T,type='l',lty=1)
      if(pp==npop){
         mtext("Numbers (ages 2-9)",2,line=2.5)
      }
      if(nsim<3){
        pass=mean(abs(1-abs(N_M3[pp,]/N_R[1,pp,])),na.rm=T)<passmark
        legend('bottomright',legend=paste("Pass:",as.character(pass)),text.col=c('red','green')[pass+1],bty='n')
      }
    }

    #     p y s a r
    ageclasses<-10:nages
    muN<-apply(OM@checks$N[,,,ageclasses,],c(1,2,4,5),mean) # mean over seasons p y a r
    N_M3<-apply(muN,1:2,sum)
    #      sim p a y s a
    muN<-apply(N[,,ageclasses,,,],c(1,2,3,4,6),mean) # p a y a
    N_R<-apply(muN,c(1,2,4),sum)[,,1:nyears]

    yrs<-1964+(1:nyears)
    for(pp in npop:1){
      plot(yrs,N_M3[pp,],col='red',lwd=2,type="l",ylim=c(0,max(N_M3[pp,],N_R[,pp,])))
      matplot(yrs,t(N_R[,pp,]),col=col,lwd=1,add=T,type='l',lty=1)

      if(pp==npop){
        mtext("Numbers (age 10+)",2,line=2.5)

      }
      if(nsim<3){
        pass=mean(abs(1-abs(N_M3[pp,]/N_R[1,pp,])))<passmark
        legend('bottomright',legend=paste("Pass:",as.character(pass)),text.col=c('red','green')[pass+1],bty='n')
      }
    }

    F_max<-apply(FM,c(1:2,4:7),max,na.rm=T)
    F_sum<-apply(F_max,c(1,2,3,5),sum,na.rm=T)
    F_mu<-apply(F_sum,1:3,mean,na.rm=T)

    #mtext("Check of M3 estimation versus ABTMSE reconstruction (a pass is less than 5% mean error)",3,adj=0.7,line=0.5,outer=T)
    mtext("Year",1,line=0.5,outer=T)
    mtext(OM@Name,3,adj=0.01,font=2,outer=T,line=0.3)
    message("You specified check=TRUE so some diagnostics have been produced relating to the conversion from M3 - ABTMSE R package. The analysis is stopping here - no forward projections")
    .Object@SSB<-SSB
    .Object@C<-C
    .Object@BB<-BB
    .Object@Ftb<-F_mu
    return(.Object)
  }

  SSB0=array(OM@hR0[,,1],dim(surv))*apply(surv*Wt_age[,,,1]*mat[,,,1],1,sum)     #// Unfished Spawning Stock Biomass

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

  # Generate observation errors ---------------------------------------------

  .Object@Cimp<-runif(nsim,Obs@Ccv[1],Obs@Ccv[2])
  .Object@Cb<-rep(Obs@Cbias,nsim)#trlnorm(nsim,1,Obs@Cbcv)

  .Object@Cerr<-array(trlnorm(nsim*allyears,rep(.Object@Cb,allyears),rep(.Object@Cimp,allyears)),c(nsim,allyears))
  .Object@Cerr[,nyears+(1:MPlag)]<-1 # TACs from 2017-2020 for example, are known perfectly

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
  .Object@CW<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@CWa<-array(NA,c(nMPs,nsim,2,allyears))
  .Object@B_BMSY<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@F_FMSY<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@D<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@ageMb<-trlnorm(nsim,1,Obs@ageMbcv)
  .Object@SSB<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@SSBa<-array(NA,c(nMPs,nsim,npop,allyears))

  # Dynamic B0 calculations -----------------------------------------------------

  dynB0h<-array(NA,c(nsim,npop,nyears))
  dynB0<-array(NA,c(nsim,npop,proyears))

  for(pp in 1:npop){

    R0<-OM@hR0[,pp,1]

    #SSBpR=apply(surv[,pp,]*Wt_age[,pp,,1]*mat[,pp,,1],1,sum)  # SSBpR both this and SSB0 are now dynamic

    SSB_d=R0*SSBpR[,pp]
    dynB0h[,pp,1]<-SSB_d
    N_d<-R0*surv[,pp,]
    for(y in 2:nyears){
      Nplus<-N_d[,nages]*exp(-OM@M[,pp,nages,1])
      for(aa in nages:2)N_d[,aa]<-N_d[,aa-1]*exp(-OM@M[,pp,aa-1,1])
      N_d[,1]<-OM@hR0[,pp,y]
      N_d[,nages]<-N_d[,nages]+Nplus
      dynB0h[,pp,y]<-apply(Wt_age[,pp,,1]*mat[,pp,,1]*N_d,1,sum)

    }

    for(y in 1:proyears){
      Nplus<-N_d[,nages]*exp(-OM@M[,pp,nages,1])
      for(aa in nages:2)N_d[,aa]<-N_d[,aa-1]*exp(-OM@M[,pp,aa-1,1])
      SRno<-OM@Recind[pp,y]
      N_d[,1]<-OM@Recpars[,SRno,2]
      N_d[,nages]<-N_d[,nages]+Nplus
      dynB0[,pp,y]<-apply(Wt_age[,pp,,1]*mat[,pp,,1]*N_d,1,sum)
    }

  }

  .Object@dynB0<-dynB0
  .Object@dynB0h<-dynB0h


  # Run projections ------------------------------------------------
  cat("Running projections")
  cat("\n")
  if(sfIsRunning())sfExport(list=c("XSA","DD_i7","Islope1",
                  "DD_i7_4010","CDD_i7","SPslope","DD",
                  "DD_R","UMSY","CDD","Fadapt","MeanC","tiny"),  namespace="ABTMSE")

  upyrs<-(nyears+MPlag+1)+(0:(floor((OM@proyears-2)/interval)-1))*interval  # the years in which there are updates (every three years)

  testC<-array(NA,c(nsim,npop,nfleets,nareas))
  CAdist<-array(NA,c(nsim,npop,nareas,nfleets,nages))
  CAA<-sampCatch(apply(C[,,,1:(nyears-1),,0,],c(1,3,4),sum,na.rm=T),.Object@nCAAobs)
  inc<-OM@mulen[2]-OM@mulen[1]
  CAL<-makeCAL2(CAA,OM@iALK)
  CAL_bins<-c(OM@mulen,OM@mulen[OM@nlen]+inc)-0.5*inc

  # Allocation / assessment vector -------------------------
  if(is.na(MPareas[1]))MPareas<-c(rep(2,3),rep(1,nareas-3))
  nAss<-max(MPareas)

  Assess_data<-array(rep(MPareas,each=nAss)==rep(1:nAss,nareas),c(nAss,nareas)) # logical array for later calculations

  # MP index properties -------------------------------------

  PerfInfo<-max(Obs@Icv)<0.02 # should observation error be added?

  nind<-max(Obs@MPind$No)
  Index_areas<-array(FALSE,c(nind,nareas))
  Istats<-array(NA,c(nsim,nind,6)) # beta, AC, sd, cor, AC2, sd2

  Isim<-array(NA,c(nsim,nind,nyears+proyears,2))
  lastI<-array(NA,c(nsim,nind))
  lastobs<-rep(NA,nind)

  for(i in 1:nind){

    id<-subset(Obs@MPind,Obs@MPind$No==i & Obs@MPind$Year<(OM@nyears+2)) # use up to 2016 indices
    yrs<-yrs2<-id$Year
    if(!nyears%in%yrs)yrs2<-c(yrs,nyears)
    MPQ<-id$Quarter[1]
    MParea<-id$Areas[1]

    if(grepl('Fleet',id$Fleet[1])){  # is it a fleet vulnerability index?
      MPFleet<-as.numeric(strsplit(as.character(id$Fleet[1]),"Fleet_")[[1]][2])
      Bio<-apply(Biomass[,,,1:nyears,MPQ,MParea],c(1,3,4),sum)
      seli<-apply(FM[,,,nyears,MPQ,MParea,MPFleet],c(1,3),mean) # !! approximation to age selectivity as mean of stocks (very similar growth so inconsequential)
      seli<-seli/apply(seli,1,max)# normalize to max 1
      IBref<-apply(Bio*array(seli,dim(Bio)),c(1,3),sum) #; matplot(t(IBref))
    }else if(id$Fleet[1]=="SSB"){ # is it a spawning stock index?
      IBref<-apply(SSB[,,,1:nyears,MPQ,MParea],c(1,4),sum)
    }else{                        # is it a biomass index?
      IBref<-apply(Biomass[,,,1:nyears,MPQ,MParea],c(1,4),sum)
    }

    lastobs[i]<-max(yrs)
    futureind<-(lastobs[i]+1):(nyears+proyears) # new indices
    nonfit<-nyears+(1:proyears)                 # years without fit data

    for(s in 1:nsim){

      IB<-IBref[s,yrs[yrs<=nyears]]#Ilev[s,yrs,Index_areas[i,],Itype[i]]
      if(class(IB)=="matrix")IB<-apply(IB,1,sum,na.rm=T)

      IB2<-IBref[s,yrs2[yrs2<=nyears]]#,Index_areas[i,],Itype[i]]
      if(class(IB2)=="matrix")IB2<-apply(IB2,1,sum,na.rm=T)

      Isim[s,i,yrs,1]<-id$Index # these also have to include the latest model year nyears yrs2
      lastI[s,i]<-mean(Isim[s,i,yrs[length(yrs)],1]) # I = qB, I/B = q scales OM quantities to indices

      modyrs<-yrs[yrs<=nyears]
      Index_ind<-yrs%in%modyrs
      fitout<-indfit(SSBt=IB[Index_ind],ind=id$Index[Index_ind],Year=modyrs,sim=F,plot=F)

      Istats[s,i,]<-as.numeric(fitout[[1]])
      Isim[s,i,modyrs,2]<-fitout$mult
      if(PerfInfo)  Istats[s,i,c(3,6)]<-0

      procmu <- -0.5*(Istats[s,i,3])^2 # adjusted log normal mean

      if(PerfInfo){
        Isim[s,i,,2]<-1
      }else{
        Ierr<-rnorm(proyears,procmu, Istats[s,i,6])
        Ierr<-Istats[s,i,5]*Ierr+Ierr*(1-Istats[s,i,5]*Istats[s,i,5])^0.5# # apply a pseudo AR1 autocorrelation
        Isim[s,i,nonfit,2]<-exp(Ierr*Istats[s,i,6]/sd(Ierr)) # normalize so that post autocorrelation has same sd as before
      }


    }

  }

  # Catch observation error model ----
  CobsCV<-array(NA,c(nsim,nAss))
  Cobs_hist<-array(NA,c(nsim,nAss,nyears))
  # s y a    #C[SPAYMRF2]
  Ctemp<-apply(C[,,,1:nyears,,,]*array(Wt_age[,,,nyears],dim(C[,,,1:nyears,,,])),c(1,4,6),sum)

  for(AS in 1:nAss){
    Cobs_hist[,AS,]<-array(rep(Ann_Cat[Ann_Cat[,2]==AS,3],each=nsim),c(nsim,nyears))
    CobsCV[,AS]<-apply(apply(Ctemp[,,Assess_data[AS,]],1:2,sum)/Cobs_hist[,AS,],1,sd)
  }

  #  1   2  3   4   5   6
  # beta AC sd cor AC2 sd2
  .Object@Istats<-Istats
  Iobs<-Isim[,,1:nyears,1]#Itemp1/array(apply(Itemp2,1:2,mean,na.rm=T),dim(Itemp2))                # normalize to mean 1 post residual error
  # !! CHECK !! These historical indices should be identical among simulations (they are the backward recreation of the statistical fits)

  # F distribution code (Allocation sums to 1 in East area, and West area),
  Fdist1<-apply(FM[,,,(nyears-Fdistyrs+1):nyears,,,],c(1,5,6,7),sum)# s m r f  # F is the same for both stocks so summing makes no difference
  Fdist<-array(0,c(nsim,nsubyears,nareas,nfleets))

  for(AS in 1:nAss){
    AA<-Assess_data[AS,]
    for(ff in 1:nfleets){
      sumq<-apply(Fdist1[,,AA,ff],c(1,3),sum,na.rm=T)
      Fdistind<-as.matrix(expand.grid(1:nsim,1:nsubyears,(1:nareas)[AA],ff))
      sumqind<-Fdistind
      sumqind[,3]<-sumqind[,3]-min(sumqind[,3])+1
      Fdist[Fdistind]<-Fdist1[Fdistind]/sumq[sumqind[,c(1,3)]]
    }
  }
  Fdist[is.na(Fdist)]<-0
  # rbind(c(Fleets$name,"OTH"),apply(Fdist[1,,Assess_data[1,],],3,sum))

  testC<- TACdist<-array(0,c(nsim,nsubyears,nareas,nfleets,nAss))
  testCind<-TEG(dim(testC))
  for(AS in 1:nAss){
    AA<-Assess_data[AS,]
    nAA<-sum(AA)
    for(ff in 1:nfleets){
      TACdist[,,AA,ff,AS]<-Fdist[,,AA,ff]*array(rep(Allocation[AA,ff],each=nsim*nsubyears),c(nsim,nsubyears,nAA))
    }
  }
  # auto adjust TACdist *to catch the < 1% of cases where strata are zeros (ie there is fishing distributed in Fdist that isn't in the allocation ff strata)
  TACdist_adj<-1/apply(TACdist,c(1,5),sum) # summa
  TACdist[testCind]<-TACdist[testCind]*TACdist_adj[testCind[,c(1,5)]]

  dset<-new('list')

  # Preallocated arrays
  Itemp<-array(NA,c(nsim,nind,allyears,nareas))
  .Object@TAC<-array(NA,c(nsim,nMPs,nAss,proyears+2))
  .Object@TACtaken<-array(NA,c(nsim,nMPs,nAss,proyears+2))
  .Object@TAC[,,,1]<-rep(TAC2016,each=nsim*nMPs)
  .Object@TAC[,,,2]<-rep(TAC2017,each=nsim*nMPs)
  .Object@TAC[,,,3]<-rep(TAC2018,each=nsim*nMPs)
  .Object@TAC[,,,4]<-rep(TAC2019,each=nsim*nMPs)
  .Object@Fleet_comp<-array(NA,c(nsim,nMPs,nfleets,allyears,nages))

  for(MP in 1:nMPs){

    cat(paste0(paste0(MP,"/",nMPs," Running MSE for: "),paste0(MPs[[MP]]," (",.Object@Snames,")",collapse="  ")))  # print a progress report
    cat("\n")
    flush.console()                                                  # update the console

    #for(i in 1:2)dset[[i]]$MPrec<-rep(curTAC[i],nsim)

    for(y in nyears:(nyears+proyears)){ # for(y in nyears:(upyrs[1]-1)){

      # y<-y+1
      if(y==nyears){ # 2016
        TAC<-TACtrial<-array(rep(TAC2016,each=nsim),c(nsim,nAss))
      } else if (y==nyears+1){ # 2017
        TAC<-TACtrial<-array(rep(TAC2017,each=nsim),c(nsim,nAss))
      } else if (y==nyears+2){ # 2018
        TAC<-TACtrial<-array(rep(TAC2018,each=nsim),c(nsim,nAss))
      } else if(y==nyears+3){ # 2019
        TAC<-TACtrial<-array(rep(TAC2019,each=nsim),c(nsim,nAss))
      } else if(y==nyears+4){ # 2020
        TAC<-TACtrial<-array(rep(TAC2020,each=nsim),c(nsim,nAss))
      }

      if(y>=nyears & y<=nyears+MPlag){
        testC[testCind]<-TAC[testCind[,c(1,5)]]*TACdist[testCind] # predicted catch by TAC
        aggC<-apply(testC,1:4,sum)

        #apply(testC,c(1,5),sum)==TAC; apply(testC[1,,,,],c(2,4),sum)
      }

      cat(".")

      if(y%in%upyrs){# Operate MP S P A Y M R

        # Simulate indices ---------------------------------------------
        Iobs<-array(0,c(nsim,nind,y-1))

        for(i in 1:nind){

          id<-subset(Obs@MPind,Obs@MPind$No==i) # use up to 2016 indices
          yrs<-1:(y-1)

          MPQ<-id$Quarter[1]
          MParea<-id$Areas[1]

          if(grepl('Fleet',id$Fleet[1])){  # is it a fleet vulnerability index?
            MPFleet<-as.numeric(strsplit(as.character(id$Fleet[1]),"Fleet_")[[1]][2])
            Bio<-apply(Biomass[,,,yrs,MPQ,MParea],c(1,3,4),sum)
            seli<-apply(FM[,,,nyears,MPQ,MParea,MPFleet],c(1,3),mean) # !! approximation to age selectivity as mean of stocks (very similar growth so inconsequential)
            seli<-seli/apply(seli,1,max)# normalize to max 1
            IBref<-apply(Bio*array(seli,dim(Bio)),c(1,3),sum) #; matplot(t(IBref))
          }else if(id$Fleet[1]=="SSB"){ # is it a spawning stock index?
            IBref<-apply(SSB[,,,yrs,MPQ,MParea],c(1,4),sum)
          }else{                        # is it a biomass index?
            IBref<-apply(Biomass[,,,yrs,MPQ,MParea],c(1,4),sum)
          }

          update<-(lastobs[i]+1):(y-1)

          if(!OM@Ibeta_ignore){ # add hyperstability
            IBref[,update]<-IBref[,update]^array(Istats[,i,1],c(nsim,length(update)))
          }

          if(grepl('Fleet',id$Fleet[1])){ # if a CPUE index add catchability increase
            IBref[,update]<-IBref[,update]*(1+OM@qinc/100)^(1:((y-1)-lastobs[i]))
          }
          #        True biomass index (removed error)
          imod<-(lastI[,i]/Isim[,i,lastobs[i],2])/(IBref[,lastobs[i]]) # The error x mag differential between last observation and first simulated observation
          Isim[,i,update,1]<-IBref[,update]*imod
          Iobs[,i,1:lastobs[i]]<-Isim[,i,1:lastobs[i],1]        # don't add error to past observations
          Iobs[,i,update]<- Isim[,i,update,1]*Isim[,i,update,2] # add error to future observations

        } # end of indices

        # if additional data are required
        if(y==nyears+MPlag+1){
          nuy<-nyears:(y-MPlag)
        }else{
          nuy<-(upyrs[match(y,upyrs)-1]):(y-1)
        }

        nCAA<-sampCatch(apply(C[,,,nuy,,,],c(1,3,4),sum,na.rm=T),.Object@nCAAobs)
        CAA<-abind(CAA,nCAA,along=3)
        CAL<-abind(CAL,makeCAL3(nCAA,OM@iALK[,,nyears,,]),along=3)

        for(AS in 1:nAss){

          #SPAYMRF
          AA<-Assess_data[AS,]
          nA<-sum(AA)

          Cobs<-array(NA,c(nsim,y-3))
          Cobs[,1:nyears]<-Cobs_hist[,AS,] # copy over all catch before projection
          upind<-(nyears+1):(y-3) # copy over all catch since projection

          if(length(upind>0)){
            Cobs[,upind]<-apply(array(C[,,,upind,,AA,],c(nsim,npop,nages,length(upind),nsubyears,nA,nfleets))*array(Wt_age[,,,nyears],c(nsim,npop,nages,length(upind),nsubyears,nA,nfleets)),c(1,4),sum,na.rm=T)*.Object@Cerr[,upind]
          }
          #Cobs<-apply(C[,,,1:(y-3),,AA,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,y-3,nsubyears,nA,nfleets)),c(1,4),sum,na.rm=T)*.Object@Cerr[,1:(y-3)]

          dset[[AS]]<-list("Cobs"=cbind(Cobs,.Object@TAC[,MP,AS,y-nyears-1]),
                           "Iobs"=Iobs[,,1:(y-2)],
                           "K"=OM@Kmu[,AS]*.Object@Kb,        # for now these assume same growth by stock
                           "Linf"=OM@Linfmu[,AS]*.Object@Kb,  # for now these assume same growth by stock
                           "t0"=OM@t0[,AS],                   # no error in t0
                           "M"=OM@M[,AS,,(y-2)]*.Object@Mb,  # assume AS is same as stock
                           #"Bt"=apply(N[,,,y-1,nsubyears,AA]*
                            #            array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nA)),1,sum)*.Object@Bterr[,(y-1)],#apply(VBA[,,,(y-1),4,],1,sum)*.Object@Bterr[,(y-1)], # you were here
                           "Bt"=apply(N[,,,y-2,nsubyears,AA]*
                                        array(Wt_age[,,,nyears],c(nsim,npop,nages,nA))*exp(-Ftot[,,,AA]),1,sum)*.Object@Bterr[,y-2],#
                           "MSY"=OM@MSY[,AS]*.Object@MSYb,
                           "BMSY"=OM@BMSY[,AS]*.Object@BMSYb,
                           "UMSY"=OM@UMSY[,AS]*.Object@FMSYb,
                           "a"=rep(OM@a,nsim),
                           "b"=rep(OM@b,nsim),
                           "nages"=OM@nages,
                           "ageM"=OM@ageM[,AS]*.Object@ageMb,
                           "Mat"=OM@mat[,AS,,nyears],
                           "Bt_PI"=apply(N[,,,y-1,nsubyears,AA]*
                                        array(Wt_age[,,,nyears],c(nsim,npop,nages,nA))*exp(-Ftot[,,,AA]),1,sum),
                           #"Bt_PI"=apply(N[,,,y-1,nsubyears,]*
                            #               array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),1,sum),
                           "UMSY_PI"=OM@UMSY[,AS],
                           "CAA"=CAA,
                           "CAL"=CAL,
                           "CAL_bins"=CAL_bins,
                           "MPrec"=TAC[,AS],
                           "TAC"=matrix(.Object@TAC[,MP,AS,1:(y-nyears)],ncol=(y-nyears),nrow=nsim),
                           "curTAC"=rep(TAC2019[AS],nsim)
                           )
        }

        for(AS in 1:nAss){
          #SPAYMRF
          AA<-Assess_data[AS,]

          assign("dset",dset,envir=globalenv()) # debugging
          if(sfIsRunning())sfExport("dset")
          #if(MPs[[MP]][AS]=="XSA"|!sfIsRunning()) TACtrial[,AS]<-sapply(1:nsim,get(MPs[[MP]][AS]),dset[[AS]])
          if(class(get(MPs[[MP]][AS]))=="MP"){
            if(sfIsRunning()&!MSEparallel){
              TACtrial[,AS]<-sfSapply(1:nsim,get(MPs[[MP]][AS]),dset[[AS]])
            }else{
              TACtrial[,AS]<-sapply(1:nsim,get(MPs[[MP]][AS]),dset[[AS]])
            }
          }else if(class(get(MPs[[MP]][AS]))=="MSMP"){
            if(sfIsRunning()&!MSEparallel){
              TACtrial[,AS]<-sfSapply(1:nsim,get(MPs[[MP]][AS]),dset,AS=AS)
            }else{
              TACtrial[,AS]<-sapply(1:nsim,get(MPs[[MP]][AS]),dset,AS=AS)
            }
          }

          if(MPs[[MP]][AS]!="ZeroC"){
            TACmax=(1+maxTAC[AS])*TAC[,AS]
            TACmin=(max(0.01,1-maxTAC[AS]))*TAC[,AS]
            cond=TACtrial[,AS]<TACmin
            TACtrial[cond,AS]=TACmin[cond]
            cond=TACtrial[,AS]>TACmax
            TACtrial[cond,AS]=TACmax[cond]
          }

          if(y<allyears).Object@TAC[,MP,AS,y-nyears+1]<-TAC[,AS]<-TACtrial[,AS]

        }

        testC[testCind]<-TAC[testCind[,c(1,5)]]*TACdist[testCind] # predicted catch by TAC
        aggC<-apply(testC,1:4,sum) #nsim, nsubyears, nareas, nfleets
        # -- Some tests ---
        # round(apply(aggC,3,sum)/sum(aggC)*100,1)
        # apply(aggC,1,sum)==apply(TAC,1,sum) # simulation totals match?

      }else{

        if(y>(nyears+3)).Object@TAC[,MP,,y-nyears+1]<-.Object@TAC[,MP,,y-nyears] # TAC for next year

      } # end of upyrs

      SPAYMR[,4]<-y
      SPAYMRF2[,4]<-y
      SPAY<-SPAYMR[,1:4]
      SPAY2<-SPAYMRF2[,1:4]
      SFAY2<-SPAYMRF2[,c(1,7,3,4)]

      for(m in 1:nsubyears){
        #  m<-m+1

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
        mi<-movIndex[y]
        N[,,,y,m,]<-domov(N[,,,y,m,],mov[,,,mi,m,,])

        Biomass[,,,y,m,]<-N[,,,y,m,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas))

        #VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAL]*sel[SPARF2]                    # Calculate vunerable biomass

        Btemp<-apply(Biomass[,,,y,m,],c(1,4),sum)

        for(AS in 1:nAss){ # Max F redistribution at the catch-at-numbers scale

          AA<-Assess_data[AS,]
          testU<-aggC[,m,AA,]/array(Btemp[,AA],dim(aggC[,m,AA,])) # implied harvest rate
          cond<-testU>0.9
          cond[is.na(cond)]<-T
          Cunder<-array(0,c(nsim,sum(AA),nfleets))
          Cunder[cond]<-aggC[,m,AA,][cond]*(testU[cond]-0.9)/testU[cond]
          aggC[,m,AA,][cond]<-aggC[,m,AA,][cond]-Cunder[cond]
          CunderT<-apply(Cunder,1,sum)
          Cdist<-array(0,c(nsim,sum(AA),nfleets))
          Cdist[!cond]<-aggC[,m,AA,][!cond]
          Cdist<-(Cdist/array(apply(Cdist,1,sum,na.rm=T),c(nsim,sum(AA),nfleets)))*array(CunderT,c(nsim,sum(AA),nfleets))
          aggC[,m,AA,]<-aggC[,m,AA,]+Cdist

        }

        testU<-aggC[,m,,]/array(Btemp,dim(aggC[,m,,])) # implied harvest rate
        testC2<-do.call(IE,list(testU))*array(Btemp,dim(aggC[,m,,]))
        # sum(aggC[1,m,1:3,])
        # sum(testC2[1,1:3,])
        # apply(testC2,1,sum)==apply(aggC[,m,,],1,sum) # simulation totals match?
        # apply(testC2[,1:4,],1:2,sum)==apply(aggC[,m,1:4,],1:2,sum) # simulation totals match?

        #This next line is where it goes wrong
        CAdist[SPRFA2]<-N[SPAYMR2]*Wt_age[SPAL]*sel[SPARF2] # predicted vulnerable biomass each strata
        #tempN<-apply(N[1,,,,,],c(1,5),sum)
        #round(temp/sum(temp)*100,2)
        #CAdist[CAdist<tiny]<-tiny
        CAdist[CAdist==0]<-tiny # you have to do this otherwise zero fish can lead to missing catches
        CAdistsum<-apply(CAdist,c(1,3,4),sum)                # total in each sim, region and fleet

        #tempC<-apply(CAdistsum[1,,],1,sum)
        #round(CAdistsum[1,,]/sum(CAdistsum[1,,])*100,2)

        CAdist[SPRFA2]<-CAdist[SPRFA2]/CAdistsum[SPRFA2[,c(1,3,4)]] # fraction in each stock and age class per sim region and fleet
        CAdist[is.na(CAdist)]<-0

        # apply(CAdist,c(2,5),sum)
        # validated to here

        C[SPAYMRF2]<-testC2[SRF2]*CAdist[SPRFA2]
        C[SPAYMRF2][is.na(C[SPAYMRF2])]<-0

        # -- Some tests ---
        # sum(C[1,,,y,m,1:3,])
        # apply(C[1,1,,y,m,,],2:3,sum) # stock distribution
        # apply(C[1,1,,y,m,,],2,sum)
        # apply(C[,,,y,m,,],1,sum,na.rm=T)==apply(testC2,1,sum)
        # apply(C[,,,y,m,1:4,],c(1,4),sum,na.rm=T)==apply(testC2[,1:4,],1:2,sum)

        C[SPAYMRF2]<-C[SPAYMRF2]/Wt_age[SPAL] # divide by weight to get numbers

        # --Some tests ---
        #test12<-apply(C[,,,y,m,Assess_data[1,],],1:2,sum)
        #test22<-apply(C[,,,y,m,Assess_data[2,],],1:2,sum)
        #testW<-array(C[SPAYMRF2]*Wt_age[SPAL],c(nsim,npop,nages,nareas,nfleets)) # divide by weight to get numbers
        # apply(testW,1,sum,na.rm=T)==apply(testC2,1,sum)
        # apply(testW[,,,1:4,],c(1,4),sum,na.rm=T)==apply(testC2[,1:4,],1:2,sum)
        # apply(testW[,,,1:4,],c(1,4),sum,na.rm=T)== apply(aggC[,m,1:4,],1:2,sum)
        # apply(testW[,,,1:4,],1,sum,na.rm=T)== apply(aggC[,m,1:4,],1,sum)

        Up<-C[,,,y,m,,]/array(N[,,,y,m,],c(nsim,npop,nages,nareas,nfleets))
        Up[is.na(Up)|Up<tiny]<-tiny # otherwise you can't generate some of the automatic fishery data
        Up[Up>0.9]<-0.9
        FM[SPAYMRF2]<-(-log(1-Up[SPARF2]))

        Ftot<-apply(FM[,,,y,m,,],1:4,sum,na.rm=T)
        Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears

        for(pp in 1:npop){

          SSB[,pp,,y,m,]<-N[,pp,,y,m,]*array(Wt_age[,pp,,nyears]*mat[,pp,,nyears],dim=c(nsim,nages,nareas))

          if(Recsubyr[pp]==m){

            SSBtemp<-apply(SSB[,pp,,y,m,],c(1,3),sum)*canspawn[,pp,] # viable spawning biomass
            spawnr<-SSBtemp/array(apply(SSBtemp,1,sum),dim(SSBtemp))
            N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,] # plus group
            N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]
            maxind<-min(y-nyears+1,dim(OM@Recind)[2])
            SRno<-OM@Recind[pp,maxind]
            #SSBpR=apply(surv[,pp,]*Wt_age[,pp,,nyears]*mat[,pp,,nyears],1,sum)  # SSBpR both this and SSB0 are now dynamic
            R0<-OM@Recpars[,SRno,2]
            SSB0=R0*SSBpR[,pp]    #// Unfished Spawning Stock Biomass
            SSBt<-apply(SSB[,pp,,y,m,],1,sum)

            if(OM@Rectype[SRno]=="BH"){

              h<- OM@Recpars[,SRno,1]
              N[,pp,1,y,m,]<-Pe[,SRno,y+LTyrs]*spawnr*((0.8*R0*h*SSBt) / (0.2*SSBpR[,pp]*R0*(1-h) + (h-0.2)*SSBt))

            }else{ # hockey stick

              inflect<-OM@Recpars[,SRno,1]
              N[,pp,1,y,m,]<-Pe[,SRno,y+LTyrs]*spawnr*R0
              cond<-SSBt<(SSB0*inflect)
              N[cond,pp,1,y,m,]<-N[cond,pp,1,y,m,]*SSBt[cond]/(SSB0[cond]*inflect[cond])

            }

          } # if its the right subyear
        } # end of pop

      } # end of subyear

      BBd<-N[,,,y,,]*rep(Wt_age[,,,1],nareas*nsubyears)
      BBa[MP,,,nHyears+y]<-apply(BBd[,,,2,],1:2,sum)
      BB[MP,,1,nHyears+y]<-apply(BBd[,,,2,4:7],1,sum)
      BB[MP,,2,nHyears+y]<-apply(BBd[,,,2,1:3],1,sum)

    } # end of year

    # Store results
    SSBmu<-apply(SSB,c(1:4,6),mean) #nsim,npop,nages,allyears,nareas
    .Object@SSB[MP,,,]<-apply(SSBmu,c(1:2,4),sum)

    # SSB by assessment area:
    for(aa in 1:2).Object@SSBa[MP,,aa,]<-apply(SSBmu[,,,,MPareas==aa],c(1,4),sum)
    rm(SSBmu)

    Ctemp<-apply(C[,,,1:allyears,,,]*array(Wt_age[,,,nyears],dim(C[,,,1:allyears,,,])),c(1,2,4,6),sum) # s p y a
    .Object@CW[MP,,,]<-apply(Ctemp,1:3,sum) # s p y
    for(aa in 1:2){
      #.Object@CWa[MP,,aa,1:nHyears]=rep(apply(HCobs[,,,MPareas==aa],1,sum),each=nsim)
      .Object@CWa[MP,,aa,]=apply(Ctemp[,,,MPareas==aa],c(1,3),sum)
      .Object@TACtaken[,MP,aa,1:(proyears+1)]<-.Object@CWa[MP,,aa,nyears+(-0:proyears)]  # .Object@TACtaken<-array(NA,c(nsim,nMPs,nAss,proyears+2))
    }

    Ctemp2<-apply(Ctemp,c(1,3,4),sum)# SYR
    for(aa in 1:2).Object@C[MP,,aa,]=apply(Ctemp2[,,MPareas==aa],1:2,sum)

    .Object@Fleet_comp[,MP,,,]<-apply(C,c(1,7,4,3),sum) # S,F,P,A from SPAYMRF2

    SSB2<-apply(N[,,,1:allyears,4,]*array(mat[,,,nyears]*Wt_age[,,,nyears],c(nsim,npop,nages,allyears,nareas)),c(1,2,4),sum)
    .Object@D[MP,,,]<-SSB2/array(SSB2[,,1],dim(SSB2))

    B<-apply(N[,,,1:allyears,4,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,allyears,nareas)),c(1:2,4),sum)
    #.Object@B_BMSY[MP,,,]<-B/array(OM@BMSY,dim(B))

    # dynamic B0 stock status calcs
    .Object@B_BMSY[MP,,,1:nyears]<-SSB2[,,1:nyears]/(dynB0h*array(.Object@SSBMSY_SSB0,c(nsim,npop,nyears)))
    .Object@B_BMSY[MP,,,nyears+1:proyears]<-SSB2[,,nyears+1:proyears]/(dynB0*array(.Object@SSBMSY_SSB0,c(nsim,npop,proyears)))

    U<-.Object@CW[MP,,,]/(.Object@CW[MP,,,]+B)
    .Object@F_FMSY[MP,,,]<-U/apply(array(OM@UMSY[,],c(nsim,npop)),1,mean)

    cat("\n")

  } # end of MP

  .Object@SSB0<-apply(array(OM@Recpars[,OM@Recind[,1],2],dim(surv))*surv*Wt_age[,,,nyears]*mat[,,,nyears],1:2,sum)


  # This is all about calculating the equilibrium unfished SSB0 for the various recruitment types in the future
  .Object@BB<-BB
  .Object@BBa<-BBa


  .Object@MPs<-MPs
  .Object@area_defs<-OM@area_defs
  .Object@areanams<-OM@areanams
  invisible(gc()) # garbage collection is automatic in R, I'm doing this mannual to test memory requirements for computers with less RAM
  .Object

})

