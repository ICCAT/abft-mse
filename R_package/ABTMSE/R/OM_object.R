#' An S4 class object that contains all Operating Model estimates in the format for M3 operating model estimation
#'
#'\describe{
#' \item{Name}{The name of the operating model input}
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
#' \item{Rectype}{a matrix specifying the recruitment type (a stock can have multiple types of recruitment over the projected time period) by stock [type, stock]}
#' \item{h}{a vector nstocks long specifying the steepness of the B-H stock recruitment curve (currently unused)}
#' \item{recgrad}{a matrix specifying a possible future gradient in recruitment expressed as a per cent per year [stock x 2 (upper / lower)]}
#' \item{Recind}{an array specifyign which historical recruitment type to use in each projected year [nsim x nstock x projection year]}
#' \item{Recpars}{an array of stock recruitment parameters by recruitment type [nsim x nstock x rectype (Hockey stock / B-H) x 2 (h or jointpoint/R0)]}
#' \item{Reccv}{a vector nstock long of recruitment variability by stock}
#' \item{AC}{a vector nstock long of recruitment autocorrelation}
#' \item{Recsubyr}{a vector nstock long specifying which subyear recruitment occurs in}
#' \item{Linfmu}{a vector nstock long specifying the mean maximum length of individuals}
#' \item{Kmu}{a vector nstock long specifying the mean growth parameter K}
#' \item{t0}{a vector nstock long specifying the theoretical age at length zero}
#' \item{a}{a vector nstock long, the weight-length parameter a W=aL^b}
#' \item{b}{a vector nstock long, the weight-length parameter b W=aL^b}
#' \item{Len_age}{an array of length at age by stock [stock x age]}
#' \item{Wt_age}{an array of weight at age by stock [stock x age]}
#' \item{ageM}{an array of maturity at age by stock [stock x age]}
#' \item{D}{current stock depletion by stock (currently unused)}
#' \item{Size_area}{a vector nareas long, the relative size of each area}
#' \item{mov}{a very large array storing simulated movement by stock [sim x stock x age x subyear x movtype x from area x to area]}
#' \item{movIndex}{a vector of movement types [nyears+proyears]}
#' \item{nfleets}{the number of fishing fleets}
#' \item{iALK}{an array representing the inverse age-length key (conditional probability of length given age) [sim x stock x year x age x length class]}
#' \item{hZ}{an array of historical mortality rate estimates [stock, year, subyear, age, area]}
#' \item{HCobs}{an array of historical catch observations [year, subyear, age, area]}
#' \item{Cobs}{an array of catch observations [year, subyear, area, fleet]}
#' \item{E}{an array of standardized effort (partial effort)[sim, fleet, year, subyear, area]}
#' \item{qE}{a vector nfleets long, the catchability of the fleets}
#' \item{qI}{a vector nI long of catchabilities by fishery independent index}
#' \item{qCPUE}{a vector nCPUE long of catchabilities by fishery CPUE index}
#' \item{sel}{an array of selectivities [sim, fleets, length class]}
#' \item{selpars}{array of selectivity parameters (currently unused)}
#' \item{Fmod}{array of season x area mean F parameters}
#' \item{FDY}{array of annual F deviations [sim, year, quarter, area]}
#' \item{mat}{array of maturity [sim, stock, age, year]}
#' \item{M}{array of natural mortality rate [sim, stock, age, year]}
#' \item{Recdevs}{of historical recruitment deviations [sim, stock, year]}
#' \item{R0}{matrix of unfished recruitment [sim, stock, year]}
#' \item{Recpar}{matrix of recruitment pars [sim, stock, year]}
#' \item{Rectypes}{matrix of recruitment types [stock, year]}
#' \item{MSY}{a matrix of MSY values per simulation and stock [sim x stock]}
#' \item{BMSY}{a matrix of Biomass at MSY values per simulation and stock [sim x stock]}
#' \item{VBMSY}{a matrix of vulnerable Biomass at MSY values per simulation and stock [sim x stock]}
#' \item{SSBMSY}{a matrix of spawning biomass at MSY values per simulation and stock [sim x stock]}
#' \item{FMSY}{a matrix of fishing exploitation rate at MSY values per simulation and stock [sim x stock]}
#' \item{UMSY}{a matrix of fishing harvest rate at MSY values per simulation and stock [sim x stock]}
#' \item{FMSYa}{a matrix of apical (coresponding with age that is most vulnerable) fishing exploitation rate at MSY values per simulation and stock [sim x stock]}
#' \item{SSBMSY_SSB0}{a matrix of spawning biomass at MSY relative to unfished, values per simulation and stock [sim x stock]}
#' \item{nydist}{the number of years used to determine equilibrium spatial distribution}
#' \item{Snames}{a vector nstock long naming each stock}
#' \item{area_defs}{a list of area definitions for graphing (lons and lats describing the polygon)}
#' \item{areanams}{a character vector of area names}
#' \item{Ibeta_ignore}{logical, should hyperstability of indices be included?}
#' \item{qinc}{real number, percentage annual changes in catchability affecting the CPUE indices}
#' \item{SSBpR}{matrix simulation by stock}
#' \item{Fleets}{list of fleet attributes from OMI}
#' \item{Istats}{data.frame of Index statistics}
#' \item{Ires}{array of historical index residuals (sim,index,year)}
#' \item{Deterministic}{logical, should deterministic recruitment be simulated in projections}
#' \item{Obs}{character, the type of observation error model assumed for simulating future catch data}
#' \item{Imp}{character, the implementaiton error model assumed for simulating how well MP advice is adhered to}
#' \item{checks}{returns m3 matching information}
#' \item{seed}{a random seed for generation of simulations to ensure reproducibility}
#' }
setClass("OM",representation(
  # Description --------------------------------------------------------
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",
  # Dimensions ---------------------------------------------------------
  nsim="integer",npop="integer",nages="integer",             # MSE dimensions
  nyears="integer",nHyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
  proyears="integer",nlen="integer",lenbins="numeric",       # Proyears
  interval="integer",                                        # Update interval
  nma="integer",ma="array",                                  # Number of movement age classes, age class definitions
  mulen="numeric",                                           # Mean length of each length bin
  # Sampled parameters -------------------------------------------------
  #Magemu="array",Mrange="array",Msd="array",Mgrad="array",  # Mean natural mortality rate at age, interannual variability and gradient per cent yr-1
  Rectype="character",h="array",recgrad="array",                 # Stock-recruitment relationship type, steepness, underlying gradient per cent yr-1
  Recind="array",                                            # Proyear array determining which SR curve in what proyear nsim x nstock x proyears
  Recpars="array",                                           # Parameters of various SR curves nsim x nstock x rectypes x 2
  Reccv="array",AC="array",                                  # CV of recruitment deviations and recruitment auto-correlation
  Recsubyr="integer",                                        # Sub-year in which recruitment occurs, area in which recruitment occurs
  Linfmu="array",Kmu="array",t0="array",                     # Mean growth parameters
  #Ksd="array",Kgrad="array",Linfsd="array",Linfgrad="array",# Interannual variability in growth and mean trajectory per cent yr-1
  a="numeric",b="numeric",                                   # Weight - Length conversion W=aL^b
  Len_age="array",Wt_age="array",                            # Growth
  #ageMmu="array",#ageMsd="array",ageMgrad="array", # Age-at-maturity, interannual variability and gradient per cent yr-1
  ageM="array",
  D="array",#SSBcur="array",                                 # Current stock depletion, abundance
  Size_area="array",mov="array",movIndex="numeric",#Mmov="array",               # Size of regions, Markov movement matrix for all fish and mature fish #movvar="matrix",movsd="array",movgrad="array",             # Inter-simulation variability in movement, interannual-variability in movement, gradient changes in area gravity weights#excl="array",                                              # Exclusion matrix [0,1] depending on whether the stock can go there
  nfleets="integer",                                         # Number of fleets,#L05="array",VmaxL="array", LFS="array",                    # Length at 5per cent vulnerability, vulnerability of largest fish, length at full selection
  iALK="array",                                              # Inverse Age-Length-Key#Fsd="array",Fgrad="array", Frat="array",                   # Interannual variability in F, Final gradient in F yr-1Area_names="character", Area_defs="list",                  # Area definitions (polygons)Spat_targ="array",                                         # Spatial targetting parameter F =prop= V^Spat_targ
  hZ="array", HCobs="array",   Cobs="array",

  # Simulation data -------
  E="array",#dFfinal="array",
  qE="array",
  qI="array",qCPUE="array",            # Catchability of effort, fishery ind indices and fishery CPUE indices
  sel="array",selpars="array",
  Fmod="array",                        # The regional/seasonal Fmodifiers
  FDY="array",                         # The annual MI deviations
  mat="array",M="array",
  Recdevs="array",
  hR0="array", hRecpar ="array", hRectype="array",
  MSY="array",BMSY="array", VBMSY="array",        # Maximum sustainable yield, biomass at maximum sustainable yield
  SSBMSY="array",
  FMSY="array",UMSY="array",FMSYa="array",         # Fishing mortality rate at maximum sustainable yield
  SSBMSY_SSB0="array",#IMSY="numeric",                                  # Relative abundance index at maximum sustainable yield#Linf="array",K="array", #Idist="array",                                   # for plotting OM unfished spatial dist#targpop="numeric",                               # What populations are to be used in MSY / depletion optimization#nZeq="integer",                                 # The number of initial years to calculation equilibrium F
  nydist="integer",                                # The number of years (iterations) taken to find equilibrium spatial distribution#nyeq="integer",                                 # The number of years (iterations) taken to find equilibrium F
  Snames="character",                              # Name of the stocks
  area_defs="list",                                # Area definitions list (copied from OMI)
  areanams="character",                            # Names of areas        (copied from OMI)
  Ibeta_ignore = "logical",                        # Logical, should hyperstability and hyperdepletion be ignored?
  qinc="numeric",                                  # % annual changes in catchability affecting CPUE indices
  SSBpR="array",
  Fleets="list",
  checks='list',
  Istats='data.frame',
  Ires='array',
  Deterministic="logical",
  Obs="character",
  IE="character",
  seed="numeric"                                   # Random seed from which this object was made
))

setMethod("initialize", "OM", function(.Object, OMd="C:/M3", nsim=48, proyears=50, seed=1,
                                       Recruitment=NULL, Snames=c("East","West"),
                                       MLEonly=F, Deterministic=F, Obs = "Good_Obs", IE = "Umax_90",
                                       ploty=F, debug=F, CPUEinds, Iinds, SD_override, AC_override, Yrs_override){

  # .Object});   .Object<-new('OM',OMd="C:/M3")
  # .Object});   .Object<-new('OM',OMd)

  .Object@Snames=Snames

  if(!dir.exists(OMd))stop(paste('You specified a directory',OMd,'that does not exist or an object',OMd, 'that does not exist'))
  if(!file.exists(paste0(OMd,"/M3.dat")))stop(paste('Could not build operating model: M3 output file ',paste0(OMd,"/M3.dat")))

  cat(paste("Loading operating model input object :",OMd))
  cat("\n") #.Object@targpop=targpop
  load(file=paste0(OMd,"/OMI"))

  cat(paste("Reading operating model fit data from directory:",OMd))
  cat("\n")#.Object@targpop=targpop
  out<-M3read(OMd)

  .Object@checks<-list(N=out$N, SSB=out$SSB, B=out$B, VB=out$VB, VL=out$VL, FL=out$FL)

  set.seed(seed)
  .Object@seed<-seed

  .Object@Name=paste("From",OMd)
  .Object@Date=as.character(Sys.time())
  .Object@Author="NA"
  .Object@Notes="Made from an empirical fit of the M3 model"
  .Object@PrimarySource="NA"

  # Dimensions

  npop<-np<-out$np
  nages<-na<-out$na
  nyears<-out$ny
  nHyears<-out$nHy
  nsubyears<-out$ns
  nareas<-out$nr
  nlen<-out$nl
  nfleets<-out$nf
  nma<-out$nma

  .Object@nsim<-as.integer(nsim)
  .Object@npop<-as.integer(out$np)
  .Object@nages<-as.integer(out$na)
  .Object@nyears<-as.integer(out$ny)
  .Object@nHyears<-as.integer(out$nHy)
  .Object@nsubyears<-as.integer(out$ns)
  .Object@nareas<-as.integer(out$nr)
  .Object@proyears<-as.integer(proyears)
  allyears<-.Object@nyears+.Object@proyears
  .Object@nlen<-as.integer(out$nl)
  .Object@nma<-as.integer(out$nma)
  .Object@ma<-ma<-array(rep(c(rep(1,4),rep(2,4),rep(3,na-8)),each=2),c(np,na)) # for some reason this doesn't yet exist.

  .Object@interval<-as.integer(2)   # Management interval

  .Object@Wt_age<-out$wt_age
  .Object@Len_age<-out$len_age

  .Object@Kmu<-.Object@t0<-.Object@Linfmu<-array(0,c(nsim,npop))
  .Object@a<-.Object@b<-rep(0,npop)

  for(pp in 1:npop){

    opt<-calcVBpars(.Object@Len_age[1,,pp],plot=ploty)
    .Object@Kmu[,pp]<-opt$K
    .Object@t0[,pp]<-opt$t0
    .Object@Linfmu[,pp]<-opt$Linf

    opt<-calcABpars(La=.Object@Len_age[1,,pp],Wa=.Object@Wt_age[1,,pp],plot=ploty)
    .Object@a[pp]<-opt$a
    .Object@b[pp]<-opt$b

  }

  .Object@Cobs<-array(0,c(nyears,nsubyears,nareas,nfleets))
  .Object@Cobs[out$Cobs[,1:4]]<-out$Cobs[,5]

  .Object@lenbins<-rep(0,out$nl+1)
  .Object@mulen<-out$ml
  .Object@nfleets<-as.integer(out$nf)
  .Object@M<-array(rep(out$M_age,each=nsim),c(nsim,npop,nages,nyears+proyears))

  .Object@hZ<-out$hZ
  .Object@HCobs<-out$HCobs

  # ---- Get covariance matrix and sample correlated parameters -------

  vcv<-read.fit(file=OMd,digits=10,cor_ignore = FALSE)
  pnam<-vcv$names[1:vcv$nopar]
  nparams<-length(pnam)

  if(MLEonly){

    message("Only MLE point estimate taken - historical simulations are identical and deterministic")
    samps<-as.data.frame(matrix(rep(vcv$est[1:vcv$nopar],each=nsim),nrow=nsim))
    names(samps)<-pnam

  }else{

    if(file.exists(paste0(OMd,"/nodes.cha"))){

        message(paste0("MCMC output file located (",OMd,"/nodes.cha), posterior samples used to specify uncertainty among simulations"))
        samps<-read.table(paste(OMd,"/nodes.cha",sep=""), sep=" ")
        #if(ncol(samps) != nparams+1) stop(paste0("Error: a different number of parameters (", vcv$nopar,") were estimated than mcmc posteriors are available (", ncol(samps)-1,")"))

        allnams<-c("NAmonkey",pnam[1:(match("lnqE",pnam)-1)],rep("visc",nsubyears*npop),rep("lnqE",OMI@nE),rep("lnqI",OMI@nI),rep("lnqCPUE",OMI@nCPUEq),rep("Fmod",nsubyears*nareas),pnam[match("lnRD1",pnam):length(pnam)])
        #test<-allnams%in%pnam; allnams[!test]
        samps<-samps[,allnams%in%pnam]

        mupar<-apply(samps,2,mean)
        #mupar<-samps[1,]
        #testratio<-(((mupar-vcv$est[1:vcv$nopar])/mupar)^2)^0.5
        #testlev<-2
        #if(sum(testratio>testlev)>0)stop(paste("Error: it seems that one or more parameters are significantly different among MLE and MCMC (mean) estimators: ", pnam[testratio>testlev]))
        keep<-(1:nrow(samps))[samps[,1]!=samps[1,1]]
        samps<-samps[keep,]
        nmcmc<-nrow(samps)

        if(nsim<nmcmc){

          samps<-samps[sample(1:nmcmc,nsim,replace=F),]

        }else{

          message(paste0("Fewer simulations required (",nsim,") than mcmc samples provided (",nmcmc,") - resampling with replacement"))
          samps<-samps[sample(1:nmcmc,nsim,replace=T),]

        }

        names(samps)<-pnam<-allnams[allnams%in%pnam]
        FDYind<-(1:nparams)[grepl("FDY",names(samps))]

    }else{

      message("No MCMC output file located- simulations are identical and deterministic")
      samps<-as.data.frame(matrix(rep(vcv$est[1:vcv$nopar],each=nsim),nrow=nsim))
      names(samps)<-pnam

    }
  }

  # FMod
  Fmodind<-(1:nparams)[names(samps)=="Fmod"]
  Fmodvec<-exp(as.numeric(as.matrix(samps)[,Fmodind]))
  .Object@Fmod<-array(Fmodvec,c(nsim,nareas,nsubyears))

  # FDY
  FDYind<-(1:nparams)[grepl("FDY",names(samps))]
  FDYtemp<-as.numeric(as.matrix(samps)[,FDYind])
  if(mean(FDYtemp,na.rm=T)>0.5){ # the mcmc version is already exponentiated
    FDYvec<-FDYtemp
  }else{
    FDYvec<-exp(FDYtemp) # for the mle version
  }
  #FDYvec<-exp(as.numeric(as.matrix(samps)[,FDYind]))
  .Object@FDY<-array(FDYvec,c(nsim,nyears,nsubyears,nareas))

  # ---- Stock-recruit relationships -------

  if(is.null(Recruitment)){

    Recruitment<-list(

      proyears=array(c(1,Inf),c(1,2)),                           # Future recruitment follows just a single curve per stock
      SRno=array(c(2,4),c(2,1)),                                 # East then west most recent modelled SR
      type=array(OMI@SRtype[c(2,4)],c(2,1)),
      fixpar=array(OMI@SRpar[c(2,4)],c(2,1))                    # East - West - future recruitment follows just a single curve per stock

    )

  }

  .Object@Rectype<-OMI@SRtype          # time period x stock

   opt<-SRplot(out=out,years=OMI@years,type=OMI@SRtype,plot=F,SRminyr=OMI@SRminyr, SRmaxyr=OMI@SRmaxyr)  # general purpose SR info

  # Future recruitment indexing
  nprotypes<-ncol(Recruitment$type)
  yvec<-rep(1,proyears)
  .Object@Recind<-array(rep(1,each=nsim),c(npop,proyears))
  ntypes=ncol(Recruitment$type)

  for(i in 1:ntypes){
    yind<-Recruitment$proyears[i,1]:min(proyears,Recruitment$proyears[i,2])
    .Object@Recind[,yind]<-Recruitment$SRno[,i]
  }

  .Object@Recpars<-array(NA,c(nsim,OMI@nSR,2))   #  1 is steepness or hingepoint, 2 is R0
  .Object@Reccv<-array(NA,c(nsim,OMI@nSR))       #  sigma R (sd of lognormal rec devs)
  .Object@AC<-array(NA,c(nsim,OMI@nSR))          #  lag-1 autocorrelation
  R0adj<-c(1,0) # M3 automatically separates east and west by a scale of 1 for initialization

  for(SRno in 1:OMI@nSR){

    pp<-OMI@SRp[SRno]
    type=OMI@SRtype[SRno]

    if(type=="BH"){

      .Object@Recpars[,SRno,1]<-OMI@SRpar[SRno]
      recind<-(1:nparams)[paste0("lnRD",SRno)==pnam]

      if(MLEonly){
        .Object@Recpars[,SRno,2]<-exp(vcv$est[SRno]+R0adj[pp]) # R0
        recs<-vcv$est[recind]         # correct rec dev sequence
        .Object@Reccv[,SRno]=sd(recs) # by simulation rec cv
        .Object@AC[,SRno]<-stats::acf(recs,plot=F)$acf[2,1,1] # by simulation rec autocorrelation

      }else{
        .Object@Recpars[,SRno,2]<-exp(samps[,SRno]+R0adj[pp])
        recs<-samps[,recind] # correct rec dev sequence
        .Object@Reccv[,SRno]=apply(recs,1,sd) # by simulation rec cv
        .Object@AC[,SRno]<-apply(recs,1,FUN=function(x)stats::acf(x,plot=F)$acf[2,1,1]) # by simulation rec autocorrelation

      }

    } else{ # Hockey stick with fixed hinge point

      # get MLE hingepoint
      .Object@Recpars[,SRno,1]<-opt$par1[SRno] # always this hinge point (comparable to fixed steepness h=0.98)
      recind<-(1:nparams)[paste0("lnRD",SRno)==pnam]

      if(MLEonly){
        .Object@Recpars[,SRno,2]<-exp(vcv$est[SRno])
        recs<-vcv$est[recind]
        .Object@Reccv[,SRno]=sd(recs)
        .Object@AC[,SRno]<-stats::acf(recs,plot=F)$acf[2,1,1]
      }else{
        .Object@Recpars[,SRno,2]<-exp(samps[,SRno])
        recs<-samps[,recind]
        .Object@Reccv[,SRno]=apply(recs,1,sd)
        .Object@AC[,SRno]<-apply(recs,1,FUN=function(x)stats::acf(x,plot=F)$acf[2,1,1])
      }

    }

  }   # end of stocks

  # Historical recruitment --------------------------------------------------------
  .Object@hR0 <- array(NA,c(nsim,npop,nyears))
  .Object@hRecpar <- array(NA,c(nsim,npop,nyears))
  .Object@hRectype <- array(NA,c(npop,nyears))


  # ---- Get recruitment deviations -----
  .Object@Recdevs<-array(0,dim=c(nsim,npop,nyears))
  yblock<-2

  for(SRno in 1:OMI@nSR){

    pp<-OMI@SRp[SRno]
    yind<-OMI@SRminyr[SRno]:OMI@SRmaxyr[SRno]
    recind<-(1:nparams)[paste0("lnRD",SRno)==pnam]
    estind<-rep(recind,each=yblock)[1:length(yind)]

    Ntemp<-apply(as.matrix(samps[,recind]),1,function(x)sum(x^2)/(length(x)-1))

    if(MLEonly){
      .Object@Recdevs[,pp,yind]<-exp(as.matrix(samps[,estind])-Ntemp/2)
      .Object@hR0[,pp,yind]<-exp(samps[,SRno]+R0adj[pp])
      .Object@hRecpar[,pp,yind]<-.Object@Recpars[,SRno,1]
      .Object@hRectype[pp,yind]<-.Object@Rectype[SRno]
    }else{
      .Object@Recdevs[,pp,yind]<-exp(as.matrix(samps[,estind])-Ntemp/2)
      .Object@hR0[,pp,yind]<-exp(samps[,SRno]+R0adj[pp]) # m3 initialization gap of 1
      .Object@hRecpar[,pp,yind]<-.Object@Recpars[,SRno,1]
      .Object@hRectype[pp,yind]<-.Object@Rectype[SRno]

    }

  }

  .Object@Recsubyr<-as.integer(out$spawns)

  .Object@mat<-array(rep(out$mat_age,each=nsim),c(nsim,npop,nages,nyears))

  .Object@ageM<-array(rep(apply((out$mat_age-0.51)^2+rep(1:out$na,each=out$np)/10000,1,which.min),each=nsim),c(nsim,npop))

  # calcmovements

  mov<-array(NA, c(nsim,npop,nages,nsubyears,nareas,nareas)) # spamrr
  movTV<-array(NA, c(nsim,npop,nages,2,nsubyears,nareas,nareas))
  .Object@movIndex<-rep(1, nyears+proyears+4)
  movcalc<-array(-10,c(nsim,npop,nma,nsubyears,nareas,nareas)) # start from very low chance of moving

  visc<-array(unlist(samps[,grep("visc",pnam)]),c(nsim,nsubyears,npop))

  movest_p1_a1<-array(unlist(samps[,grep("movest_p1_a1",pnam)]),c(nsim,nareas-2,nsubyears))
  movest_p1_a2<-array(unlist(samps[,grep("movest_p1_a2",pnam)]),c(nsim,nareas-2,nsubyears))
  movest_p1_a3<-array(unlist(samps[,grep("movest_p1_a3",pnam)]),c(nsim,nareas-2,nsubyears))
  movest_p2_a1<-array(unlist(samps[,grep("movest_p2_a1",pnam)]),c(nsim,nareas-2,nsubyears))
  movest_p2_a2<-array(unlist(samps[,grep("movest_p2_a2",pnam)]),c(nsim,nareas-2,nsubyears))
  movest_p2_a3<-array(unlist(samps[,grep("movest_p2_a3",pnam)]),c(nsim,nareas-2,nsubyears))

  movcalc[,1,,,,nareas]<-0 # anchor for med area eastern stock
  movcalc[,2,,,,1]<-0      # anchor for GOM area western stock

  # Base - age independent around age class 2

  ind_p1a2<-as.matrix(expand.grid(1:nsim,1,2,1:nsubyears,1:nareas,2:6)) # sim pop ageclass subyear area area
  ind_p1a2_<-ind_p1a2[,c(1,6,4)]                             # sim area subyear
  ind_p1a2_[,2]<-ind_p1a2_[,2]-1 # should map to 1:5 estimated areas
  movcalc[ind_p1a2]<-movest_p1_a2[ind_p1a2_] # sim area subyear

  ind_p2a2<-as.matrix(expand.grid(1:nsim,2,2,1:nsubyears,1:nareas,2:6)) # sim pop ageclass subyear area area
  ind_p2a2_<-ind_p2a2[,c(1,6,4)]                             # sim area subyear
  ind_p2a2_[,2]<-ind_p2a2_[,2]-1 # should map to 1:5 estimated areas
  movcalc[ind_p2a2]<-movest_p2_a2[ind_p2a2_] # sim area subyear

  # East pop 1, age class 1 and 3 deviations
  ind_p1a1<-as.matrix(expand.grid(1:nsim,1,1,1:nsubyears,1:nareas,2:6)) # sim pop ageclass subyear area area
  ind_p1a1_<-ind_p1a1[,c(1,6,4)]                             # sim area subyear
  ind_p1a1_[,2]<-ind_p1a1_[,2]-1 # should map to 1:5 estimated areas
  movcalc[ind_p1a1]<-movest_p1_a2[ind_p1a2_]+ movest_p1_a1[ind_p1a1_] # sim area subyear

  ind_p1a3<-as.matrix(expand.grid(1:nsim,1,3,1:nsubyears,1:nareas,2:6)) # sim pop ageclass subyear area area
  ind_p1a3_<-ind_p1a3[,c(1,6,4)]                             # sim area subyear
  ind_p1a3_[,2]<-ind_p1a3_[,2]-1 # should map to 1:5 estimated areas
  movcalc[ind_p1a3]<-movest_p1_a2[ind_p1a2_]+ movest_p1_a3[ind_p1a3_] # sim area subyear

  # west pop 2, age class 1 and 3 deviations
  ind_p2a1<-as.matrix(expand.grid(1:nsim,2,1,1:nsubyears,1:nareas,2:6)) # sim pop ageclass subyear area area
  ind_p2a1_<-ind_p2a1[,c(1,6,4)]                             # sim area subyear
  ind_p2a1_[,2]<-ind_p2a1_[,2]-1 # should map to 1:5 estimated areas
  movcalc[ind_p2a1]<-movest_p2_a2[ind_p2a2_]+ movest_p2_a1[ind_p2a1_] # sim area subyear

  ind_p2a3<-as.matrix(expand.grid(1:nsim,2,3,1:nsubyears,1:nareas,2:6)) # sim pop ageclass subyear area area
  ind_p2a3_<-ind_p2a3[,c(1,6,4)]                             # sim area subyear
  ind_p2a3_[,2]<-ind_p2a3_[,2]-1 # should map to 1:5 estimated areas
  movcalc[ind_p2a3]<-movest_p2_a2[ind_p2a2_]+ movest_p2_a3[ind_p2a3_] # sim area subyear

  for(i in 1:nsim)for(ss in 1:nsubyears)for(rr in 1:nareas)movcalc[i,,,ss,rr,rr]<-movcalc[i,,,ss,rr,rr]+exp(visc[i,ss,])

  for(i in 1:nrow(OMI@MovExc)){
    for(ss in 1:nsim){
      movcalc[ss, OMI@MovExc[i,1],OMI@MovExc[i,2],OMI@MovExc[i,3],OMI@MovExc[i,4],OMI@MovExc[i,5]]<-OMI@MovExc[i,6]
    }
  }

  #movcalc[movcalc>5]<-5
  movcalc<-exp(movcalc)
  movsum<-apply(movcalc,1:5,sum)
  movcalc<-movcalc/array(movsum,dim(movcalc))

  movind<-movindc<-TEG(dim(mov))
  movindc[,3]<-ma[movind[,2:3]]
  mov[movind]<-movcalc[movindc]
  #rm(movcalc)
  movTVind<-TEG(dim(movTV))
  movTV[movTVind]<-mov[movTVind[,c(1:3,5:7)]]

  ## mov(nsim,npop,nages,nsubyears,nareas,nareas)) # spamrr
  #  mov[1,     2,   35,        1,      ,    ]
  #movest_p2_a2[1,,1] #c(nsim,nareas-2,nsubyears)

  #mov[1,2,35,1,,]
  #movest_p2_a2[1,,2]

  .Object@mov<-movTV

  if(debug){

    p<-1; s<-3; a<-9;
    type=1

    movd<-.Object@mov[2,p,a,type,s,,]
    movd

    .Object@mov[2,1,1,type,s,,]

    print("-----")

    .Object@mov[2,1,5,type,s,,]

    print("-----")

    .Object@mov[2,1,9,type,s,,]

    print("-----")

    .Object@mov[2,2,1,type,s,,]

    print("-----")

    .Object@mov[2,2,5,type,s,,]

    print("-----")

    .Object@mov[2,2,9,type,s,,]

  }

    #          nsim,npop,nages,nyears+proyears
  .Object@Wt_age<-tomt(array(out$wt_age,c(dim(out$wt_age),nsim)))
  .Object@iALK<-array(rep(out$iALK,each=nsim),c(nsim,dim(out$iALK)))

  .Object@sel<-array(NA,c(nsim,nfleets,nlen))

  sela<-array(NA,c(nsim,out$nsel,nlen))
  sels<-samps[,grep("selpar",pnam)]
  ml<-out$ml
  LB<-out$LB
  UB<-out$UB

  sc<-1

  for(ss in 1:out$nsel){

    spars<-sels[,sc:(sc+out$seltype[ss]-1)]

    if(out$seltype[ss]==2){

      spar2<-LB[ss]+(UB[ss]-LB[ss])*(0.05+0.85*exp(spars[,2])/(1+exp(spars[,2])));       #// Inflection point (2) as a fraction of largest length I(0.1|0.8)
      spar1<-(UB[ss]-LB[ss])*(0.005+0.11*(exp(spars[,1])/(1+exp(spars[,1]))));  # // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
      ind<-as.matrix(expand.grid(1:nsim,ss,1:nlen))
      sela[ind]<-1/(1+exp((spar2[ind[,1]]-ml[ind[,3]])/spar1[ind[,1]]))
      sela[,ss,]<-sela[,ss,]/apply(sela[,ss,],1,max)

    }else if(out$seltype[ss]==3){

      spar1=LB[ss]+(UB[ss]-LB[ss])*(0.05+0.95*exp(spars[,1])/(1+exp(spars[,1]))) #// Max selectivity bounded between 5 and 95 percent of length range
      spar2=2* spar1*exp(spars[,2])/(1+exp(spars[,2]))      # // Lower sd (divided by 4 just to start at a reasonable guess)
      spar3=(UB[ss]-LB[ss])*exp(spars[,3])         #// Upper sd
      ind<-as.matrix(expand.grid(1:nsim,ss,1:nlen))
      par2or3<-matrix(spar2,nrow=nsim,ncol=nlen)
      cond<-rep(ml,each=nsim)>rep(spar1,nlen) # length is greater than spar1
      par2or3[cond]<-rep(spar3,nlen)[cond]
      sela[ind]<-2^(-(ml[ind[,3]]-spar1[ind[,1]]) / par2or3[ind[,c(1,3)]] *
                      (ml[ind[,3]]-spar1[ind[,1]]) / par2or3[ind[,c(1,3)]] )
      sela[,ss,]<-sela[,ss,]/apply(sela[,ss,],1,max)

    }

    sela[,ss,ml<LB[ss]]<-0
    sela[,ss,ml>UB[ss]]<-0

    sc<-sc+out$seltype[ss]

  }

  if(ploty)matplot(t(sela[1,,]),type='l')

  for(ff in 1:nfleets) .Object@sel[,ff,]<-sela[,out$selind[ff],]

  .Object@qE<-as.matrix(exp(samps[,grep("lnqE",pnam)]))

  maxF<-apply(out$FL,1:4,max)
  E<-maxF/array(rep(exp(out$lnqE),each=prod(dim(maxF)[1:3])),dim(maxF))
  .Object@E<-Etest<-array(NA,c(nsim,nfleets,nyears,nsubyears,nareas))
  ind<-TEG(dim(.Object@E))
  indE<-ind[,c(3,4,5,2)]
  Etest[ind]<-E[indE]
  for(i in 1:nrow(OMI@Eobs)){
    y<-OMI@Eobs[i,1]
    s<-OMI@Eobs[i,2]
    r<-OMI@Eobs[i,3]
    f<-OMI@Eobs[i,4]
    .Object@E[,f,y,s,r]<-OMI@Eobs[i,6]
  }

  .Object@nydist<-as.integer(out$nydist)


  cat("Calculating MSY reference points")
  cat("\n")

  #FML s, m, r, f, l
  FMLs<-array(NA,c(nsim,nsubyears,nareas,nfleets,nlen))
  Find<-TEG(dim(FMLs))
  #Find<-as.matrix(expand.grid(8,1:nsubyears,1:nareas,13,1:nlen))             #some trouble with selectivity
  Eind<-as.matrix(cbind(Find[,c(1,4)],rep(nyears,nrow(Find)),Find[,c(2,3)]))
  FMLs[Find]<-Etest[Eind]*.Object@qE[Find[,c(1,4)]]*.Object@sel[Find[,c(1,4,5)]]

  iALK=out$iALK[,nyears,,]
  N=out$N[,out$ny,,,]
  wt_age=t(out$wt_age[out$ny,,])
  M_age=out$M_age
  mat_age=out$mat_age
  ntypes=cumsum(aggregate(rep(1,OMI@nSR),by=list(OMI@SRp),sum)$x) # the relevant

  R0_arr<-.Object@Recpars[,ntypes,2]
  fixpar_arr<-.Object@Recpars[,ntypes,1]
  SSBpR<-OMI@SSBpR
  SRtypes<-OMI@SRtype[ntypes]
  maxage<-OMI@na

  optMSY_eq<-ABTMSE::optMSY_eq
  MSYCalcs<-ABTMSE::MSYCalcs
  meanFs<-ABTMSE::meanFs
  #meanFs2<-ABTMSE::meanFs2
  MSYMLE_parallel<-ABTMSE::MSYMLE_parallel
  res<-array(NA,c(nsim,np,8)) # MSY, FMSYa, UMSY, BMSY, SSBMSY, BMSY/B0, SSBMSY/SSB0, RMSY/R0

  if(MLEonly){
    MSYmat<-as.matrix(MSYMLE_parallel(1,FMLs, iALK, N, wt_age, M_age, mat_age, R0_arr, fixpar_arr, SSBpR, SRtypes, maxage)) # recruitment curve assumed to be first of the future assumed types
    for(i in 1:nsim)res[i,,]<-MSYmat
  }else{
    for(i in 1:nsim)res[i,,]<-as.matrix(MSYMLE_parallel(i,FMLs, iALK, N, wt_age, M_age, mat_age, R0_arr, fixpar_arr, SSBpR, SRtypes, maxage)) # recruitment curve assumed to be first of the future assumed types
  }

  .Object@MSY<-res[,,1]
  .Object@BMSY<-res[,,4]
  .Object@VBMSY<-res[,,1]/res[,,3]
  .Object@SSBMSY<-res[,,5]
  .Object@UMSY<-res[,,3]
  .Object@FMSYa<-res[,,2]
  .Object@SSBMSY_SSB0<-res[,,7]

  OMIfile<-paste0(OMd,"/OMI")

  if(file.exists(OMIfile)){

    load(OMIfile)
    .Object@area_defs=OMI@area_defs
    .Object@areanams=OMI@areanams
    .Object@Fleets=OMI@Fleets

  }

  .Object@Ibeta_ignore = TRUE                      # hyperstability and hyperdepletion should be ignored?
  .Object@qinc = 0                                 # no catchability increases affecting cpue indices

  surv<-exp(-t(apply(cbind(c(0,0),OMI@Ma[,1:(OMI@na-1)]),1,cumsum)))
  SSBpR<-apply(surv*OMI@Fec,1,sum)+surv[,OMI@na]*exp(-OMI@Ma[,OMI@na])/(1-exp(-OMI@Ma[,OMI@na]))*OMI@Fec[,OMI@na]
  .Object@SSBpR<-array(rep(SSBpR,each=nsim),c(nsim,OMI@np))

  # Index statistics =======================================================
  cat('Generating future index deviations')
  cat("\n")

  CPUEnos<-match(CPUEinds,OMI@CPUEnames)
  Inos<-match(Iinds,OMI@Inames)
  Istats<-array(NA,c(length(CPUEnos)+length(Inos),6)) # index x name, lnq, sd, ac1,lencatLB, lencatUB
  Ires<-array(NA,c(length(CPUEnos)+length(Inos),nyears+proyears)) # MLE residual error

  AC1_int0<-function(res){ # first order autocorrelation with intercept = 0
    nr<-length(res)
    sum(res[2:nr]*res[1:(nr-1)]) / sum(res[1:nr]^2)
  }

  # Calculate statistics for generating future deviations (sd, AC1) and calibrating future data (q)
  k<-0
  for(j in CPUEnos){

    k<-k+1
    ind<-OMI@CPUEobs[,4]==j
    tempdat<-subset(OMI@CPUEobs,ind)
    if(CPUEinds[k]%in%Yrs_override$Name){
      pos<-match(CPUEinds[k]%in%Yrs_override$Name)
      tempdat<-tempdat[tempdat$Year%in%Yrs_override$start[pos]:Yrs_override$end[pos],] # only specified years
    }
    obs<-as.numeric(tempdat[,6])
    pred<-out$CPUEpred_vec[ind]
    yrs<-as.numeric(tempdat[,1]+OMI@years[1]-1)
    yind<-tempdat[,1]
    res<-log(obs)-log(pred)
    Ires[k,yind]<-res
    Istats[k,1]<-CPUEinds[k]
    Istats[k,2]<-out$lnqCPUE[j]
    Istats[k,3]<-round(sd(res),4)
    Istats[k,4]<- max(0,round(AC1_int0(res),4)) # was decided this should be max 0
    if(tempdat[1,7]>0){
     Istats[k,5]<-OMI@Ilencat[1,tempdat[1,7]]
     Istats[k,6]<-OMI@Ilencat[2,tempdat[1,7]]
    }

    if(CPUEinds[k]%in%SD_override$Name){
      pos<-match(CPUEinds[k],SD_override$Name)
      Istats[k,3]<-SD_override$SD[pos]
    }
    if(CPUEinds[k]%in%AC_override$Name){
      pos<-match(CPUEinds[k],AC_override$Name)
      Istats[k,4]<-AC_override$AC[pos]
    }

   }

  for(j in Inos){
    k<-k+1
    ind<-OMI@Iobs[,5]==j
    if(Iinds[k-length(CPUEinds)]%in%Yrs_override$Name){
      pos<-match(Iinds[k-length(CPUEinds)],Yrs_override$Name)
      ind<-OMI@Iobs[,5]==j & OMI@Iobs[,1]%in%(Yrs_override$start[pos]:Yrs_override$end[pos]) # only specified years
    }
    tempdat<-subset(OMI@Iobs,ind)

    obs<-as.numeric(tempdat[,7])
    pred<-out$Ipred_vec[ind]
    yrs<-as.numeric(tempdat[,1]+OMI@years[1]-1)
    yind<-tempdat[,1]
    res<-log(obs)-log(pred)
    Ires[k,yind]<-res
    Istats[k,1]<-Iinds[k-length(CPUEinds)]
    Istats[k,2]<-out$lnqI[j]
    Istats[k,3]<-round(sd(res),4)
    Istats[k,4]<-max(0,round(AC1_int0(res),4))

    if(Iinds[k-length(CPUEinds)]%in%SD_override$Name){
      pos<-match(Iinds[k-length(CPUEinds)],SD_override$Name)
      Istats[k,3]<-SD_override$SD[pos]
    }
    if(Iinds[k-length(CPUEinds)]%in%AC_override$Name){
      pos<-match(Iinds[k-length(CPUEinds)],AC_override$Name)
      Istats[k,4]<-AC_override$AC[pos]
    }
    if(tempdat[1,11]>0){
      Istats[k,5]<-OMI@Ilencat[1,tempdat[1,11]]
      Istats[k,6]<-OMI@Ilencat[2,tempdat[1,11]]
    }

  }
  .Object@Istats=as.data.frame(Istats,stringsAsFactors = F)
  names(.Object@Istats)<-c("Name","lnq","SD","AC")
  .Object@Ires<-Ires

  .Object@Deterministic = Deterministic  # defaults to recruitment process error
  .Object@Obs<-Obs # defaults to Good obs model information
  .Object@IE<-IE     # defaults to maximum harvest rate of 90%


  .Object

})
