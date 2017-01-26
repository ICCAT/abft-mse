# ===========================================================================================================================================================================
# ==== ABT MSE object classes ===============================================================================================================================================
# ===========================================================================================================================================================================

#' An S4 class object that contains all Operating Model Inputs in the format for M3 operating model estimation
#'
#' \describe{
#' \item{Name}{The name of the operating model input}
#' \item{Date}{The date and time the object was created}
#' \item{Author}{Who made the object}
#' \item{Notes}{Anything noteworthy}
#' \item{PrimarySource}{The primary origins of these data}
#' \item{OMfactors}{The various levels and factors of this operating model}
#' \item{years}{The real year range of the operating model}
#' \item{Hyears}{The historical year range of the operating model}
#' \item{areas}{The codes of the various geographical areas of the model}
#' \item{areanams}{Full names of the various geographical areas}
#' \item{area_defs}{A list of the polygons representing each of the areas (for plotting)}
#' \item{Fleets}{A list object containing the definitions of the fleets}
#' \item{L1}{Richards growth curve parameter}
#' \item{L2}{Richards growth curve parameter}
#' \item{K}{Richards growth curve parameter}
#' \item{p}{Richards growth curve parameter}
#' \item{ageM}{Age at 50 percent maturity (not currently specified)}
#' \item{ageMsd}{slope in the maturity ogive (not currently specified)}
#' \item{mat}{maturity at age ogive [stock, age class, year]}
#' \item{lenbins}{vector of length bins that define length classes}
#' \item{nHy}{number of historical years (the period of stock reduction analysis)}
#' \item{ny}{number of years for the statistical catch at length analysis}
#' \item{ns}{number of subyears (e.g. 4 quarters)}
#' \item{np}{number of stocks (e.g. 2, East-West)}
#' \item{na}{number of ages (e.g. 18: 1-17 and 18+)}
#' \item{nr}{number of areas}
#' \item{nf}{number of fleets}
#' \item{nl}{number of length classes}
#' \item{nRPT}{number of recapture time periods (currently unused)}
#' \item{RPTind}{the correct release subyear for a recapture in a given subyear at a particular recapture period (currently unused)}
#' \item{sdur}{the relative size of each subyear duration e.g. c(0.25,0.25,0.25,0.25) for a quarterly model}
#' \item{nydist}{number of years used to initialize the spatial distribution of the movement age classes}
#' \item{mulen}{mean length of individuals in each length class}
#' \item{RDblock}{a vector nyears long indicating which estimated recruitment to use in each year}
#' \item{nRD}{the number of estimated recruitments allocated to year by RDblock}
#' \item{iALK}{inverse age-length key, the conditional probability of a length class given an age class [stock, year, age class, length class]}
#' \item{lwa}{length-weight parameter a W=aL^b(currently unused)}
#' \item{lwb}{length-weight parameter b W=aL^b(currently unused)}
#' \item{len_age}{an array of length-at-age [stock, age, year]}
#' \item{wt_age}{an array of weight-at-age [stock, age, year]}
#' \item{Fec}{an array of spawning biomass-at-age [stock, age, year]}
#' \item{spawns}{the spawning season for each stock (a vector nstocks long)}
#' \item{canspawn}{an array [nstock, nareas] indicating which areas each stock can spawn in (then calculated by the fraction of SSB in each area during spawns)}
#' \item{Ma}{mortality at age [nstock, nages]}
#' \item{nCobs}{the number of catch observations in the statistical catch at length phase}
#' \item{Cobs}{a dataframe of catch observations, one row per observation [year, subyear, area, fleet observation]}
#' \item{nCPUEq}{the number of CPUE indices used in the model fitting}
#' \item{nCPUEobs}{the number of CPUE index observations used in model fitting}
#' \item{CPUEobs}{a data frame of CPUE catch rate observations [year, stock, area, subyear, fleet, index] fitted against vulnerable biomass}
#' \item{nE}{the number of partial F series (typically nfleets)}
#' \item{nEobs}{the number effort observations (partial F's) one for each catch observations}
#' \item{Eobs}{a data frame of effort observations one line per observation [year, subyear, area, fleet, effort]}
#' \item{nCLobs}{the number of length observations}
#' \item{CLobs}{a data frame of length observations [year, subyear]}
#' \item{HCobs}{an array of historical catch observations [year x subyear x area x age]}
#' \item{RAI}{an array of relative abundance - this is pass through data - it is the master index used to derive the standardized effort (partial F data Eobs)}
#' \item{nI}{the number of fishery independent indices (e.g. 2 one spawning biomass survey in the GOM and MED)}
#' \item{nIobs}{the number of fishery independent indices}
#' \item{Iobs}{a data frame of fishery independent relative abundance observations [year, subyear, area, stock, index number, type (biomass/ssb), index]}
#' \item{nPSAT}{the number of electronic tags of known stock of origin}
#' \item{PSAT}{a data frame of electronic tag movements [stock, age, subyear, duration til recapture (subyears), from area, to area, number of tags]}
#' \item{nPSAT2}{the number of electronic tags of unknown stock of origin}
#' \item{PSAT2}{a data frame of electronic tag movements [age, subyear, duration til recapture (subyear), from area, to area, prob(stock1), prob(stock2)]}
#' \item{nTag}{the number of conventional tagging movement observations}
#' \item{Tag}{a data frame of conventional tagging observations (currently unused)}
#' \item{nSOOobs}{the number of stock-of-origin observations}
#' \item{SOOobs}{a data frame of stock-of-origin observations [stock, age, year, subyear, area, number of observations]}
#' \item{nsel}{the number of selectivity types (not necessarily nf due to selectivity mirroring)}
#' \item{seltype}{a vector nsel long specifying the type of selectivity (2 = logistic, 3 = Thompson (potentially domed))}
#' \item{selind}{a vector nf long specifying the correct selectivity for each fleet}
#' \item{ratiolim}{a vector indicating the ratio of precision to inflection point for the logistic selectivities}
#' \item{infleclim}{the min/max for the position of selectivity inflection point relative to maximum length for logistic selectivities}
#' \item{nma}{the number of movement age groups}
#' \item{ma}{a vector na long specfying the correct movement for each age class}
#' \item{nMP}{the number of movement parameters that are estimated}
#' \item{nmovind}{the number of indices for assigning estimated movement parameters to the movement matrices}
#' \item{movind}{a data frame containing the indexing (correct destination) of each movement parameter [stock, subyear, from area, to area]}
#' \item{nmov1}{the number of indices for assiging a zero (not an estimated parameter, rows sum to 1 so the first movement is allocated a zero rather than an estimated parameter)}
#' \item{mov1}{a data frame containing the indexing (correct destination) of a zero value in the movement matrices}
#' \item{movtype}{the type of movement model (1: Gravity model, 2: Markov model)}
#' \item{CobsCV}{a vector nf long, the catch observation error (lognormal sd)}
#' \item{CPUEobsCV}{a vector nCPUEq long, the index observation error (lognormal sd)}
#' \item{IobsCV}{a vector nI long, the fishery independent index observation error (lognormal sd)}
#' \item{RDCV}{the recruitment deviation penalty (sigma R)(lognormal sd)}
#' \item{SSBprior}{a vector np long, an optional prior on current spawning biomass}
#' \item{SSBpriorCV}{the precision of the SSBprior (lognormal sd)}
#' \item{nLHw}{the number of likelihood components (for weighting)}
#' \item{LHw}{a vector nLHw long specifying the relative weight of the various data types 1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB}
#' \item{muR_ini}{a vector np long specifying an initial value of mean absolute recruitment}
#' \item{sel_ini}{a matrix of initial values for selectivity [nf x nl]}
#' \item{selpar_ini}{a matrix of initial value of selectivity parameters [nf x 3]}
#' \item{lnF_ini}{a vector nCobs long specifying initial value for fishing mortality rates}
#' \item{ilnRD_ini}{a vector of initial (year 1 disparities in age comp) recruitment deviations (currently unused)}
#' \item{lnRD_ini}{a vector ny long of recruitment recruitment deviations}
#' \item{mov_ini}{an array of initial movement [stock x age x subyear x area from x area to]}
#' \item{qCPUE_ini}{a vector nCPUEq long of initial catchability estimates for CPUE indices}
#' \item{qI_ini}{a vector nI long specifying initial value for catchabilities of fishery independent indices}
#' \item{D_ini}{a vector np long specifying initial (comparative) depletion (SSB current / SSB unfished) estimates}
#' \item{ComplexRD}{an integer specifying whether recruitments can be blocked or estiamted for each year (currently unused)}
#' \item{ComplexF}{an integer specifying whether an F should be estimated for each catch observation rather than F=qE (currently unused)}
#' \item{nF}{the number estimated F parameters (usually =nCobs)}
#' \item{nMPind}{the number of fishery indices used for informing management procedures in subsequent MSE analysis (currently unused)}
#' \item{MPind}{a data frame of fishery indices used for informing management procedures in subsequent MSE analysis (currently unused)}
#' \item{debug}{a logical (0 or 1) value determing whether the model should be run for a single iteration to check for errors}
#' \item{verbose}{a logical (0 or 1) value determing whether more complete information should be provided in each iteration of the model estimation}
#' \item{datacheck}{a unique number for checking that data were read into the model correctly}
#' \item{CPUEnames}{a character string nCPUEq long, recording the names of CPUE indices}
#' }
setClass("OMI",representation(
  # Description
  Name="character",Date="character",Author="character", Notes="character",PrimarySource="character",
  OMfactors="list",
  # other slots --------------------------------------------------------------------------------------------------
  years="numeric", Hyears="numeric", areas='character',
  areanams='character', area_defs='list', Fleets='list',
  L1='numeric',L2='numeric',K='numeric',p='numeric',
  ageM='numeric',ageMsd='numeric',mat='array',lenbins='numeric',
  # Inputs to M3 (in order) --------------------------------------------------------------------------------------
  nHy='integer', ny='integer', ns='integer',np='integer',na='integer',nr='integer',nf='integer',
  nl='integer',nRPT='integer',RPTind='matrix',sdur='numeric',nydist='integer',#nZeq='integer',nyeq='integer',
  mulen='numeric',RDblock='numeric',nRD='integer',
  iALK='array',#p y a l (trans)
  lwa='numeric', lwb='numeric',
  len_age='array', wt_age='array', # p a y (trans)
  Fec='array',#steep='numeric', # p a (wt*mat) (trans)
  spawns='numeric',canspawn='matrix', # p r (trans)
  Ma='array', # p a (trans)
  nCobs='integer',  Cobs='matrix', # nCobs x 5 (y s r f Cobs) (trans)
  nCPUEq='integer',  nCPUEobs='integer',  CPUEobs='matrix', # nCPUEobs x 6 (y s r q f index) (trans)
  nE='integer',                  # number of partial f series (basically nfleets but coding seperatly to later account for catchability mirroring)
  nEobs='integer',Eobs='matrix', # nE x 5 (y s r f partial F)
  nCLobs='integer',CLobs='matrix', # nCLobs x 6 (y s r f l N) (trans)
  HCobs='array', # (y x s, x r x a) (trans)
  RAI='array', # r s y (not trans)
  nI='integer',nIobs='integer',Iobs='matrix', # nI x 7 (y s spawn_area pp index(=pp if SSB) type(biomass/ssb) index) (trans)
  nPSAT='integer',PSAT='matrix', # nPSAT x 7 (p a s t fr tr N) (trans)
  nPSAT2='integer',PSAT2='matrix', # nPSAT2 x 5+(np) (a s t fr tr SOOp1 SOOp2) (trans)
  nTag='integer',Tag='matrix', # nTag x 10 (y s r a - y s r f a N)
  nSOOobs ='integer',SOOobs='matrix',# nSOOobs x 6 (p a y s r N) (trans)
  nsel='integer',seltype='numeric', selind='numeric', # nf
  ratiolim='numeric', infleclim='numeric',
  nma='integer',ma='numeric', # na
  nMP='integer',nmovind='integer',movind='matrix',# nmovind x 4 (p s r r)
  nmov1='integer',mov1='matrix', # nmov1 x 4 (p s r r)
  movtype='integer',
  CobsCV='numeric', CPUEobsCV='numeric',# fleets,
  IobsCV='numeric',# nI (np if SSB)
  RDCV='numeric',
  SSBprior='numeric',SSBCV='numeric',
  nLHw='integer',  LHw='numeric', # 12: 1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB
  muR_ini='numeric',# np
  sel_ini='matrix',# f l (trans)
  selpar_ini='matrix', # f 3 (trans)
  lnF_ini='numeric', # nCobs
  ilnRD_ini='matrix', # na-1
  lnRD_ini='matrix',# ny
  mov_ini='array', # p a s r r (trans)
  qCPUE_ini='numeric', # nf
  qI_ini='numeric',# nI
  D_ini='numeric',# np
  complexRD='integer',
  complexF='integer',
  nF='integer',# nCobs
  nMPind='integer',# number of MP index observations
  MPind="data.frame",# the MP indices
  debug='integer',verbose='integer',datacheck='integer',

  # Misc
  CPUEnames='character'

))

#' An S4 class object that contains all Operating Model Inputs in the format for M3 operating model estimation
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
#' \item{mov}{a very large array storing simulated movement by stock [sim x stock x age x subyear x from area x to area]}
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
#' \item{mat}{array of maturity [sim, stock, age, year]}
#' \item{M}{array of natural mortality rate [sim, stock, age, year]}
#' \item{Recdevs}{of historical recruitment deviations [sim, stock, year]}
#' \item{R0}{vector nstock long of unfished recruitment (currently unused - recpars)}
#' \item{muR}{a vector nstock long of mean recruitment}
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
              Rectype="array",h="array",recgrad="array",                 # Stock-recruitment relationship type, steepness, underlying gradient per cent yr-1
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
              Size_area="array",mov="array",#Mmov="array",               # Size of regions, Markov movement matrix for all fish and mature fish #movvar="matrix",movsd="array",movgrad="array",             # Inter-simulation variability in movement, interannual-variability in movement, gradient changes in area gravity weights#excl="array",                                              # Exclusion matrix [0,1] depending on whether the stock can go there
              nfleets="integer",                                         # Number of fleets,#L05="array",VmaxL="array", LFS="array",                    # Length at 5per cent vulnerability, vulnerability of largest fish, length at full selection
              iALK="array",                                              # Inverse Age-Length-Key#Fsd="array",Fgrad="array", Frat="array",                   # Interannual variability in F, Final gradient in F yr-1Area_names="character", Area_defs="list",                  # Area definitions (polygons)Spat_targ="array",                                         # Spatial targetting parameter F =prop= V^Spat_targ
              hZ="array", HCobs="array",   Cobs="array",

              # Simulation data -------
              E="array",#dFfinal="array",
              qE="array",
              qI="array",qCPUE="array",            # Catchability of effort, fishery ind indices and fishery CPUE indices
              sel="array",selpars="array",
              mat="array",M="array",
              Recdevs="array", R0="array",  muR="array",      # Recruitment deviations, unfished recruitment, mean historical recruitment              #FM="array",,Z="array",                 # Fishing, natural and total instantaneous mortality rate.#B="array",SSB="array",NSB="array",              # Biomass, spawning stock biomass #N="array",SSN="array",NSN="array",              # Numbers, spawning stock numbers #C="array",CAA="array",                          # Catches taken, catch at age taken
              MSY="array",BMSY="array", VBMSY="array",        # Maximum sustainable yield, biomass at maximum sustainable yield
              SSBMSY="array",
              FMSY="array",UMSY="array",FMSYa="array",         # Fishing mortality rate at maximum sustainable yield
              SSBMSY_SSB0="array",#IMSY="numeric",                                  # Relative abundance index at maximum sustainable yield#Linf="array",K="array", #Idist="array",                                   # for plotting OM unfished spatial dist#targpop="numeric",                               # What populations are to be used in MSY / depletion optimization#nZeq="integer",                                 # The number of initial years to calculation equilibrium F
              nydist="integer",                                # The number of years (iterations) taken to find equilibrium spatial distribution#nyeq="integer",                                 # The number of years (iterations) taken to find equilibrium F
              Snames="character",                              # Name of the stocks
              area_defs="list",                                # Area definitions list (copied from OMI)
              areanams="character",                            # Names of areas        (copied from OMI)
              seed="numeric"                                   # Random seed from which this object was made
              ))

setMethod("initialize", "OM", function(.Object,OMd="C:/M3",nsim=32,proyears=30,seed=1,
                                      Recruitment=NULL,Snames=c("East","West"),ploty=F){
  # .Object})
  # .Object<-new('OM',OMd="C:/M3")

  .Object@Snames=Snames

  if(!dir.exists(OMd))stop(paste('You specified a directory',OMd,'that does not exist or an object',OMd, 'that does not exist'))
  if(!file.exists(paste0(OMd,"/M3.dat")))stop(paste('Could not build operating model: M3 output file ',paste0(OMd,"/M3.dat")))

  cat(paste("Loading operating model input object :",OMd))
  cat("\n")#.Object@targpop=targpop
  load(file=paste0(OMd,"/OMI"))

  cat(paste("Reading operating model fit data from directory:",OMd))
  cat("\n")#.Object@targpop=targpop
  out<-M3read(OMDir=OMd)

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

  # ---- Stock-recruit relationships -------

  if(is.null(Recruitment)){

    Recruitment<-list(

      proyears=array(c(1,Inf),c(1,2)),                         # Future recruitment follows just a single curve per stock
      years=array(c(-Inf,Inf),c(2,np,1)),                      # historical time period,
      type=array('BH',c(1,np)),                                # future recuitment follows just a single curve per stock
      h=array(NA,c(1,np)),                                     # future recruitment follows just a single curve per stock
      prob=array(1,c(1,np))                                    # Probability of flipping recruitment scenarios

    )

  }

  .Object@Rectype<-Recruitment$type                                            # time period x stock
  ntypes<-nrow(Recruitment$type)
  yvec<-rep(1,proyears)
  if(ntypes>1)for(i in 2:ntypes)yvec[Recruitment$proyears[i,1]:min(Recruitment$proyears[i,2],proyears)]<-i
  .Object@Recind<-array(rep(yvec,each=nsim),c(nsim,proyears))
  .Object@Recpars<-array(NA,c(nsim,np,ntypes,2))
  .Object@Reccv<-array(NA,c(nsim,np))         # sigma R (sd of lognormal rec devs)
  .Object@AC<-array(NA,c(nsim,np))

  for(pp in 1:np){
    for(tt in 1:ntypes){
      type=strsplit(Recruitment$type[tt,pp],split="_")[[1]][1]
      just_R0<-grepl("R0",Recruitment$type[tt,pp])
      opt<-SRopt(out,plot=ploty,quiet=F,years=Recruitment$years[,pp,tt],
                 type=type,just_R0=just_R0,h=Recruitment$fixpar[tt,pp])

      if(just_R0){ # only second parameter is subject to error
        .Object@Recpars[,pp,tt,1]<-Recruitment$fixpar[tt,pp] # either logit h or logit inflection
        .Object@Recpars[,pp,tt,2]<-exp(rnorm(nsim,opt$lnR0[pp],opt$VC[[pp]]^0.5)) # always lnR0
      }else{
        posdef<-(sum(eigen(opt$VC[[pp]])$values>0)==2)
        if(posdef).Object@Recpars[,pp,tt,]<-rmvnorm(nsim,mean=c(opt$par1[pp],opt$lnR0[pp]),sigma=opt$VC[[pp]])
        if(!posdef).Object@Recpars[,pp,tt,]<-rnorm(nsim*2,mean=c(rep(opt$par1[pp],nsim),rep(opt$lnR0[pp],nsim)),0.1) # default 10% CV in the odd case of a non-positive definite hessian
        if(type=="BH").Object@Recpars[,pp,tt,1]<-0.2+1/(1+exp(-.Object@Recpars[,pp,tt,1]))*0.8
        if(type=="HS").Object@Recpars[,pp,tt,1]<-1/(1+exp(-.Object@Recpars[,pp,tt,1]))
        .Object@Recpars[,pp,tt,2]<-exp(.Object@Recpars[,pp,tt,2])
      }

      if(tt==1){ # some properties of recruitment deviations
        recpred<-opt$resid[[pp]]$rec-opt$resid[[pp]]$devs
        .Object@Reccv[,pp]<-sd(opt$resid[[pp]]$devs/recpred)         # sigma R (sd of lognormal rec devs)
        .Object@AC[,pp]<-acf(opt$resid[[pp]]$devs,plot=ploty)$acf[2,1,1] # lag 1 autocorrelation
      }

    } # end of types per stock
  }   # end of stocks


  # ---- Get covariance matrix and sample correlated parameters -------

  vcv<-read.fit(paste(OMd))
  #if(length(vcv$cov)==1){ # no valid variance-covariance matrix from which to sample parameter valuecat(paste('You specified a directory',OMd,'that does not contain a valid M3.cor file. The M3.cor file is generated when the convergence
    #            criterion of a positive definite hessian, is met.'))
    #cat("\n")
  samps<-as.data.frame(matrix(rep(vcv$est[1:vcv$nopar],each=nsim)+rnorm(vcv$nopar*nsim,0,0.1),nrow=nsim))
  names(samps)<-vcv$names[1:vcv$nopar]
  #}else{
   # samps<-as.data.frame(mvrnorm(nsim,vcv$est,vcv$cov))
   # names(samps)<-vcv$names
  #}


  # you were here and have no idea what this is all about: !
  # ---- Get recruitment deviations -----
  Rdind<-grep("lnRD",vcv$names)
  nrest<-length(Rdind)/npop
  Rdest<-t(exp(matrix(vcv$est[Rdind],nrow=nrest)))
  Rdest<-Rdest/array(apply(Rdest,1,mean),dim(Rdest))
  test<-samps[,Rdind]
  Rdsamps<-exp(array(as.matrix(samps[,Rdind]),c(nsim,nrest,npop)))#nsim nrest
  Rdmu<-apply(Rdsamps,c(1,3),mean)
  ind<-TEG(dim(Rdsamps))
  indmu<-ind[,c(1,3)]
  Rdsamps[ind]<-Rdsamps[ind]/Rdmu[indmu]

  #out$RDblock<-rep(1:11,each=5)
  ind<-TEG(c(nsim,npop,nyears))
  indest<-cbind(ind[,1],out$RDblock[ind[,3]],ind[,2])

  .Object@Recdevs<-array(NA,dim=c(nsim,npop,nyears))
  .Object@Recdevs[ind]<-Rdsamps[indest]
  .Object@Reccv<-apply(Rdsamps,c(1,3),sd)*sqrt(nyears/nrest) # convert from std err to st dev
  .Object@Recsubyr<-as.integer(out$spawns)

  .Object@muR<-as.matrix(exp(samps[,grep("lnmuR",vcv$names)]))
  .Object@D<-array(NA,dim(.Object@R0)) # don't need this for empirical fit
  .Object@mat<-array(rep(out$mat_age,each=nsim),c(nsim,npop,nages,nyears))

  .Object@ageM<-array(rep(apply((out$mat_age-0.51)^2+rep(1:out$na,each=out$np)/10000,1,which.min),each=nsim),c(nsim,npop))

  # calcmovements
  movs<-samps[,grep("movest",vcv$names)]
  # movs[1,]<-vcv$est[grep("movest",vcv$names)]
  # used in MSE with indexing spaymrr but think we dropped age varying movement so spamrr
  # send to get MSYrefs as mov=.Object@mov[ss,,,,,]
  # goes to domov2 as mov[,,m,,]
  # intepretted by domov2 as parr
  # points to OM@mov[sim,,,m,,]

  mov<-array(NA, c(nsim,npop,nages,nsubyears,nareas,nareas)) # spamrr
  movcalc<-array(-10,c(nsim,npop,nma,nsubyears,nareas,nareas))
  for(ss in 1:nsim){

    if(out$movtype==1){   # gravity model
      # areas that gravities are fixed to zero for (gravities are applied by row so column 4 is now 'to area')
      # p ma s tr
      ind<-cbind(rep(ss,nareas*out$nmov1),  # sim
                 rep(out$mov1[,1],each=nareas), # pop
                 rep(out$mov1[,2],each=nareas), # age class
                 rep(out$mov1[,3],each=nareas), # subyear
                 rep(1:nareas,out$nmov1),       # from area
                 rep(out$mov1[,4],each=nareas)) # to area
      movcalc[ind]<-0

      refind<-cbind(rep(ss,out$nmovind*nareas),rep((1:out$nmovind),each=nareas)+npop*nsubyears*nma)
      ind<-cbind(rep(ss,nareas*out$nmovind),
                 rep(out$movind[,1],each=nareas),
                 rep(out$movind[,2],each=nareas),
                 rep(out$movind[,3],each=nareas),
                 rep(1:nareas,out$nmovind),
                 rep(out$movind[,4],each=nareas))
      movcalc[ind]<-movs[refind]

      ind<-TEG(c(nareas,nsubyears,nma,npop))[,c(4:1,1)]
      ind0<-TEG(c(nsubyears,nma,npop))[,3:1]
      refno<-tomt(array(1:(nma*nsubyears*npop),c(nsubyears,nma,npop)))

      refind<-cbind(rep(ss,nrow(ind)),refno[ind[,1:3]])
      ind<-cbind(rep(ss,nrow(ind)),ind)
      movcalc[ind]<-movcalc[ind]+exp(movs[refind]/12)


    }else{            # full markov matrix

      ind<-cbind(rep(ss,out$nmov1), out$mov1[,1:4])
      movcalc[ind]<-0
      ind<-cbind(rep(ss,out$nmovind),out$movind[,1:4])
      movcalc[ind]<-movs[ss,]

    }

  }    # loop over sims

  movcalc[movcalc>5]<-5 # set some kind of upper bound for sampled movement parameters (or else get Infs)
   #nsim,npop,nsubyears,nareas,nareas
  movcalc<-exp(movcalc)
  movsum<-apply(movcalc,1:5,sum)
  movcalc<-movcalc/array(movsum,dim(movcalc))
  #nsim,npop,nages,nsubyears,nareas,nareas)
  movind<-movindc<-TEG(dim(mov))
  movindc[,3]<-ma[movind[,2:3]]
  mov[movind]<-movcalc[movindc]
  .Object@mov<-mov

  #          nsim,npop,nages,nyears+proyears
  .Object@Wt_age<-tomt(array(out$wt_age,c(dim(out$wt_age),nsim)))
  .Object@iALK<-array(rep(out$iALK,each=nsim),c(nsim,dim(out$iALK)))

  .Object@sel<-array(NA,c(nsim,nfleets,nlen))

  sela<-array(NA,c(nsim,out$nsel,nlen))
  sels<-samps[,grep("selpar",vcv$names)]
  ml<-out$ml

  sc<-1

  for(ss in 1:out$nsel){

    spars<-sels[,sc:(sc+out$seltype[ss]-1)]

    if(out$seltype[ss]==2){
      spar2<-ml[nlen]*(0.2+0.5*exp(spars[,2])/(1+exp(spars[,2])));       #// Inflection point (2) as a fraction of largest length I(0.1|0.8)
      spar1<-ml[nlen]*(0.02+0.08*(exp(spars[,1])/(1+exp(spars[,1]))));  # // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
      ind<-as.matrix(expand.grid(1:nsim,ss,1:nlen))
      sela[ind]<-1/(1+exp((spar2[ind[,1]]-ml[ind[,3]])/spar1[ind[,1]]))
      sela[,ss,]<-sela[,ss,]/apply(sela[,ss,],1,max)
    }else if(out$seltype[ss]==3){
      spar1<-0.01+((spars[,1]+2)/5)^3;                     # // Dome-shape parameter I(0|0.2)
      spar2<-0.01+0.01*(spars[,2]+2)^3;               #// Precision as the ratio of the inflection point I(0.1|0.7)
      spar3<-ml[nlen]*(0.1+0.8*(spars[,3]+2)/4);   #// Inflection point as a fraction of largest length I(0.15|0.9)
      ind<-as.matrix(expand.grid(1:nsim,ss,1:nlen))
      inds<-ind[,1]
      indl<-ind[,3]
      sela[ind]<-(1/(1-spar1[inds]))*((1-spar1[inds])/spar1[inds])^spar1[inds] * exp(spar2[inds]*spar1[inds]*(spar3[inds]-ml[indl]))/(1+exp(spar2[inds]*(spar3[inds]-ml[indl])));
      sela[,ss,]<-sela[,ss,]/apply(sela[,ss,],1,max)
    }
    sc<-sc+out$seltype[ss]
  }

  if(ploty)matplot(t(sela[1,,]),type='l')

  for(ff in 1:nfleets).Object@sel[,ff,]<-sela[,out$selind[ff],]

  .Object@qE<-as.matrix(exp(samps[,grep("lnqE",vcv$names)]))
  .Object@qI<-as.matrix(exp(samps[,grep("lnqI",vcv$names)]))
  .Object@qCPUE<-as.matrix(exp(samps[,grep("lnqCPUE",vcv$names)]))

  maxF<-apply(out$FL,1:4,max)
  E<-maxF/array(rep(exp(out$lnqE),each=prod(dim(maxF)[1:3])),dim(maxF))
  .Object@E<-array(NA,c(nsim,nfleets,nyears,nsubyears,nareas))
  ind<-TEG(dim(.Object@E))
  indE<-ind[,c(3,4,5,2)]
  .Object@E[ind]<-E[indE]
  .Object@nydist<-as.integer(out$nydist)


  cat("Calculating MSY reference points")
  cat("\n")

  res<-array(NA,c(nsim,np,8)) # MSY, FMSYa, UMSY, BMSY, SSBMSY, BMSY/B0, SSBMSY/SSB0, RMSY/R0

  #FML s, m, r, f, l
  FMLs<-array(NA,c(nsim,nsubyears,nareas,nfleets,nlen))
  Find<-TEG(dim(FMLs))
  #Find<-as.matrix(expand.grid(8,1:nsubyears,1:nareas,13,1:nlen))             #some trouble with selectivity
  Eind<-as.matrix(cbind(Find[,c(1,4)],rep(nyears,nrow(Find)),Find[,c(2,3)]))
  FMLs[Find]<-.Object@E[Eind]*.Object@qE[Find[,c(1,4)]]*.Object@sel[Find[,c(1,4,5)]]

  for(i in 1:nsim){

    res[i,,]<-as.matrix(MSY_FAST(FML=FMLs[i,,,,], iALK=out$iALK[,nyears,,], N=out$N[,out$ny,,,],
                wt_age=t(out$wt_age[out$ny,,]), M_age=out$M_age, mat_age=out$mat_age,
                R0s=.Object@Recpars[i,,1,2], fixpars=.Object@Recpars[i,,1,1],
                SRtypes=.Object@Rectype[1,])) # recruitment curve assumed to be first of the future assumed types

  }


  .Object@MSY<-res[,,1]#cbind(MSYrefs1[,1],MSYrefs2[,1])
  .Object@BMSY<-res[,,4]#cbind(MSYrefs1[,2],MSYrefs2[,2])
  .Object@VBMSY<-array(NA,c(nsim,npop)) # not calculated
  .Object@SSBMSY<-res[,,5]#cbind(MSYrefs1[,4],MSYrefs2[,4])
  .Object@UMSY<-res[,,3]#cbind(MSYrefs1[,5],MSYrefs2[,5])
  .Object@FMSYa<-res[,,2]#cbind(MSYrefs1[,6],MSYrefs2[,6])
  .Object@SSBMSY_SSB0<-res[,,7]#cbind(MSYrefs1[,7],MSYrefs2[,7])

  OMIfile<-paste0(OMd,"/OMI")

  if(file.exists(OMIfile)){

    load(OMIfile)
    .Object@area_defs=OMI@area_defs
    .Object@areanams=OMI@areanams

  }

  .Object

})

#' An S4 class object that contains all observation model
#'
#' \describe{
#' \item{Name}{The name of the observation model object}
#' \item{Ccv}{a 2 item vector specifying the range in catch observation error as lognormal sd (upper lower)}
#' \item{Cbcv}{a 2 item vector specifying the maximum extend of catch bias as a lognormal sd}
#' \item{nCAAobs}{a 2 item vector specifying the range in the number of annual catch-at-age observations}
#' \item{nCALobs}{a 2 item vector specifying the range in the number of annual catch-at-length observations}
#' \item{Lcv}{the variance in lengths by age class log normal sd (currently unused)}
#' \item{Ibeta}{a 2 item vector representing the range of hyperstability in indices (currently unused)}
#' \item{Icv}{a 2 item vector representing bounds the degree of uncertainty (noise) in relative abundance indices as a lognormal sd (upper lower)(currently unused)}
#' \item{Mbcv}{a 2 item vector specifying the maximum extend of bias in natural mortality rate as a lognormal sd}
#' \item{Kbcv}{a 2 item vector specifying the maximum extend of bias in growth rate K as a lognormal sd}
#' \item{t0bcv}{a 2 item vector specifying the maximum extend of bias in growth parameter t0 as a normal cv}
#' \item{Linfbcv}{a 2 item vector specifying the maximum extend of bias in maximum size Linf as a lognormal sd}
#' \item{LFCbcv}{a 2 item vector specifying the maximum extend of bias in the length at first capture as a lognormal sd}
#' \item{LFSbcv}{a 2 item vector specifying the maximum extend of bias in the length at full selection as a lognormal sd}
#' \item{FMSYbcv}{a 2 item vector specifying the maximum extend of bias in simulated FMSY as a lognormal sd}
#' \item{FMSY_Mbcv}{a 2 item vector specifying the maximum extend of bias in simulated FMSY / natural mortality rate as a lognormal sd}
#' \item{BMSY_B0bcv}{a 2 item vector specifying the maximum extend of bias in simulated ratio of BMSY relative to unfished as a lognormal sd}
#' \item{ageMbcv}{a 2 item vector specifying the maximum extend of bias in simulated FMSY / natural mortality rate as a lognormal sd}
#' \item{Dbcv}{a 2 item vector specifying the maximum extend of bias in simulated depletion (current SSB relative to unfished) as a lognormal sd}
#' \item{Dcv}{a 2 item vector specifying the range in interannual variability in depletion (current SSB relative to unfished) lognormal sd}
#' \item{Btbcv}{a 2 item vector specifying the maximum extend of bias in simulated current biomass as a lognormal sd}
#' \item{Btcv}{a 2 item vector specifying the range in interannual variability in current biomass as a lognormal sd}
#' \item{Ftbcv}{a 2 item vector specifying the maximum extend of bias in simulated current exploitation rate as a lognormal sd}
#' \item{Ftcv}{a 2 item vector specifying the range in interannual variability in current exploitation rate as a lognormal sd}
#' \item{hbcv}{a 2 item vector specifying the maximum extend of bias in simulated steepness as a lognormal sd}
#' \item{Recbcv}{a 2 item vector specifying the maximum extend of bias in simulated recent recruitment strength rate as a lognormal sd}
#' \item{IMSYbcv}{a 2 item vector specifying the maximum extend of bias in simulated target relative abundance index as a lognormal sd}
#' \item{MSYbcv}{a 2 item vector specifying the maximum extend of bias in simulated MSY (a target catch rate) as a lognormal sd}
#' \item{BMSYbcv}{a 2 item vector specifying the maximum extend of bias in simulated BMSY (a target biomass level) as a lognormal sd}
#' \item{MPind}{a data frame containing the indices that may be used by MPs in closed loop simulation [year, index number, index name, index, CV, stock, type (1: vulnerable biomass, 2: ssb), source, applies to model areas]}
#' \item{MPind_stats}{a list containing the statistical properties of model fits (currently unused)}
#' }
setClass("Obs",representation(Name="character",
               Ccv="numeric",Cbcv="numeric",                                  # Observation error and bias in total annual catches
               nCAAobs="numeric",nCALobs="numeric", Lcv="numeric",            # Number of annual catch at age (CAA) and catch-at-length (CAL) observations
               Ibeta="numeric", Icv="numeric",                                # Hyperstability parameter I^beta and observation error in relative abundance indices
               Mbcv="numeric",                                                # Bias in observation of natural mortality rate
               Kbcv="numeric",t0bcv="numeric",Linfbcv="numeric",              # Bias in estimation of growth parameters
               LFCbcv="numeric", LFSbcv="numeric",                            # Bias in observation of length at first capture (LFC) and length at full selection (LFS)
               FMSYbcv="numeric",FMSY_Mbcv="numeric",BMSY_B0bcv="numeric",    # Bias in observaton of FMSY, ratio of FMSY/M, BMSY/B0
               ageMbcv="numeric",                                             # Bias in observation of age at 50per cent maturity and
               Dbcv="numeric",Dcv="numeric",                                  # Bias and imprecision in observation of current stock depletion
               Btbcv="numeric",Btcv="numeric",                                # Bias and imprecision in observation of current stock biomass
               Ftbcv="numeric",Ftcv="numeric",                                # Bias and imprecision in observation of current fishing mortality rate
               hbcv="numeric",                                                # Bias in observation of steepness
               Recbcv="numeric",IMSYbcv="numeric",                            # Bias in observation of recent recrutiment, target CPUE (CPUE @ MSY)
               MSYbcv="numeric",BMSYbcv="numeric",                            # Bias in observation of target catch and biomass (MSY and BMSY)
               MPind="data.frame",MPind_stats="list"                          # Indices that may be used in MPs
))


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

  N<-SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  #SSBA<-array(NA,c(nsim,npop,allyears))
  FD<-array(NA,c(nsim,nfleets,allyears,nsubyears,nareas))              # Fishing distribution
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
  RFL[indL]<-OM@qE[indL[,c(1,2)]]*nsubyears*OM@sel[indL[,1:3]]*OM@E[indL[,c(1,2,4,5,6)]]
  iALK<-OM@iALK

  aseltemp<-array(NA,c(nsim,npop,nfleets,nages,nlen))
  aselind<-TEG(dim(aseltemp))
  iALKs<-OM@iALK[,,1,,] # time invariant
  aseltemp[aselind]<-OM@sel[aselind[,c(1,3,5)]]*iALKs[aselind[,c(1,2,4,5)]]
  asel<-apply(aseltemp,1:4,sum)

  FM<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  Find<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nfleets))
  FM[Find]<-OM@qE[Find[,c(1,7)]]*nsubyears*asel[Find[,c(1,2,7,3)]]*OM@E[Find[,c(1,7,4,5,6)]]

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
  N[indN]=Rec1[indN[,1:2]]*surv[indN[,1:3]]*stemp[indN[,c(1,2,5,6)]]
  N[,,nages,1,,]<-N[,,nages,1,,]+N[,,nages,1,,]*array(exp(-M[,,nages,1])/(1-exp(-M[,,nages,1])),c(nsim,npop,nsubyears,nareas)) # plus group

  SSB[,,,1,,]<-N[,,,1,,]*rep(Wt_age[,,,1],nareas*nsubyears)*rep(mat[,,,1],nareas*nsubyears)

  sdur<-1/nsubyears
  canspawn<-array(rep(c(0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0),each=nsim),c(nsim,npop,nareas))
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
        N[,,1,1,m,]<-spawnr*array(Rec1,dim(spawnr)) # Initial recruitment distributed by viable spawning biomass

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

  #test
  #OM@hZ[1,90,2,,1]
  #hZ[1,1,,90,2,1]

  #bR<-log(5*h)/(0.8*SSB0)                                      # Ricker SR params
  #aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params

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
  procmu <- -0.5*(proccv)^2
  Pe<-array(exp(rnorm(nsim*npop*allyears,procmu,proccv)),c(nsim,npop,allyears))

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
                   "Bt"=apply(N[,,,y-1,nsubyears,AA]*
                                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nA)),1,sum)*.Object@Bterr[,(y-1)],#apply(VBA[,,,(y-1),4,],1,sum)*.Object@Bterr[,(y-1)], # you were here
                   "MSY"=OM@MSY[,AS]*.Object@MSYb,
                   "BMSY"=OM@BMSY[,AS]*.Object@BMSYb,
                   "UMSY"=OM@UMSY[,AS]*.Object@FMSYb,
                   "a"=rep(OM@a,nsim),
                   "b"=rep(OM@b,nsim),
                   "nages"=OM@nages,
                   "ageM"=OM@ageM[,AS]*.Object@ageMb,
                   "Mat"=OM@mat[,AS,,nyears],
                   "Bt_PI"=apply(N[,,,y-1,nsubyears,]*
                                   array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),1,sum),
                   "UMSY_PI"=apply(array(OM@UMSY[,AS],c(nsim,AS)),1,mean),
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


# ===========================================================================================================================================================================
# ==== ABT MSE plotting Methods ======================================================================================================================================================
# ===========================================================================================================================================================================

# Plot spatial distribution implied by the OM model
setMethod("plot", signature(x = "OM"),function(x,outfile=NA){

  .Object<-x
  Istore<-array(NA,c(.Object@npop,.Object@nma,.Object@nsubyears,.Object@nareas))
  Idist<-array(1/.Object@nareas,c(.Object@npop,.Object@nages,.Object@nareas))
  ind<-as.matrix(expand.grid(1:.Object@npop,1:.Object@nages,1:.Object@nareas))
  mref<-c(2:.Object@nsubyears,1)
  # OM@mov s p a m r r

  for(i in 1:100){for(m in 1:.Object@nsubyears){
    Idist<-domov2(Idist,.Object@mov[1,,,m,,])
    if(i==100){
      for(p in 1:.Object@npop){
        for(j in 1:.Object@nma){
          ref<-match(j,.Object@ma[,p])
          Istore[p,j,mref[m],]<-Idist[p,ref,]
        }
      }
    }
  }}

  #pid<-3
  #nplots<-ceiling(.Object@npop/2)
  # if(nplots>2)pid<-(((1:nplots)*2)-1)[2:nplots]
  # if(!is.na(outfile))jpeg(paste(getwd(),"/Images/",outfile,".jpg",sep=""),res=300,units="in",width=8,height=8)
  par(mfrow=c(.Object@nma,.Object@npop*.Object@nsubyears),mai=c(0.1,0.1,0.01,0.01),omi=c(0.4,0.4,0.6,0.05))

  for(ma in 1:.Object@nma){

    for(p in 1:.Object@npop){
      for(m in 1:.Object@nsubyears){

        sdensplot(Istore[p,ma,m,],.Object@area_defs,.Object@areanams)

        if(p==1&m==1)mtext(paste("Age class",ma),2,line=0.6)
        if(ma==.Object@nma)mtext(paste("Subyear",m),1,line=0.6)
        #if(m==1&mat==1)mtext("Juvenile",3,line=0.3)
        #if(m==1&mat==2)mtext("Mature",3,line=0.3)
        #if(m==1&mat==1)mtext(paste("Population",p),3,adj=1.5,line=1.6)

      }}}

  mtext(paste("Stock",1:.Object@npop),3,at=seq(0,1,length.out=.Object@npop*2+1)[(1:.Object@npop)*2],outer=T,line=0.5)
  if(!is.na(outfile))dev.off()
})


#  tmse<-x
#  cols<-c("black","orange","blue","red","dark green","grey","purple","brown","pink")
#  mnam<-c("Yield (no Disc. rate)","Yield (5% Disc. rate)",
#          "Yield (10% Disc. rate)","Prob. Green Kobe",
#          "Av Ann. Var. Yield")

#  MPs<-tmse@MPs
#  nMP<-length(MPs)
#  yind<-(tmse@nyears+1):(tmse@nyears+tmse@proyears)
#  Y<-apply(tmse@C[,,,yind],1,mean)
#  PGK<-apply(tmse@B_BMSY[,,yind]>1&tmse@F_FMSY[,,yind]>1,1,mean)
#  y1<-yind[1:(tmse@proyears-1)]
#  y2<-yind[2:tmse@proyears]
#  AAV<-apply(apply(((tmse@C[,,,y1]-tmse@C[,,,y2])^2)^0.5,1:2,mean) / apply(tmse@C[,,,yind],1:2,mean),1,mean)

#  Yinc<-(max(Y)-min(Y))/15
#Y5inc<-(max(Y5)-min(Y5))/15
#Y10inc<-(max(Y10)-min(Y10))/15
#  PGKinc<-(max(PGK)-min(PGK))/15
#  AAVinc<-(max(AAV)-min(AAV))/15

#  par(mfrow=c(1,2),mai=c(1.1,1.1,0.05,0.05),omi=c(0.01,0.01,0.7,0.01))

#  plot(range(PGK)+c(-PGKinc,PGKinc),range(Y)+c(-Yinc,Yinc),col='white',xlab="PGK",ylab="Y")
#  addgg(PGK,Y)
#  textplot(PGK,Y,MPs,col=cols,new=F,font=2)

#  plot(range(AAV)+c(-AAVinc,AAVinc),range(Y)+c(-Yinc,Yinc),col='white',xlab="AAVY",ylab="Y")
#  addgg(AAV,Y)
#  textplot(AAV,Y,MPs,col=cols,new=F,font=2)
#  mtext(paste(tmse@Name," (n =",tmse@nsim,")",sep=""),3,line=0.3,outer=T)

#})

setMethod("plot",
          signature(x = "MSE"),
          function(x,quants=c(0.05,0.5,0.95),nworms=10, startyr=2014,rev=T){

            MSE<-x
            nsim<-MSE@nsim
            proyears<-MSE@proyears
            allyears<-MSE@proyears+MSE@nyears
            nMPs<-MSE@nMPs

            #somenames=c("Green Kobe","Final depletion","AAV Yield","Yield","Yield 5% DR", "Yield 10% DR", "Yield -5% DR")

            #stats<-getperf(MSE)
            yrs<-startyr:(startyr+MSE@proyears-1)
            refyears<-MSE@nyears+1:MSE@proyears-1
            worms<-1:min(nworms,MSE@nsim)

            xtick<-pretty(seq(yrs[1],yrs[MSE@proyears],length.out=3))

            SSBcol="light blue"
            Catcol="light grey"

            Cq<-apply(MSE@C,c(1,3,4),quantile,p=quants)
            Clim<-cbind(rep(0,MSE@npop),apply(Cq[,,,refyears],3,max))
            SSBnorm<-MSE@SSB/array(MSE@SSB[,,,MSE@nyears],dim(MSE@SSB))
            SSBq<-apply(SSBnorm,c(1,3,4),quantile,p=quants)
            SSBlim<-cbind(rep(0,MSE@npop),apply(SSBq,3,max))

            linecols<-rep(c("black","orange","blue","red","green","light grey","grey","pink","purple","brown"),100)

            MPnams<-unlist(MSE@MPs)
            MPnamsj<-paste(MPnams[(1:MSE@nMPs)*2-1],MPnams[(1:MSE@nMPs)*2],sep="-")

            par(mfrow=c(MSE@nMPs,MSE@npop*4),mai=c(0.05,0.05,0.35,0.05),omi=c(0.5,0.05,0.15,0.02))
            rsz<-5
            fill<-NA
            rw<-c(fill,rep(1,rsz),rep(2,rsz),fill,rep(3,rsz),rep(4,rsz),fill,fill,rep(5,rsz),rep(6,rsz),fill,rep(7,rsz),rep(8,rsz))
            lmat<-matrix(NA,ncol=rsz*MSE@nMPs,nrow=rsz*8+5)
            for(i in 1:nMPs)lmat[,(i-1)*rsz+1:rsz]<-rw+(i-1)*8
            lmat<-t(lmat)
            lmat[is.na(lmat)]<-max(lmat,na.rm=T)+1
            layout(lmat)

            pind<-1:MSE@npop
            if(rev)pind=MSE@npop:1

            gridcol='light grey'

            for(MP in 1:MSE@nMPs){
              for(pp in pind){
                # Catch projection  ---
                # Col 1: Catch quantiles
                ytick<-pretty(seq(0,Clim[pp,2],length.out=4))
                plot(range(yrs),Clim[pp,],axes=F,col="white",xlab="",ylab="",ylim=Clim[pp,])
                abline(h=ytick,col=gridcol)
                abline(v=xtick,col=gridcol)
                polygon(c(yrs,yrs[MSE@proyears:1]),
                        c(Cq[1,MP,pp,refyears],Cq[3,MP,pp,refyears[MSE@proyears:1]]),
                        col=Catcol,border=F)
                lines(yrs,Cq[2,MP,pp,refyears],lwd=1.5,col="black")
                if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
                if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
                abline(h=0)

                axis(2,ytick,labels=ytick)
                #legend('topright',legend="Catches (kg)",bty='n')


                # Col 2: Catch worms
                plot(range(yrs),Clim[pp,],axes=F,col="white",xlab="",ylab="",ylim=Clim[pp,])
                abline(h=ytick,col=gridcol)
                abline(v=xtick,col=gridcol)
                for(i in 1:length(worms))lines(yrs,MSE@C[MP,i,pp,refyears],col=linecols[i],lwd=1.2)
                if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
                if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
                abline(h=0)

                mtext('Catches (kg)',3,adj=-0.8,line=-1,cex=0.7)


                # SSB projection  ---
                # Col 3: SSB quantiles
                ytick<-pretty(seq(0,SSBlim[pp,2],length.out=4))
                plot(range(yrs),SSBlim[pp,],axes=F,col="white",xlab="",ylab="",ylim=SSBlim[pp,])
                abline(h=ytick,col=gridcol)
                abline(v=xtick,col=gridcol)
                polygon(c(yrs,yrs[MSE@proyears:1]),
                        c(SSBq[1,MP,pp,refyears],SSBq[3,MP,pp,refyears[MSE@proyears:1]]),
                        col=SSBcol,border=F)
                lines(yrs,SSBq[2,MP,pp,refyears],lwd=1.5,col="black")
                if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
                if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
                abline(h=0)
                abline(h=1,lty=2)
                axis(2,ytick,labels=ytick)
                #legend('topright',legend="Relative SSB",bty='n')

                mtext(MPnams[(MP-1)*2+pp],3,adj=-0.35,line=0.3,cex=0.9)

                # Col 4: SSB worms
                plot(range(yrs),SSBlim[pp,],axes=F,col="white",xlab="",ylab="",ylim=SSBlim[pp,])
                abline(h=ytick,col=gridcol)
                abline(v=xtick,col=gridcol)
                for(i in 1:length(worms))lines(yrs,SSBnorm[MP,i,pp,refyears],col=linecols[i],lwd=1.2)
                if(MP<MSE@nMPs)axis(1,at=xtick,labels=NA)
                if(MP==MSE@nMPs)axis(1,at=xtick,labels=xtick,las=2)
                abline(h=0)
                abline(h=1,lty=2)

                mtext('Relative SSB',3,adj=-0.8,line=-1,cex=0.7)


              }
            }

            mtext(MSE@Snames[pind],3,adj=c(0.26,0.78),line=-0.45,outer=T,font=2)

          })


