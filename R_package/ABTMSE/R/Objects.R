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
#' \item{A2}{Richards growth curve parameter}
#' \item{L1}{Richards growth curve parameter}
#' \item{L2}{Richards growth curve parameter}
#' \item{K}{Richards growth curve parameter}
#' \item{p}{Richards growth curve parameter}
#' \item{t0}{VB growth curve parameter}
#' \item{Linf}{VB growth curve parameter}
#' \item{Lvar_a}{Length variability at length parameter}
#' \item{Lvar_b}{Length variability at length parameter}
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
#' \item{wt_len}{a matrix of weight-at-length [stock, len]}
#' \item{Fec_len}{a matrix of Fecundity-at-length [stock, len]}
#' \item{Fec}{an array of spawning biomass-at-age [stock, age, year]}
#' \item{SRtype}{a character vector denoting the form of the stock-recruitment relationship: BH Beverton Holt, HS Hockey Stick [stock]}
#' \item{steep}{a vector of steepness by stock [stock]}
#' \item{SSBpR}{a vector of unfished spawning stock biomass per recruit [stock]}
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
#' \item{nI}{the number of fishery independent indices (e.g. 2: a spawning biomass survey in the GOM and MED)}
#' \item{nIobs}{the number of fishery independent indices}
#' \item{Iobs}{a data frame of fishery independent relative abundance observations [year, subyear, area, stock, index number, type (biomass/ssb), index]}
#' \item{nPSAT}{the number of electronic tags of known stock of origin}
#' \item{PSAT}{a data frame of electronic tag movements [stock, age, subyear, duration til recapture (subyears), from area, to area, number of tags, total from area]}
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
#' \item{Ilencat}{the categories of length specific vulnerable biomass}
#' \item{CobsCV}{a vector nf long, the catch observation error (lognormal sd)}
#' \item{CLCV_num}{the numerator of the normal observation error on lengths}
#' \item{RDCV}{the recruitment deviation penalty (sigma R)(lognormal sd)}
#' \item{SSBfit}{the type of SSBfit 1:SSB0 2:SSBnow}
#' \item{SSBprior}{a vector np long, an optional prior on current spawning biomass}
#' \item{SSBpriorCV}{the precision of the SSBprior (lognormal sd)}
#' \item{SSBinc}{a numerical value representing the ratio of SSB from year SSBy2 / SSBy1}
#' \item{SSBy}{vector of integers representing years over which desired increase in SSB matches SSBinc}
#' \item{SSBincstock}{the stock that SSBinc refers to}
#' \item{BSfrac}{a matrix of fractions, stock by season that is the predicted seasonal biomass of Eastern / Western biomass in Western and Eastern areas, respectively}
#' \item{FCV}{the prior on deviations from the mean F}
#' \item{movCV}{the prior on deviations from homogenous movement}
#' \item{selCV}{the prior on selectivity parameters}
#' \item{SSBincCV}{the prior precision of SSB trend}
#' \item{BSfracCV}{the prior precision of the Seasonal biomass fraction}
#' \item{R0diffCV}{the CV on the log ratio of early versus late R0 for the same stock (where applicable)}
#' \item{nLHw}{the number of likelihood components (for weighting)}
#' \item{LHw}{a vector nLHw long specifying the relative weight of the various data types 1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB, 13 E-W Distr}
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
#' \item{MIinv}{a 1 or 0 determining whether the fit should attempt to be MI invariant}
#' \item{MICV}{numerical value of the CV in additional to regional FMod deviations}
#' \item{SpatPr}{matrix of Spatial priors}
#' \item{SpatFrac}{matrix of spatial priors}
#' \item{debug}{a logical (0 or 1) value determing whether the model should be run for a single iteration to check for errors}
#' \item{verbose}{a logical (0 or 1) value determing whether more complete information should be provided in each iteration of the model estimation}
#' \item{datacheck}{a unique number for checking that data were read into the model correctly}
#' \item{CPUEnames}{a character string nCPUEq long, recording the names of CPUE indices}
#' \item{Inames}{a character string nI long, recording the names of the fishery indepdendent indices}
#' \item{nMovExc}{integer the number of rows in the movement exclusion data frame}
#' \item{MovExc}{a data frame of movement exceptions}
#' \item{Phases}{the phases of the various parameter estimates (1,2,3)}
#' \item{ET_LHF}{the type of likelihood function used for electronic tagging data (1,2: zero intercept)}
#' \item{LC_LHF}{the type of likelihood function used for length composition data (1,2: zero intercept)}
#' \item{beta}{numerical value, the hyperstability parameter assumed for all indices (e.g. 0.9 = hyperstable, 1.1 = hyperdeplete)}
#' }
setClass("OMI",representation(
  # Description
  Name="character",Date="character",Author="character", Notes="character",PrimarySource="character",
  OMfactors="list",
  # other slots --------------------------------------------------------------------------------------------------
  years="numeric", Hyears="numeric", areas='character',
  areanams='character', area_defs='list', Fleets='list',
  A2='numeric',L1='numeric',L2='numeric',K='numeric',p='numeric',
  t0='numeric',Linf='numeric',Lvar_a='numeric',Lvar_b='numeric',
  ageM='numeric',ageMsd='numeric',mat='array',lenbins='numeric',
  # Inputs to M3 (in order) --------------------------------------------------------------------------------------
  nHy='integer', ny='integer', ns='integer',np='integer',na='integer',nr='integer',nf='integer',
  nl='integer',nRPT='integer',RPTind='matrix',sdur='numeric',nydist='integer',#nZeq='integer',nyeq='integer',
  mulen='numeric',RDblock='numeric',nRD='integer',
  iALK='array',#p y a l (trans)
  lwa='numeric', lwb='numeric',
  len_age='array', wt_age='array', # p a y (trans)
  wt_len="matrix",Fec_len="matrix",
  Fec='array', SSBpR='numeric',# p a (wt*mat) (trans)
  nSR='numeric',SRminyr='numeric',SRmaxyr='numeric',SRp="numeric",SRpar="numeric",SRtype="character",
  nRDs='numeric', RDno ="matrix", RDts="matrix",
  spawns='numeric',canspawn='matrix', # p r (trans)
  Ma='array', # p a (trans)
  nCobs='integer',  Cobs='matrix', # nCobs x 5 (y s r f Cobs) (trans)
  nCPUEq='integer',  nCPUEobs='integer',  CPUEobs='matrix', #CPUEwt='numeric', # nCPUEobs x 6 (y s r q f index) (trans)
  nE='integer',                  # number of partial f series (basically nfleets but coding seperatly to later account for catchability mirroring)
  nEobs='integer',Eobs='matrix', # nE x 5 (y s r f partial F)
  nCLobs='integer',CLobs='matrix', # nCLobs x 6 (y s r f l N) (trans)
  HCobs='array', # (y x s, x r x a) (trans)
  RAI='array', # r s y (not trans)
  nI='integer',nIobs='integer',Iobs='matrix', #Iwt='numeric', # nI x 7 (y s spawn_area pp index(=pp if SSB) type(biomass/ssb) index) (trans)
  nPSAT='integer',PSAT='matrix', # nPSAT x 7 (p a s t fr tr N) (trans)
  nPSAT2='integer',PSAT2='matrix', # nPSAT2 x 5+(np) (a s t fr tr SOOp1 SOOp2) (trans)
  nTag='integer',Tag='matrix', # nTag x 10 (y s r a - y s r f a N)
  nSOOobs ='integer',SOOobs='matrix',# nSOOobs x 6 (p a y s r N) (trans)
  nsel='integer',seltype='numeric', selind='numeric', # nf
  ratiolim='numeric', infleclim='numeric',
  nma='integer',ma='numeric', macat='matrix',# na
  nMP='integer',nmovind='integer',movind='matrix',# nmovind x 4 (p s r r)
  nmov1='integer',mov1='matrix', # nmov1 x 4 (p s r r)
  Ilencat='matrix',
  movtype='integer',
  #CobsCV='numeric', CPUEobsCV='numeric',# fleets,
  #IobsCV='numeric',# nI (np if SSB)
  CLCV_num='numeric',
  RDCV='numeric',
  nSSBprior='numeric',SSBprior='matrix',SSBCV='numeric',
  nDepprior='numeric',Depprior='matrix',DepCV="numeric",
  BSfrac='matrix',
  FCV='numeric', movCV='numeric', selCV='numeric', SSBincCV='numeric',BSfracCV="numeric", R0diffCV="numeric",
  nLHw='integer',  LHw='numeric', # 13: 1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB, 13 SSBinc
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
  MIinv="integer",# Master index - independent mode?
  MICV="numeric",# Master index CV around FMod
  SpatPr="matrix", SpatFrac="matrix",
  debug='integer',verbose='integer',datacheck='integer',

  # Misc
  CPUEnames='character',
  Inames='character',
  nMovExc='integer',
  MovExc="data.frame",
  Phases="numeric",
  ET_LHF="numeric",
  LC_LHF="numeric",
  beta="numeric"

))


#' An S4 class object that contains all observation model
#'
#' \describe{
#' \item{Name}{The name of the observation model object}
#' \item{Ccv}{a 2 item vector specifying the range in catch observation error as lognormal sd (upper lower)}
#' \item{Cbcv}{a 2 item vector specifying the maximum extend of catch bias as a lognormal sd}
#' \item{Cbias}{a single value specifying the fraction of catch that is reported}
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
#' \item{MinAC}{a minimum value for index lag-1 autocorrelation in residuals}
#' }
setClass("Obs",representation(Name="character",
               Ccv="numeric",Cbcv="numeric", Cbias="numeric",                 # Observation error and bias in total annual catches
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
               MPind="data.frame",MPind_stats="list",                         # Indices that may be used in MPs
               MinAC="numeric"                                                # Minimum simulated lag-1 autocorrelation in residuals

))



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


#' An S4 class object that contains test unit Sim-Sam results for the M3 operating model
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
#' \item{lnHR1}{a vector nstock long of historical recruitment before 10 year prior to model initialization}
#' \item{lnHR2}{a vector nstock long of historical recruitment between model init and minus 10 years}
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
setClass("TEST",representation(
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
  qI="array",qCPUE="array",                                  # Catchability of effort, fishery ind indices and fishery CPUE indices
  sel="array",selpars="array",
  mat="array",M="array",
  Recdevs="array", R0="array",  muR="array",                 # Recruitment deviations, unfished recruitment, mean historical recruitment              #FM="array",,Z="array",                 # Fishing, natural and total instantaneous mortality rate.#B="array",SSB="array",NSB="array",              # Biomass, spawning stock biomass #N="array",SSN="array",NSN="array",              # Numbers, spawning stock numbers #C="array",CAA="array",                          # Catches taken, catch at age taken
  lnHR1="array",lnHR2="array",
  MSY="array",BMSY="array", VBMSY="array",                   # Maximum sustainable yield, biomass at maximum sustainable yield
  SSBMSY="array",
  FMSY="array",UMSY="array",FMSYa="array",                   # Fishing mortality rate at maximum sustainable yield
  SSBMSY_SSB0="array",#IMSY="numeric",                                  # Relative abundance index at maximum sustainable yield#Linf="array",K="array", #Idist="array",                                   # for plotting OM unfished spatial dist#targpop="numeric",                               # What populations are to be used in MSY / depletion optimization#nZeq="integer",                                 # The number of initial years to calculation equilibrium F

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
  B_BMSY="array",
  F_FMSY="array",
  B="array",
  SSB="array",
  SSB0="array",
  SSB0proj="array",
  TAC="array",

  sim="list",
  sam="list",

  nydist="integer",                                # The number of years (iterations) taken to find equilibrium spatial distribution#nyeq="integer",                                 # The number of years (iterations) taken to find equilibrium F
  Snames="character",                              # Name of the stocks
  area_defs="list",                                # Area definitions list (copied from OMI)
  areanams="character",                            # Names of areas        (copied from OMI)
  seed="numeric"                                   # Random seed from which this object was made
))

setMethod("initialize", "TEST", function(.Object,OM,OMI,Obs,Testdir="C:/TEST",M3dir="C:/M3",nsim=32,seed=1,
                                       Recruitment=NULL,Snames=c("East","West"),ploty=F){
  # .Object})
  # .Object<-new('TEST')

  .Object@Snames=Snames

  if(!dir.exists(Testdir))stop(paste('You specified a directory',Testdir,'that does not exist'))
  if(!dir.exists(M3dir))stop(paste('You specified a directory',M3dir,'that does not exist'))


  # copy over dimensions ------

  cat("Constructing arrays")
  cat("\n")
  flush.console()

  # Dimensions  S P A Y M R
  nsim<-.Object@nsim<-OM@nsim
  npop<-.Object@npop<-OM@npop
  nyears<-.Object@nyears<-OM@nyears
  nHyears<-.Object@nHyears<-OM@nHyears
  nages<-.Object@nages<-OM@nages
  nsubyears<-.Object@nsubyears<-OM@nsubyears
  nareas<-.Object@nareas<-OM@nareas
  nfleets<-.Object@nfleets<-OM@nfleets
  nlen<-.Object@nlen<-OM@nlen
  lenbins<-.Object@lenbins<-OM@lenbins
  mulen<-.Object@mulen<-OM@mulen
  Wt_age<-.Object@Wt_age<-OM@Wt_age
  nydist<-.Object@nydist<-OM@nydist
  nma<-.Object@nma<-OM@nma
  #R0<-OM@Recpars[,,1,2]
  mat<-.Object@mat<-OM@mat
  mov<-.Object@mov<-OM@mov
  h<-.Object@h<-OM@h
  Recsubyr<-.Object@Recsubyr<-OM@Recsubyr
  Recdevs<-.Object@Recdevs<-OM@Recdevs

  HCobs<-OM@HCobs

  M<-.Object@M<-OM@M
  Mtemp<-array(0,dim(.Object@M))
  Mtemp[,,2:nages,]<-.Object@M[,,1:(nages-1),]

  surv=tomt(exp(-apply(Mtemp[,,,1],2:1,cumsum)))
  surv[,,nages]<-surv[,,nages]*exp(-Mtemp[,,nages,1])/(1-exp(-Mtemp[,,nages,1]))

  N<-SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,nyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  FD<-array(NA,c(nsim,nfleets,nyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  VB<-C<-array(NA,c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))
  CA<-array(NA,c(nsim,npop,nyears,nsubyears,nareas))

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

  FM<-array(NA,c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))
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

  apply(SSB[1,    ,   ,nyears,,],c(1,3),sum)

  SSB0=OM@muR*exp(OM@lnHR1)*apply(surv*Wt_age[,,,1]*mat[,,,1],1:2,sum)     #// Unfished Spawning Stock Biomass

  SSB1<-apply(N[,,,1,1,]*
                array(Wt_age[,,,1]*OM@mat[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)


  SSBcur<-apply(N[,,,nyears,nsubyears,]*
                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)

  Biomass<-array(0,c(nsim,npop,nages,nyears,nsubyears,nareas))

  Biomass[,,,1:nyears,,]<-N[,,,1:nyears,,]*array(Wt_age,c(nsim,npop,nages,nyears,nsubyears,nareas))


  SSBall<-N*array(Wt_age,dim(N))*array(OM@mat,dim(N))
  RAI<-apply(SSBall,c(1,4,5,6),sum,na.rm=T)
  RAI<-RAI/array(apply(RAI,1,mean),dim(RAI))

  D<-SSBcur/SSB0 # sim sam Check against OM@D (remember only targetpop is matched)
  Dt<-SSBcur/SSB1

  # Generate observation errors ---------------------------------------------

  .Object@Cimp<-runif(nsim,Obs@Ccv[1],Obs@Ccv[2])
  .Object@Cb<-trlnorm(nsim,1,Obs@Cbcv)
  .Object@Cerr<-array(trlnorm(nsim*nyears,rep(.Object@Cb,nyears),rep(.Object@Cimp,nyears)),c(nsim,nyears))

  .Object@Iimp<-runif(nsim,Obs@Icv[1],Obs@Icv[2])
  .Object@Ierr<-array(trlnorm(nsim*nyears,1,rep(.Object@Iimp,nyears)),c(nsim,nyears))
  .Object@Ibeta<-exp(runif(nsim,log(Obs@Ibeta[1]),log(Obs@Ibeta[2])))

  .Object@Btimp<-runif(nsim,Obs@Btcv[1],Obs@Btcv[2])
  .Object@Btb<-trlnorm(nsim,1,Obs@Btbcv)
  .Object@Bterr<-array(trlnorm(nsim*nyears,rep(.Object@Btb,nyears),rep(.Object@Btimp,nyears)),c(nsim,nyears))

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

  .Object@C<-array(NA,c(nsim,npop,nyears))
  .Object@B_BMSY<-array(NA,c(nsim,npop,nyears))
  .Object@F_FMSY<-array(NA,c(nsim,npop,nyears))
  .Object@D<-array(NA,c(nsim,npop,nyears))
  .Object@ageMb<-trlnorm(nsim,1,Obs@ageMbcv)
  .Object@SSB<-array(NA,c(nsim,npop,nyears))

  # Building simualted data test files ------------------------------------------------

  cat("Creating simulated data")
  cat("\n")

  testC<-array(NA,c(nsim,npop,nfleets,nareas))
  CAdist<-array(NA,c(nsim,npop,nareas,nfleets,nages))

  CAA<-SampCatch2(C,.Object@nCAAobs)
  CAL<-makeCAL4(CAA,OM@iALK) # CAL # nsim, year, subyear, area, fleet, length category, N

  inc<-OM@mulen[2]-OM@mulen[1]
  CAL_bins<-c(OM@mulen,OM@mulen[OM@nlen]+inc)-0.5*inc

  for(i in 1:nsim){

    # Catch observation error
    Cobs<-as.matrix(cbind(expand.grid(1:nyears,1:nsubyears,1:nareas,1:nfleets),
                          as.vector(apply(C,4:7,sum,na.rm=T)*
                                      trlnorm(nyears*nsubyears*nareas*nfleets,.Object@Cb[i],.Object@Cimp[i]))))
    Cobs<-Cobs[Cobs[,5]>0,]
    OMI@nCobs<-nrow(Cobs)
    OMI@Cobs<-Cobs


    OMI@nCPUEq<-as.integer(1)
    OMI@nCPUEobs<-as.integer(1)
    OMI@CPUEobs<-matrix(c(1,1,1,1,1,1),nrow=1)
    OMI@CPUEobsCV<-0.25
    OMI@qCPUE_ini<-1

    ind<-as.matrix(expand.grid(i,1:nyears,1:nsubyears,1:nareas,1:nfleets,1:nlen))
    CLobs<-cbind(ind[,2:6],CAL[ind])
    CLobs<-subset(CLobs,CLobs[,6]>0.001)

    OMI@nCLobs<-nrow(CLobs)
    OMI@CLobs<-CLobs

    #OMI@Iobs year, subyear, area, stock, index number (nI), type (1) biomass (2) SSB, index

    SSB1<-apply(SSB[i,1,,,,],2,sum,na.rm=T)
    SSB2<-apply(SSB[i,2,,,,],2,sum,na.rm=T)
    SSB1<-SSB1/mean(SSB1)
    SSB2<-SSB2/mean(SSB2)

    #y s r p i type(biomass/ssb) index
    #             y                  s            r                         p                   i                type
    Iobs<-cbind(rep(1:nyears,npop),rep(2,nyears*2),rep(c(4,1),each=nyears),rep(1:2,each=nyears),rep(1:2,each=nyears),rep(2,nyears*2),c(SSB1,SSB2))

    OMI@nI<-as.integer(2)
    OMI@nIobs<-nrow(Iobs)
    OMI@Iobs<-Iobs

    movage<-match(1:3,OM@ma[1,])
    movs<-OM@mov[i,,movage,,,]
    PSATa<-array(0,dim(movs)) # p, ma, s, r,  r

    for(p in 1:npop){
      for(ma in 1:nma){
        for(ss in 1:nsubyears){
          for(fr in 1:nareas){

            PSATa[p,ma,ss,fr,]<-rmultinom(1,100,movs[p,ma,ss,fr,])
          }
        }
      }
    }

    ind<-as.matrix(expand.grid(1:npop,1:nma,1:nsubyears,1:nareas,1:nareas))
    PSAT<-as.data.frame(cbind(ind[,1:3],rep(1,nrow(ind)),ind[,4:5],PSATa[ind]))
    names(PSAT)<-c("p","a","s","t","fr","tr","N")
    PSAT<-PSAT[PSAT$N!=0,]

    # p a s t fr tr N
    OMI@nPSAT<-nrow(PSAT)
    OMI@PSAT<-as.matrix(PSAT)

    # Stock of origin ==========
    # N[sim,p,a,y,m,r]
    SOOa<-array(0,c(npop,nma,nyears,nsubyears,nareas))

    for(ma in 1:nma){
      for(y in 1:nyears){
        for(ss in 1:nsubyears){
          for(rr in 1:nareas){
            SOOa[,ma,y,ss,rr]<-rmultinom(1,100,N[i,,movage[ma]+3,y,ss,rr]/sum(N[i,,movage[ma]+3,y,ss,rr]))
          }
        }
      }
    }

    ind<-TEG(c(npop,nma,nyears,nsubyears,nareas))
    SOO<-cbind(ind,SOOa[ind])
    SOO<-SOO[SOO[,6]!=0,]

        # p a y s r N
    OMI@nSOOobs<-nrow(SOO)
    OMI@SOOobs<-SOO

    OMI@lnF_ini<-rep(-6,OMI@nCobs)

    writedir<-paste0(Testdir,"/",i)
    if(!dir.exists(writedir))dir.create(writedir)
    save(OMI,file=paste0(writedir,"/OMI"))
    M3write(OMI,OMdir=writedir)

    file.copy(paste(M3dir,"/M3.exe",sep=""),writedir,overwrite=T)      # copy the latest executable to the temporary
    file.copy(paste(M3dir,"/stats.cxx",sep=""),writedir,overwrite=T)   # copy over the statistics library
    #file.copy(paste(M3dir,"/M3.pin",sep=""),writedir,overwrite=T)      # copy over the parameter initialization file

  }

  sfLapply(1:nsim,runM3p,OMdir=Testdir)             # Run the M3 executables in parallel

  out<-new('list')
  for(i in 1:nsim)out[[i]]<-M3read(paste0(Testdir,"/",i))

  ex<-function(x,obj,slot)unlist(obj[[x]][slot])
  exSSBcur<-function(x,obj)obj[[x]]$SSB[,obj[[x]]$ny,obj[[x]]$ns]

  .Object@sam$D<-t(sapply(1:nsim,ex,obj=out,slot="D"))
  .Object@sam$Dt<-t(sapply(1:nsim,ex,obj=out,slot="Dt"))
  .Object@sam$SSB0<-t(sapply(1:nsim,ex,obj=out,slot="SSB0"))
  .Object@sam$muR<-t(sapply(1:nsim,ex,obj=out,slot="muR"))
  .Object@sam$SSBcur<-t(sapply(1:nsim,exSSBcur,obj=out))

  .Object@sim$D<-D
  .Object@sim$Dt<-Dt
  .Object@sim$SSB0<-SSB0
  .Object@sim$muR<-OM@muR
  .Object@sim$SSBcur<-SSBcur

  .Object

})


simsamplot<-function(x,y,quants,dens=F,main=""){

  xlim<-quantile(x,c(0.01,0.99))
  ylim<-quantile(y,c(0.01,0.99))
  xmu<-mean(x)
  ymu<-mean(y)

  plot(x,y,xlab="",ylab="",main="",pch=19,col="white",xlim=xlim,ylim=ylim)
  abline(v=c(mean(x),quantile(x,quants)),col="light blue",lty=c(1,2,2))
  abline(h=c(mean(y),quantile(y,quants)),col="light blue",lty=c(1,2,2))


  if(dens){

    dens<-density(x)
    den<-dens$y
    den<-den/max(den)*(ymu-ylim[1])*0.66+ylim[1]
    polygon(dens$x,den,col=makeTrans('grey',50),border=NA)
    abline(v=0)

    dens<-density(y)
    den<-dens$y
    den<-den/max(den)*(xmu-xlim[1])*0.66+xlim[1]
    polygon(den,dens$x,col=makeTrans('grey',60),border=NA)
    abline(h=0)

    mtext(main,3,line=0.15,adj=0.5,cex=0.8)

  }

  points(x,y,pch=19,col=makeTrans("orange",80))

}



setMethod("plot",
          signature(x = "TEST"),
          function(x,quants=c(0.05,0.95),detplot=F){

            TEST<-x
            nsim<-TEST@nsim
            nyears<-TEST@nyears

            slots<-c("D","Dt","SSB0","SSBcur")
            slnames<-c("Spawning stock depletion (SSBcur / SSB0)", "SSB depletion over time series",
                       "Unfished spawning biomass","Current spawning biomass")

            if(detplot){ # optional plot

              par(mfcol=c(length(slots),2),mai=c(0.3,0.3,0.3,0.01),omi=c(0.5,0.5,0.4,0.01))

              for(pp in 1:TEST@npop){

                for(sl in 1:length(slots)){
                  datsim<-matrix(unlist(TEST@sim[slots[sl]]),ncol=TEST@npop)
                  datsam<-matrix(unlist(TEST@sam[slots[sl]]),ncol=TEST@npop)
                  simsamplot(datsim[,pp],datsam[,pp],quants=quants)
                  if(pp==1)mtext(paste("---", slnames[sl],"---"),3,line=0.45,cex=0.85)
                }

              }

              mtext("Simulated",1,line=0.8,outer=T)
              mtext("Estimated",2,line=0.8,outer=T)
              mtext(TEST@Snames,3,adj=c(0.26,0.78),line=0.45,outer=T,font=2)

            }

            par(mfcol=c(2,2),mai=c(0.45,0.45,0.35,0.01),omi=c(0.5,0.5,0.5,0.01))

            for(sl in 1:length(slots)){
              datsim<-matrix(unlist(TEST@sim[slots[sl]]),ncol=TEST@npop)
              datsam<-matrix(unlist(TEST@sam[slots[sl]]),ncol=TEST@npop)
              bias<-(datsam-datsim)/datsim*100
              simsamplot(bias[,1],bias[,2],quants=quants,dens=T,main=slnames[sl])
            }
            mtext("Percentage biases (Sam-Sim)/Sim",3,line=0.8,outer=T)
            mtext(TEST@Snames[1],1,line=0.8,outer=T)
            mtext(TEST@Snames[2],2,line=0.8,outer=T)

})





