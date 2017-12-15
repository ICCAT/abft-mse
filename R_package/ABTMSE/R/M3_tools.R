# M3 tools

#' Create an M3.dat input file from an object of class OMI
#'
#' @param OMI and object of class OMI
#' @param OMdir a character string representing the location of an M3 folder containing M3.exe and stats.cxx files
#' @return creates an M3.dat file of the correct format in \code{OMdir}
#' @examples
#' M3write(OMI_example,C:/ABT-MSE/M3)
M3write<-function(OMI,OMdir="C:/M3"){

  datfile<-paste0(OMdir,"/M3.dat")
  if(!dir.exists(unlist(strsplit(datfile,split="M3.dat"))))stop("You must specify a valid M3 directory")

  #write("# name",datfile,1,append=F)
  #write(OMI@Name,datfile,1,append=T)

  #write("# nOMfactors",datfile,1,append=T)
  #write(length(OMI@OMfactors),datfile,1,append=T)

  #write("# OMfactors",datfile,1,append=T)
  #write(unlist(OMI@OMfactors),datfile,length(OMI@OMfactors),append=T)

  write("# nHy number of historical years for SRA",datfile,1,append=F)
  write(OMI@nHy,datfile,1,append=T)

  write("# ny number of years",datfile,1,append=T)
  write(OMI@ny,datfile,1,append=T)

  write("# ns number of subyears",datfile,1,append=T)
  write(OMI@ns,datfile,1,append=T)

  write("# np number of populations/stocks",datfile,1,append=T)
  write(OMI@np,datfile,1,append=T)

  write("# na number of age classes",datfile,1,append=T)
  write(OMI@na,datfile,1,append=T)

  write("# nr number of regions/areas",datfile,1,append=T)
  write(OMI@nr,datfile,1,append=T)

  write("# nf number of fleets",datfile,1,append=T)
  write(OMI@nf,datfile,1,append=T)

  write("# nl number of length classes",datfile,1,append=T)
  write(OMI@nl,datfile,1,append=T)

  write("# nRPT maximum number of time steps that a PSAT can be recaptured",datfile,1,append=T)
  write(OMI@nRPT,datfile,1,append=T)

  write("# RPtind correct subyear recapture index",datfile,1,append=T)
  write(t(OMI@RPTind),datfile,OMI@nRPT,append=T)

  write("# sdur the duration of the various subyears (sums to 1)",datfile,1,append=T)
  write(OMI@sdur,datfile,OMI@ns,append=T)

  #write("# nZeq Number of years at the start of the model to calculate equilibrium Z from (mean of nZep years)",datfile,1,append=T)
  #write(OMI@nZeq,datfile,OMI@ns,append=T)

  write("# nydist Number of years over which initial stock distribution is calculated (prior to spool up)",datfile,1,append=T)
  write(OMI@nydist,datfile,OMI@ns,append=T)

  #write("# nyeq Number of spool-up years over which the stock is subject to nZeq, used to define equilibrium conditions",datfile,1,append=T)
  #write(OMI@nyeq,datfile,OMI@ns,append=T)

  write("# ml the mean length of the length categories",datfile,1,append=T)
  write(OMI@mulen,datfile,OMI@nl,append=T)

  write("# RDblock the RD parameter for each year",datfile,1,append=T)
  write(OMI@RDblock,datfile,OMI@ny,append=T)

  write("# nRD the number of estimated recruitment strengths",datfile,1,append=T)
  write(OMI@nRD,datfile,1,append=T)

  # -- Growth --

  write("# iALK the age-length key by population and year p y a l",datfile,1,append=T)
  write(tomt(OMI@iALK),datfile,OMI@nl,append=T)

  write("# lwa weight-length parameter a w=al^ b",datfile,1,append=T)
  write(OMI@lwa,datfile,OMI@np,append=T)

  write("# lwa weight-length parameter b w=al^ b",datfile,1,append=T)
  write(OMI@lwb,datfile,OMI@np,append=T)

  write("# len_age (pay)",datfile,1,append=T)
  write(OMI@len_age,datfile,OMI@ny,append=T)

  write("# wt_age (pay)",datfile,1,append=T)
  write(OMI@wt_age,datfile,OMI@ny,append=T)

  # -- Maturity --

  write("# Fec, fecundity at age, SSB at age",datfile,1,append=T)
  write(t(OMI@Fec),datfile,OMI@na,append=T)

  #write("# steep, steepness of the Bev-Holt SR relationship",datfile,1,append=T)
  #write(OMI@steep,datfile,OMI@np,append=T)

  # -- Spawning --

  write("# spawns, the subyear in which the stock spawns",datfile,1,append=T)
  write(OMI@spawns,datfile,OMI@np,append=T)

  write("# canspawn, areas in which the stock spawns",datfile,1,append=T)
  write(OMI@canspawn,datfile,OMI@nr,append=T)

  # -- Natural Mortality rate --

  write("# Ma, instantaneous natural mortality rate at age",datfile,1,append=T)
  write(t(OMI@Ma),datfile,OMI@na,append=T)

  # -- Fishery data --

  write("# nCobs, the number of catch weight observations y s r f CW",datfile,1,append=T)
  write(OMI@nCobs,datfile,1,append=T)

  write("# Cobs, catch weight observations y s r f C(weight)",datfile,1,append=T)
  write(t(OMI@Cobs),datfile,5,append=T)

  # CPUE

  write("# nCPUEq, the number of CPUE series",datfile,1,append=T)
  write(OMI@nCPUEq,datfile,1,append=T) # in this simulation this is the same as the number of fleets

  write("# nCPUEobs, the number of CPUE observations y s r f CPUE(weight)",datfile,1,append=T)
  write(OMI@nCPUEobs,datfile,1,append=T)

  write("# CPUEobs, CPUE observations y s r cpueindex f CPUE(weight)",datfile,1,append=T)
  write(t(OMI@CPUEobs),datfile,ncol(OMI@CPUEobs),append=T)

  # Partial F's (standardized effort)

  write("# nE, the number of effort series (typically nF but could be mirroring)",datfile,1,append=T)
  write(OMI@nE,datfile,1,append=T) # in this simulation this is the same as the number of fleets

  write("# nEobs, the number of effort observations ",datfile,1,append=T)
  write(OMI@nEobs,datfile,1,append=T)

  write("# Eobs, CPUE observations y s r f eindex CPUE(weight)",datfile,1,append=T)
  write(t(OMI@Eobs),datfile,ncol(OMI@Eobs),append=T)

  # Length composition

  write("# nCLobs, the number of catch-at-length observations y s r f l N",datfile,1,append=T)
  write(OMI@nCLobs,datfile,1,append=T)

  write("# CLobs, catch-at-length observations y s r f l N",datfile,1,append=T)
  write(t(OMI@CLobs),datfile,ncol(OMI@CLobs),append=T)

  # Historical catches

  write("# HCobs, the historical catch at age observations by Hy x s x r x a",datfile,1,append=T)
  write(tomt(OMI@HCobs),datfile,OMI@nHy,append=T)

  # The real relative abundance index RAI (y, s, r) (pass through for model fitting)
  write("# RAI, Relative Abundance index r x s x y",datfile,1,append=T)
  write(OMI@RAI,datfile,OMI@ny,append=T)

  # Fishery-independent indices y s r i type(biomass/ssb) index

  write("# nI, the number of fishery independent indices series",datfile,1,append=T)
  write(OMI@nI,datfile,1,append=T) # in this simulation this is the same as the number of populations

  write("# nIobs, the number of fishery independent observations y s r pp i type(biomass/ssb) index",datfile,1,append=T)
  write(OMI@nIobs,datfile,1,append=T)

  write("# Iobs, fishery independent observations y s r p i type(biomass/ssb) index",datfile,1,append=T)
  write(t(OMI@Iobs),datfile,7,append=T)

  # PSAT tagging --

  write("# nPSAT, PSATs data of known stock of origin p a s t fr tr N",datfile,1,append=T)
  write(OMI@nPSAT,datfile,1,append=T)

  write("# PSAT data of known stock of origin p a s t fr tr N",datfile,1,append=T)
  write(t(OMI@PSAT),datfile,7,append=T)

  write("# nPSAT2, PSATs data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
  write(t(OMI@nPSAT2),datfile,1,append=T)

  write("# PSAT2 data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
  write(OMI@PSAT2,datfile,5+OMI@np,append=T)

  # Placeholder for conventional tags

  write("# nTag, number of conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
  write(OMI@nTag,datfile,1,append=T)

  write("# Tag, conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
  write(t(OMI@Tag),datfile,10,append=T)

  # Stock of origin

  write("# nSOOobs, number of stock of origin observations p a y s r N",datfile,1,append=T)
  write(nrow(OMI@SOOobs),datfile,1,append=T)

  write("# SOOobs, stock of origin observations p a y s r N",datfile,1,append=T)
  write(t(OMI@SOOobs),datfile,6,append=T)

  # -- Selectivity controls

  write("# nsel, number of estimated selectivities",datfile,1,append=T)
  write(OMI@nsel,datfile,1,append=T) # same as number of fleets

  write("# seltype, 2:logistic, 3:Thompson",datfile,1,append=T)
  #       PS,TP,LL,OTH
  write(OMI@seltype,datfile,OMI@nsel,append=T) # LL fleet is logistic

  write("# selind, which selectivity is assigned to each fleet",datfile,1,append=T)
  write(OMI@selind,datfile,OMI@nf,append=T) # same as fleets

  write("# ratiolim, limits on logistic slope parameter relative to inflection point",datfile,1,append=T)
  write(OMI@ratiolim,datfile,2,append=T) # same as fleets

  write("# infleclim, limits on model selectivity",datfile,1,append=T)
  write(OMI@infleclim,datfile,2,append=T) # same as fleets

  # -- Movement estimation

  write("# nMP, number of estimated movement parameters",datfile,1,append=T)
  write(OMI@nMP,datfile,1,append=T)

  write("# nma, max number of age classes",datfile,1,append=T)
  write(OMI@nma,datfile,1,append=T)

  write("# ,ma, age class assignment by age ",datfile,1,append=T)
  write(array(OMI@ma,c(OMI@na,OMI@np)),datfile,1,append=T)

  write("# nmovind, number of estimated movement parameters minus viscosity",datfile,1,append=T)
  write(OMI@nmovind,datfile,1,append=T)

  write("# movind, the location of estimated movement parameters p s r r",datfile,1,append=T)
  write(t(OMI@movind),datfile,5,append=T)

  write("# nmov1, number of initial non-estimated movement parameters",datfile,1,append=T)
  write(OMI@nmov1,datfile,1,append=T)

  write("# mov1, the location of initial non-estimated movement parameters p s r r",datfile,1,append=T)
  write(t(OMI@mov1),datfile,5,append=T)

  write("# movtype, the type of movement parameterization 1: gravity 2:markov matrix",datfile,1,append=T)
  write(OMI@movtype,datfile,1,append=T)


  # -- Extra length selectivity types for indices - basically short to long fish

  write("# Ilencat, the length bin indexing of new indices",datfile,1,append=T)
  write(OMI@Ilencat,datfile,2,append=T)


  # -- Observation errors

  write("# CobsCV, lognormal CV of the observed catches",datfile,1,append=T)
  write(OMI@CobsCV,datfile,OMI@nf,append=T)

  write("# CPUEobsCV, lognormal CV of the CPUE indices",datfile,1,append=T)
  write(OMI@CPUEobsCV,datfile,OMI@nf,append=T) # CPUE index for each fleet

  write("# IobsCV, lognormal CV of the fishery independent indices",datfile,1,append=T)
  write(OMI@IobsCV,datfile,OMI@np,append=T) # SSB index for each population


  # -- Priors

  write("# RDCV, lognormal penalty on recruitment deviations",datfile,1,append=T)
  write(OMI@RDCV,datfile,1,append=T)

  write("# SSBprior, prior on current SSB",datfile,1,append=T)
  write(OMI@SSBprior,datfile,1,append=T) # Absolute tonnage of SSB in current model year

  write("# SSBCV, lognormal penalty SSBprior",datfile,1,append=T)
  write(OMI@SSBCV,datfile,1,append=T) # CV on SSB prior in current model year

  write("# SSBfit, type of ssb fit",datfile,1,append=T)
  write(OMI@SSBfit,datfile,1,append=T) # CV on SSB prior in current model year

  write("# SSBinc, ratio of SSBy[2]/SSBy[1]",datfile,1,append=T)
  write(OMI@SSBinc,datfile,1,append=T) # CV on SSB prior in current model year

  write("# SSBy, years for SSBinc ratio calculation",datfile,1,append=T)
  write(OMI@SSBy,datfile,1,append=T) # CV on SSB prior in current model year

  write("# SSBincstock, stock for SSBinc ratio calculation",datfile,1,append=T)
  write(OMI@SSBincstock,datfile,1,append=T) # CV on SSB prior in current model year

  write("# BSfrac, mixing assumptions the proportion of E/W stock biomass in W/E areas",datfile,1,append=T)
  write(t(OMI@BSfrac),datfile,4,append=T) # CV on SSB prior in current model year

  write("# FCV, prior precision of deviations from mean F from master index x q",datfile,1,append=T)
  write(OMI@FCV,datfile,1,append=T) # CV on SSB prior in current model year

  write("# movCV, prior precision of deviations from homogeneous movement",datfile,1,append=T)
  write(OMI@movCV,datfile,1,append=T) # CV on SSB prior in current model year

  write("# selCV, prior precision of mean selectivity parameters ",datfile,1,append=T)
  write(OMI@selCV,datfile,1,append=T) # CV on SSB prior in current model year

  write("# SSBincCV, precision of specified prior on SSB ratio (SSBinc, SSBy, SSBincstock)",datfile,1,append=T)
  write(OMI@SSBincCV,datfile,1,append=T) # CV on SSB prior in current model year

  write("# BSfracCV, precision of mixing prior E/W stock biomass in W/E area",datfile,1,append=T)
  write(OMI@BSfracCV,datfile,1,append=T) # CV on SSB prior in current model year

  # -- Likelihood weights

  write("# nLHw, number of likelihood components",datfile,1,append=T)
  write(OMI@nLHw,datfile,1,append=T)

  write("# LHw,  likelihood components",datfile,1,append=T)
  write(OMI@LHw,datfile,OMI@nLHw,append=T) # Likelihood weights (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA penalty, 12 SSB prior, 13 SSBinc, 14 Fmod, 15 BSfrac)

  # -- Initial values

  write("# muR_ini, initial values for mean historical recruitment",datfile,1,append=T)
  write(OMI@muR_ini,datfile,OMI@np,append=T) # Simulated R0 for each population

  write("# sel_ini, initial values for selectivity",datfile,1,append=T)
  write(t(OMI@sel_ini),datfile,OMI@nl,append=T) # Actual selectivity

  write("# selpar_ini, initial values for selectivity parameters",datfile,1,append=T)
  write(t(OMI@selpar_ini),datfile,3,append=T) # Actual selectivity

  write("# lnF_ini, initial values for log F",datfile,1,append=T)
  write(OMI@lnF_ini,datfile,OMI@nCobs,append=T) # log apical F

  write("# lnRD_ini, recruitment deviations y=1:nyears",datfile,1,append=T)
  write(t(OMI@lnRD_ini),datfile,OMI@ny,append=T) # Recruitment deviations

  write("# mov_ini, simulated movement p s r r",datfile,1,append=T)
  write(OMI@mov_ini,datfile,OMI@nr,append=T) # Movement probabilities

  write("# qCPUE_ini, initial values for CPUE catchability nCPUE",datfile,1,append=T)
  write(log(OMI@qCPUE_ini),datfile,OMI@nCPUEq,append=T)

  write("# lnqI_ini, initial values for fishery independent catchability nI",datfile,1,append=T)
  write(log(OMI@qI_ini),datfile,OMI@nI,append=T) # Catchabilities I=qSSB or I=qB

  write("# D_ini, reference (simulated) depletion",datfile,1,append=T)
  write(OMI@D_ini,datfile,OMI@np,append=T) # Catchabilities I=qSSB or I=qB

  # -- Misc

  write("# complexRD 1= run with full estimation of all recruitment deviations by year",datfile,1,append=T)
  write(OMI@complexRD,datfile,1,append=T) # debug switch

  write("# complexF 1= run with full estimation of all F's by year, subyear, fleet, region",datfile,1,append=T)
  write(OMI@complexF,datfile,1,append=T) # debug switch

  write("# nF either nCobs or 1 if complexF=0",datfile,1,append=T)
  write(OMI@nF,datfile,1,append=T) # debug switch

  #write("# nMPind",datfile,1,append=T)
  #write(nrow(OMI@MPind),datfile,1,append=T)

  #write("# MPind",datfile,1,append=T)
  #write(t(OMI@MPind),datfile,7,append=T)

  write("# debug 1= run with initial values",datfile,1,append=T)
  write(OMI@debug,datfile,1,append=T) # debug switch

  write("# verbose 1= run with printouts",datfile,1,append=T)
  write(OMI@verbose,datfile,1,append=T) # debug switch

  write("# datacheck",datfile,1,append=T)
  write(OMI@datacheck,datfile,1,append=T) # datacheck

}

runM3p1<-function(x,OMdir='C:/M3temp',hess=F){
  setwd(paste0(OMdir,"/",x))
  if(hess){
    system("M3.exe",wait=T,show.output.on.console = F)
    return(paste("M3 ran with hessian calculation at",OMdir[x]))
  }else{
    system("M3.exe -est",wait=T,show.output.on.console = F)
    return(paste("M3 ran at",OMdir[x]))
  }

}


#' Run an M3 operating model (a multistock spatial seasonal statistical catch at length model)
#'
#' @param x the position in the vector OMdir (one of the folders containing the M3 exe)
#' @param OMdir a character string representing the location of an M3 folder containing M3.exe, M3.dat and stats.cxx files
#' @return runs the M3 model to create standardized ADMB reporting in folder \code{OMdir}
#' @examples
#' #runM3p(1,"C:/ABT-MSE/M3")
runM3p<-function(x,OMdir='C:/ABT-MSE/M3',hess=F,mcmc=F, nits=10000,thin=40){

  curdir<-paste0(OMdir,"/",x)
  setwd(curdir)

  if(hess&!mcmc){
    system("M3.exe",wait=T,show.output.on.console = F)
    return(paste("M3 ran with hessian calculation at",curdir))
  }else if(!mcmc){
    system("M3.exe -est",wait=T,show.output.on.console = F)
    return(paste("M3 ran at",curdir))
  }else{
    system(paste0("M3.exe -mcmc ",nits," -mcsave ",thin),wait=T,show.output.on.console = F)
    system("M3.exe -mceval",wait=T,show.output.on.console = F)
    return(paste("M3 ran with mcmc calculation at",curdir))
  }

}

runM3mcmcp<-function(x,OMdir='C:/ABT-MSE/M3'){
  curdir<-paste0(OMdir,"/",x)
  setwd(curdir)
  system(paste0("M3.exe -mceval"),wait=T,show.output.on.console = F)
  return(paste("M3 ran with mcmc calculation at",curdir))
}



#' Run an M3 operating model (a multistock spatial seasonal statistical catch at length model)
#'
#' @param OMdir a character string representing the location of an M3 folder containing M3.exe, M3.dat and stats.cxx files
#' @return runs the M3 model to create standardized ADMB reporting in folder \code{OMdir}
#' @examples
#' #runM3("C:/ABT-MSE/M3")
runM3<-function(OMdir='C:/ABT-MSE/M3',hess=F,mcmc=F, nits=40000,thin=100){
  setwd(OMdir)

  if(hess&!mcmc){
    system("M3.exe",wait=T,show.output.on.console = F)
    return(paste("M3 ran with hessian calculation at",OMdir[x]))
  }else if(!mcmc){
    system("M3.exe -est",wait=T,show.output.on.console = F)
    return(paste("M3 ran at",OMdir))
  }else{
    system(paste0("M3.exe -mcmc ",nits," -mcsave ",thin),wait=T,show.output.on.console = F)
    system("M3.exe -mceval",wait=T,show.output.on.console = F)
    return(paste("M3 ran with mcmc calculation at",OMdir))
  }

}


#' Reads the outputs of an M3 model fit into a list
#'
#' @param OMdir a character string representing the location of an M3 folder containing M3.rep, M3.par and M3.cor files
#' @return runs a list object of M3 outputs from folder \code{OMdir}
#' @examples
#' M3read("C:/ABT-MSE/M3")
M3read<-function(OMDir="C:/M3",quiet=T){

  out<-list()

  repfile<-paste(OMDir,"/M3.rep",sep="")
  out$np<-scan(repfile,skip=1,nlines=1,quiet=quiet)
  out$nHy<-scan(repfile,skip=3,nlines=1,quiet=quiet)
  out$ny<-scan(repfile,skip=5,nlines=1,quiet=quiet)
  out$ns<-scan(repfile,skip=7,nlines=1,quiet=quiet)
  out$nr<-scan(repfile,skip=9,nlines=1,quiet=quiet)
  out$nf<-scan(repfile,skip=11,nlines=1,quiet=quiet)
  out$na<-scan(repfile,skip=13,nlines=1,quiet=quiet)
  out$nl<-scan(repfile,skip=15,nlines=1,quiet=quiet)
  out$nma<-scan(repfile,skip=17,nlines=1,quiet=quiet)

  np<-out$np
  nHy<-out$nHy
  ny<-out$ny
  ns<-out$ns
  nr<-out$nr
  nf<-out$nf
  na<-out$na
  nl<-out$nl
  nma<-out$nma

  st<-19
  #out$ma<-ADMBrep(repfile,st,ADMBdim=c(np,na),quiet=quiet)
  #st<-st+1+np
  out$SSB<-ADMBrep(repfile,st,ADMBdim=c(np,ny,ns),quiet=quiet)
  st<-st+1+np*ny
  out$hSSB<-ADMBrep(repfile,st,ADMBdim=c(np,nHy,ns),quiet=quiet)
  st<-st+1+np*nHy
  out$FL<-ADMBrep(repfile,st,c(ny,ns,nr,nf,nl),quiet=quiet)
  st<-st+1+ny*ns*nr*nf
  out$HCobs<-ADMBrep(repfile,st,c(nHy,ns,na,nr),quiet=quiet)
  st<-st+1+nHy*ns*na
  out$nCobs<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$Cobs<-ADMBrep(repfile,st,c(out$nCobs,5),quiet=quiet)
  st<-st+1+out$nCobs
  out$Cpred<-ADMBrep(repfile,st,c(ny,ns,nr,nf),quiet=quiet)
  st<-st+1+ny*ns*nr
  out$nCLobs<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$CLobs<-ADMBrep(repfile,st,c(out$nCLobs,6),quiet=quiet)
  st<-st+1+out$nCLobs
  out$CLtotpred<-ADMBrep(repfile,st,c(ny,ns,nr,nf,nl),quiet=quiet)
  st<-st+1+ny*ns*nr*nf
  out$VL<-ADMBrep(repfile,st,c(ny,ns,nr,nf,nl),quiet=quiet)
  st<-st+1+ny*ns*nr*nf
  out$ma<-ADMBrep(repfile,st,c(np,na),quiet=quiet)
  st<-st+1+np
  out$mov<-ADMBrep(repfile,st,c(np,ns,na,nr,nr),quiet=quiet)
  st<-st+1+np*ns*na*nr
  out$sel<-ADMBrep(repfile,st,c(nf,nl),quiet=quiet)
  st<-st+1+nf
  out$RAI<-ADMBrep(repfile,st,c(nr,ns,ny),quiet=quiet)
  st<-st+1+nr*ns
  out$ml<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$VB<-ADMBrep(repfile,st,c(ny,ns,nr,nf),quiet=quiet)
  st<-st+1+ny*ns*nr
  out$B<-ADMBrep(repfile,st,c(ny,ns,nr),quiet=quiet)
  st<-st+1+ny*ns
  out$N<-ADMBrep(repfile,st,c(np,ny,ns,na,nr),quiet=quiet)
  st<-st+1+np*ny*ns*na
  out$lwa<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lwb<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$len_age<-ADMBrep(repfile,st,c(ny,na,np),quiet=quiet)
  st<-st+1+ny*na
  out$wt_age<-ADMBrep(repfile,st,c(ny,na,np),quiet=quiet)
  st<-st+1+ny*na
  out$nMP<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$nmovind<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$movind<-ADMBrep(repfile,st,c(out$nmovind,5),quiet=quiet)
  st<-st+1+out$nmovind
  out$nmov1<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$mov1<-ADMBrep(repfile,st,c(out$nmov1,5),quiet=quiet)
  st<-st+1+out$nmov1
  out$movtype<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$Ilencat<-ADMBrep(repfile,st,c(5,2),quiet=quiet)
  st<-st+6
  out$M_age<-ADMBrep(repfile,st,c(np,na),quiet=quiet)
  st<-st+1+np
  #out$h<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  #st<-st+2
  out$RDblock<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  fec<-ADMBrep(repfile,st,c(np,na),quiet=quiet)
  out$mat_age<-fec/t(out$wt_age[1,,]) # for some UNEXPLAINED reason this also assigns this to out$ma (pointer?)
  st<-st+1+np
  out$nsel<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$seltype<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$selind<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$spawns<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$iALK<-ADMBrep(repfile,st,c(np,ny,na,nl),quiet=quiet)
  st<-st+1+np*ny*na
  out$lnqE<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnqI<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnqCPUE<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$hZ<-ADMBrep(repfile,st,c(np,nHy,ns,na,nr))
  st<-st+1+np*nHy*ns*na
  out$Ipred<-ADMBrep(repfile,st,c(ny,ns,nr,np))
  st<-st+1+ny*ns*nr
  #out$nZeq<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  #st<-st+2
  out$nydist<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  #out$nyeq<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  #st<-st+2
  out$SSB0<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$muR<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnHR1<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnHR2<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$nRD<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnRD<-ADMBrep(repfile,st,c(np,out$nRD))
  st<-st+1+np
  out$D_ini<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$D<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$Dt<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$SSBnow<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$objnam<-c("Model","Global","Catch","CPUE","Ind.","Length","SOO","PSAT","P Rec.","P mov","P sel","SRA pen","P SSB","P Fmod","P ratio")
  #st<-st+1
  out$obj<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$datacheck<-scan(repfile,skip=st,nlines=1,quiet=quiet)

  opt<-SRopt(out,plot=F)
  out$h<-opt$h
  out$R0<-opt$R0
  out$logith_lnR0_VC<-opt$VC
  #out$h_logit<-h_est[,2:3]
  out
}

#' Read the outputs of an ADMB model run
#'
#' @param file a character string representing the location of ADMB model outputs (.par, .cor, .rep)
#' @return a list of model parameter estimates and a hessian matrix in folder \code{file}
#' @author this is code that was originally written by Anders Nielsen
#' @examples
#' #read.fit("C:/ABT-MSE/M3")
read.fit<-function(file="C:/M3"){
  # ! Borrowing some of Anders Nielsen's code !
  # Function to read a basic AD Model Builder fit.
  # Use for instance by:
  # simple.fit <- read.fit('c:/admb/examples/simple')
  # M3.fit<-read.fit("C:/M3/M3")
  # Then the object 'simple.fit' is a list containing sub?objects
  # 'names', 'est', 'std', 'cor', and 'cov' for all model
  # parameters and sdreport quantities.
  #
  ret<-list()
  #parfile<-as.numeric(scan(paste(file,'.par', sep=''),
  #                         what='', n=16, quiet=TRUE)[c(6,11,16)])
  parf<-paste(file,"M3.par",sep="/")
  parfile<-as.numeric(scan(parf, what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nlogl<-parfile[2]
  ret$maxgrad<-parfile[3]
  cfile<-paste(file,'M3.cor', sep='/')
  if(file.exists(cfile)){

    lin<-readLines(cfile)
    ret$npar<-length(lin)-2
    ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
    sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
    ret$names<-unlist(lapply(sublin,function(x)x[2]))
    ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
    ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
    ret$cor<-matrix(NA, ret$npar, ret$npar)
    corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
    ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
    ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
    ret$cov<-ret$cor*(ret$std%o%ret$std)

  }else{
    cat(paste0("No .cor file: ",cfile," is not available, storing MLE parameter estimates only"))
    cat("\n")
    lin<-readLines(parf)
    sublin<-lapply(strsplit(lin, ' '),function(x)x[x!=''&x!="#"])
    parnams<-list()
    parind<-list() # indexing of where parnames are stored so values can be taken in the interim
    est<-list()
    j=0

    for(i in 1:length(sublin)){

      if(grepl(":",sublin[[i]][1])){

        j<-j+1
        parnams[[j]]<-unlist(strsplit(sublin[[i]][1],":"))
        firstvals=TRUE

      }else if(j>0){

        if(firstvals){
          est[[j]]<-as.numeric(sublin[[i]])
        }else{
          est[[j]]<-c(est[[j]],as.numeric(sublin[[i]]))
        }
        firstvals=FALSE

      }

    }

    ret$names<-rep(unlist(parnams),lapply(est,function(x)length(x)))
    ret$est<-unlist(est)
    ret$cov<-NA

  }
  #fileo<-strsplit(file,"/")
  #ret$hes<-read.hessian(file)#read.hessian(paste(c(fileo[[1]][1:(length(fileo[[1]])-1)],""),collapse='/'))
  return(ret)
}

pin_from_cor<-function(file="C:/M3/M3",pinfile=NA){
  #pin_from_cor(file="C:/M3/M3")
  ret<-read.fit(file)
  est<-matrix(ret$est,ncol=1)
  if(is.na(pinfile))write.table(est,file=paste(file,".pin",sep=""),col.names=F,row.names=F)
  if(!is.na(pinfile))write.table(est,file=pinfile,col.names=F,row.names=F)
}


#' Create an M3 parameter initialization file (.pin) from the MLE estimates (.par) of a previous run
#'
#' @param OMdir a character string representing the location of an M3 par file
#' @return wrotes an M3.pin file in \code{OMdir}
#' @examples
#' #pin_from_par("C:/ABT-MSE/M3")
pin_from_par<-function(OMdir="C:/M3",pinfile=NA){
  file<-paste0(OMdir,"/M3")
  #pin_from_cor(file="C:/M3/M3")
  f <- file(paste(file,".par",sep=""), open="rb")
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
    nlines <- nlines + sum(chunk == as.raw(10L))
  }

  parfile<-paste(file,".par",sep="")
  est<-numeric()
  for(l in 2:nlines){
    test<-read.table(parfile,skip=l-1,nrows=1,comment.char="")
    if(test[1]!="#"){
      est<-c(est,as.numeric(test))
    }
  }
  close(f)
  est<-matrix(est,ncol=1)
  if(is.na(pinfile))write.table(est,file=paste(file,".pin",sep=""),col.names=F,row.names=F)
  if(!is.na(pinfile))write.table(est,file=pinfile,col.names=F,row.names=F)
}



#' Plot area definitions of an OMI, OM or MSE object
#'
#' @param .Object an object of class OMI, OM or MSE
#' @return a map of the areas of \code{classy}
#' @examples
#' loadABT()
#' areaplot(OM_1)
#' areaplot(OMI_1)
#' areaplot(MSE_example)
read.hessian<-function(file="C:/M3"){

  filen <- file(paste(file,"admodel.hes",sep="/"), "rb")
  nopar <- readBin(filen, what = "integer", n = 1)
  hes <- readBin(filen, what = "double", n = nopar * nopar)
  hes <- matrix(hes, byrow = TRUE, ncol = nopar)
  close(filen)

  f <- file(paste(file,"M3.par",sep="/"), open="rb")
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
    nlines <- nlines + sum(chunk == as.raw(10L))
  }

  parfile<-paste(file,"M3.par",sep="/")
  parname<-character()
  for(l in 2:nlines){
    test<-read.table(parfile,skip=l-1,nrows=1,comment.char="")
    if(test[1]=="#"){
      partext<-as.character(test[1,2])
    }else{
      parname<-c(parname,paste(partext,1:length(test)))
    }
  }

  close(f)
  colnames(hes) <- rownames(hes) <- parname
  hes

}



make_fit_reports<-function(dirs="C:/M3",addlab=FALSE) {

  nOMs<-length(dirs)
  Obs<-Bad_Obs #load(paste0(getwd(),"/Objects/Observation_models/Bad_Obs"))

  for(i in 1:nOMs){

    input_dir<-dirs[i]

    if(!file.exists(input_dir)){
      print(paste('Directory:',input_dir,'does not exist'))
      stop()
    }

    out<-M3read(input_dir)
    load(paste(input_dir,"/OMI",sep=""))
    load(system.file("ts2017.Rdata", package="ABTMSE"))
    dat<-ts2017
    #getM3res(out,outdir=input_dir,firstyr=1960,fleetnams=c(OMI@Fleets$name,'ALL OTH'),   areanams=OMI@areanams)
    #plotM3fit(out,outdir=input_dir,firstyr=1960, fleetnams=c(OMI@Fleets$name,'ALL OTH'), areanams=OMI@areanams)
    if(!addlab){
      render(input=system.file("OMreport.Rmd", package="ABTMSE"), output_file=paste(input_dir,"/Report.pdf",sep=""))
      print(paste0("Report written: ", paste(input_dir,"/Report.pdf",sep="")))
    }else{
      split<-strsplit(input_dir,"/")[[1]]
      lab<-split[length(split)]
      render(input=system.file("OMreport.Rmd", package="ABTMSE"), output_file=paste(input_dir,"/Report_",lab,".pdf",sep=""))
      print(paste0("Report written: ", paste(input_dir,"/Report_",lab,".pdf",sep="")))
    }

  }
}




make_summary_report<-function(dir,OMdirs=NA){

  if(!file.exists(dir)){
    print(paste('Directory:',dir,'does not exist'))
    stop()
  }

  designdir<-paste0(dir,"/Design.Rdata")
  if(!file.exists(designdir)){
    print(paste('Object:',designdir,'is required to build the summary table'))
    stop()
  }

  load(designdir)

  if(is.na(OMdirs[1])){
    OMdirs<-list.dirs(dir)
    OMdirs<-OMdirs[2:length(OMdirs)]
    nOMs<-length(OMdirs)
    fileind<-rep(NA,nOMs)
    foldnams<-strsplit(OMdirs,"/")
    lastfolder<-length(foldnams[[1]])
    lfnams<-unlist(lapply(foldnams,FUN=function(X)X[lastfolder]))
    OMdirs<-OMdirs[order(as.numeric(lfnams))]

  }
  nOMs<-length(OMdirs)
  #render(input="Source/OMsummary.Rmd", output_file=paste(dir,"/Summary report.pdf",sep=""))
  render(input=system.file("OMsummary.Rmd", package="ABTMSE"), output_file=paste(dir,"/Summary_Report.pdf",sep=""))

}

make_comp_report<-function(OMdirs,dir){
  load(system.file("ts2017.Rdata", package="ABTMSE"))
  dat<-ts2017

  nOMs<-length(OMdirs)
  #render(input="Source/OMsummary.Rmd", output_file=paste(dir,"/Summary report.pdf",sep=""))
  render(input=system.file("OMcomp.Rmd", package="ABTMSE"), output_file=paste(dir,"/Comparison_Report.pdf",sep=""))

}

