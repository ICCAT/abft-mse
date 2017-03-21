
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a base operating model that can be modified for each of trial specifications
# The basic dimensionality / structure of this OM is the same for all trial specifications
# ie the fleets, areas, times and data are the same

# Tom Carruthers UBC
# Laurie Kell ICCAT

# 8th August 2016


# === Outstanding issues ==============

# iALK is currently made up!
# Area size in q modification in M3
# need to put the new indices in the model
# need to rename the slots of the OMI object
# need to recalculate depletion SSB/SSB0 and B/B0 inside model due to new SRA features
# add absolute abundance prior and slots

# === Main script =====================

# --- Set working directory ------

setwd("C:/ABT-MSE/")


# --- Source MSE functions and objects ------
library(ABTMSE)
#source("Source/MSE_source.r")
#source("Source/Objects.r")


# --- Define dimensions ------

Base<-new('OMI')
Base@Date<-as.character(Sys.time())
Base@Author<-"T. Carruthers (t.carruthers@oceans.ubc.ca)"
Base@Notes<-"This object serves as a framework for populating the various Trial Specifications"
Base@PrimarySource<-"ICCAT Bluefin data prep. meeting July 2016"
Base@years<-years<-c(1960,2014)
Base@Hyears<-Hyears<-c(1864,1959)


# --- (a) Create some prespecified areas (polygons with lons and lats) ------

source("RScripts/Data processing/Area definitions.r") # Creates objects: AreaNames, AreaDefs


# --- Define areas ------

Base@areas<-areas<-c("GOM_11","CAR_11","WATL_11","GSL_11","SCATL_11", "NCATL_11","NEATL_11", "EATL_11", "SEATL_11", "MED_11")
Base@areanams<-areanams<-c("GOM","CAR","WATL","GSL","SCATL","NCATL","NEATL","EATL","SEATL","MED")
Base@area_defs<-new('list')
for(i in 1:length(Base@areas))Base@area_defs[[i]]<-AreaDefs[[match(areas[i],AreaNames)]]


# --- (b) Define fleets ------

source("Rscripts/Data processing/Define fleets.r") # Returns a list Fleets

Base@Fleets<-Fleets


# --- Length bin set up ------

Base@lenbins<-(1:16)*25                         # 25 cm length bins
nlen<-length(Base@lenbins)-1
Base@mulen<-(Base@lenbins[1:nlen]+Base@lenbins[2:(nlen+1)])/2


# --- Assign known dimensions ------

Base@nHy<-as.integer(Hyears[2]-Hyears[1]+1)       # No. historical years
Base@ny<-as.integer(years[2]-years[1]+1)          # No. years
Base@ns<-as.integer(4)                            # No. subyears
Base@np<-as.integer(2)                            # No. stocks
Base@na<-as.integer(18)                           # No. ages
Base@nr<-as.integer(length(areas))                # No. areas
Base@nf<-as.integer(length(Base@Fleets$name)+1)   # No. fleets
Base@nl<-as.integer(nlen)


# --- Recapture indexing by subyear

Base@nRPT<-as.integer(2) # recaptures are only calculated in consecutive timesteps
temp<-rep(1:Base@ns,ceiling(Base@nRPT/Base@ns)+Base@ns)
Base@RPTind<-array(NA,c(Base@ns,Base@nRPT))
for(ss in 1:Base@ns)Base@RPTind[ss,]<-temp[ss:(ss+Base@nRPT-1)]


# --- Misc ------

Base@sdur<-rep(1/Base@ns,Base@ns) # the duration of each subyear - we make these equal
#Base@nZeq<-as.integer(40)
Base@nydist<-as.integer(10)
#Base@nyeq<-as.integer(15)
yblock<-3 # the duration of recruitment deviation blocks
Base@RDblock<-rep(1:100,each=yblock)[1:Base@ny]
Base@nRD<-max(Base@RDblock)


# --- (c) Create iALK ------

Base@lwa<-c(2.95*10^-5,1.96*10^-5)   # length-weight conversion w=al^b
Base@lwb<-c(2.899,3.009)             # length-weight conversion w=al^b
Base@L1<-c(33.0,33.0)      # Richards growth curve
Base@L2<-c(270.6,270.6)    # Richards growth curve
Base@K<-c(0.22,0.22)       # Richards growth curve
Base@p<-c(0.97,0.97)       # Richards growth curve

source("RScripts/Data processing/iALK.r") # returns len_age wt_age iALK

Base@iALK<-iALK
Base@wt_age<-wt_age
Base@len_age<-len_age

# Fecundity / maturity / recruitment / mortality

#Base@mat<-array(rep(c(0,0,0,0.25,0.5, 1,rep(1,Base@na-6)),each=Base@np),c(Base@np,Base@na,Base@ny))
Base@mat<-array(t(array(c(rep(0,5),rep(1,Base@na-5),
                    rep(0,9),rep(1,Base@na-9)),c(Base@na,Base@np))),c(Base@np,Base@na,Base@ny))
Base@Fec<-Base@mat[,,1]*Base@wt_age[,,1]
#Base@steep<-c(0.7,0.7)
Base@spawns<-c(2,2)
Base@canspawn<-matrix(c(0,0,0,0,0,0,0,0,0,1,  1,0,1,0,0,0,0,0,0,0),ncol=Base@np) # matrix of spawning areas 1=can spawn

#OldM<-c(0.49,rep(0.24,4),0.2,0.175,0.15,0.125,rep(0.10,Base@na-9)) # P, A  mean M at age
#Oldsurv<-exp(-cumsum(c(0,OldM)))[1:Base@na]
#sum(Oldsurv)

#Base@Ma<-c(0.8318,0.864)*wt_age[,,Base@ny]^-0.288
Base@Ma<-t(array(c(0.49,rep(0.24,4),0.2,0.175,0.15,0.125,rep(0.10,Base@na-9),
        rep(0.14,Base@na)),c(Base@na,Base@np)))
#surv<-exp(-t(rbind(c(0,0),apply(Base@Ma,1,cumsum)[1:(Base@na-1),])))
#apply(surv,1,sum)
#round(Base@Ma[2,],2)



# --- (d) Process CATDIS data ------

source("RScripts/Data processing/CATDIS.r") # returns Cobs in the right format

Base@Cobs<-as.matrix(Cobs)
Base@nCobs<-nrow(Cobs)


# --- (e) Calculate master indices ------

source("Rscripts/Data processing/Master index.r") # returns list MI: MI[[1]] 0% efficiency increase MI[[2]] 2% efficiency increase MI[[3]] minus MA TP plus CAN LL 0% efficiency increase MI[[4]] minus MA TP plus CAN LL 2% efficiency increase

Base@RAI=MI[[1]]


# --- (f) Calculate partial F's ------

source("Rscripts/Data processing/Partial Fs.r") # returns CPUEobs

Base@nE<-Base@nf # same number of cpue indices (if not using master index) as fleets
Base@nEobs<-nrow(Eobs)
Base@Eobs<-Eobs


# --- (g) Process assessment indices ----

source("Rscripts/Data processing/Fishery indices.r") # returns CPUEqvec and CPUEobs in the right format

Base@nCPUEq<-max(CPUEobs$qNo)
Base@nCPUEobs<-nrow(CPUEobs)
Base@CPUEobs<-as.matrix(CPUEobs)
Base@CPUEnames<-CPUEnames

# --- (h) Calculate length sample data ------

source("Rscripts/Data processing/Length observations.r") # returns CLobs

Base@nCLobs<-nrow(CLobs)
Base@CLobs<-as.matrix(CLobs)


# --- (i) Calculate historical catches ------

source("Rscripts/Data processing/Historical catches.r") # returns HCobs

Base@HCobs<-HCobs # a 4D array y x s x a x  r


# --- (j) Fishery independent indices ------

source("Rscripts/Data processing/FI indices.r") # returns Iobs

Base@nI<-as.integer(max(Iobs[,5])) # number of series
Base@nIobs<-nrow(Iobs) # number of data
Base@Iobs<-Iobs   # y s r i type(biomass/ssb) index


# --- (k) PSAT tags ------

Base@nma<-as.integer(3)
Base@ma<-c(rep(1,4),rep(2,4),rep(3,Base@na-8))

source("Rscripts/Data processing/PSAT.r") # returns a table of electronic tag tracks of known (PSAT) and unknown (PSAT2) stock of origin

Base@nPSAT<-nrow(PSAT)
Base@PSAT<-PSAT
Base@nPSAT2<-nrow(PSAT2)
Base@PSAT2<-PSAT2


# --- Conventional tags ------

Base@nTag<-as.integer(1) # currently this is placeholder for conventional tags that are ignored by M3
Base@Tag<-array(c(2,1,7,1,2,2,7,1,2,1),c(1,10))


# --- (l) Stock of origin observations ------

ma<-array(rep(c(rep(1,3),rep(2,5),rep(3,Base@na-8)),each=Base@np),c(Base@np,Base@na))
nma<-max(ma)

source("Rscripts/Data processing/SOO.r")

Base@nSOOobs<-nrow(SOOobs)
Base@SOOobs<-SOOobs


# --- Selectivities ------

Base@nsel<-Base@nf

Base@seltype<-c(rep(3,Base@nf-1),2) # all fleets have thompson (potentially) dome-shaped selectivity except the combined other fleet
Base@seltype[Fleets$gearTC=="LL"]<-2 # set longline to asymptotic
Base@seltype[Fleets$gearTC=="RR"]<-2 # set rod and reel to asymptotic
Base@seltype[c(8,10)]<-2 # PSwestold TPwestold are currently estimated to be asymptotic..

Base@selind<-1:Base@nf # No selectivity mirroring - selectivities correspond to fleets
Base@ratiolim<-c(0.1,0.4) # limits on the logistic slope paramter relative to position of inflection point
Base@infleclim<-c(4,15) # limits on the location of the inflection point (age)


# --- (m) Movement estimation and definition ------

Base@movtype<-as.integer(1)

source("Rscripts/Data processing/Movement definitions.r") # returns movind and mov1

Base@nMP<-nrow(movind)+Base@ns*Base@np*Base@nma
Base@nmovind<-nrow(movind)
Base@movind<-movind
Base@nmov1<-nrow(mov1)
Base@mov1<-mov1


# --- Relating to likelihood functions ------

Base@CobsCV<-rep(0.2,Base@nf)         # CV on seasonal catch observations by area
Base@CPUEobsCV<-rep(0.1,Base@nCPUEq) # CV on seasonal CPUE observations by area
Base@IobsCV<-rep(0.25,Base@nI)        # CV on fishery independent indices
Base@RDCV<-2/(Base@ny/Base@nRD)^0.5   # CV for penalty on recruitment deviations (if blocked this is Std. Err.)
Base@SSBprior=c(1,1)                  # dummy prior for SSB (some operating models use fractions of other model estimated current SSB)
Base@SSBCV=0.01                       # default is a very tight prior on SSB
Base@nLHw<-as.integer(12)             # number of likelihood components that may be weighted
#          (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB )",datfile,1,append=T)
Base@LHw<-c(1/200,   1/50 ,  1,         1/3000,   50,    1,      1,       5,        2,     1,      20,     0      ) # SSB index for each population


# --- Initial Values ------

Base@muR_ini<-c(350*8,350)*5000
Base@sel_ini<-t(array(c(0,0,(1:4)/4,rep(1,Base@nl-6)),c(Base@nl,Base@nf)))
Base@selpar_ini<-t(array(c(-5,0,-1),c(3,Base@nf)))
Base@selpar_ini[match('LLJPN',Base@Fleets$name),]<- c(1,-1,99)# LL  - logistic - mode and sd are 0.26
Base@lnF_ini<-rep(log(0.001),nrow(Base@Cobs))
Base@lnRD_ini<-t(array(seq(-0.1,0.1,length.out=Base@ny),c(Base@ny,Base@np)))
Base@mov_ini<-tomt(array(1/Base@nr,c(Base@np,Base@ns,Base@na,Base@nr,Base@nr)))
Base@qCPUE_ini<-rep(1,Base@nCPUEq)
Base@qI_ini<-rep(1,Base@nI)
Base@D_ini<-c(sum(Base@RAI[,2,Base@canspawn[,1]==1][(Base@ny-2):Base@ny])/sum(Base@RAI[,2,Base@canspawn[,1]==1][1:3]),sum(Base@RAI[,2,Base@canspawn[,2]==1][(Base@ny-2):Base@ny])/sum(Base@RAI[,2,Base@canspawn[,2]==1][1:3]))# just for comparison with simulations
Base@complexRD<-as.integer(0)
Base@complexF<-as.integer(0)
Base@nF<-as.integer(1)
Base@MPind<-read.csv("Data/Processed/MP Indices/MP indices compiled.csv")
Base@nMPind<-nrow(Base@MPind)
Base@debug<-as.integer(0)
Base@verbose<-as.integer(1)
Base@datacheck<-as.integer(99999)


# Naming the Base operating model according to the various OM factors at level 1 -------

Base@Name<-"Comp1: Comparison with 2014 assessments"#paste(c("Base OM:",Names_Base),collapse=" ")
Base@OMfactors<-list("Mimicking","the", "2014 assessments")


# Save the base OMI  ------

OMI<-Base
OMI<-MatM_Ref(OMI,1)
M3write(OMI,OMdir=paste0(getwd(),"/M3"))  # Store this base operating model in the M3 operating model (precalculation of initial values if desired)
#M3write(OMI,datfile="C:/M3/M3.dat")  # Store this base operating model in the M3 operating model (precalculation of initial values if desired)

save(OMI,file=paste(getwd(),"/Objects/OMs/Base_OM",sep=""))
save(OMI,file=paste(getwd(),"/M3/OMI",sep=""))


# ==== END =======================================================================================



