
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
# Maturity is currently made up!
# Growth is currently made up!
# Mortality is currently made up!
# Area size in q modification in M3
# Length comp data are missing for two fleets USA RR and Late PS east


# === Main script =====================

# --- Set working directory ------

setwd("E:/ABT-MSE/")

# --- Source MSE functions and objects ------

source("Source/MSE_source.r")
source("Source/Objects.r")

# --- Define dimensions ------

Base<-new('OMI')
Base@Name<-"Base operating model"
Base@Date<-as.character(Sys.time())
Base@Author<-"T. Carruthers (t.carruthers@oceans.ubc.ca)"
Base@Notes<-"This object serves as a framework for populating the various Trial Specifications"
Base@PrimarySource<-"ICCAT Bluefin data prep. meeting July 2016"
Base@years<-years<-c(1960,2014) 

# --- Create some prespecified areas (polygons with lons and lats) ------

source("RScripts/Data processing/Area definitions.r") # Creates objects: AreaNames, AreaDefs

# --- Define areas ------

Base@areas<-areas<-c("GOM_11","CAR_11","WATL_11","GSL_11","SCATL_11", "NCATL_11","NEATL_11", "EATL_11", "SEATL_11", "MED_11")
Base@areanams<-areanams<-c("GOM","CAR","WATL","GSL","SCATL","NCATL","NEATL","EATL","SEATL","MED")
Base@area_defs<-new('list')
for(i in 1:length(Base@areas))Base@area_defs[[i]]<-AreaDefs[[match(areas[i],AreaNames)]]

# --- Define fleets ------

source("Rscripts/Data processing/Define fleets.r") # Returns a list Fleets

Base@Fleets<-Fleets

# --- Length bin set up ------

Base@lenbins<-(1:20)*20
nlen<-length(Base@lenbins)-1
Base@mulen<-(Base@lenbins[1:nlen]+Base@lenbins[2:(nlen+1)])/2

# --- Assign known dimensions ------

Base@ny<-as.integer(years[2]-years[1]+1)           # No. years
Base@ns<-as.integer(4)                             # No. subyears
Base@np<-as.integer(2)                             # No. stocks
Base@na<-as.integer(35)                            # No. ages
Base@nr<-as.integer(length(areas))              # No. areas
Base@nf<-as.integer(length(Base@Fleets$name)+1) # No. fleets
Base@nl<-as.integer(nlen)

# --- Recapture indexing by subyear

Base@nRPT<-as.integer(2) # recaptures are only calculated in consecutive timesteps
temp<-rep(1:Base@ns,ceiling(Base@nRPT/Base@ns)+Base@ns)
Base@RPTind<-array(NA,c(Base@ns,Base@nRPT))
for(ss in 1:Base@ns)Base@RPTind[ss,]<-temp[ss:(ss+Base@nRPT-1)]

# --- Misc ------

Base@sdur<-rep(1/Base@ns,Base@ns) # the duration of each subyear - we make these equal
Base@nZeq<-as.integer(5)
Base@nydist<-as.integer(100)
Base@nyeq<-as.integer(10)
yblock<-5 # the duration of recruitment deviation blocks
Base@RDblock<-rep(1:100,each=yblock)[1:Base@ny]
Base@nRD<-max(Base@RDblock)

# --- Create iALK ------

Base@lwa<-c(2.95*10^-5,1.96*10^-5)   # length-weight conversion w=al^b   
Base@lwb<-c(2.899,3.009)             # length-weight conversion w=al^b   
Base@L1<-c(32.43,32.43)    # Richards growth curve
Base@L2<-c(263.64,263.64)  # Richards growth curve
Base@K<-c(0.26,0.26)       # Richards growth curve
Base@p<-c(0.97,0.97)       # Richards growth curve

source("RScripts/Data processing/iALK.r") # returns len_age wt_age iALK

Base@iALK<-iALK
Base@wt_age<-wt_age
Base@len_age<-len_age

# Fecundity / maturity / recruitment / mortality

Base@ageM<-c(5,5)
Base@ageMsd<-c(0.2,0.2)
mat<-array(NA,c(Base@np,Base@na,Base@ny))
ages<-(1:Base@na)-0.5
ind<-TEG(dim(mat))
mat[ind]<-1/(1+exp((Base@ageM[ind[,1]]-ages[ind[,2]])/(Base@ageM[ind[,1]]*Base@ageMsd[ind[,1]])))
Base@mat<-mat
Base@Fec<-Base@mat[,,1]*Base@wt_age[,,1]
Base@steep<-c(0.7,0.7)
Base@spawns<-c(2,2)
Base@canspawn<-matrix(c(0,0,0,0,0,0,0,0,0,1,  1,0,1,0,0,0,0,0,0,0),ncol=Base@np) # matrix of spawning areas 1=can spawn

Base@Ma<-t(array(c(0.49,rep(0.24,4),0.2,0.175,0.15,0.125,rep(0.10,Base@na-9)),c(Base@na,Base@np))) # P, A  mean M at age

# --- Process CATDIS data ------

source("RScripts/Data processing/CATDIS.r") # returns Cobs in the right format

Base@Cobs<-as.matrix(Cobs)
Base@nCobs<-nrow(Cobs)

# --- Calculate master indices ------

source("Rscripts/Data processing/Master index.r") # returns list MI: MI[[1]] 0% efficiency increase MI[[2]] 2% efficiency increase MI[[3]] minus MA TP plus CAN LL 0% efficiency increase MI[[4]] minus MA TP plus CAN LL 2% efficiency increase

Base@RAI=MI[[1]]
#plotindex(MI[[1]])

# --- Calculate partial F's ------

source("Rscripts/Data processing/Partial Fs.r") # returns CPUEobs

Base@nCPUE<-Base@nf # same number of cpue indices (if not using master index) as fleets
Base@nCPUEobs<-nrow(CPUEobs)
Base@CPUEobs<-CPUEobs

# --- Calculate length sample data ------

source("Rscripts/Data processing/Length observations.r") # returns CLobs

Base@nCLobs<-nrow(CLobs)
Base@CLobs<-as.matrix(CLobs)

# --- Fishery independent indices ------

source("Rscripts/Data processing/FI indices.r") # returns Iobs

Base@nI<-as.integer(max(Iobs[,4])) # number of series
Base@nIobs<-nrow(Iobs) # number of data
Base@Iobs<-Iobs   # y s r i type(biomass/ssb) index

# --- PSAT tags ------

Base@nma<-as.integer(3)
Base@ma<-c(rep(1,4),rep(2,4),rep(3,Base@na-8))

source("Rscripts/Data processing/PSAT.r") # returns a table of electronic tag tracks of known (PSAT) and unknown (PSAT2) stock of origin

Base@nPSAT<-nrow(PSAT)
Base@PSAT<-PSAT
Base@nPSAT2<-nrow(PSAT2)
Base@PSAT2<-PSAT2

# --- Conventional tags ------

Base@nTag<-as.integer(1) # currently this is placeholder for conventional tags that are ignored by M3
Base@Tag<-array(c(2,1,7,1, 2,2,7,1,2,1),c(1,10))

# --- Stock of origin observations ------

ma<-array(rep(c(rep(1,3),rep(2,5),rep(3,Base@na-8)),each=Base@np),c(Base@np,Base@na))
nma<-max(ma)

source("Rscripts/Data processing/SOO.r") 

Base@nSOOobs<-nrow(SOOobs)
Base@SOOobs<-SOOobs

# --- Selectivities ------

Base@nsel<-Base@nf

Base@seltype<-rep(3,Base@nf) # all fleets have thompson (potentially) dome-shaped selectivity
Base@seltype[Fleets$gearTC=="LL"&Fleets$flag=="JPN"]<-2 # set Japanese longline to asymptotic
Base@selind<-1:Base@nf # No selectivity mirroring - selectivities correspond to fleets
Base@ratiolim<-c(0.1,0.4) # limits on the logistic slope paramter relative to position of inflection point
Base@infleclim<-c(4,15) # limits on the location of the inflection point (age)

# --- Movement estimation and definition ------

Base@movtype<-as.integer(1)

source("Rscripts/Data processing/Movement definitions.r") # returns movind and mov1

Base@nMP<-nrow(movind)+Base@ns*Base@np*Base@nma
Base@nmovind<-nrow(movind)
Base@movind<-movind
Base@nmov1<-nrow(mov1)
Base@mov1<-mov1


# --- Relating to likelihood functions ------

Base@CobsCV<-rep(0.25,Base@nf)    # CV on seasonal catch observations by area
Base@CPUEobsCV<-rep(0.25,Base@nCPUE) # CV on seasonal CPUE observations by area (only applicable if not using the master index approach (which only predicts catch observations))
Base@IobsCV<-rep(0.25,Base@nI)    # CV on fishery independent indices
Base@RDCV<-5/(Base@ny/Base@nRD)^0.5       # CV for penalty on recruitment deviations (if blocked this is Std. Err.)
Base@nLHw<-as.integer(10)        # number of likelihood components that may be weighted
#          (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel)",datfile,1,append=T)
Base@LHw<-c(1/100, 1,      1,           1/1000, 5,   1/10,   1,       1,        1/50,  1) # SSB index for each population


# --- Initial Values ------

Base@R0_ini<-c(350*8,350)*1000
Base@sel_ini<-t(array(c(0,0,(1:10)/10,rep(1,Base@nl-12)),c(Base@nl,Base@nf)))
Base@selpar_ini<-t(array(c(-5,0,-1),c(3,Base@nf)))
Base@selpar_ini[match('LLJPN',Base@Fleets$name),]<- c(1,-1,99)# LL  - logistic - mode and sd are 0.26
Base@lnF_ini<-rep(log(0.001),nrow(Base@Cobs))
Base@ilnRD_ini<-t(array(rep(0,Base@na-1),c(Base@na-1,Base@np)))
Base@lnRD_ini<-t(array(rep(0,Base@ny),c(Base@ny,Base@np)))
Base@mov_ini<-tomt(array(1/Base@nr,c(Base@np,Base@ns,Base@na,Base@nr,Base@nr)))
Base@qCPUE_ini<-rep(0.001,Base@nCPUE) # Base@nCPUE = Base@nf if there are as many indices as fleets
Base@qI_ini<-rep(1,Base@nI)
Base@D_ini<-c(sum(Base@RAI[,2,Base@canspawn[,1]==1][1:3])/sum(Base@RAI[,2,Base@canspawn[,1]==1][(Base@ny-2):Base@ny]),sum(Base@RAI[,2,Base@canspawn[,2]==1][1:3])/sum(Base@RAI[,2,Base@canspawn[,2]==1][(Base@ny-2):Base@ny]))# just for comparison with simulations
Base@complexRD<-as.integer(0)
Base@complexF<-as.integer(0)
Base@nF<-as.integer(1)
Base@debug<-as.integer(0)
Base@verbose<-as.integer(1)
Base@datacheck<-as.integer(99999)

OMI<-Base

save(OMI,file=paste(getwd(),"/Objects/Reference OMs/Base_OM",sep=""))


M3write(Base)

out<-M3read()

plotM3fit(out,outdir="G:/ABT-MSE/Results/OM_fits/First real OM/")

