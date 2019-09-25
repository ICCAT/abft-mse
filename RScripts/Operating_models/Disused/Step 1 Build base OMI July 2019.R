
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a base operating model that can be modified for each of trial specifications
# The basic dimensionality / structure of this OM is the same for all trial specifications
# ie the fleets, areas, times and data are the same

# In this edition a number of data-processing steps have be replaced with data processed by ICCAT (Ai Kimoto is contact)

# Tom Carruthers UBC

# 27th March 2019


# === Outstanding issues ==============

# === Main script =====================

# --- Set working directory ------

#setwd("C:/Users/tcar_/Dropbox/abft-mse")

setwd("C:/Users/tcarruth/Dropbox/abft-mse")

# --- Source MSE functions and objects ------

library(ABTMSE)
packageVersion('ABTMSE')

# --- Define dimensions ------

Base<-new('OMI')
Base@Date<-as.character(Sys.time())
Base@Author<-"T. Carruthers (t.carruthers@oceans.ubc.ca)"
Base@Notes<-"This object serves as a framework for populating the various Trial Specifications"
Base@PrimarySource<-"ICCAT Bluefin assessment data preparatory meeting, July 2017"
Base@years<-years<-c(1965,2016)   #c(1960,2014)
Base@Hyears<-Hyears<-c(1864,1964) #c(1864,1959)


# --- (a) Create some prespecified areas (polygons with lons and lats) ------

source("RScripts/Data processing/Area definitions.r") # Creates objects: AreaNames, AreaDefs


# --- Define areas ------

Base@areas<-areas<-      c("GOM_7","WATL_7","GSL_7","SATL_7", "NATL_7","EATL_7","MED_7")
Base@areanams<-areanams<-c("GOM",  "WATL",  "GSL",  "SATL",   "NATL",  "EATL",  "MED")
Base@area_defs<-new('list')
for(i in 1:length(Base@areas))Base@area_defs[[i]]<-AreaDefs[[match(areas[i],AreaNames)]]


# --- (b) Define fleets ------

source("Rscripts/Data processing/Define fleets 2019.r") # Returns a list Fleets

Base@Fleets<-Fleets


# --- Length bin set up ------

Base@lenbins<-(1:16)*25         #Base@lenbins<-(0:30)*12.5                   # 25 cm length bins
nlen<-length(Base@lenbins)-1
Base@mulen<-(Base@lenbins[1:nlen]+Base@lenbins[2:(nlen+1)])/2


# --- Assign known dimensions ------

Base@nHy<-as.integer(Hyears[2]-Hyears[1]+1)       # No. historical years
Base@ny<-as.integer(years[2]-years[1]+1)          # No. years
Base@ns<-as.integer(4)                            # No. subyears
Base@np<-as.integer(2)                            # No. stocks
Base@na<-as.integer(35)                           # No. ages
Base@nr<-as.integer(length(areas))                # No. areas
Base@nf<-as.integer(length(Base@Fleets$name)+1)   # No. fleets
Base@nl<-as.integer(nlen)


# --- Recapture indexing by subyear

Base@nRPT<-as.integer(2) # recaptures are only calculated in consecutive timesteps
temp<-rep(1:Base@ns,ceiling(Base@nRPT/Base@ns)+Base@ns)
Base@RPTind<-array(NA,c(Base@ns,Base@nRPT))
for(ss in 1:Base@ns)Base@RPTind[ss,]<-temp[ss:(ss+Base@nRPT-1)]


# --- Misc ----------------

Base@sdur<-rep(1/Base@ns,Base@ns) # the duration of each subyear - we make these equal
Base@nydist<-as.integer(10)



# --- (c) Create ALK ------

Base@lwa<-c(2.95*10^-5,1.96*10^-5)   # length-weight conversion w=al^b
Base@lwb<-c(2.899,3.009)             # length-weight conversion w=al^b

Base@A2<-c(NA,34)        # Richards growth curve
Base@L1<-c(NA,33.0)      # Richards growth curve
Base@L2<-c(NA,270.6)     # Richards growth curve
Base@K<-c(0.093,0.22)    # Richards / von B growth curve
Base@p<-c(NA,-0.12)      # Richards growth curve

Base@t0<-c(-0.97,NA)
Base@Linf<-c(318.85,NA)

Base@Lvar_a<-c(0.06,0.06)
Base@Lvar_b<-c(5.84,5.84)

source("RScripts/Data processing/iALK.r") # returns len_age wt_age iALK

Base@iALK<-iALK
Base@wt_age<-wt_age
Base@len_age<-len_age

# Fecundity / maturity / recruitment / mortality

#   Age      1  2  3  4     5    6  7  8  9  10 11 12 13 14 15 16 17 18+
mat<-array(c(0, 0, 0.25, 0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,rep(1,Base@na-18)),c(Base@na,Base@np))
Base@mat<-array(t(mat),c(Base@np,Base@na,Base@ny))
Base@Fec<-Base@mat[,,1]*Base@wt_age[,,1]
Base@nSR=4 # now many recruitments are estimated
Base@SRminyr=c(Base@years[1],1988,Base@years[1],1975)-Base@years[1]+1 # starting year of each recruitment
Base@SRmaxyr=c(1987,Base@years[2],1974,Base@years[2])-Base@years[1]+1 # end year of each recruitment
Base@SRp=c(1,1,2,2) # which stock
Base@SRpar=c(0.98, 0.98, 0.6, 0.9) # what steepness
Base@SRtype=c("BH","BH","BH","BH")
Base@nRDs<-rep(1,4)

RDts<-array(4,c(Base@np,Base@ny))
RDno<-array(1,c(Base@np,Base@ny))
yblock<-2 # the duration of recruitment deviation blocks

for(rd in 1:Base@nSR){
  RDts[Base@SRp[rd],Base@SRminyr[rd]:Base@SRmaxyr[rd]]=rd
  tempvec<-rep(1:100,each=yblock)[1:(Base@SRmaxyr[rd]-Base@SRminyr[rd]+1)]
  RDno[Base@SRp[rd],Base@SRminyr[rd]:Base@SRmaxyr[rd]]=tempvec
  Base@nRDs[rd]<-max(tempvec)

}

Base@RDno=RDno
Base@RDts=RDts

Base@spawns<-c(2,2)
Base@canspawn<-matrix(c(0,0,0,0,0,0,1,  1,1,0,0,0,0,0),ncol=Base@np) # matrix of spawning areas 1=can spawn

#   Age             1      2     3     4    5      6    7     8     9     10    11    12    13    14   15   16   17  18+
Base@Ma<-t(array(c(0.38, 0.30, 0.24, 0.20, 0.18, 0.16, 0.14, 0.13, 0.12, 0.12, 0.11, 0.11, 0.11, 0.1, 0.1, 0.1, 0.1, 0.1,rep(0.1,Base@na-18)),c(Base@na,Base@np)))
surv<-exp(-t(apply(cbind(c(0,0),Base@Ma[,1:(Base@na-1)]),1,cumsum)))
Base@SSBpR<-apply(surv*Base@Fec,1,sum)+surv[,Base@na]*exp(-Base@Ma[,Base@na])/(1-exp(-Base@Ma[,Base@na]))*Base@Fec[,Base@na]

# --- (d) Process CATDIS data ------

#source("RScripts/Data processing/CATDIS.r") # returns Cobs in the right format

Cobs<-read.csv("data/ICCAT_2019_2/Cobs.csv")
Cobs<-Cobs[Cobs$Year>=years[1],]
Cobs$Year<-Cobs$Year-years[1]+1
Cobs$Catch<-Cobs$Catch*1000 # tonnes to kg
CCV<-0.05
CV<-rep(CCV,nrow(Cobs))
wt<-rep(1/(CCV^2),nrow(Cobs))
Cobs<-cbind(Cobs,CV,wt)

Base@Cobs<-as.matrix(Cobs)
Base@nCobs<-nrow(Cobs)
Base@nf<-max(Cobs$Fleet)


# --- (e) Calculate master indices ------

source("Rscripts/Data processing/Master index.r") # returns list MI: MI[[1]] GLM from task II CPUE MI[[2]] SS assessment abundance x etagging  movement MI[[3]] flat, constant
Base@RAI=MI[[2]]  # uses assessment - etag movement


# --- (f) Calculate partial F's ------

source("Rscripts/Data processing/Partial Fs.r") # returns CPUEobs

Base@nE<-Base@nf
Base@nEobs<-nrow(Eobs)
Base@Eobs<-Eobs


# --- (g) Process assessment indices ----

source("Rscripts/Data processing/Fishery indices.r") # returns CPUEqvec and CPUEobs and Ilencat in the right format

Base@nCPUEq<-max(CPUEobs$qNo)
Base@nCPUEobs<-nrow(CPUEobs)
Base@CPUEobs<-as.matrix(CPUEobs)
Base@CPUEnames<-CPUEnames
#Base@CPUEwt <-rep(1,Base@nCPUEq)
Ilencat<-matrix(c(2,4,4,5,5,15,1,5,7,15,rep(0,2*(Base@nCPUEq-5))),nrow=2) # make this generic nfleets long
Base@Ilencat<-Ilencat


# --- (h) Calculate length sample data ------

source("Rscripts/Data processing/Length observations 2019.r") # returns CLobs

Base@nCLobs<-nrow(CLobs)
Base@CLobs<-as.matrix(CLobs)

CLrangeByft<-aggregate(Base@CLobs[,5],by=list(Base@CLobs[,4]),range)
CLrng<-CLrangeByft[,2]
Base@Fleets$LB<-CLrng[,1]
Base@Fleets$LB[Base@Fleets$LB>1]<-Base@Fleets$LB[Base@Fleets$LB>1]-1
Base@Fleets$UB<-CLrng[,2]
Base@Fleets$UB[Base@Fleets$UB<Base@nl]<-Base@Fleets$UB[Base@Fleets$UB<Base@nl]+1
mulendiff<-Base@mulen[2]-Base@mulen[1]
Base@Fleets$LB<-Base@mulen[Base@Fleets$LB]-mulendiff
Base@Fleets$UB<-Base@mulen[Base@Fleets$UB]+mulendiff

# cbind(CLrangeByft,Base@Fleets$LB,Base@Fleets$UB)

# --- (i) Calculate historical catches ------

source("Rscripts/Data processing/Historical catches 2019.r") # returns HCobs

Base@HCobs<-HCobs # a 4D array y x s x a x  r


# --- (j) Fishery independent indices ------

source("Rscripts/Data processing/FI indices 2019.r") # returns Iobs and Inames

Base@nI<-as.integer(max(Iobs[,5])) # number of series
Base@nIobs<-nrow(Iobs) # number of data
Base@Iobs<-Iobs   # y s r i type(biomass/ssb) index
Base@Inames<-Inames
#Base@Iwt<-rep(1,Base@nI)


# --- (k) PSAT tags ------

Base@nma<-as.integer(3)
Base@ma<-c(rep(1,4),rep(2,4),rep(3,Base@na-8))

Impute=FALSE # Do you want to use movement fingerprinting to identify more PSATs?
source("Rscripts/Data processing/PSAT.r") # returns a table of electronic tag tracks of known (PSAT) and unknown (PSAT2) stock of origin

Base@nPSAT<-nrow(PSAT)
Base@PSAT<-as.matrix(PSAT)
Base@nPSAT2<-as.integer(2)#nrow(PSAT2)
Base@PSAT2<-as.matrix(PSAT2[1:2,])#as.matrix(PSAT2)

# --- Conventional tags ------

Base@nTag<-as.integer(1) # currently this is placeholder for conventional tags that are ignored by M3
Base@Tag<-array(c(2,1,7,1,2,2,7,1,2,1),c(1,10))


# --- (l) Stock of origin observations ------

ma<-array(rep(c(rep(1,3),rep(2,5),rep(3,Base@na-8)),each=Base@np),c(Base@np,Base@na))
nma<-max(ma)
add_Genetics=T
summary=F
testSOO=F
source("Rscripts/Data processing/SOO 2019.r")

Base@nSOOobs<-nrow(SOOobs)
Base@SOOobs<-SOOobs
Base@SOOobs[Base@SOOobs[,4]==4,]

# --- Selectivities ------

Base@nsel<-Base@nf
Base@seltype<-rep(3,Base@nf) #c(3,3,2,3,2,2,3,3,2,3,3,3,3)#rep(3,Base@nf) # all fleets have thompson (potentially) dome-shaped selectivity except the combined other fleet
#Base@seltype[(1:length(Fleets$name))[Fleets$name%in%c("LLOTH","RRCan","RRUSA")]]<-2 # set longline to asymptotic
Base@seltype[14]<-2 # RRCAN
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

source("Rscripts/Data processing/Movement exclusions.r") # returns MovExc
Base@nMovExc<-nrow(MovExc)
Base@MovExc<-MovExc

# --- BSfrac a prior on asymptotic stock mixing (not used in reference OMs) ---------

Base@BSfrac<-array(0.5,c(Base@np,Base@ns))


# --- Relating to likelihood functions ------

#Base@CobsCV <-   rep(0.05,Base@nf)        # CV on seasonal catch observations by area

#Base@CPUEobsCV <- CPUEcv# from Fishery indices.R
#Base@IobsCV    <-  Iobscv # from FI indices.R
Base@CLCV_num  <- 0.025
wt<-1/(Base@CLCV_num /Base@CLobs[,6]) # sd is sqrt of denominator
Base@CLobs<-cbind(Base@CLobs,wt)

Base@RDCV      <-0.5/(Base@ny/(Base@ny/yblock))^0.5 # CV for penalty on recruitment deviations (if blocked this is Std. Err.)
Base@nSSBprior =1
Base@SSBprior  = matrix(c(1,1,20000),nrow=1)      # dummy prior for SSB (some operating models use fractions of other model estimated current SSB)
Base@SSBCV     =10                       # default is a very imprecise prior on SSB
Base@nDepprior =1
Base@Depprior  = matrix(c(1,1,0.5),nrow=1)      # dummy prior for SSB (some operating models use fractions of other model estimated current SSB)
Base@DepCV     =10                        # default is a very imprecise prior on SSB
Base@FCV       =0.8                  # was 0.4        # Prior precision of season-area deviations around mean F
Base@movCV     =1.2                 # was 1        # Prior precision of deviations from homogeneous movement
Base@selCV     =0.8                        # Prior precision of selectivity parameters
Base@SSBincCV=0.01                    # Prior precision of SSB increase ratio
Base@R0diffCV=0.45                   #was 0.5  # Prior on diferrence in early and late R0 estimation (where applicable)
Base@BSfracCV=0.02
Base@nLHw<-as.integer(17)             # number of likelihood components that may be weighted

#                           (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov,  10 sel,  11 SRA, 12 SSB, 13 SSBinc, 14 Fmod, 15 R0diff, 16 BSfrac, 17 MICV)",datfile,1,append=T)
#Base@LHw<-           0.1*  c(1/1000,  0.2,   0.1,         0.004,  21,    28,     0,       5,      0.1,    1.5,    0.0002, 0,      0,          24,     22,        0,         0.07)
Base@LHw<-           0.1*  c(1/Base@nCobs,  1/Base@nCPUEobs,  1/Base@nIobs, 1/Base@nCLobs,  1/Base@nSOOobs, 1/Base@nPSAT,   0,       5,      0.1,    1.5,    0.0002, 0,      0,          24,     22,        0,         0.07)

#Base@LHw<-          0.1*  c(1,        1,      1,          1,      1,     1,      0,       20,       3,      60,      0.0002,  0,      0,        12,      200,       0,        12) # SSB index for each population

#Base@LHw<-          0.1*  c(1,       1,      1,          1/20,     1,    2,      0,       20,       3,      60,      0.0002,  0,      0,        12,      200,       0,        12) # SSB index for each population
# Base@LHw<-         0.01* c(8,       30,      140,       1/2,    200,   40,     0,       100,       3,    60,      0.0002,       0,      0,         125,    2000,      0,     12) # SSB index for each population
# Base@LHw<-        0.01* c(8,       30,      140,       1/2,   200,   75,     0,       100,       3,    60,      0.0002,       0,      0,         125,    2000,      0,     12) # SSB index for each population
# Base@LHw<-        0.01* c(8,       30,      140,       1/2,    200,   320,     0,       100,       3,    30,      0.0002,       0,      0,         125,    2000,      0,     12) # SSB index for each population
# note that this weighting got all but 1 OM converged with MCMC (OM 10) - I've now doubled the selectivity prior and lowered the ADMB convergence to 1e-6 lets see.
# Base@LHw<-        0.01* c(8,       30,      140,       1/5,    200,   30,     0,       100,       1/10,   10,      0.0002,       0,      0,         125,    2000,      0,     10) # SSB index for each population
# Base@LHw<-        0.01* c(2,       20,      140,       1/10,    200,   30,     0,       300,       1/10,   10,      0.0002,       0,      0,         75,    2000,      0,     10) # SSB index for each population


# MI invariant specifications
Base@Phases<-as.integer(c(1,2,3,4))
Base@MICV<-1

# --- Initial Values ------

Base@muR_ini<-c(350*8,350)*5000
Base@sel_ini<-t(array(c(0,0,(1:4)/4,rep(1,Base@nl-6)),c(Base@nl,Base@nf)))
Base@selpar_ini<-t(array(c(-5,0,-1),c(3,Base@nf)))
#Base@selpar_ini[match('LLJPN',Base@Fleets$name),]<- c(1,-1,99)# LL  - logistic - mode and sd are 0.26
Base@lnF_ini<-rep(log(0.001),nrow(Base@Cobs))
Base@lnRD_ini<-t(array(seq(-0.1,0.1,length.out=Base@ny),c(Base@ny,Base@np)))
Base@mov_ini<-tomt(array(1/Base@nr,c(Base@np,Base@ns,Base@na,Base@nr,Base@nr)))
Base@qCPUE_ini<-rep(1,Base@nCPUEq)
Base@qI_ini<-rep(1,Base@nI)
Base@D_ini<-c(0.1,0.1)#c(sum(Base@RAI[,2,Base@canspawn[,1]==1][(Base@ny-2):Base@ny])/sum(Base@RAI[,2,Base@canspawn[,1]==1][1:3]),sum(Base@RAI[,2,Base@canspawn[,2]==1][(Base@ny-2):Base@ny])/sum(Base@RAI[,2,Base@canspawn[,2]==1][1:3]))# just for comparison with simulations
Base@complexRD<-as.integer(0)
Base@complexF<-as.integer(0)
Base@nF<-as.integer(1)
Base@MPind<-read.csv("Data/Processed/MP Indices/MP indices compiled assessment 2017.csv")
Base@nMPind<-nrow(Base@MPind)
Base@debug<-as.integer(0)
Base@verbose<-as.integer(1)
Base@datacheck<-as.integer(99999)

# Naming the Base operating model according to the various OM factors at level 1 -------
Base@Name<-"1AI"#paste(c("Base OM:",Names_Base),collapse=" ")
Base@OMfactors<-list("Prelim fits for DSB","", "")

# Save the base OMI  ------
OMI<-Base
M3write(OMI,OMdir=paste0(getwd(),"/M3"))  # Store this base operating model in the M3 directory

save(OMI,file=paste(getwd(),"/Objects/OMs/Base_OM",sep=""))
save(OMI,file=paste(getwd(),"/M3/OMI",sep=""))
