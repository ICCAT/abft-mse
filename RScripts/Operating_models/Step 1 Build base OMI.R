# =========================================================================================================================
# === Step 1. Build Base Operating Model Input object =====================================================================
# =========================================================================================================================

# April 2021
# Tom Carruthers (tom@bluematterscience.com)

# === NOTES ==============

# Cross referencing with the Trial Specifications Document is denoted in each line with the character string !TSD
# TSD cross referencing checks are commented out and denoted by !CHECK

# === Main script =====================

setwd("C:/Users/tcar_/Dropbox/abft-mse") # Set the working directly
setwd("C:/Users/tcarruth/Dropbox/abft-mse")


# --- Source MSE functions and objects ------

library(ABTMSE)
packageVersion('ABTMSE')  # should be v7.0.1 or greater


# --- Define dimensions ------

Base<-new('OMI')                     # Creates a new operating model input object from the ABTMSE package
Base@Date<-as.character(Sys.time())
Base@Author<-"T. Carruthers (tom@bluematterscience.com)"
Base@Notes<-"This object serves as a framework for populating the various reference set and robustness set operating models"
Base@PrimarySource<-"ICCAT Bluefin intersessional data preparatory meeting, April 2021"
Base@years<-years<-c(1965,2019)   # Forward calculation of numbers at age and F at age using a statistical catch at length
Base@Hyears<-Hyears<-c(1864,1964) # Spool-up period removing catches at age prior to 1965


# --- (a) Create some prespecified areas (polygons with lons and lats) ------

source("RScripts/Data processing/Area definitions.r") # Creates objects: AreaNames, AreaDefs. # !TSD Section 1, Figure 1.1


# --- Define areas ------

Base@areas <- areas <-       c("GOM_7","WATL_7","GSL_7","SATL_7", "NATL_7","EATL_7","MED_7") # Names from AreaNames above
Base@areanams <- areanams <- c("GOM",  "WATL",  "GSL",  "SATL",   "NATL",  "EATL",  "MED")   # New names for object
Base@area_defs<-new('list')
for(i in 1:length(Base@areas))Base@area_defs[[i]]<-AreaDefs[[match(areas[i],AreaNames)]]     # Copy polygons to object


# --- Length bin set up ------

Base@lenbins<-(1:16)*25         #  25 cm length bins # !TSD Section 3. Equation 3.3. Table 3.1
nlen<-length(Base@lenbins)-1
Base@mulen<-(Base@lenbins[1:nlen]+Base@lenbins[2:(nlen+1)])/2 # Mean length in each bin


# --- (b) Define fleets ------

source("Rscripts/Data processing/Define fleets.r") # Returns a list Fleets # !TSD Table 3.1

Base@Fleets<-Fleets


# --- Assign known dimensions ------

Base@nHy<-as.integer(Hyears[2]-Hyears[1]+1)       # No. historical years
Base@ny<-as.integer(years[2]-years[1]+1)          # No. years
Base@ns<-as.integer(4)                            # No. subyears
Base@np<-as.integer(2)                            # No. stocks
Base@na<-as.integer(35)                           # No. ages
Base@nr<-as.integer(length(areas))                # No. areas
Base@nf<-as.integer(length(Base@Fleets$name)+1)   # No. fleets
Base@nl<-as.integer(nlen)


# --- Recapture indexing by subyear (this isn't very important in the current model as it only tracks one future time-step of recaptures)

Base@nRPT<-as.integer(2) # recaptures are only calculated in consecutive timesteps
temp<-rep(1:Base@ns,ceiling(Base@nRPT/Base@ns)+Base@ns)
Base@RPTind<-array(NA,c(Base@ns,Base@nRPT))
for(ss in 1:Base@ns)Base@RPTind[ss,]<-temp[ss:(ss+Base@nRPT-1)] # Essentially this matrix just stores what the following quarter is in each year. Row is quarter of the year. First column is release quarter, second column is recapture quarter.


# --- Misc ----------------

Base@sdur<-rep(1/Base@ns,Base@ns) # the duration of each subyear - we make these equal. I.ee one quarter of a year
Base@nydist<-as.integer(10)       # the number of years for calculating the stable spatial distribution / movement matrix # !TSD Equations 3.22 and 3.23 (and text between)


# --- (c) Create ALK ------
Base@lwa <- c(3.50801,  1.77054)*10^-5   # length-weight conversion w=al^b Rodriguez-Marin et al. 2015 !TSD Table 8.2
Base@lwb <- c(2.878451, 3.001252)        # length-weight conversion w=al^b

Base@A2<-c(NA,34)        # Richards growth curve   # for Western stock Allioud et al. 2017 !TSD Table 8.2
Base@L1<-c(NA,33.0)      # Richards growth curve
Base@L2<-c(NA,270.6)     # Richards growth curve
Base@K<-c(0.093,0.22)    # Richards / von B growth curve
Base@p<-c(NA,-0.12)      # Richards growth curve

Base@t0<-c(-0.97,   NA)
Base@Linf<-c(318.85,NA)

Base@Lvar_a<-c(0.06,0.06)  # the CV is a linear function of length age # !TSD Table 8.2
Base@Lvar_b<-c(5.84,5.84)

source("RScripts/Data processing/iALK.r") # returns len_age wt_age iALK using Alliouds' calculation of CV in length as a linear function of length # !TSD (Equation 3.3)

Base@iALK<-iALK
Base@wt_age<-wt_age
Base@len_age<-len_age

# Fecundity / maturity / recruitment / mortality

#   Age      1  2  3  4     5    6+  Note this is age not age of life as in TSD (e.g. age class 1 is age zeros)
mat<-array(c(0, 0, 0.25, 0.5, rep(1,Base@na-4)),c(Base@na,Base@np)) #!TSD Table 8.2
Base@mat<-array(t(mat),c(Base@np,Base@na,Base@ny))
Base@Fec<-Base@mat[,,1]*Base@wt_age[,,1]

Base@nSR=4 # now many recruitments are estimated?
Base@SRminyr=c(Base@years[1],1988,Base@years[1],1975)-Base@years[1]+1 # starting year of each recruitment
Base@SRmaxyr=c(1987,Base@years[2],1974,Base@years[2])-Base@years[1]+1 # end year of each recruitment
Base@SRp=c(1,1,2,2) # which stock
Base@SRpar=c(0.98, 0.98, 0.6, 0.9) # what steepness # !TSD Table 8.2
Base@SRtype=c("BH","BH","BH","BH")
Base@nRDs<-rep(1,4)

RDts<-array(4,c(Base@np,Base@ny)) # Matrix that records what recruitment function is used for each stock (row) and year (column)  # !TSD Table 8.3.
RDno<-array(1,c(Base@np,Base@ny)) # matrix that records which recruitment deviation is used for each stock (row) and year (column)
yblock<-2 # the duration of recruitment deviation blocks # !TSD Equation 3.13., Table 8.3. # The model estimates recruitment deviations in 2 year blocks (lack of CAA data and young CAL data prevents easy estimation of annual rec devs)

for(rd in 1:Base@nSR){

  RDts[Base@SRp[rd],Base@SRminyr[rd]:Base@SRmaxyr[rd]]=rd
  tempvec<-rep(1:100,each=yblock)[1:(Base@SRmaxyr[rd]-Base@SRminyr[rd]+1)]
  RDno[Base@SRp[rd],Base@SRminyr[rd]:Base@SRmaxyr[rd]]=tempvec
  Base@nRDs[rd]<-max(tempvec)

}

Base@RDno=RDno
Base@RDts=RDts

Base@spawns<-c(2,2) # Both spawn in quarter 2 # !TSD Eqn 3.13
Base@canspawn<-matrix(c(0,0,0,0,0,0,1,  1,1,0,0,0,0,0),ncol=Base@np) # matrix of spawning areas 1=can spawn # !TSD Eqn 3.13

#   Age             1      2     3     4    5      6    7     8     9     10    11    12    13    14   15+
Base@Ma<-t(array(c(0.38, 0.30, 0.24, 0.20, 0.18, 0.16, 0.14, 0.13, 0.12, 0.12, 0.11, 0.11, 0.11, 0.1, 0.1, rep(0.1,Base@na-15)),c(Base@na,Base@np))) # !TSD Table 8.2.
surv<-exp(-t(apply(cbind(c(0,0),Base@Ma[,1:(Base@na-1)]),1,cumsum))) # assumption is that age class 0 have zero natural mortality
Base@SSBpR<-apply(surv*Base@Fec,1,sum)+surv[,Base@na]*exp(-Base@Ma[,Base@na])/(1-exp(-Base@Ma[,Base@na]))*Base@Fec[,Base@na]


# --- (d) Process CATDIS data ------

Cobs<-read.csv("data/ICCAT_2021_2/Cobs_Apr2021.csv")
Cobs<-Cobs[Cobs$Year>=years[1],]
Cobs$Year<-Cobs$Year-years[1]+1 # Year indexing
Cobs$Catch<-Cobs$Catch*1000 # tonnes to kg
CCV<- 0.01 # default CV is 1% # !TSD Eqn 8.8
CV<-rep(CCV,nrow(Cobs))
# wt<-rep(1/(CCV^2),nrow(Cobs)) # inverse variance weights
wt <- rep(1, nrow(Cobs)) # constant weighting
Cobs<-cbind(Cobs,CV,wt)

Base@Cobs<-as.matrix(Cobs)
Base@nCobs<-nrow(Cobs)
Base@nf<-max(Cobs$Fleet)


# --- (e) Calculate master indices ------

# !TSD Eqns 2.1 - 2.3, see SCRS/2019/133 for purpose and usage (essentially it is just a fast way to intialize a model)
source("Rscripts/Data processing/Master index.r") # returns SS assessment abundance x etagging  movement
Base@RAI=MI  # uses assessment - etag movement


# --- (f) Calculate partial F's ------

# TSD Eqn 3.4 - these are derived from catch / I(index-tag) from Eqn 2.3
source("Rscripts/Data processing/Partial Fs.r") # returns CPUEobs

Base@nE<-Base@nf
Base@nEobs<-nrow(Eobs) # Partial Fs are labelled Eobs as a sort of 'Effort' where F=qE
Base@Eobs<-Eobs


# --- (g) Process assessment indices ----

source("Rscripts/Data processing/Fishery indices.r") # returns CPUEqvec and CPUEobs and Ilencat in the right format

Base@nCPUEq<-max(CPUEobs$qNo)
Base@nCPUEobs<-nrow(CPUEobs)
Base@CPUEobs<-as.matrix(CPUEobs)
Base@CPUEnames<-CPUEnames

# The Index length categories are necessary because some clever chap decided that the US rod and reel CPUE indices should come from a single fleet selectivity but be
# calculated from truncated ranges of that length selectivity. These are recorded here for use in the calculation of CPUE indices. Complicated business and even more so
# when these truncations need to be preserved in the generation of the projected data. This is one area where code checking is vital!
Ilencat<-matrix(c(2, 4, 7,  2, 7,  2, rep(0,Base@nCPUEq-6),
                  4, 5, 15, 5, 15, 5, rep(0,Base@nCPUEq-6)),nrow=2,byrow=T) # make this generic nfleets long (for no other reason than that dimension is already sent to M3 and it can't be longer than nfleets)
Base@Ilencat<-Ilencat

# Although these length categories (lt) are provided in the .csv input file - I overwrite it again here to be doubley sure because of previous inconsistencies in how ICCAT processed these inputs
#               lencat 1        lencat 2       lencat 3     lencat 4   lencat 5      lencat 6
lencatfleets<-c("US_RR_66_114","US_RR_115_144","US_RR_177","US_RR_145","US_RR_195", "US_RR_66_144")
for(l in 1:length(lencatfleets)) Base@CPUEobs[Base@CPUEobs[,4]==match(lencatfleets[l],Base@CPUEnames),7]<-l


# --- (h) Calculate length sample data ------

source("Rscripts/Data processing/Length observations.r") # returns CLobs

Base@nCLobs<-nrow(CLobs)
Base@CLobs<-as.matrix(CLobs)
wt<-rep(1,Base@nCLobs) # Add a constant weighting term per data point
Base@CLobs<-cbind(Base@CLobs,wt)


# --- (i) Calculate historical catches ------

source("Rscripts/Data processing/Historical catches.r") # returns HCobs

Base@HCobs<-HCobs # a 4D array y x s x a x  r


# --- (j) Fishery independent indices ------

source("Rscripts/Data processing/FI indices.r") # returns Iobs and Inames # !TSD Table 2.2  (Table 7.1 is projection statistical properties)

Base@nI<-as.integer(max(Iobs[,5])) # number of series
Base@nIobs<-nrow(Iobs) # number of data
Base@Iobs<-Iobs   # y s r i type(biomass/ssb) index
Base@Inames<-Inames


# --- (k) PSAT tags ------

Base@nma<-as.integer(3) # number of movement age classes
Base@ma<-c(rep(1,4),rep(2,4),rep(3,Base@na-8))  # age class index by age (e.g. first four ages 0-3 are age class 1)
Base@macat<-matrix(c(1,4,5,8,9,Base@na),nrow=2) # start-end ages for each age class

Impute=FALSE # Do you want to use movement fingerprinting to identify more PSATs?
source("Rscripts/Data processing/PSAT.r") # returns a table of electronic tag tracks of known (PSAT) and unknown (PSAT2) stock of origin

Base@nPSAT<-nrow(PSAT)
Base@PSAT<-as.matrix(PSAT)
Base@nPSAT2<-as.integer(2)#nrow(PSAT2)
Base@PSAT2<-as.matrix(PSAT2[1:2,])#as.matrix(PSAT2)


# --- Conventional tags ------

Base@nTag<-as.integer(1) # currently this is placeholder for conventional tags that are ignored by M3 for reasons of unknown and highly variable reporting rates
Base@Tag<-array(c(2,1,7,1,2,2,7,1,2,1),c(1,10))


# --- (l) Stock of origin observations ------

ma<-array(rep(Base@ma,each=Base@np),c(Base@np,Base@na))
nma<-max(ma)
add_Genetics=T
summary=F
testSOO=F
source("Rscripts/Data processing/SOO.r") # !TSD Tables 2.6A, 2.6B, 2.6C, 2.6D  SCRS/2018/133 Carruthers & Butterworth

Base@nSOOobs<-nrow(SOOobs)
Base@SOOobs<-SOOobs

# --- Selectivities ------

Base@nsel<-Base@nf
Base@seltype<-rep(3,Base@nf)  # all fleets have Thompson (potentially) dome-shaped selectivity except the combined other fleet
Base@seltype[match('RRCAN',Fleets$name)]<-2 # Logistic
Base@selind<-1:Base@nf # No selectivity mirroring - selectivities correspond to fleets
Base@ratiolim<-c(0.1,0.4) # limits on the logistic slope paramter relative to position of inflection point
Base@infleclim<-c(4,15) # limits on the location of the inflection point (age)


# --- (m) Movement estimation and definition ------

Base@movtype<-as.integer(1)

source("Rscripts/Data processing/Movement definitions.r") # returns movind and mov1

Base@nMP<-nrow(movind)+Base@ns*Base@np*Base@nma
Base@nmovind<-nrow(movind) # number of movement parameters to be estimated
Base@movind<-movind        # the indexing of those parameters
Base@nmov1<-nrow(mov1)     # number of first movement parameter that doesn't need a estimation (they sum to 1 across to-areas)
Base@mov1<-mov1            # the indexing of those parameters that don't need estimating

use_tag_exclusions<-T      # should certain transitions be removed as a possibility? # !TSD paragraph following Eqn 3.22
GOM_EXC<-F                 # should GOM fish in season 3 be excluded as a possibility?

source("Rscripts/Data processing/Movement exclusions.r") # returns MovExc

Base@nMovExc<-nrow(MovExc)
Base@MovExc<-MovExc

# --- BSfrac a prior on asymptotic stock mixing (not used in reference OMs) ---------

Base@BSfrac<-matrix(0.01) # fraction west stock found in East area (old ref factor level 3)

# --- Spatial seasonal priors -------------------------------------------------------

source("Rscripts/Data processing/Spatial_Priors.r") # returns SpatPr # !TSD Eqn 8.17a
Base@SpatPr<-as.matrix(SpatPr)


# --- Disused spatial fractions (used for sensitivity analyses) ---------------------

# Spatial fraction pp, age class, start year, end year, r, logit fraction, CV, wt
Base@SpatFrac<-matrix(c(1,1,1,1,1,1,1,0),nrow=1)

# --- Relating to likelihood functions ------

# Used in all ref case OMs
Base@Phases<-as.integer(c(1,2,3,4)) # Estimation phases for various M3 components
Base@CLCV_num  = 0.02                               # Numerator for (optional) lognormal length composition likelihood function # !M3 switch(LC_LHF)
Base@RDCV      = 0.5/(Base@ny/(Base@ny/yblock))^0.5 # CV for penalty on recruitment deviations (if blocked this is Std. Err.)
Base@FCV       = 1                                  # Prior precision of season-area deviations around mean F # !TSD Table 8.4
Base@movCV     = 4                                  # Prior precision of deviations from homogeneous movement # !TSD Table 8.4
Base@selCV     = 4                                  # Prior precision of selectivity parameters # !TSD Table 8.4
Base@R0diffCV  = 0.4                                # Prior on diferrence in early and late R0 estimation (where applicable) # !TSD Table 8.4
Base@BSfracCV  = 0.025                              # Prior on western stock mixing !TSD Table 8.4
Base@MICV      = 1                                  # Prior on master index deviations
Base@ET_LHF    = 1                                  # Multinomial electronic tagging likelihood function
Base@LC_LHF    = 2                                  # Log normal Length composition likelihood function

# Load historical SSB estimated by Stock synthesis assessments (just to get year range)
load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017
cond<-dat$assessment=='SS'&dat[,3]>=Base@years[1]
SSBprior<-cbind(match(dat[cond,1],c("East","West")),dat[cond,3]-Base@years[1]+1,dat[cond,4])
Base@SSBprior<-SSBprior
Base@nSSBprior<-nrow(SSBprior)
Base@SSBCV<-0.05

# Unused in ref case OMs right now (these are features of the M3 operating model that were used for sensitivity testing and 'what matters' analyses - and are still needed in case of backwards compatibility)
Base@nDepprior = 1                                  # only one ununsed prior for depletion
Base@Depprior  = matrix(c(1,1,0.5),nrow=1)          # dummy prior for stock depletion (some operating models use fractions of other model estimated current SSB)
Base@DepCV     = 10                                 # default is a very imprecise prior on SSB
Base@SSBincCV  = 0.01                               # Prior precision of SSB increase ratio (no longer used)


# Weightings  # !TSD Table 8.6 -------------------------------------------------------------------------------
#              1 catch, 2 cpue, 3 FIindex,  4 Lcomp,   5 SOO, 6 PSAT,  7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB, 13 SSBinc, 14 Fmod,  15 R0diff, 16 BSfrac, 17 MICV, 18 SpatPr
Base@LHw <-   c(1/50,  1,       2,          0.05,      1,     5,       0,       1,        1,     1,      1,      1,      0,         1,        1,         1,         1,       1) # !TSD Table 8.6

Base@nLHw<-as.integer(length(Base@LHw))

Base@Iobs[Base@Iobs[,5]==match("GOM_LAR_SUV",Base@Inames),10]<-10   # upweight gom lar suv !TSD Table 8.6 - note that the '20' weight quoted in Table 8.6 is the value of 2 in Base@LHw x this additional factor 10
Base@Iobs[Base@Iobs[,5]==match("MED_LAR_SUV",Base@Inames),10]<-10   # upweight med lar suv


# --- Other Values ------

Base@complexRD<-as.integer(0) # for sensitivity analyses / backwards compatibility
Base@complexF<-as.integer(0)  # for sensitivity analyses / backwards compatibility
Base@nF<-as.integer(1)        # for sensitivity analyses / backwards compatibility
#Base@MPind<-read.csv("Data/Processed/MP Indices/MP indices compiled assessment 2017.csv") # Store the index observations that will be used in forward projection
#Base@nMPind<-nrow(Base@MPind)
Base@debug<-as.integer(0)         # return debugging info at the command prompt?
Base@verbose<-as.integer(1)       # print info as the M3 model runs?
Base@datacheck<-as.integer(99999) # check that data have read correctly

# Naming the Base operating model according to the various OM factors at level 1 -------
Base@Name<-"Reference set"
Base@OMfactors<-list("Prelim fits", "", "")

# Index hyperstability parameter
Base@beta<-1                      # defaults to linear relationship between indices and their corresponding model estimated biomass/numbers

#  --- Set all reference OM grid factors levels to 1 -------

# (should be identical to what is specified above - but useful to show here - for example can make initial parameters for rec level 2 using this code)
Base<-Rec_Ref(Base,lev=1)  # Factor 1, overwrite recruitment level 1 parameters
Base<-MatM_Ref(Base,lev=1) # Factor 2, overwrite maturity - M level 2 parameters
Base<-Bmu_Ref(Base,lev=1)  # Factor 3, overwrite biomass prior
Base<-Lcomp_Ref(Base,lev=1)# Factor 4, overwrite length composition weight

# ---- Write files ------------

M3write(Base, "M3")  # Write a working M3 file to the working directory based on the Base OMI (Operating Model Input) object
OMI<-Base
save(OMI,file='M3/OMI')
save(OMI,file='Objects/OMs/OMI')

# ==== END OF SCRIPT ===========================================================================

