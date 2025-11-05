# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of robustness operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC

# 3 February 2018

# There are a set of high priority robustness OMs relating to stock mixing
# reconditioning:
# (1) Half the degree of stock mixing
# (2) Zero western fish in the East
# (3) Artifically increase GOM quarter 2 none in quarter 3
# (4) Brazilian catches in the East not west.

# not reconditioned
# (5) Time-varying mixing zero / 150% alternating every 3 years
# (6) Half goes to 150% after year 10

# Other old high priority robustness OMs

# (7) 20% overages in both East and West areas
# (8) Undetected increase in catchability of 1% (OM@qinc=1)
# (9) Non-linear index relationships (hyperstability / hyper depletion) (OM@Ibeta_ignore=F)

#rm(list=ls(all=TRUE))                       # Remove all existing objects from environment

# === Set up ================================================================

setwd("C:/Users/tcarruth/Dropbox/abft-mse")
setwd("C:/Users/tcar_/Dropbox/abft-mse")
load(file=paste0(getwd(),"/Objects/OMs/Design"))

# --- Source MSE functions and objects ------

library(ABTMSE)
loadABT()
M3dir<-paste0(getwd(),"/M3")
ROMnams<-paste0("ROM_",1:12)
proyears<-54
seed<-1

# ROM grids ---------------------------------
FourX<-c(55,  56,  58, 59)
TwoX<-c(58, 59)

# Make directories -------------------------------

ROMnos<-1:12
ROMlevs<-c("Senes","WstGw","BrzCt")
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

# Make folders

for(i in 1:length(output_dirs)){
  if(!file.exists(output_dirs[i])) dir.create(output_dirs[i])
  file.copy(paste0(M3dir,"/M3.exe"),output_dirs[i],overwrite=T)      # copy the latest executable to the temporary
  file.copy(paste0(M3dir,"/stats.cxx"),output_dirs[i],overwrite=T)   # copy over the statistics library
  cat(i);cat("-")
}


# === Step 1: ROMS 1-4 # Senescence ===========================================

ROMnos<-1:4
input_dirs <- paste0(getwd(),"/Objects/OMs/",FourX)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-ROMnams[ROMnos[i]]
  OMI@Ma[,26:OMI@na]<-0.47
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(i)

}

# === Step 2: ROMS 5-8 # Western Growth ===========================================

ROMnos<-5:8
input_dirs <- paste0(getwd(),"/Objects/OMs/",FourX)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-ROMnams[ROMnos[i]]
  OMI@wt_age[1,,]<- OMI@wt_age[2,,]
  OMI@iALK[1,,,]<-OMI@iALK[2,,,]
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(i)

}

# === Step 3: ROMS 9-12 # Brazilian Catches ===========================================

ROMnos<-9:12
input_dirs <- paste0(getwd(),"/Objects/OMs/",FourX)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

source("C:/Users/tcarruth/Dropbox/abft-mse/RScripts/Data processing/Historical catches 2019 Brazilian to East 3.R")

for(i in 1:length(ROMnos)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-ROMnams[ROMnos[i]]
  OMI@HCobs<-HCobs # the adjusted brazilian cathes
  OMI@Cobs<-as.matrix(Cobs)
  OMI@nCobs<-nrow(Cobs)
  
  Catches<-Eobs<-array(0,c(OMI@ny,OMI@ns,OMI@nr,OMI@nf))
  Catches[as.matrix(OMI@Cobs[,1:4])]<-OMI@Cobs[,5]
  Catches<-(Catches/(mean(Catches,na.rm=T)))*0.001
  Catches[is.na(Catches)]<-0
  cond<-Catches>0
  ind<-TEG(c(OMI@ny,OMI@ns,OMI@nr,OMI@nf))[cond,]
  Eobs[ind]<--log((1-Catches[ind]/OMI@RAI[ind[,1:3]]))
  Eobs2<-cbind(ind,ind[,4],Eobs[ind]) # y s r f i cpue/pf
  Eobs2<-Eobs2[!is.na(Eobs2[,6]),]
  mubyfleet<-aggregate(Eobs2[,6],by=list(Eobs2[,4]),FUN=mean)
  Eobs2[,6]<-Eobs2[,6]/mubyfleet[Eobs2[,4],2]*0.2
  Eobs<-Eobs2
  
  OMI@nE<-OMI@nf
  OMI@nEobs<-nrow(Eobs)
  OMI@Eobs<-Eobs
  
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(i)

}


# ============ Now run all conditionings =============================================================================================

#ncond<-12          # fitting 1:9
#foldernos<-1:ncond # foldernos<-c(1,6,8,9,13,17,21,25,29,33) foldernos<-c(3,11,16,20,21,23,27,31,33,35)
foldernos<-1:12 # only Brazilian cases

sfInit(parallel=T, cpus=4)                                                                        # Initiate the cluster
system.time(sfLapply(foldernos, runM3p, OMdir=paste0(getwd(),"/Objects/ROMs"), mcmc=F, hess=F))   # Run the M3 executables in parallel
for(i in ncond) pin_from_par(paste0(getwd(), "/Objects/ROMs/", i))                                # Record the MLE parameter estimates as initial values

# ==== build individual reports ================
dirs<-paste0(getwd(),"/Objects/ROMs/",foldernos)

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017

for(ij in 1:length(dirs)){
 
  utffile<-paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.utf8.md")
  if(file.exists(utffile))file.remove(utffile)
  load(paste0(dirs[ij],"/OMI"))
  out<-M3read(OMDir=dirs[ij])
  outfile<-paste0(dirs[ij],"/Report_R",ij,".html")
  render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.Rmd"),
         output_file=outfile)

}

# compile individual reports

# copy ind reps -------------------------------------------

nOMs<-12
repdir<-"C:/Users/tcar_/Dropbox/BFT MSE/Communications/ROM reps/"
for(i in 1:nOMs)  file.copy(paste0(dirs[i],"/Report_R",i,".html"),repdir,overwrite=T)




# Summary report =====================================================

FreeComp<-TRUE # ignore design grid stuff
custom_cols<-rep(c('black','blue','orange','grey'),each=4)
custom_lwds<-rep(rep(c(1,2),each=2),5)
custom_ltys<-rep(c(1:2),10)
custom_cexs<-0.5

introtext<-"A comparison report for high priority robustness grid OMs relating to senescence (senes),
assuming the West stock growth curve for the East stock (WstGw)"
#introtext<-"A comparison report for high priority robustness grid OMs relating to senescence (senes),
#assuming the West stock growth curve for the East stock (WstGw), and the Brazilian catch scenario (BrzCt)"
OMnos <- rep(FourX,length(ROMlevs))
OMnames <-c(FourX, paste0(OMnos[1:8],"-",rep(ROMlevs[1:2],each=4)))
OMdirs  <-  c(paste0(getwd(),"/Objects/OMs/",FourX),dirs[1:8])
outfile <- paste0(repdir,"ROM_1_8_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)



CPUEnos<-match(c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_177","US_GOM_PLL2","JPN_LL_West2","CAN GSL","CAN SWNS"),OMI@CPUEnames)
Inos<-match(c("FR_AER_SUV2","MED_LAR_SUV","CAN_ACO_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR"),OMI@Inames)
Iarea<-c(OMI@CPUEobs[match(CPUEnos,OMI@CPUEobs[,4]),3],OMI@Iobs[match(Inos,OMI@Iobs[,5]),3])
Proposed <-c("GOM_LAR_SUV","US_RR_66_114","JPN_LL_West2","CAN_ACO_SUV",
             "JPN_LL_NEAtl2","MED_LAR_SUV","FR_AER_SUV2","GBYP_AER_SUV_BAR")
discont<-c("MED_LAR_SUV","FR_AER_SUV2","GBYP_AER_SUV_BAR")



nOMs<-length(OMnos)
introtext<-"Index fitting report forfor high priority robustness grid OMs relating to senescence (senes),
assuming the West stock growth curve for the East stock (WstGw)"

outfile<-paste0(repdir,"ROM_1_8_index_fit.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary_manyOMs.Rmd"),output_file=outfile)









# === Build operating models ==========================================================================

load(file=paste0(getwd(),"/Objects/OMs/1/OMI")) # Load any OMI object to get a real year reference
load(file=paste(getwd(),"/Objects/Recruitment_scenarios/Trial specifications",sep=""))

nOMs<-12
output_dirs<-paste0(getwd(),"/objects/ROMs/",1:nOMs)

SD_override<-data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR"),
                        SD=c(  0.45,         0.45,            0.8,          0.45              ))

AC_override<- data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR","JPN_LL_West2"),
                         AC=c(  0.2,         0,               0.2,          0.2,                         0))

Yrs_override<-data.frame(Name='MED_LAR_SUV',start=48, end=52)
CPUEinds<-c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_177","US_GOM_PLL2","JPN_LL_West2","CAN GSL","CAN SWNS")
Iinds<-c("FR_AER_SUV2","MED_LAR_SUV","CAN_ACO_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR")

recno<-rep(1:2,6) # the reference recruitment scenarios for ROMs 1-12

for(i in 1:nOMs){

  OMd<-dirs[i]

  OM<-new('OM',OMd=OMd,nsim=48,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[recno[i]]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)

  save(OM,file=paste0(OMd,'/OM'))

  OM<-new('OM',OMd=OMd,nsim=2,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[recno[i]]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)

  save(OM,file=paste0(OMd,'/OMd'))

}









# ===== Second round ROMS ======================================================================================================================


setwd("C:/Users/tcarruth/Dropbox/abft-mse")
setwd("C:/Users/tcar_/Dropbox/abft-mse")
load(file=paste0(getwd(),"/Objects/OMs/Design"))

library(ABTMSE)
loadABT()



# --- set up ------------------------------------------------------------------------------------

M3dir<-paste0(getwd(),"/M3")
TwoX<-c(58, 59)
ROMnos<-13:24
ROMnams<-paste0("ROM_",ROMnos)
proyears<-54
seed<-1
ROMlevs<-c("TVarMix","ChnMix","qInc","qDec","NonLin","Overage")
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

# Make folders

for(i in 1:length(output_dirs)){
  if(!file.exists(output_dirs[i])) dir.create(output_dirs[i])
  cat(i);cat("-")
}

load(paste0(getwd(),"/Objects/OMs/",TwoX[1],"/OM_",TwoX[1]))
ROM58<-OM
load(paste0(getwd(),"/Objects/OMs/",TwoX[1],"/OM_",TwoX[1],"d"))
ROM58d<-OM

load(paste0(getwd(),"/Objects/OMs/",TwoX[2],"/OM_",TwoX[2]))
ROM59<-OM
load(paste0(getwd(),"/Objects/OMs/",TwoX[2],"/OM_",TwoX[2],"d"))
ROM59d<-OM


# ----------------- Time varying mixing  ROM 13-16   ------------------------------------------------------------------------------------------------------
# This script actually fits ROMs in 13 and 14 with 10% and 30% western mixing then uses both in ROM 13 and the 30% one in ROM 14
# The purpose of the reconditioning is the determination of alternative movement matrices for uses in time-varying movement
# The 10% and 30% scenarios are run on 58 - in both cases the movement matrices are virtually identical whether fitted to 58 or 59.
# These are then copied to movement scenarios 2 and 3 (ROM 13, 10% and 30%) and movement scenario 2 (ROM 14, 30% after 10 years)

ROMnos<-13:14 # fitting of 10% and 30% western mixing (ROM13 is 3 year OM58, ROM14 is 3 year switch OM59, ROM15 is 10 year switch OM58, ROM16 is 10 year switch OM59)
input_dirs <- paste0(getwd(),"/Objects/OMs/",TwoX)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  file.copy(paste0(M3dir,"/M3.exe"),output_dirs[i],overwrite=T)      # copy the latest executable to the temporary
  file.copy(paste0(M3dir,"/stats.cxx"),output_dirs[i],overwrite=T)   # copy over the statistics library
  file.copy(paste0(input_dirs[i],"/M3.pin"),output_dirs[i],overwrite=T)
    load(paste(input_dirs[1],"/OMI",sep="")) # applied to 58
  OMI@Name<-ROMnams[ROMnos[i]]
  OMI@BSfrac<-matrix(c(0.1,0.3)[i])
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(i)

}

sfInit(parallel=T,cpus=4)
#runM3(OMdir=paste0(getwd(),"/Objects/ROMs/13"))
#runM3(OMdir=paste0(getwd(),"/Objects/ROMs/14"))
# Initiate the cluster
system.time(sfLapply(ROMnos,runM3p,OMdir=paste0(getwd(),"/Objects/ROMs"),mcmc=F,hess=F))   # Run the M3 executables in parallel
for(i in ROMnos)pin_from_par(paste0(getwd(),"/Objects/ROMs/",i))       # Record the MLE parameter estimates as initial values

# ==== build individual reports =======================================================================

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017
dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(ij in 1:length(dirs)){

  utffile<-paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.utf8.md")
  if(file.exists(utffile))file.remove(utffile)
  load(paste0(dirs[ij],"/OMI"))
  out<-M3read(OMDir=dirs[ij])
  outfile<-paste0(dirs[ij],"/Report_R",ROMnos[ij],".html")
  render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.Rmd"),
         output_file=outfile)

}


FreeComp<-TRUE # ignore design grid stuff
custom_cols<-rep(c('black','blue','grey'),each=4)
custom_lwds<-rep(rep(c(1,2),each=2),3)
custom_ltys<-rep(c(1:2),3)
custom_cexs<-0.5

# Mix I Lcomp L
nOMs<-12
introtext<-"A comparison report for reference grid OMs with 1% western biomass mixing (mixing factor level I) and length composition weighting of 1/20 (factor level L)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="L" & Design$Design_Ref[,3] == "I"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_I_Lcomp_L_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)












# === Build unmodified operating models ==========================================================================

load(file=paste0(getwd(),"/Objects/OMs/1/OMI")) # Load any OMI object to get a real year reference
load(file=paste(getwd(),"/Objects/Recruitment_scenarios/Trial specifications",sep=""))

nOMs<-2
output_dirs<-paste0(getwd(),"/objects/ROMs/",1:nOMs)

SD_override<-data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR"),
                        SD=c(  0.45,         0.45,            0.8,          0.45              ))

AC_override<- data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR","JPN_LL_West2"),
                         AC=c(  0.2,         0,               0.2,          0.2,                         0))

Yrs_override<-data.frame(Name='MED_LAR_SUV',start=48, end=52)
CPUEinds<-c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_177","US_GOM_PLL2","JPN_LL_West2","CAN GSL","CAN SWNS")
Iinds<-c("FR_AER_SUV2","MED_LAR_SUV","CAN_ACO_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR")

recno<-1:2 # the reference recruitment scenarios for ROMs 1-12

for(i in 1:nOMs){

  OMd<-dirs[i]

  OM<-new('OM',OMd=OMd,nsim=48,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[recno[i]]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)

  save(OM,file=paste0(OMd,'/OM'))

  OM<-new('OM',OMd=OMd,nsim=2,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[recno[i]]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)

  save(OM,file=paste0(OMd,'/OMd'))

}

# ==== Modify operating models ===============================================================

load(paste0(getwd(),"/Objects/ROMs/13/OM"))
mov10<-OM@mov[,,,1,,,]
load(paste0(getwd(),"/Objects/ROMs/14/OM"))
mov30<-OM@mov[,,,1,,,]

load(paste0(getwd(),"/Objects/ROMs/13/OMd"))
mov10d<-OM@mov[,,,1,,,]
load(paste0(getwd(),"/Objects/ROMs/14/OMd"))
mov30d<-OM@mov[,,,1,,,]

# switches between 10%  and 30% west mixing every three years ----------------------------

# ROM 13 ---------------- 3 year switch

OM<-ROM58
OMd<-ROM58d

OM@Name<-"ROM_13"
OMd@Name<-"ROM_13d"


OM@mov<-array(NA,c(dim(ROM58@mov)[1:3],3,dim(ROM58@mov)[5:7]))
OMd@mov<-array(NA,c(dim(ROM58d@mov)[1:3],3,dim(ROM58d@mov)[5:7]))

OM@mov[,,,1,,,]<-ROM58@mov[,,,1,,,]
OM@mov[,,,2,,,]<-mov10
OM@mov[,,,3,,,]<-mov30
OM@movIndex[OM@nyears+1:OM@proyears]<-rep(rep(c(2,3),each=3),50)[1:OM@proyears]

OMd@mov[,,,1,,,]<-ROM58d@mov[,,,1,,,]
OMd@mov[,,,2,,,]<-mov10d
OMd@mov[,,,3,,,]<-mov30d
OMd@movIndex[OMd@nyears+1:OMd@proyears]<-rep(rep(c(2,3),each=3),50)[1:OMd@proyears]

save(OM,file=paste0(getwd(),"/Objects/ROMs/13/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/13/OMd"))

# ROM 14 ---------------- 3 year switch

OM<-ROM59
OMd<-ROM59d

OM@Name<-"ROM_14"
OMd@Name<-"ROM_14d"

OM@mov<-array(NA,c(dim(ROM58@mov)[1:3],3,dim(ROM59@mov)[5:7]))
OMd@mov<-array(NA,c(dim(ROM58d@mov)[1:3],3,dim(ROM59d@mov)[5:7]))

OM@mov[,,,1,,,]<-ROM58@mov[,,,1,,,]
OM@mov[,,,2,,,]<-mov10
OM@mov[,,,3,,,]<-mov30
OM@movIndex[OM@nyears+1:OM@proyears]<-rep(rep(c(2,3),each=3),50)[1:OM@proyears]

OMd@mov[,,,1,,,]<-ROM58d@mov[,,,1,,,]
OMd@mov[,,,2,,,]<-mov10d
OMd@mov[,,,3,,,]<-mov30d
OMd@movIndex[OMd@nyears+1:OMd@proyears]<-rep(rep(c(2,3),each=3),50)[1:OMd@proyears]

save(OM,file=paste0(getwd(),"/Objects/ROMs/14/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/14/OMd"))


# Changes to 30% after 10 years --------------------------------------------------

# ROM 15 ---------------

OM<-ROM58
OMd<-ROM58d

OM@Name<-"ROM_15"
OMd@Name<-"ROM_15d"

OM@mov[,,,1,,,]<-ROM58@mov[,,,1,,,]
OM@mov[,,,2,,,]<-mov30
OM@movIndex[OM@nyears+1:OM@proyears]<-c(rep(1,10),rep(2,100))[1:OMd@proyears]

OMd@mov[,,,1,,,]<-ROM58d@mov[,,,1,,,]
OMd@mov[,,,2,,,]<-mov30d
OMd@movIndex[OMd@nyears+1:OMd@proyears]<-c(rep(1,10),rep(2,100))[1:OMd@proyears]

save(OM,file=paste0(getwd(),"/Objects/ROMs/15/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/15/OMd"))

# ROM 16 ---------------

OM<-ROM59
OMd<-ROM59d

OM@Name<-"ROM_16"
OMd@Name<-"ROM_16d"

OM@mov[,,,1,,,]<-ROM58@mov[,,,1,,,]
OM@mov[,,,2,,,]<-mov30
OM@movIndex[OM@nyears+1:OM@proyears]<-c(rep(1,10),rep(2,100))[1:OMd@proyears]

OMd@mov[,,,1,,,]<-ROM58d@mov[,,,1,,,]
OMd@mov[,,,2,,,]<-mov30d
OMd@movIndex[OMd@nyears+1:OMd@proyears]<-c(rep(1,10),rep(2,100))[1:OMd@proyears]

save(OM,file=paste0(getwd(),"/Objects/ROMs/16/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/16/OMd"))



# Catchability changes ============================================================================================================

# --- Catchability increase of 2% in indices 17-18 -------------------------------

# ROM 17 ---------------------------

OM<-ROM58
OMd<-ROM58d

OM@Name<-"ROM_17"
OMd@Name<-"ROM_17d"

OM@qinc<-2
OMd@qinc<-2

save(OM,file=paste0(getwd(),"/Objects/ROMs/17/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/17/OMd"))


# ROM 18 ----------------------------

OM<-ROM59
OMd<-ROM59d

OM@Name<-"ROM_18"
OMd@Name<-"ROM_18d"

OM@qinc<-2
OMd@qinc<-2

save(OM,file=paste0(getwd(),"/Objects/ROMs/18/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/18/OMd"))


# --- Catchability decrease of 2% in indices 28-29 ----------------------------------

# ROM 19 -------------------------------

OM<-ROM58
OMd<-ROM58d

OM@Name<-"ROM_19"
OMd@Name<-"ROM_19d"

OM@qinc<-(-2)
OMd@qinc<-(-2)

save(OM,file=paste0(getwd(),"/Objects/ROMs/19/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/19/OMd"))


# ROM 20 ------------------------------

OM<-ROM59
OMd<-ROM59d

OM@Name<-"ROM_20"
OMd@Name<-"ROM_20d"

OM@qinc<-(-2)
OMd@qinc<-(-2)

save(OM,file=paste0(getwd(),"/Objects/ROMs/20/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/20/OMd"))



# --- Add hyperstability / hyperdepletion to indices 21-22 -------------------------------------

# ROM 21 --------------

OM<-ROM58
OMd<-ROM58d

OM@Name<-"ROM_21"
OMd@Name<-"ROM_21d"

OM@Ibeta_ignore<-FALSE
OMd@Ibeta_ignore<-FALSE

save(OM,file=paste0(getwd(),"/Objects/ROMs/21/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/21/OMd"))


# ROM 22 -----------------

OM<-ROM59
OMd<-ROM59d

OM@Name<-"ROM_22"
OMd@Name<-"ROM_22d"

OM@Ibeta_ignore<-FALSE
OMd@Ibeta_ignore<-FALSE

save(OM,file=paste0(getwd(),"/Objects/ROMs/22/OM"))
OM<-OMd
save(OM,file=paste0(getwd(),"/Objects/ROMs/22/OMd"))


# ROMs 23 and 24 are just OM 58 and 59 using the Unreported_20 observation error model and Overage_20 implementation error model


# ==== END ==================================================================================================================================

