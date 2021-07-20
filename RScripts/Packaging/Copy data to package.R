# ============================================================
# === Copy datafiles to the  R package
# ============================================================


library('ABTMSE')
loadABT()

setwd("C:/Users/tcarruth/Dropbox/abft-mse")
setwd("C:/Users/tcar_/Dropbox/abft-mse")

OMdir<-paste0(getwd(),"/Objects/OMs/")
OMdirs<-list.dirs(OMdir)
OMdirs<-OMdirs[!grepl("Report",OMdirs)]
OMdirs<-OMdirs[2:length(OMdirs)]# get rid of the root directory
OMdirs<-OMdirs[!grepl("New",OMdirs)]

datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")


# Operating models and operating model input files


for(o in 1:length(OMdirs)){

  OMcode<-strsplit(OMdirs[o],split="//")[[1]][2]

  from_file<-paste0(OMdirs[o],"/OMI")
  to_file<-paste0(datadir,"OMI_",OMcode)
  load(from_file)
  objname<-paste0("OMI_",OMcode)
  assign(objname,OMI)
  do.call(save, list(objname,file=to_file))

  temp<-TRUE
  if(temp){
  from_file<-paste0(OMdirs[o],"/OM_",OMcode)
  to_file<-paste0(datadir,"OM_",OMcode)
  load(from_file)
  objname<-paste0("OM_",OMcode)
  assign(objname,OM)
  do.call(save, list(objname,file=to_file))

  from_file<-paste0(OMdirs[o],"/OM_",OMcode,"d")
  to_file<-paste0(datadir,"OM_",OMcode,"d")
  load(from_file)
  objname<-paste0("OM_",OMcode,"d")
  assign(objname,OM)
  do.call(save, list(objname,file=to_file))
  }
  cat(paste0(OMcode," - "))

}


# -- Do robustness OMs here ---------------------------------

nOMs<-44
OMnams<-paste0("ROM_",1:nOMs)
OMdirs<-paste(getwd(),"/objects/ROMs/",1:nOMs,sep="")

for(o in 1:length(OMdirs)){

    OMIfile<-paste0(OMdirs[o],"/OMI")
    if(file.exists(OMIfile)){
      from_file<-OMIfile
      to_file<-paste0(datadir,"OMI_R",o)
      load(from_file)
      objname<-paste0("OMI_R",o)
      assign(objname,OMI)
      do.call(save, list(objname,file=to_file))
    }

    from_file<-paste0(OMdirs[o],"/OMd")
    load(from_file)
    to_file<-paste0(datadir,"ROM_",o,'d')
    objname<-paste0("ROM_",o,"d")
    assign(objname,OMd)
    do.call(save, list(objname,file=to_file))

    from_file<-paste0(OMdirs[o],"/OM")
    load(from_file)
    to_file<-paste0(datadir,"ROM_",o)
    objname<-paste0("ROM_",o)
    assign(objname,OM)
    do.call(save, list(objname,file=to_file))

    cat(paste0(o," - "))

}


# Allocation

Allocation<-array(0,c(OM_1d@nareas,OM_1d@nfleets))
Cdist<-apply(OM_1d@Cobs[(OM_1d@nyears-2):OM_1d@nyears,,,],3:4,sum)
MPareas<-c(2,2,2,1,1,1,1)
Assess_data<-array(rep(MPareas,each=2)==rep(1:2,OM_1d@nareas),c(2,OM_1d@nareas))
#byfleet0<-read.csv(paste0(getwd(),"/data/Processed/Allocations/Allocation_by_fleet_2020_2.csv"))
byfleet<-read.csv(paste0(getwd(),"/data/Processed/Allocations/Allocation_by_fleet_2021.csv"))


for(f in 1:nrow(byfleet)){

  fleetno<-match(byfleet$Fleetnam[f],OMI_1@Fleets$name)
  areas<-as.numeric(strsplit(as.character(byfleet$Areas[f])," and ")[[1]])
  fracs<-Cdist[areas,fleetno]/sum(Cdist[areas,fleetno])
  if(all(is.na(fracs)))fracs<-1/length(fracs)
  Allocation[areas,fleetno]<-byfleet$Total[f]*fracs

}

for(AS in 1:2)  Allocation[Assess_data[AS,],]<-Allocation[Assess_data[AS,],]/sum(Allocation[Assess_data[AS,],])
save(Allocation,file=paste0(datadir,"Allocation.rda"))


# OM plausibility weighting from Kimoto and Walter Feb 2021

r_wt<-c(0.4,0.4,0.2)
m_wt<-c(0.5,0.5)
s_wt<-c(0.3,0.3,0.15,0.25)
w_wt<-c(0.5,0.5)

OM_wt1<- r_wt[match(Design$Design_Ref[,1],Design$all_levs[[1]])] *
        m_wt[match(Design$Design_Ref[,2],Design$all_levs[[2]])] *
        s_wt[match(Design$Design_Ref[,3],Design$all_levs[[3]])] *
        w_wt[match(Design$Design_Ref[,4],Design$all_levs[[4]])]

ROM_wt<-OM_wt1[rep(c(1,2,4,5),11)]
ROM_wt[33:36]<-OM_wt[c(3,6,3,6)]

OM_wt<-c(OM_wt1,ROM_wt)*1E4

save(OM_wt,file=paste0(datadir,"OM_wt.RData"))




# =======================================================================================================================================================================================
# =======================================================================================================================================================================================
# =======================================================================================================================================================================================
# you need to rebuild before carrying on here
# =======================================================================================================================================================================================
# =======================================================================================================================================================================================
# =======================================================================================================================================================================================

library('ABTMSE')
loadABT()
#for(i in c(1,5,7,8,10,11,2,3)){
#  OMtemp<-new('OM',OMdirs[i],nsim=10,proyears=35,seed=1,Recruitment<-Recs[[1]])
#  test<-new('MSE',OM=OMtemp,check=T)
#}

#test_1d<-new('MSE',OM=OM_1d,check=T)
#test_1<-new('MSE',OM=OM_1,check=T)

# Observation models =============================================

obsdir<-paste0(getwd(),"/Objects/Observation_models/")
files<-list.files(obsdir)
from_files<-paste0(obsdir,files)
to_files<-paste0(datadir,files)

file.copy(from_files,to_files,overwrite=T)


# Examples ==========================

OMdir<-paste0(getwd(),"/Objects/OMs/")
OMdirs<-list.dirs(OMdir)

# Example OMI ----------------------------------------------------------------------------------------

from_file<-paste0(OMdirs[2],"/OMI")
load(from_file)
OMI_example<-OMI
save(OMI_example,file=paste0(datadir,"OMI_example"))

# Example OM ------------------------------------------------------------------------------------------
# might need to add recruitment examples for OMs not level 1
load(paste0(getwd(),"/Objects/Recruitment_scenarios/Trial specifications"))

SD_override<-data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR"),
                        SD=c(  0.45,         0.45,            0.8,          0.45              ))

AC_override<- data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR","JPN_LL_West2"),
                         AC=c(  0.2,         0,               0.2,          0.2,                         0))

Yrs_override<-data.frame(Name='MED_LAR_SUV',start=48, end=55)
CPUEinds<-c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_66_144","US_RR_177","MEXUS_GOM_PLL","JPN_LL_West2","CAN GSL","CAN SWNS")
Iinds<-c("FR_AER_SUV2","MED_LAR_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR")


OM<-OM_example<-new('OM',OMdirs[2],nsim=16,proyears=54,seed=1,Recruitment=Recs[[1]],MLEonly=T,SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)
#OMd<-new('OM',OMd=OMdirs[2],nsim=2,proyears=35,seed=1,MLEonly=T,Recruitment=Recs[[1]])

#test_d<-new('MSE',OM=OMd,check=T)
#test<-new('MSE',OM=OM,check=T)

save(OM_example,file=paste0(datadir,"OM_example"))



# Example Obs ---------------------------------------------------------------------------------------

obsdir<-paste0(getwd(),"/Objects/Observation_models/")
files<-list.files(obsdir)
from_file<-paste0(obsdir,files[1])
load(from_file)
Obs_example<-Bad_Obs
save(Obs_example,file=paste0(datadir,"Obs_example"))

# Example Recruitment -------------------------------------------------------------------------------

load(paste0(getwd(),"/Objects/Recruitment_scenarios/Trial specifications"))
Recruitment_example<-Recs[[3]]
save(Recruitment_example,file=paste0(datadir,"Recruitment_example"))

# Example MSE (these should match the vignette documentation) -----------------------------------------

# MSE_example ----------
#load(paste0(getwd(),"/Objects/OMs/1/OM"))
sfInit(parallel=T,cpus=detectCores())

myMPs<-list(c('U5','U5'),
            c('MeanC','DD_i4'),
            c('MeanC','MeanC'))

MSE_example<-new('MSE',OM=OM_example,MPs=myMPs)
save(MSE_example,file=paste0(datadir,"MSE_example"))

# myMSE -----------------
MPs<-list(
  c("MeanC",      "MeanC"),
  c("MeanC",      "SP_i4"),
  c("MeanC",      "DD_i4"),
  c("MeanC", "DD_i4_4010"),
  c("Islope1_i4", "Islope1_i7"))

myMSE<-new('MSE',OM=OM_example,MPs=MPs)
save(myMSE,file=paste0(datadir,"myMSE"))

# MSE_default etc ------------
MPs<-list(c("U5","U5"),
          c("MeanC","MeanC"))

MSE_default<-new('MSE',OM_example,MPs)
save(MSE_default,file=paste0(datadir,"MSE_default"))


MSE_alt_MPareas<-new('MSE',OM_example,MPs,
                     MPareas=c(rep(2,4),rep(1,3))) # NOrth Atlantic area goes to 'West'

save(MSE_alt_MPareas,file=paste0(datadir,"MSE_alt_MPareas"))


# Example dset (requires MSE run) --------------------------------------------------------------------

MSE<-new('MSE',OM_example)

dset_example_East<-dset[[1]]
dset_example_West<-dset[[2]]

save(dset_example_East,file=paste0(datadir,"dset_example_East"))
save(dset_example_West,file=paste0(datadir,"dset_example_West"))

# The MSE design matrix ------------------------------------------------------------------------------
datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")

from_file<-paste0(getwd(),"/Objects/OMs/Design")
to_file<-paste0(datadir,"Design")
file.copy(from_file,to_file,overwrite=T)
load(paste0(getwd(),"/Objects/OMs/Design"))
save(Design,file=paste0(datadir,"Design.RData"))

# Time series data -----------------------------------------------------------------------------------

ts2017<-read.csv(paste0(getwd(),"/data/Assessment_2017/ts2017update.csv"))
save(ts2017,file=paste0(getwd(),"/R_package/ABTMSE/inst/ts2017"))
# add a copy for ts.Rdata

# Custom MPs -----------------------------------------------------------------------------------------

library(ABTMSE)
loadABT()
setwd("C:/Users/tcar_/dropbox/abft-mse")
datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")


MP_E = function(x,dset,Targ=22.56,Deltaup=0.05,Deltadown=0.20,IndexNo=2,yrs4mean=3){

  lastyr = dim(dset$Iobs)[3]                # Most recent year
  datayrs = lastyr-(yrs4mean-1):0           # Position of data for calculating current index
  curI = mean(dset$Iobs[x,IndexNo,datayrs],na.rm=T) # mean of last four years
  Irat = curI/Targ                          # Index ratio
  oldTAC = dset$MPrec[x]                    # The last TAC recommendation

  if(Irat<(1-Deltadown)){                   # If index ratio is less than minimum adjustment
    TAC = oldTAC*(1-Deltadown)

  }else if(Irat>(1+Deltaup)){               # If index ratio is greater than maximum adjustment
    TAC = oldTAC*(1+Deltaup)

  }else{
    TAC = oldTAC*Irat
  }

  TAC                                       # Last thing returned is the TAC recommendation

}

class(MP_E) = "MP"                         # Finally make sure it is of class MP

MP_W = function(x,dset,Targ=4,Deltaup=0.05,Deltadown=0.20,IndexNo=4,yrs4mean=3){

  lastyr = dim(dset$Iobs)[3]                # Most recent year
  datayrs = lastyr-(yrs4mean-1):0           # Position of data for calculating current index
  curI = mean(dset$Iobs[x,IndexNo,datayrs],na.rm=T) # mean of last four years
  Irat = curI/Targ                          # Index ratio
  oldTAC = dset$MPrec[x]                    # The last TAC recommendation

  if(Irat<(1-Deltadown)){                       # If index ratio is less than minimum adjustment
    TAC = oldTAC*(1-Deltadown)

  }else if(Irat>(1+Deltaup)){                 # If index ratio is greater than maximum adjustment
    TAC = oldTAC*(1+Deltaup)

  }else{
    TAC = oldTAC*Irat
  }

  TAC                                       # Last thing returned is the TAC recommendation

}

class(MP_W) = "MP"                         # Finally make sure it is of class MP


myMPs<-list(c('U5','U5'),
            c('MeanC','MeanC'),
            c('MP_E','MP_W'))

sfStop()
myMSE2<-new('MSE',OM=OM_1,MPs=myMPs)
save(myMSE2,file=paste0(datadir,"myMSE2"))


# Annual catches

source(paste0(getwd(),"/RScripts/Observation_models/Build Annual Catches.r"))





# END OF CREATE EXAMPLES =========================================================================================



