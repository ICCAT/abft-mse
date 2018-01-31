# ============================================================
# === Copy datafiles to the  R package
# ============================================================
library('ABTMSE')
loadABT()

setwd("C:/GitHub/abft-mse")
datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")


# Operating models and operating model input files

OMdir<-paste0(getwd(),"/Objects/OMs/")
OMdirs<-list.dirs(OMdir)
OMdirs<-OMdirs[2:length(OMdirs)]# get rid of the root directory


for(o in 1:length(OMdirs)){

  OMcode<-strsplit(OMdirs[o],split="//")[[1]][2]

  from_file<-paste0(OMdirs[o],"/OMI")
  to_file<-paste0(datadir,"OMI_",OMcode)
  load(from_file)
  objname<-paste0("OMI_",OMcode)
  assign(objname,OMI)
  do.call(save, list(objname,file=to_file))

  from_file<-paste0(OMdirs[o],"/OM")
  to_file<-paste0(datadir,"OM_",OMcode)
  load(from_file)
  objname<-paste0("OM_",OMcode)
  assign(objname,OM)
  do.call(save, list(objname,file=to_file))
  cat(paste0(o," - "))

}

# Observation models

obsdir<-paste0(getwd(),"/Objects/Observation_models/")
files<-list.files(obsdir)
from_files<-paste0(obsdir,files)
to_files<-paste0(datadir,files)

file.copy(from_files,to_files,overwrite=T)


# Examples ==========================

OMdir<-paste0(getwd(),"/Objects/OMs/")
OMdirs<-list.dirs(OMdir)

# Example OMI ----------------------------------------------------------------------------------------

from_file<-paste0(OMdirs[10],"/OMI")
load(from_file)
OMI_example<-OMI
save(OMI_example,file=paste0(datadir,"OMI_example"))

# Example OM ------------------------------------------------------------------------------------------

OM_example<-new('OM',OMdirs[10],nsim=16,proyears=30,seed=1)
save(OM_example,file=paste0(datadir,"OM_example"))

# Example Allocation ----------------------------------------------------------------------------------

load(from_file)
Assess_areas<-c(rep(2,4),rep(1,OM@nareas-4))
nAss<-max(Assess_areas)
Allocation_example<-array(0,c(nAss,OM@nfleets))
Cdist<-apply(OM@Cobs[(OM@nyears-2):OM@nyears,,,],3:4,sum)
for(a in 1:nAss)Allocation_example[a,]<-apply(Cdist[Assess_areas==a,],2,sum)/sum(Cdist[Assess_areas==a,])
save(Allocation_example,file=paste0(datadir,"Allocation_example"))

MPareas_example<-Assess_areas
save(MPareas_example,file=paste0(datadir,"MPareas_example"))



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

myMPs<-list(c('UMSY','UMSY'),
            c('MeanC','DD_i7'),
            c('MeanC','MeanC'))

#OM<-OM_example
#Obs<-Bad_Obs
#MPs<-myMPs
#interval=5
#IE="Umax_90"
#curTAC=c(13500000,2000000)
#Allocation=NA
#MPareas=NA
#Fdistyrs=3

MSE_example<-new('MSE',OM=OM_example,Obs=Bad_Obs,MPs=myMPs,interval=5,IE="Umax_90")
save(MSE_example,file=paste0(datadir,"MSE_example"))

# myMSE -----------------
MPs<-list(
  c("MeanC",      "MeanC"),
  c("MeanC",      "SP_i7"),
  c("MeanC",      "DD_i7"),
  c("MeanC", "DD_i7_4010"),
  c("Islope1_i7", "Islope1_i3"))

myMSE<-new('MSE',OM=OM_example,Obs=Bad_Obs,MPs=MPs)
save(myMSE,file=paste0(datadir,"myMSE"))

# MSE_default etc ------------
MPs<-list(c("UMSY","UMSY"),
          c("MeanC","MeanC"))

MSE_default<-new('MSE',OM_example,Bad_Obs,MPs)
save(MSE_default,file=paste0(datadir,"MSE_default"))


MSE_alt_MPareas<-new('MSE',OM_example,Bad_Obs,MPs,
                     MPareas=c(rep(2,6),rep(1,4))) # Central Atlantic areas go to 'West'

save(MSE_alt_MPareas,file=paste0(datadir,"MSE_alt_MPareas"))


alt_Allocation<-array(as.integer(Allocation>0),dim(Allocation)) # binary allocation
alt_Allocation<-alt_Allocation/apply(alt_Allocation,1,sum)      # even fractions among fleets

MSE_alt_Allocation<-new('MSE',OM_example,Bad_Obs,MPs,Allocation=alt_Allocation)
save(MSE_alt_Allocation,file=paste0(datadir,"MSE_alt_Allocation"))



# Example dset (requires MSE run) --------------------------------------------------------------------

MSE<-new('MSE',interval=10,IE="Umax_90")

dset_example_East<-dset[[1]]
dset_example_West<-dset[[2]]

save(dset_example_East,file=paste0(datadir,"dset_example_East"))
save(dset_example_West,file=paste0(datadir,"dset_example_West"))

# The MSE design matrix ------------------------------------------------------------------------------
datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")

from_file<-paste0(getwd(),"/Objects/OMs/Design.Rdata")
to_file<-paste0(datadir,"Design.Rdata")
file.copy(from_file,to_file,overwrite=T)

# Time series data -----------------------------------------------------------------------------------

ts2017<-read.csv(paste0(getwd(),"/data/Assessment_2017/ts2017.csv"))
save(ts2017,file=paste0(getwd(),"/R_package/ABTMSE/inst/ts2017.Rdata"))
# add a copy for ts.Rdata

# Custom MPs -----------------------------------------------------------------------------------------

library(ABTMSE)
loadABT()
setwd("C:/GitHub/abft-mse")
datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")


MP_E = function(x,dset,Targ=22.56,Deltaup=0.05,Deltadown=0.20,IndexNo=3,yrs4mean=3){

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

class(MP_E) = "MP"                         # Finally make sure it is of class MP

MP_W = function(x,dset,Targ=0.42,Deltaup=0.05,Deltadown=0.20,IndexNo=7,yrs4mean=3){

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


#setMethod("initialize", "MSE", function(.Object,OM=OM_example,Obs=Good_Obs,MPs=list(c("UMSY","UMSY")),interval=3,IE="Umax_90",
                                        curTAC=c(13500000,2000000),Allocation=NA,MPareas=NA,Fdistyrs=3){
#.Object})
#.Object<-new('MSE')

OM=OM_1
Obs=Good_Obs
interval=3
IE="Umax_90"
curTAC=c(13500000,2000000)
Allocation=NA
MPareas=NA
Fdistyrs=3

myMPs<-list(c('UMSY','UMSY'),
            c('MeanC','MeanC'),
            c('MP_E','MP_W'))
#c('UMSY','UMSY'),
#c('MeanC','MeanC'))
sfInit(detectCores())
myMSE2<-new('MSE',OM=OM_1,Obs=Perfect_Obs,MPs=myMPs,interval=5,IE="Umax_90")
save(myMSE2,file=paste0(datadir,"myMSE2"))



# END OF CREATE EXAMPLES =========================================================================================



