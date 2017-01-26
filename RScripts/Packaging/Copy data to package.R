# ============================================================
# === Copy datafiles to the  R package
# ============================================================
library('ABTMSE')
loadABT()

setwd("C:/ABT-MSE")
datadir<-paste0(getwd(),"/R package/ABTMSE/data/")


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

}

# Observation models

obsdir<-paste0(getwd(),"/Objects/Observation models/")
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
OM_example<-new('OM',OMdirs[10],nsim=48,proyears=30,seed=1)
load(paste0(getwd(),"/Objects/OMs/1/OM"))
sfInit(parallel=T,cpus=detectCores())
myMPs<-list(c('UMSY','UMSY'),
            c('DD_i4','DD_i2'),
            c('MeanC','MeanC'))
MSE_example<-new('MSE',OM=OM_example,Obs=Bad_Obs,MPs=myMPs,interval=5,IE="Umax_90")
save(MSE_example,file=paste0(datadir,"MSE_example"))

# myMSE -----------------
MPs<-list(
  c("MeanC",      "MeanC"),
  c("SP_i4",      "SP_i2"),
  c("DD_i4",      "DD_i2"),
  c("DD_i4_4010", "DD_i2_4010"),
  c("Islope1_i4", "Islope1_i2"))

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



# Example pset (requires MSE run) --------------------------------------------------------------------

dset_example_East<-dset[[1]]
dset_example_West<-dset[[2]]

save(dset_example_East,file=paste0(datadir,"dset_example_East"))
save(dset_example_West,file=paste0(datadir,"dset_example_West"))

# The MSE design matrix -------------------------------------------------------------------------------

from_file<-paste0(getwd(),"/Objects/OMs/Design.Rdata")
to_file<-paste0(datadir,"Design.Rdata")
file.copy(from_file,to_file,overwrite=T)


# END OF CREATE EXAMPLES =========================================================================================



