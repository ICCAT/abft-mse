# ============================================================
# === Copy datafiles to the  R package
# ============================================================

setwd("C:/ABT-MSE")
datadir<-paste0(getwd(),"/R package/ABTMSE/data/")


# Observation models

obsdir<-paste0(getwd(),"/Objects/Observation models/")
files<-list.files(obsdir)
from_files<-paste0(obsdir,files)
to_files<-paste0(datadir,files)

file.copy(from_files,to_files,overwrite=T)


# Operating models and operating model input files 

OMdir<-paste0(getwd(),"/Objects/OMs/")
OMdirs<-list.dirs(OMdir)
OMdirs<-OMdirs[2:length(OMdirs)]# get rid of the root directory

for(o in 1:length(OMdirs)){
  
  OMcode<-strsplit(OMdirs[o],split="//")[[1]][2]
  
  from_file<-paste0(OMdirs[o],"/OMI")
  to_file<-paste0(datadir,"OMI_",OMcode)
  file.copy(from_file,to_file,overwrite=T)
  
  from_file<-paste0(OMdirs[o],"/OM")
  to_file<-paste0(datadir,"OM_",OMcode)
  file.copy(from_file,to_file,overwrite=T)
 
}


# Examples ==========================

OMdir<-paste0(getwd(),"/Objects/OMs/")
OMdirs<-list.dirs(OMdir)
outdir<-c(datadir,paste0(getwd(),"/Objects/Examples/"))

# Example OMI

from_file<-paste0(OMdirs[2],"/OMI")
to_file<-paste0(outdir,"OMI_example")
file.copy(from_file,to_file,overwrite=T)

# Example OM

from_file<-paste0(OMdirs[2],"/OM")
to_file<-paste0(outdir,"OM_example")
file.copy(from_file,to_file,overwrite=T)

# Example Allocation

load(from_file)
Assess_areas<-c(rep(2,4),rep(1,OM@nareas-4))
nAss<-max(Assess_areas)
Allocation_example<-array(0,c(nAss,OM@nfleets))
Cdist<-apply(OM@Cobs[(OM@nyears-2):OM@nyears,,,],3:4,sum)
for(a in 1:nAss)Allocation_example[a,]<-apply(Cdist[Assess_areas==a,],2,sum)/sum(Cdist[Assess_areas==a,])
for(o in 1:length(outdir))save(Allocation_example,file=paste0(outdir[o],"Allocation_example"))

# Example Obs

obsdir<-paste0(getwd(),"/Objects/Observation models/")
files<-list.files(obsdir)
from_files<-paste0(obsdir,files[1])
to_files<-paste0(outdir,"Obs_example")
file.copy(from_files,to_files,overwrite=T)

# Example Recruitment

load(paste0(getwd(),"/Objects/Recruitment scenarios/Trial specifications"))
Recruitment_example<-Recs[[3]]
for(o in 1:length(outdir))save(Recruitment_example,file=paste0(outdir[o],"Recruitment_example"))

# Example MSE

load(paste0(getwd(),"/Objects/Examples/OM_example"))
load(paste0(getwd(),"/Objects/Observation models/Bad_Obs"))
sfInit(parallel=T,cpus=detectCores())
MSE_example<-new('MSE',OM,Obs,MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI")),interval=3,IE="Umax")
sfStop()
for(o in 1:length(outdir))save(MSE_example,file=paste0(outdir[o],"MSE_example"))

# Example pset (requires MSE run)

pset_example_East<-pset[[1]]
pset_example_West<-pset[[2]]

save(pset_example_East,file=)
for(o in 1:length(outdir)){
  save(pset_example_East,file=paste0(outdir[o],"pset_example_East"))
  save(pset_example_West,file=paste0(outdir[o],"pset_example_West"))
}

