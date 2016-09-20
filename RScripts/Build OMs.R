# Trial specifications
rm(list=ls(all=TRUE))                  # Remove all existing objects from environment
set.seed(2)                            # Ensure reproducible results by setting random seed

setwd("G:/ABT-MSE/")                   # The workign location

# --- Source MSE functions and objects ------

source("Source/MSE_source.r")
source("Source/Objects.r")

funcs<-c("MatM_Ref","Steep_Ref","Ind_Ref","Dep_Ref")                            # The names of the various modifying functions that alter a base input file object (Base_OM) into the various reference OMs

getlevs<-function(x,funcs)1:do.call(funcs[x],args=list(NA))                     # Returns a vector 1:nlevels for each of the 'funcs' e.g. this is c(1,2,3) for a 3-level factor (e.g. three mortality scenarios)
getnams<-function(x,funcs,lev='Names')do.call(funcs[x],args=list(lev=lev))      # Returns the short name of each factor level
getlnams<-function(x,funcs,lev='LongNames')do.call(funcs[x],args=list(lev=lev)) # Returns the long name of each factor level

all_levs<-sapply(1:length(funcs),getlevs,funcs=funcs)                           # Returns a list length(funcs) long of 1:nlevels for all funcs
all_nams<-sapply(1:length(funcs),getnams,funcs=funcs)                           # Returns a list length(funcs) long of the short names of the levels for all funcs
all_lnams<-sapply(1:length(funcs),getlnams,funcs=funcs)                         # Returns a list length(funcs) long of the long names of the levels for all funcs

Design_Ref<-expand.grid(all_levs)                                               # The full design grid (all combinations) of the various factors and their levels
Names_Ref<-expand.grid(all_nams)
LNames_Ref<-expand.grid(all_lnams)

Names_Ref[] <- lapply(Names_Ref, as.character)
LNames_Ref[] <- lapply(LNames_Ref, as.character)


nOMs<-nrow(Design_Ref)                                                          # The total number of operating models
nFactors<-ncol(Design_Ref)                                                      # The total number of factors (= length(funcs))

M3temp<-"C:/M3temp/"                                                            # A temporary directory for running operating models (M3.exe model fitting) in parallel
OMDir<-"C:/M3"                                                                  # The location of the latest version of the M3 operating model

for(i in 1:nOMs){
  
  # --- Load an operating model input object template, modify it and (1) save the inputs to home folders and (2) write the appropriate input data files in temporary parallel directories
  
  load(file=paste(getwd(),"/Objects/Reference OMs/Base_OM",sep=""))    # reference base operating model
  OMfolder<-paste(getwd(),"/Objects/Reference OMs/",i,sep="")          # the home directory for a new modified operating model
  
  if(!dir.exists(OMfolder))dir.create(OMfolder)                        # create the directory if necessary
  
  for(ff in 1:4){#nFactors){ # apply all modifications
    
    OMI<-do.call(funcs[ff],args=list(OMI=OMI,lev=Design_Ref[i,ff]))    # loop over the various axes (funcs) for the reference set of operating models applying the appropriate changes
  
  }
  
  OMI@Name<-paste(unlist(c(i,"/",nOMs,":",Design_Ref[i,],":",Names_Ref[i,])),collapse=" ")
  save(OMI,file=paste(OMfolder,"/OMI",sep=""))                         # save the input object into its home folder

  tempfolder<-paste(M3temp,i,sep="")                                   # create a temporary folder for running the operating models in parallel
  if(!dir.exists(tempfolder))dir.create(tempfolder)                    # make the folder if necessary
  
  file.copy(paste(OMDir,"/M3.exe",sep=""),tempfolder,overwrite=T)      # copy the latest executable to the temporary 
  file.copy(paste(OMDir,"/stats.cxx",sep=""),tempfolder,overwrite=T)   # copy over the statistics library
  file.copy(paste(OMDir,"/M3.pin",sep=""),tempfolder,overwrite=T)      # copy over the parameter initialization file
  
  M3write(OMI,datfile=paste(tempfolder,"/M3.dat",sep=""))              # write the appropriate data file into the temporary folder ready to be run in parallel

}


sfStop()
sfInit(parallel=T,cpus=8)              # Initiate the cluster

sfLapply(1:nOMs,runM3,M3temp="C:/M3temp") # run the M3 script

storeoutputs(fromdir=paste(M3temp,1:nOMs,sep=""),todir=paste(getwd(),"/Objects/Reference OMs/",1:nOMs,sep=""))


MPInd<-read.csv(paste0(getwd(),"/Data/Processed/MP Indices/MP indices compiled.csv"))
make_fit_reports(dirs=paste(getwd(),"/Objects/Reference OMs/",1:nOMs,sep=""))



dim(out$SSB)
dim(out$N)


SSB<-apply(out$SSB,1:2,mean)
SSBt<-SSB/apply(SSB,1,mean)
N<-apply(apply(out$N[,,,6:out$na,],1:3,sum),1:2,mean)
Nt<-N/apply(N,1,mean)



