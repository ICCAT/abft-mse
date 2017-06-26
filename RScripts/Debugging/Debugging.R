
setwd("E:/ABT-MSE/")                   # The workign location

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


# Replicate reference OM 1 to all folders to test code

nOMs<-nrow(Design_Ref)

reffolder<-paste(getwd(),"/Objects/Reference OMs/",1,sep="")   

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
  
  file.copy(paste(reffolder,"/M3.rep",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary 
  file.copy(paste(reffolder,"/M3.cor",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary 
  file.copy(paste(reffolder,"/M3.par",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary 
  file.copy(paste(reffolder,"/out",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
 
  
}


# OMd conversion

sfInit(parallel=T,cpus=detectCores()-4)

setMethod("initialize", "OM", function(.Object,OMd="C:/M3",nsim=4,proyears=30,seed=1){
  .Object})

.Object<-new('OM',OMd)

OMd=paste(getwd(),"/Objects/Reference OMs/",24,sep="") 
nsim=4
proyears=30
seed=1
targpop<-1



