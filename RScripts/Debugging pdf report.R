
rm(list=ls(all=TRUE))                  # Remove all existing objects from environment
set.seed(2)                            # Ensure reproducible results by setting random seed

setwd("E:/ABT-MSE/")                   # The workign location

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

source("Source/MSE_source.r")


make_fit_reports(dirs="E:/M3")

