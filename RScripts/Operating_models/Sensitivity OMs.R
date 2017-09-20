# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of reference operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC
# Laurie Kell ICCAT

# 12th July 2017

# Because some operating models (the B scenarios) require prior fitting of other operating models
# (the A scenarios, since B scenarios are fitted to a fraction of the A scenarios current abundance)
# operating model definition and fitting runs in several phases.

# There are five phases to reference operating model specification:

# (1) Fit the base model (parameterized like the most recent stock assessment)
# (2) Fitting of various natural-mortality rate and maturity rate scenarios (I, II and III) (1, 7, 13)
# (3) Taking the outputs of step 2 and fitting the  modified depletion scenarios from them (A and B)
# (4) Copying the fitted models of 1 and 2 (to expand to the future recruitment scenarios) (1, 2 and 3) (4, 10, 16)
# (5) Create the future recruitment scenarios (1, 2, 3) and build operating model objects


#rm(list=ls(all=TRUE))                  # Remove all existing objects from environment
#setwd("C:/ABT-MSE/")                   # The working location
setwd("C:/Users/tcar_/Documents/abft-mse")

# --- Source MSE functions and objects ------

library(ABTMSE)
loadABT()
OMDir<-paste(getwd(),"/M3",sep="")


# ---- Alternative M -----------------------

load(file=paste(getwd(),"/Objects/OMs/Base_OM",sep=""))       # reference base operating model
OMcode<-"M07"
OMfolder<-paste(getwd(),"/Objects/Sensitivity_OMs/",OMcode,sep="")          # the home directory for a new modified operating model
if(!dir.exists(OMfolder))dir.create(OMfolder)                 # create the directory if necessary

OMI@Ma<-t(array(c(0.36, 0.27, 0.21, 0.17, 0.14, 0.12, 0.11, 0.10, 0.09, 0.09, 0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.07,rep(0.07,Base@na-18)),c(Base@na,Base@np)))
OMI@Name<-OMcode
OMI@OMfactors<-as.list(c("Exploratory sensitivity run","Alternative M vector scaled to 0.07 for older age classes","(A derivative of OM 1: 1 A I)"))
save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

file.copy(paste(OMDir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
file.copy(paste(OMDir,"/stats.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
file.copy(paste(OMDir,"/M3.pin",sep=""),OMfolder,overwrite=T)      # copy over the parameter initialization file

M3write(OMI,OMdir=OMfolder)              # write the appropriate data file into the temporary folder ready to be run in parallel
runM3(OMdir=paste0(getwd(),"/Objects/OMs"))
make_fit_reports(dirs=OMfolder,addlab=TRUE)


# ---- Alternative SSB0 starts -----------------

out<-M3read(paste0(getwd(),"/Objects/OMs/1"))
load(paste(input_dir,"/OMI",sep=""))
refdir<-paste0(getwd(),"/Objects/OMs/1")

# East ---

OMI@SSBprior<-out$SSB0*c(0.5,1)                                    # downward adjustment in estimated current stock size for level B operating models
OMI@SSBfit<-1
OMI@LHw[12]<-100                                                        # put a likelihood weight on the desired current SSB
OMI@SSBCV<-0.025                                                        # specify a very informative (low CV) prior

OMfolder<-paste0(getwd(),"/Objects/Sensitivity_OMs/SSB0_half_East")
if(!dir.exists(OMfolder))dir.create(OMfolder)                 # create the directory if necessary

OMI@Name<-"SSB0_half_East"
OMI@OMfactors<-as.list(c("Eastern SSB0 is half of estimated value","a derivative of OM1",""))
save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

file.copy(paste(refdir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
file.copy(paste(refdir,"/stats.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
file.copy(paste(refdir,"/M3.pin",sep=""),OMfolder,overwrite=T)      # copy over the parameter initialization file

M3write(OMI,OMdir=OMfolder)              # write the appropriate data file into the temporary folder ready to be run in parallel


# West

OMI@SSBprior<-out$SSB0*c(1,0.5)                                    # downward adjustment in estimated current stock size for level B operating models
OMI@SSBfit<-1
OMI@LHw[12]<-100                                                        # put a likelihood weight on the desired current SSB
OMI@SSBCV<-0.025                                                        # specify a very informative (low CV) prior

OMfolder<-paste0(getwd(),"/Objects/Sensitivity_OMs/SSB0_half_West")
if(!dir.exists(OMfolder))dir.create(OMfolder)                 # create the directory if necessary

OMI@Name<-"SSB0_half_West"
OMI@OMfactors<-as.list(c("Western SSB0 is half of estimated value","a derivative of OM1",""))
save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

file.copy(paste(refdir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
file.copy(paste(refdir,"/stats.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
file.copy(paste(refdir,"/M3.pin",sep=""),OMfolder,overwrite=T)      # copy over the parameter initialization file

M3write(OMI,OMdir=OMfolder)              # write the appropriate data file into the temporary folder ready to be run in parallel


# West like assess --------

OMfolder<-paste0(getwd(),"/Objects/Sensitivity_OMs/West_like_assess/")
load(paste0(OMfolder,"/OMI"))
OMI@Name<-"West area SSB like assess"
OMI@OMfactors<-as.list(c("Mean western SSB is comparable to the assessment","~30 000t","a derivative of OM1"))
save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder


# East four inc---------

OMfolder<-paste0(getwd(),"/Objects/Sensitivity_OMs/East_four_inc")
load(paste0(OMfolder,"/OMI"))
OMI@Name<-"East area  SSB increases like VPA"
OMI@OMfactors<-as.list(c("From 2008 - 2015 East area SSB has a four fold increase","a derivative of OM1",""))
save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder




# Run the models

foldernos<-c("SSB0_half_East","SSB0_half_West","West_like_assess","East_four_inc")[3:4]
                                                 # Initiate the cluster
OMdir<-paste0(getwd(),"/Objects/Sensitivity_OMs/")

sfInit(parallel=T,cpus=2)
sfLapply(foldernos,runM3p,OMdir=OMdir)             # Run the M3 executables in parallel


# Summarize results ======================
foldernos<-c("SSB0_half_East","SSB0_half_West","West_like_assess","East_four_inc")
OMdir<-paste0(getwd(),"/Objects/Sensitivity_OMs/")

OMdirs<-paste0(OMdir,foldernos)
make_fit_reports(dirs=OMdirs,addlab=TRUE)
make_comp_report(OMdirs,dir=OMdir)

make_fit_reports(dirs=OMdirs[3:4],addlab=TRUE)


# === Make all fit reports ===== ===============================
if(reportbuild){
  OMdirs<-paste0(getwd(),"/Objects/OMs/",c(1,7,13,19,4,10,16,22)) # the actual fitted models
  #OMdirs<-list.dirs(OMdir)
  #OMdirs<-OMdirs[2:length(OMdirs)]# get rid of the root directory
  #make_fit_reports(dirs=OMdirs[1],addlab=TRUE)
  make_fit_reports(dirs=OMdirs,addlab=TRUE)           # make_fit_reports(dirs=paste(getwd(),"/Objects/OMs/",1,sep="")) #make_fit_reports(dirs=paste0(getwd(),"/M3"))
  # make_summary_report(dir=paste0(getwd(),"/Objects/OMs"),OMdirs=OMdirs)
  make_summary_report(dir=paste0(getwd(),"/Objects/OMs"))

}


# === Optional code =========================================================================================================================

# ---- Some code for testing a single model fit ------------------
#out<-M3read("C:/M3/")
#load(file=paste(getwd(),"/M3/OMI",sep=""))
#save(OMI,file="C:/M3/OMI")
#make_fit_reports(dirs="C:/M3")
# ----------------------------------------------------------------


# ==== END ==================================================================================================================================

