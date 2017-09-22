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
# (3) Taking the OMs from step 2 and add the  modified abundance scenarios from them (A, B and C)
# (4) Copying the fitted models of 1 and 2 (to expand to the future recruitment scenarios) (1, 2 and 3)
# (5) Create the future recruitment scenarios (1, 2, 3) and build operating model objects


#rm(list=ls(all=TRUE))                       # Remove all existing objects from environment
#setwd("C:/ABT-MSE/")                        # The working location
#setwd("C:/Users/tcar_/Documents/abft-mse")
setwd("C:/GitHub/abft-mse")

# Some toggles
runinitialOM<-F
runM3OM<-T                             # Should the operating models be fitted (ie only new OMI and OM objects?)
reportbuild<-T                         # should the OM reports / summary reports be built?
OMbuild<-T                             # should the OM objects be built?

# --- Source MSE functions and objects ------

library(ABTMSE)
loadABT()
#source("Source/MSE_source.r")
#source("Source/Objects.r")
OMDir<-paste(getwd(),"/M3",sep="")


# --- Set up the MSE design matrix

all_levs<-list(c("1","2","3"), c("A","B","C"), c("I","II","III","IV"))
all_lnams<-list( c("1: West - Hockey stick, East - '83+ B-H h=0.98",
                   "2: West - B-H h estimated, East - '83+ B-H h=0.7",
                   "3: West - Hockey stock changes to B-H after 10 yrs, East - 83+ B-H with h=0.98 changes to '50-82 B-H with h=0.98 after 10 years"),

                 c("A: West - Best estimates of SSB",
                   "B: Western Area SSB matches VPA",
                   "C: Eastern Area SSB inc as VPA"),

                  c("I: Younger spawning, High M",
                    "II: Younger spawning, Low M",
                    "III: Older spawning, High M",
                    "IV: Older spawning, Low M")
               )

Design_Ref<-expand.grid(all_levs)                                               # The full design grid (all combinations) of the various factors and their levels
LNames_Ref<-expand.grid(all_lnams)

Design<-list()                 # Summarize the MSE OM design
Design$all_levs<-all_levs      # Returns a list length(funcs) long of 1:nlevels for all funcs
Design$all_lnams<-all_lnams    # Returns a list length(funcs) long of the long names of the levels for all funcs
Design$Design_Ref<-Design_Ref  #  The full design grid (all combinations) of the various factors and their levels
Design$LNames_Ref<-LNames_Ref
save(Design,file=paste0(getwd(),"/Objects/OMs/Design.Rdata"))
save(Design,file=paste0(getwd(),"/R_package/ABTMSE/data/Design.Rdata"))

nOMs<-nrow(Design_Ref)
OMcodes<-apply(Design_Ref,1,FUN=function(x)paste(x,collapse="-"))



# === Step 1: Fit the base operating model (~ 1 hour) =============================================================================================================

if(runinitialOM){
  runM3(OMDir)                      # Run the base M3 operating model
  pin_from_par(OMDir)               # Record the MLE parameter estimates as initial values
  make_fit_reports(OMDir)           # Make_fit_reports(dirs=paste(getwd(),"/Objects/OMs/",1,sep="")) #make_fit_reports(dirs=paste0(getwd(),"/M3"))
}


# === Step 2: Fit the various Fitting of various natural-mortality rate and maturity rate scenarios (I, II and III) ====================================

# --- Build operating model objects and write them to folders ------------
if(OMbuild){

  for(i in 1:length(all_levs[[3]])){

    load(file=paste(getwd(),"/Objects/OMs/Base_OM",sep=""))       # reference base operating model
    OMcode<-paste0("1-A-",all_levs[[3]][i])
    OMno<-match(OMcode,OMcodes)
    OMfolder<-paste(getwd(),"/Objects/OMs/",OMno,sep="")          # the home directory for a new modified operating model
    if(!dir.exists(OMfolder))dir.create(OMfolder)                 # create the directory if necessary

    OMI<-MatM_Ref(OMI,i)

    OMI@Name<-paste0(OMno,"/",nOMs," : ",OMcode)
    OMI@OMfactors<-as.list(LNames_Ref[OMno,])
    save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

    file.copy(paste(OMDir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
    file.copy(paste(OMDir,"/stats.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
    file.copy(paste(OMDir,"/M3.pin",sep=""),OMfolder,overwrite=T)      # copy over the parameter initialization file

    M3write(OMI,OMdir=OMfolder)              # write the appropriate data file into the temporary folder ready to be run in parallel

  }

}

setwd("C:/abft-mse/")
#setwd("C:/Users/tcar_/Documents/abft-mse")

# --- Fit the 1-A-I, 1-A-II and 1-A-III operating models (~ 1 hour)  ----

if(runM3OM){

  sfInit(parallel=T,cpus=4)                                                  # Initiate the cluster

  foldernos<-match(paste0("1-A-",all_levs[[3]]),OMcodes)                     # Get correct folder numbers

  sfLapply(foldernos,runM3p,OMdir=paste0(getwd(),"/Objects/OMs"))             # Run the M3 executables in parallel

  for(i in foldernos)pin_from_par(paste0(getwd(),"/Objects/OMs/",i))   # Record the MLE parameter estimates as initial values

}


# === Step 3: Use step 2 outputs and fit the  modified current stock abundance scenarios (A,  B  and C) ===========================================

# --- Define the copy-modify-paste ----------

OMrefcodes<-rep(paste0("1-A-",all_levs[[3]]),2)    # The OMs that are being borrowed from 1-A-I, 1-A-II and 1-A-III
OMnewcodes<-c(paste0("1-B-",all_levs[[3]]), paste0("1-C-",all_levs[[3]]))      # The new OMs 1-B-I, 1-B-II and 1-B-III

reffoldernos<-match(OMrefcodes,OMcodes)     # The numbering of the reference OMs 1-A-I, 1-A-II and 1-A-III
foldernos<-match(OMnewcodes,OMcodes)           # The numbering of the folders


# --- Copy - modify - paste -----------------
if(OMbuild){

  for(i in 1:8){

    j<-rep(2:3,each=4)[i]

    load(file=paste0(getwd(),"/Objects/OMs/",reffoldernos[i],"/OMI"))       # load the reference operating model input object
    OMI<-SSBref(OMI,j)
    OMcode<-OMnewcodes[i]
    OMno<-foldernos[i]
    OMfolder<-paste(getwd(),"/Objects/OMs/",OMno,sep="")          # the home directory for a new modified operating model
    if(!dir.exists(OMfolder))dir.create(OMfolder)                 # create the directory if necessary

    OMI@Name<-paste0(OMno,"/",nOMs," : ",OMcode)
    OMI@OMfactors<-as.list(LNames_Ref[OMno,])
    save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder
    OMDir2<-paste(getwd(),"/Objects/OMs/",reffoldernos[i],sep="")

    file.copy(paste(OMDir2,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
    file.copy(paste(OMDir2,"/stats.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
    file.copy(paste(OMDir2,"/M3.pin",sep=""),OMfolder,overwrite=T)      # copy over the parameter initialization file

    M3write(OMI,OMdir=OMfolder)              # write the appropriate data file into the temporary folder ready to be run in parallel


  }

}

# --- Fit the 1-B-I, 1-B-II and 1-B-III operating models (~ 1 hour) ----

if(runM3OM){

  sfInit(parallel=T,cpus=8)                                                  # Initiate the cluster

  #sfLapply(foldernos,runM3p,OMdir=paste0(getwd(),"/Objects/OMs"))             # Run the M3 executables in parallel
  #sfLapply(foldernos,runM3p,OMdir=paste0(getwd(),"/Objects/OMs/",foldernos))             # Run the M3 executables in parallel
  sfLapply(foldernos,runM3p,OMdir=paste0(getwd(),"/Objects/OMs"))             # Run the M3 executables in parallel

  for(i in foldernos)pin_from_par(paste0(getwd(),"/Objects/OMs/",i))   # Record the MLE parameter estimates as initial values

}


# === Step 4: Copying the fitted models of 1 and 2 (to expand to the future recruitment scenarios) (1, 2 and 3) ===================================

reffoldernos<-rep(grep("1-",OMcodes),2)              # The numbering of the reference OMs 1-
foldernos<-c(grep("2-",OMcodes),grep("3-",OMcodes))
OMnewcodes<-OMcodes[foldernos]

if(OMbuild){

  for(i in 1:length(OMnewcodes)){

    OMno<-foldernos[i]
    OMfolder<-paste(getwd(),"/Objects/OMs/",OMno,sep="")
    if(!dir.exists(OMfolder))dir.create(OMfolder)                 # create the directory if necessary

    # Copy all files
    OMDir2<-paste(getwd(),"/Objects/OMs/",reffoldernos[i],sep="")
    file.copy(paste(OMDir2,list.files(OMDir2),sep="/"),OMfolder,overwrite=T)

    # Update OMI file
    load(file=paste0(getwd(),"/Objects/OMs/",reffoldernos[i],"/OMI"))       # load the reference operating model input object

    OMcode<-OMnewcodes[i]
    OMI@Name<-paste0(OMno,"/",nOMs," : ",OMcode)
    OMI@OMfactors<-as.list(LNames_Ref[OMno,])
    save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

  }

}



# === Step 5: Create the future recruitment scenarios (1, 2, 3) and build operating model objects================================================

if(OMbuild){

  # --- MSE control variables --------------------------------------------

  nsim<-48
  proyears<-30
  seed<-1

  # --- Define the three recruitment scenarios ---------------------------

  load(file=paste0(getwd(),"/Objects/OMs/1/OMI")) # Load any OMI object to get a real year reference

  Recs<-new('list')

  Recs[[1]]<-list(# Scenario 1: West - Hockey stick, East - '83+ B-H h=0.98

    proyears=array(c(1,Inf),c(1,2)),                           # Future recruitment follows just a single curve per stock
    years=array(c(1983-OMI@years[1]+1, Inf,                    # East
                  -Inf,                Inf),c(2,2,1)),         # West  years is future recuitment types, stock, historical time period,
    type=array(c('BH_R0','HS'),c(1,2)),                        # East - West - future recuitment follows just a single curve per stock
    fixpar=array(c(0.98, NA),c(1,2)),                          # East - West - future recruitment follows just a single curve per stock
    prob=array(c(1,1),c(1,2))                                  # Probability of flipping recruitment scenarios

  )

  Recs[[2]]<-list(# Scenario 2: West - B-H h estimated, East - '83+ B-H h=0.7

    proyears=array(c(1,Inf),c(1,2)),                           # Future recruitment follows just a single curve per stock
    years=array(c(1983-OMI@years[1]+1, Inf,                    # East
                  -Inf,                Inf),c(2,2,1)),         # West  years is future recuitment types, stock, historical time period,
    type=array(c('BH_R0','BH'),c(1,2)),                        # East - West - future recuitment follows just a single curve per stock
    fixpar=array(c(0.7, NA),c(1,2)),                           # East - West - future recruitment follows just a single curve per stock
    prob=array(c(1,1),c(1,2))                                  # Probability of flipping recruitment scenarios

  )

  Recs[[3]]<-list(# Scenario 3  West - Hockey stock changes to B-H after 10 yrs, East - 83+ B-H with h=0.98 changes to '50-82 B-H with h=0.98 after 10 years"))

    proyears=array(c(1, 12,                                  # First future time period starts year 1
                     11,Inf),                                # Second future time period starts year 11
                   c(2,2)),                                 # Future recruitment follows two curve per stock
    years=array(c(1983-OMI@years[1]+1, Inf,                 # East 1st time period
                  -Inf,                Inf,                 # West 1st time period
                  1950-OMI@years[1]+1, 1982-OMI@years[1]+1, # East 2nd time period
                  -Inf,                Inf                  # West 2nd time period
                  ),c(2,2,2)),                              # West  years is future recuitment types, stock, historical time period,
    type=array(c('BH_R0','BH_R0',
                 'HS',   'BH'),c(2,2)),                     # East - West - future recuitment follows just a single curve per stock
    fixpar=array(c(0.98, 0.98,NA,NA),c(2,2)),               # East - West - future recruitment follows just a single curve per stock
    prob=array(c(1,1),c(2,2))                               # Probability of flipping recruitment scenarios

  )

  save(Recs,file=paste(getwd(),"/Objects/Recruitment_scenarios/Trial specifications",sep=""))


  # --- Make the OM objects (~ 2 minutes) ---------------------------

  for(j in 1:3){

    foldernos<-grep(paste0(j,"-"),OMcodes)

    for(i in 1:length(foldernos)){

      OMno<-foldernos[i]
      OMfolder<-paste(getwd(),"/Objects/OMs/",OMno,sep="")
      OM<-new('OM',OMd=OMfolder,nsim=nsim,proyears=proyears,seed=1,Recruitment=Recs[[j]])
      save(OM,file=paste0(OMfolder,"/OM"))
      cat(".")

      #print(OM@UMSY)
    }

  }

} # end of OMbuild toggle

#setwd("C:/Users/tcar_/Documents/abft-mse")

# === Make all fit reports ===== ===============================
if(reportbuild){

 # OMdirs<-paste0(getwd(),"/Objects/OMs/",c(1,7,13,19,4,10,16,22))
  load(file=paste0(getwd(),"/Objects/OMs/Design.Rdata"))
  OMdirs<-paste0(getwd(),"/Objects/OMs/",grep("1-",OMcodes))# the actual fitted models
  #OMdirs<-list.dirs(OMdir)
  #OMdirs<-OMdirs[2:length(OMdirs)]# get rid of the root directory
  #make_fit_reports(dirs=OMdirs[1],addlab=TRUE)
  make_fit_reports(dirs=OMdirs,addlab=TRUE)           # make_fit_reports(dirs=paste(getwd(),"/Objects/OMs/",1,sep="")) #make_fit_reports(dirs=paste0(getwd(),"/M3"))
  # make_summary_report(dir=paste0(getwd(),"/Objects/OMs"),OMdirs=OMdirs)


  OMdir<-grep("1-",OMcodes)
  OMdir<-OMdir[order(as.numeric(OMdir))]
  OMdirs<-paste0(getwd(),"/Objects/OMs/",OMdir)
  nOMs<-length(OMdirs)
  make_summary_report(dir=paste0(getwd(),"/Objects/OMs"),OMdirs=OMdirs)

}


# === Optional code =========================================================================================================================

# ---- Some code for testing a single model fit ------------------
#out<-M3read("C:/M3/")
#load(file=paste(getwd(),"/M3/OMI",sep=""))
#save(OMI,file="C:/M3/OMI")
#make_fit_reports(dirs="C:/M3")
# ----------------------------------------------------------------


# ==== END ==================================================================================================================================

