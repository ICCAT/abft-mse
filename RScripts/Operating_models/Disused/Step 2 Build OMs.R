# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of reference operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC

# 23rd April 2019

# There are five phases to reference operating model specification:

# (1) Fit the base model (parameterized like the most recent stock assessment)
# (2) Fitting of various natural-mortality rate and maturity rate scenarios (I, II and III) (1, 7, 13)
# (3) Taking the OMs from step 2 and add the  modified abundance scenarios from them (A, B and C)
# (4) Copying the fitted models of 1 and 2 (to expand to the future recruitment scenarios) (1, 2 and 3)
# (5) Create the future recruitment scenarios (1, 2, 3) and build operating model objects

#rm(list=ls(all=TRUE))                       # Remove all existing objects from environment
setwd("C:/Users/tcarruth/Dropbox/abft-mse")
setwd("C:/Users/tcar_/Dropbox/abft-mse")
wd<-getwd()

# Some toggles
runinitialOM<-F
runM3OM<-T                           # Should the operating models be fitted (ie only new OMI and OM objects?)
reportbuild<-T                       # should the OM reports / summary reports be built?
OMIbuild<-T
OMbuild<-T                           # should the OM objects be built?
overwrite<-T                        # should old OM objects be overwritten (is this just for new OMs?)

# --- Source MSE functions and objects ------

library(ABTMSE)
loadABT()
packageVersion('ABTMSE')
#source("Source/MSE_source.r")
#source("Source/Objects.r")
OMDir<-paste(getwd(),"/M3",sep="")
PinDirs<-paste(getwd(),"/M3/Pins/",1:3,sep="")

# --- Set up the MSE design matrix

all_levs<-list(c("1","2","3"), c("A","B"), c("I","II"))

all_lnams<-list( c("1: West: h=0.6 to h=0.9 1975+, East:  h=0.98 for 1987- to h=0.98 1988+",
                   "2: West: B-H h=0.6 all years, East: B-H h=0.9 all years",
                   "3: West: post 75+ changes to pre '75 after 10 yrs, East: 88+ to '50-87 after 10 years"),

                 c("A: Younger spawning, High M",
                   "B: Older spawning, Low M"),

                 c("I: Low E-tag Wt",
                   "II: High E-tag wt") )

Design_Ref<-expand.grid(all_levs)                          # The full design grid (all combinations) of the various factors and their levels
LNames_Ref<-expand.grid(all_lnams)
#LNames_Ref<-rbind(LNames_Ref,LNames_Ref[4:6,])

Design<-list()                 # Summarize the MSE OM design
Design$all_levs<-all_levs      # Returns a list length(funcs) long of 1:nlevels for all funcs
Design$all_lnams<-all_lnams    # Returns a list length(funcs) long of the long names of the levels for all funcs
Design$Design_Ref<-Design_Ref  # The full design grid (all combinations) of the various factors and their levels
Design$LNames_Ref<-LNames_Ref
save(Design,file=paste0(getwd(),"/Objects/OMs/Design"))
save(Design,file=paste0(getwd(),"/R_package/ABTMSE/data/Design"))

nOMs<-nrow(Design_Ref)
OMcodes<-apply(Design_Ref,1,FUN=function(x)paste(x,collapse="-"))


# === Step 1: Fit the base operating model (~ 1 hour) =============================================================================================================

if(runinitialOM){
  runM3(OMDir,mcmc=TRUE)            # Run the base M3 operating model
  pin_from_par(OMDir)               # Record the MLE parameter estimates as initial values
  make_fit_reports(OMDir)           # Make_fit_reports(dirs=paste(getwd(),"/Objects/OMs/",1,sep="")) #make_fit_reports(dirs=paste0(getwd(),"/M3"))
}


# === Step 2: Fit the various Fitting of various recruitment, natural-mortality rate and maturity rate scenarios (first 6 OMs) ====================================

# --- Build operating model input objects and write them to folders ------------


if(OMIbuild){

  fromdirs <- (1:6) [Design$Design_Ref[1:6, 1]!="3"]
  todirs   <- (7:12)[Design$Design_Ref[7:12,1]!="3"]

  for(i in 1:(nOMs/2)){
    # for(i in 1:2){

    print(paste(i,"/",nOMs))
    load(file=paste(getwd(),"/Objects/OMs/Base_OM",sep=""))             # reference base operating model

    OMcode<-OMcodes[i]
    OMno<-i
    OMfolder<-paste(getwd(),"/Objects/OMs/",OMno,sep="")
    if(!dir.exists(OMfolder))dir.create(OMfolder)

    OMI@Name<-paste0(OMno,"/",nOMs," : ",OMcode)
    OMI@OMfactors<-as.list(LNames_Ref[OMno,])

    # Recruitment scenario # factor 1
    Reclev<-match(Design_Ref[i,1],Design$all_levs[[1]])
    OMI<-Rec_Ref(OMI,lev=Reclev)

    # Maturity M scenario # factor 3
    MatMlev<-match(Design_Ref[i,2],Design$all_levs[[2]])
    MatMlev<-MatMlev^2 # map 1,2 on to 1,4
    OMI<-MatM_Ref2(OMI,lev=MatMlev)

    file.copy(paste(OMDir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
    file.copy(paste(OMDir,"/stats.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
    #file.copy(paste(PinDirs[Reclev],"/M3.pin",sep=""),OMfolder,overwrite=T)      # copy over the parameter initialization file

    M3write(OMI,OMdir=OMfolder)                                        # write the appropriate data file into the temporary folder ready to be run in parallel
    save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

  }

  for(i in 7:12){

    OMfolder<-paste(getwd(),"/Objects/OMs/",i,sep="")
    if(!dir.exists(OMfolder))dir.create(OMfolder)
  }


  for(i in 1:length(fromdirs)){

    print(paste(todirs[i],"/",nOMs))
    load(file=paste(getwd(),"/Objects/OMs/",fromdirs[i],"/OMI",sep=""))             # OMI loaded
    OMcode<-OMcodes[todirs[i]]
    OMI@Name<-paste0(todirs[i],"/",nOMs," : ",OMcode)
    OMI@LHw[6]<-OMI@LHw[6]*4  # two fold increase in LHw for the PSATs

    OMfolder<-paste(getwd(),"/Objects/OMs/",todirs[i],sep="")
    OMI@OMfactors<-as.list(LNames_Ref[todirs[i],])

    file.copy(paste(OMDir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
    file.copy(paste(OMDir,"/stats.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
    #file.copy(paste(PinDirs[Reclev],"/M3.pin",sep=""),OMfolder,overwrite=T)      # copy over the parameter initialization file

    M3write(OMI,OMdir=OMfolder)                                        # write the appropriate data file into the temporary folder ready to be run in parallel
    save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

  }

}

setwd(wd)


# --- Step 2 Fit operating models (~ 7 hours)  ----

if(runM3OM){

  sfInit(parallel=T,cpus=8)                                                # Initiate the cluster

  foldernos<-c(fromdirs,todirs)

  system.time(sfLapply(foldernos,runM3p,OMdir=paste0(getwd(),"/Objects/OMs"),mcmc=T))   # Run the M3 executables in parallel

  for(i in foldernos)pin_from_par(paste0(getwd(),"/Objects/OMs/",i))       # Record the MLE parameter estimates as initial values

}



# === Step 5: Create the future recruitment scenarios (1, 2, 3) and build operating model objects================================================

if(OMbuild){

  # --- MSE control variables --------------------------------------------

  nsim<-72
  proyears<-54
  seed<-1

  # --- Define the three recruitment scenarios ---------------------------

  load(file=paste0(getwd(),"/Objects/OMs/1/OMI")) # Load any OMI object to get a real year reference

  Recs<-new('list')

  Recs[[1]]<-list(# Scenario 1: West - Hockey stick, East - '77+ B-H h=0.98

    proyears=array(c(1,Inf),c(1,2)),                           # Future recruitment follows just a single curve per stock
    SRno=array(c(2,4),c(2,1)),                                 # East then west most recent modelled SR
    type=array(OMI@SRtype[c(2,4)],c(2,1)),
    fixpar=array(OMI@SRpar[c(2,4)],c(2,1))                    # East - West - future recruitment follows just a single curve per stock

  )

  Recs[[2]]<-list(# Scenario 2: West - B-H h estimated, East - '83+ B-H h=0.7

    proyears=array(c(1,Inf),c(1,2)),                           # Future recruitment follows just a single curve per stock
    SRno=array(c(1,2),c(2,1)),                                 # East then west most recent modelled SR
    type=array(OMI@SRtype[c(1,2)],c(2,1)),
    fixpar=array(OMI@SRpar[c(1,2)],c(2,1))                    # East - West - future recruitment follows just a single curve per stock

  )

  Recs[[3]]<-list(# Scenario 3  West - Hockey stock changes to B-H after 10 yrs, East - 83+ B-H with h=0.98 changes to '50-82 B-H with h=0.98 after 10 years"))

    proyears=array(c(1, 12,                                  # First future time period starts year 1
                     11,Inf),                                # Second future time period starts year 11
                   c(2,2)),                                  # Future recruitment follows two curve per stock
    SRno=array(c(2,4,1,3),c(2,2)),
    type=array(OMI@SRtype[c(2,1,4,3)],c(2,2)),
    fixpar=array(OMI@SRpar[c(2,1,4,3)],c(2,2))               # East - West - future recruitment follows just a single curve per stock

  )

  save(Recs,file=paste(getwd(),"/Objects/Recruitment_scenarios/Trial specifications",sep=""))

  # --- Make the OM objects (~ 2 minutes) ---------------------------

  # make a pass test for convergence
  converged<-rep(FALSE,nOMs)
  folderno1<-(1:nOMs)[Design$Design_Ref[,1]=="1"] # foldernos<-c(1,6,8,9,13,17,21,25,29,33) foldernos<-c(3,11,16,20,21,23,27,31,33,35)
  folderno2<-(1:nOMs)[Design$Design_Ref[,1]=="2"] # foldernos<-c(1,6,8,9,13,17,21,25,29,33) foldernos<-c(3,11,16,20,21,23,27,31,33,35)

  reffolders <- c(rbind(folderno1,folderno2,folderno1))

  for(OMno in reffolders){
    OMd<-paste(getwd(),"/Objects/OMs/",OMno,sep="")
    tryCatch({
      samps<-read.table(paste(OMd,"/nodes.cha",sep=""), sep=" ")
      converged[OMno]<-TRUE
    },
    error= function(e){
      message(paste0("Operating model ",OMno, " did not converge and is currently unavailable"))
    })
  }

  #for(j in 1:3){

  foldernos<-1:nOMs#grep(paste0(j,"-"),OMcodes)

  for(i in 1:length(reffolders)){

      OMno<-foldernos[i]
      OMfolder<-paste(getwd(),"/Objects/OMs/",OMno,sep="")
      reffolder<-paste(getwd(),"/Objects/OMs/",reffolders[i],sep="")

      if(OMno!=reffolders[i])file.copy(paste0(reffolder,"/OMI"),paste0(OMfolder,"/OMI"))

      if((!file.exists(OMfolder)&overwrite==F)|overwrite){

          if(!file.exists(OMfolder)) dir.create(OMfolder)

          j<-Design$Design_Ref[i,1]
          OM<-new('OM',OMd=reffolder,nsim=nsim,proyears=proyears,seed=1,Recruitment=Recs[[j]])
          save(OM,file=paste0(OMfolder,'/OM'))
          print(OM@Recind)
          OM<-new('OM',OMd=reffolder,nsim=2,proyears=proyears,seed=1,MLEonly=T,Recruitment=Recs[[j]])
          save(OM,file=paste0(OMfolder,'/OMd'))

      }

  }

} # end of OMbuild toggle

#setwd("C:/Users/tcar_/Documents/abft-mse")

# === Make all fit reports ===== ===============================
if(reportbuild){

  # Individual reports
  load(file=paste0(getwd(),"/Objects/OMs/Design"))
  nOMs<-12
  OMdirs<-paste0(getwd(),"/Objects/OMs/",(1:nOMs)[Design$Design_Ref[,1]!="3"])
  #OMdirs<-paste0(getwd(),"/Objects/OMs/",(1:nOMs)[Design$Design_Ref[,1]=="2"])
  load(system.file("ts2017.Rdata", package="ABTMSE"))
  dat<-ts2017

  for(ij in 1:8){#length(OMdirs)){

    input_dir<-OMdirs[ij]
    out<-M3read(input_dir)
    load(paste(input_dir,"/OMI",sep=""))

    split<-strsplit(input_dir,"/")[[1]]
    lab<-split[length(split)]
    #render(input=system.file("OMreport.Rmd", package="ABTMSE"), output_file=paste(input_dir,"/Report_",lab,".html",sep=""))
    render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.Rmd"), output_file=paste(input_dir,"/Report_",lab,".html",sep=""))
  }

  # summary reports
  dir<-paste0(wd,"/Objects/OMs")
  load(file=paste0(getwd(),"/Objects/OMs/Design")) # Design=readRDS(file=paste0(getwd(),"/Objects/OMs/Design.Rdata"))
  OMcodes<-apply(Design$Design_Ref,1,FUN=function(x)paste(x,collapse="-"))

  OMnos<-(1:length(OMcodes))[grepl("1-",OMcodes)|grepl("2-",OMcodes)]           # The numbering of the reference OMs 1-
  OMdirs<-paste0(wd,"/Objects/OMs/",OMnos)
  nOMs<-length(OMdirs)
  OMnames<-OMcodes[OMnos]
  introtext<-"This is a report of all 2019 reference grid operating models (MPD estimates). Note that operating models with recruitment level 1 and 3 are identical in their historical recruitment and are therefore identical in their model fitting. Only levels 1 and 2 are presented here."
  render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OMsummary.Rmd"), output_file=paste(dir,"/Summary_Report_2019.html",sep=""))

  # Likelihood comparison report
  # wd<-"C:/Users/tcar_/Dropbox/abft-mse"
  dir<-paste0(wd,"/Objects/OMs")
  designdir<-paste0(dir,"/Design")
  load(designdir)
  OMcodes<-apply(Design_Ref,1,FUN=function(x)paste(x,collapse="-"))
  OMnos<-(1:length(OMcodes))[!grepl("3",OMcodes)]           # The numbering of the reference OMs 1-
  OMdirs<-paste0(wd,"/Objects/OMs/",OMnos)
  nOMs<-length(OMdirs)
  OMnames<-OMcodes[OMnos]

  render(input=paste0(wd,"/R_package/ABTMSE/inst/A_B_C_comp.Rmd"), output_file=paste(dir,"/OM_LH_comp.html",sep=""))

  # Copy to OM temp reporting
  foldernos<-(1:nrow(Design$Design_Ref))[Design$Design_Ref[,1]!="3"] # foldernos<-c(1,6,8,9,13,17,21,25,29,33) foldernos<-c(3,11,16,20,21,23,27,31,33,35)

  load(file=paste0(getwd(),"/Objects/OMs/Design.Rdata"))
  OMdirs<-paste0(getwd(),"/Objects/OMs/",foldernos)# the actual fitted models
  for(i in 1:length(OMdirs))file.copy(paste0(OMdirs[i],"/Report_",foldernos[i],".html"),"C:/OMtemp",overwrite=T)      # copy the relevant report

  files<-paste0(dir,c("/Summary_Report_2019.html","/OM_LH_comp.html"))
  for(i in 1:length(files))file.copy(files[i],"C:/OMtemp",overwrite=T)      # copy the relevant report

}

# === Optional code =========================================================================================================================

# ---- Some code for testing a single model fit ------------------
#out<-M3read("C:/M3/")
#load(file=paste(getwd(),"/M3/OMI",sep=""))
#save(OMI,file="C:/M3/OMI")
#make_fit_reports(dirs="C:/M3")
# ----------------------------------------------------------------


# ==== END ==================================================================================================================================

