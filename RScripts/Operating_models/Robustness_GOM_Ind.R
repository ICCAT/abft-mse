# Exploratory runs
library(ABTMSE)
loadABT()
source("C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/R/M3_tools.r")

#setwd("C:/Users/tcar_/Dropbox/abft-mse")
# Post run
setwd("C:/Users/tcarruth/Dropbox/abft-mse")

all_levs<-list(c("1","2","3"), c("A","B"), c("I","II"))

all_lnams<-list( c("1: West: h=0.6 to hockey stick 1975+, East:  h=0.98 for 1987- to h=0.98 1988+",
                   "2: West: B-H h=0.6 all years, East: B-H h=0.6 all years",
                   "3: West: Hockey stock changes to B-H after 10 yrs, East: 88+ to '50-87 after 10 years"),

                 c("A: Younger spawning, High M",
                   "B: Older spawning, Low M"),

                 c("I: Low E-tag Wt",
                   "II: High E-tag wt") )

Design_Ref<-expand.grid(all_levs)                                               # The full design grid (all combinations) of the various factors and their levels
LNames_Ref<-expand.grid(all_lnams)

Design<-list()                 # Summarize the MSE OM design
Design$all_levs<-all_levs      # Returns a list length(funcs) long of 1:nlevels for all funcs
Design$all_lnams<-all_lnams    # Returns a list length(funcs) long of the long names of the levels for all funcs
Design$Design_Ref<-Design_Ref  # The full design grid (all combinations) of the various factors and their levels
Design$LNames_Ref<-LNames_Ref
save(Design,file=paste0(getwd(),"/Objects/OMs/Design"))
save(Design,file=paste0(getwd(),"/R_package/ABTMSE/data/Design"))

nOMs<-nrow(Design_Ref)
OMcodes<-apply(Design_Ref,1,FUN=function(x)paste(x,collapse="-"))
OMnos<-(1:nOMs)[Design$Design_Ref[, 1]!="3"]
input_dirs <- paste0(getwd(),"/Objects/OMs/",OMnos)


# Make folders
nexp<-length(OMdirs)
sfInit(parallel=T,cpus=nexp)

OMdir<-paste0(getwd(),"/Objects/ROMs/all_GOM_Ind_15/")
if(!exists(OMdir))dir.create(OMdir)

dirs<-paste0(OMdir,OMnos)
M3dir<-paste0(getwd(),"/M3")
for(i in 1:nexp){
  dir.create(dirs[i])
  file.copy(paste0(M3dir,"/M3.exe"),dirs[i],overwrite=T)      # copy the latest executable to the temporary
  file.copy(paste0(M3dir,"/stats.cxx"),dirs[i],overwrite=T)   # copy over the statistics library
  #file.copy(paste0(M3dir,"/M3.pin"),dirs[i],overwrite=T)      # copy over the parameter initialization file
  cat(i)
}

for(i in 1:length(dirs)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-paste(OMI@Name,"GOM_15")
  OMI@IobsCV[5]<-0.15
  M3write(OMI,OMdir=dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(dirs[i],"/OMI"))
  cat(i)

}


# run models
system.time({sfLapply(OMnos,runM3p,OMdir=OMdir,mcmc=F,hess=F)})

for(i in 1:nexp)pin_from_par(dirs[i])

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017

for(ij in 1:8){

  load(paste0(dirs[ij],"/OMI"))
  out<-M3read(dirs[ij])
  render(input="C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/inst/OMreport.Rmd",
         output_file=paste0(dirs[ij],"/Report_",ij,"_GOM_15.html"))

}


OMnames <-paste0(OMcodes[OMnos],"_GOM_15")
OMdirs<-dirs
introtext<-"This operating model comparison report shows the impact on all fitted OMs of greater precision in the
Gulf of Mexico Larval survey (GOM_LAR_SUV) - a CV of 0.15"

render(input="C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/inst/Free_Comp.Rmd",
       output_file="C:/Users/tcarruth/Dropbox/abft-mse/Results/GOM_LAR_SUV_CV/GOM_LAR_SUV_CV.html")








# now with last  missing =========================================================================================================================

# Make folders
nexp<-length(OMdirs)
sfInit(parallel=T,cpus=nexp)

OMdir<-paste0(getwd(),"/Objects/ROMs/all_GOM_Ind_15_last/")
if(!exists(OMdir))dir.create(OMdir)

dirs<-paste0(OMdir,OMnos)
M3dir<-paste0(getwd(),"/M3")
for(i in 1:nexp){
  dir.create(dirs[i])
  file.copy(paste0(M3dir,"/M3.exe"),dirs[i],overwrite=T)      # copy the latest executable to the temporary
  file.copy(paste0(M3dir,"/stats.cxx"),dirs[i],overwrite=T)   # copy over the statistics library
  #file.copy(paste0(M3dir,"/M3.pin"),dirs[i],overwrite=T)      # copy over the parameter initialization file
  cat(i)
}

for(i in 1:length(dirs)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-paste(OMI@Name,"GOM_15_last")
  OMI@IobsCV[5]<-0.15
  OMI@Iobs<-OMI@Iobs[!(OMI@Iobs[,3]==1&OMI@Iobs[,1]==52),] # remove last observation
  OMI@nIobs<-nrow(OMI@Iobs)
  M3write(OMI,OMdir=dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(dirs[i],"/OMI"))
  cat(i)

}


# run models
system.time({sfLapply(OMnos,runM3p,OMdir=OMdir,mcmc=F,hess=F)})

for(i in 1:nexp)pin_from_par(dirs[i])

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017

for(ij in 1:8){

  load(paste0(dirs[ij],"/OMI"))
  out<-M3read(dirs[ij])
  render(input="C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/inst/OMreport.Rmd",
         output_file=paste0(dirs[ij],"/Report_",ij,"_GOM_15_last.html"))

}


OMnames <-paste0(OMcodes[OMnos],"_GOM_15_last")
OMdirs<-dirs
introtext<-"This operating model comparison report shows the impact on all fitted OMs of greater precision in the
Gulf of Mexico Larval survey (GOM_LAR_SUV) - a CV of 0.15, and the removal of the last data point in that series"

render(input="C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/inst/Free_Comp.Rmd",
       output_file="C:/Users/tcarruth/Dropbox/abft-mse/Results/GOM_LAR_SUV_CV/GOM_LAR_SUV_CV_last.html")







# Load the template
load(paste(input_dir,"/OMI",sep=""))
OMIb<-OMI
OMIb@OMfactors<-as.list(LNames_Ref[1,])



# ---- 1 Last 0.25 -------------
Base<-OMIb
Base@Iobs<-Base@Iobs[!(Base@Iobs[,3]==1&Base@Iobs[,1]==52),] # remove last observation
#plot(Base@Iobs[Base@Iobs[,3]==1,7])
Base@nIobs<-nrow(Base@Iobs)
Base@IobsCV[5]<-0.25
OMI<-Base
OMI@Name<-"Last 0.25"
M3write(OMI,OMdir=dirs[1])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[1],"/OMI"))

# ---- 2 Last 0.2
Base<-OMIb
Base@Iobs<-Base@Iobs[!(Base@Iobs[,3]==1&Base@Iobs[,1]==52),] # remove last observation
Base@nIobs<-nrow(Base@Iobs)
Base@IobsCV[5]<-0.2
OMI<-Base
OMI@Name<-"Last 0.2"
M3write(OMI,OMdir=dirs[2])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[2],"/OMI"))

# ---- 3 Last 0.15
Base<-OMIb
Base@Iobs<-Base@Iobs[!(Base@Iobs[,3]==1&Base@Iobs[,1]==52),] # remove last observation
Base@nIobs<-nrow(Base@Iobs)
Base@IobsCV[5]<-0.15
OMI<-Base
OMI@Name<-"Last 0.15"
M3write(OMI,OMdir=dirs[3])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[3],"/OMI"))

# ---- 4 Last 0.1
Base<-OMIb
Base@Iobs<-Base@Iobs[!(Base@Iobs[,3]==1&Base@Iobs[,1]==52),] # remove last observation
Base@nIobs<-nrow(Base@Iobs)
Base@IobsCV[5]<-0.1
OMI<-Base
OMI@Name<-"Last 0.1"
M3write(OMI,OMdir=dirs[4])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[4],"/OMI"))


# ---- 5 0.25 -------------
Base<-OMIb
Base@IobsCV[5]<-0.25
OMI<-Base
OMI@Name<-"0.25"
M3write(OMI,OMdir=dirs[5])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[5],"/OMI"))

# ---- 6 0.2
Base<-OMIb
Base@IobsCV[5]<-0.2
OMI<-Base
OMI@Name<-"0.2"
M3write(OMI,OMdir=dirs[6])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[6],"/OMI"))

# ---- 7 0.15
Base<-OMIb
Base@IobsCV[5]<-0.15
OMI<-Base
OMI@Name<-"0.15"
M3write(OMI,OMdir=dirs[7])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[7],"/OMI"))

# ---- 8 0.1
Base<-OMIb
Base@IobsCV[5]<-0.1
OMI<-Base
OMI@Name<-"0.1"
M3write(OMI,OMdir=dirs[8])  # Store this base operating model in the M3 directory
save(OMI,file=paste0(dirs[8],"/OMI"))



# run models
system.time({sfLapply(1:nexp,runM3p,OMdir=OMdir,mcmc=F,hess=F)})

for(i in 1:nexp)pin_from_par(dirs[i])

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017

for(ij in 1:8){

  load(paste0(dirs[ij],"/OMI"))
  out<-M3read(dirs[ij])
  render(input="C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/inst/OMreport.Rmd",
         output_file=paste0(dirs[ij],"/Report_",ij,".html"))

}


OMnos<-rep(1,9)
OMnames <-c("Base OM#1",c("0.25","0.2","0.15","0.1"),paste0(c("0.25","0.2","0.15","0.1"),"_last"))
OMdirs<-c("C:/Users/tcarruth/Dropbox/abft-mse/Objects/OMs/1",dirs[c(5:8,1:4)])
introtext<-"This operating model comparison report shows the impact on OM#1 of greater precision in the
Gulf of Mexico Larval survey (GOM_LAR_SUV) and removal of the last (high) data point. This is the only
Western-specific index of abundance and these sensitivity analyses are investigated to determine whether
it is possible to provide credible, alternative scenarios for Western status that provide greater contrast
for the purposes of MP robustness testing. The Base CV of OM#1 is 0.4 for the GOM_LAR_SUV series. Four
alternative CVs are considered here: 0.25, 0.2, 0.15 and 0.1. Additionally another four include the removal
of the last data point (e.g. '0.15_last')"

render(input="C:/Users/tcarruth/Dropbox/abft-mse/R_package/ABTMSE/inst/Free_Comp.Rmd",
       output_file="C:/Users/tcarruth/Dropbox/abft-mse/Results/GOM_LAR_SUV_CV/GOM_LAR_SUV_CV.html")

