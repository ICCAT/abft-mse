
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of reference operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC

# 3rd April 2021

# There are four phases to reference operating model specification:

# (1) Fit the base model (parameterized like the most recent stock assessment)
# (2) Fitting of various natural-mortality rate and maturity rate scenarios
# (3) Copying the fitted models of 1 and 2 (to expand to the future recruitment scenarios) (1, 2 and 3)
# (4) Create the future recruitment scenarios (1, 2, 3) and build operating model objects


library(ABTMSE)

setwd("C:/Users/tcar_/Dropbox/abft-mse")
setwd("C:/Users/tcarruth/Dropbox/abft-mse")
wd<-getwd()

load("Objects/OMs/OMI") # Load base OM
OMIt<-OMI
OMIt@verbose<-as.integer(0)
OMIt@verbose<-as.integer(0)

packageVersion('ABTMSE')
OMDir<-paste(getwd(),"/M3",sep="")

# --- Set up the MSE design matrix
#                 rec            M/mat              scale           length comp weighting
all_levs<-list(c("1","2","3"), c("A","B"), c("--","-+","+-","++"),c("L","H"))

all_lnams<-list( c("1: West: h=0.6 to h=0.9 1975+, East:  h=0.98 for 1987- to h=0.98 1988+",
                   "2: West: B-H h=0.6 all years, East: B-H h=0.7 all years",
                   "3: West: post 75+ changes to pre '75 after 10 yrs, East: 88+ to '50-87 after 10 years"),

                 c("A: Younger spawning, High M",
                   "B: Older spawning, Low M"),

                 c("--: mean SSB 15kt West, 200kt East",
                   "-+: mean SSB 15kt West, 400kt East",
                   "+-: mean SSB 50kt West, 200kt East",
                   "++: mean SSB 50kt West, 400kt East"),

                 c("L: Low length composition weight of 1/20",
                   "H: High length composition weight of 1")
                 )

Design_Ref<-expand.grid(all_levs)      # The full design grid (all combinations) of the various factors and their levels
LNames_Ref<-expand.grid(all_lnams)

Design<-list()                 # Summarize the MSE OM design
Design$all_levs<-all_levs      # Returns a list length(funcs) long of 1:nlevels for all funcs
Design$all_lnams<-all_lnams    # Returns a list length(funcs) long of the long names of the levels for all funcs
Design$Design_Ref<-Design_Ref  # The full design grid (all combinations) of the various factors and their levels
Design$LNames_Ref<-LNames_Ref
save(Design,file=paste0(getwd(),"/Objects/OMs/Design"))
save(Design,file=paste0(getwd(),"/R_package/ABTMSE/data/Design"))

nOMs<-nrow(Design_Ref)
OMcodes<-apply(Design_Ref,1,FUN=function(x)paste(x,collapse=" "))


# --- Build operating model input objects and write them to folders ------------

for(i in 1:nOMs){

    OMI<-OMIt
    print(paste(i,"/",nOMs))
    OMcode<-OMcodes[i]
    OMno<-i
    OMfolder <- paste(getwd(),"/Objects/OMs/",OMno,sep="")
    if(!dir.exists(OMfolder))dir.create(OMfolder)

    OMI@verbose=as.integer(0)
    OMI@Name<-paste0(OMno,"/",nOMs," : ",OMcode)
    OMI@OMfactors<-as.list(LNames_Ref[OMno,])

    # Recruitment scenario # factor 1
    Reclev<-match(Design_Ref[i,1],Design$all_levs[[1]])
    OMI<-Rec_Ref(OMI,lev=Reclev)

    # Maturity M scenario # factor 2
    MatMlev<-match(Design_Ref[i,2],Design$all_levs[[2]])
    MatMlev<-MatMlev^2 # map 1,2 on to 1,4
    OMI<-MatM_Ref(OMI,lev=MatMlev)

    # Scale scenario # factor 3
    BmuLev<-match(Design_Ref[i,3],Design$all_levs[[3]])
    OMI<-Bmu_Ref(OMI,BmuLev)

    # Length comp weight scenario # factor 4
    LcompLev<-match(Design_Ref[i,4],Design$all_levs[[4]])
    OMI<-Lcomp_Ref(OMI,LcompLev)

    print(paste(i,":",OMcodes[i],Reclev,MatMlev,BmuLev,LcompLev))

    file.copy(paste(OMDir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
    file.copy(paste(OMDir,"/stats2.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
    file.copy(paste0(OMDir,"/M3.tpl"),OMfolder,overwrite=T)

    OMI@Phases[]<-1 # Estimate in a single phase from pins:
    if(Reclev==1)file.copy(paste0(getwd(),"/objects/Pins/Rec1/M3.pin"),OMfolder,overwrite=T)
    if(Reclev==2)file.copy(paste0(getwd(),"/objects/Pins/Rec2/M3.pin"),OMfolder,overwrite=T)

    M3write(OMI,OMdir=OMfolder)                                        # write the appropriate data file into the temporary folder ready to be run in parallel
    save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

}

# Remove pin files if necessary
# for(i in 1:nOMs){ OMfolder <- paste(getwd(),"/Objects/OMs/",i,sep=""); if(file.exists(paste0(OMfolder,"/M3.pin"))){file.remove(paste0(OMfolder,"/M3.pin"))}}

setwd(wd)
fitdirs <- (1:nrow(Design$Design_Ref)) [Design$Design_Ref[, 1]!="3"] #fitdirs[rep(c(T,F,F,F),20)[1:length(fitdirs)]]
OMdir<-paste0(getwd(),"/objects/OMs/")
dirs<-paste0(OMdir,1:nOMs)

sfInit(parallel=T,cpus=10)

system.time({
  sfLapply(fitdirs,runM3p,OMdir=OMdir,mcmc=F,hess=F)
})

for(i in fitdirs)pin_from_par(dirs[i])

# copy over par and repfiles to Rec 3 scenarios -------------------------

fromdirs<-paste0(OMdir, (1:nrow(Design$Design_Ref)) [Design$Design_Ref[, 1]=="1"])
todirs<-paste0(OMdir, (1:nrow(Design$Design_Ref)) [Design$Design_Ref[, 1]=="3"])

for(i in 1:length(fromdirs)){
  file.copy(paste(fromdirs[i],"/M3.par",sep=""),todirs[i],overwrite=T)   # copy the latest executable to the temporary
  file.copy(paste(fromdirs[i],"/M3.rep",sep=""),todirs[i],overwrite=T)   # copy over the statistics library
}



# --- Make reports -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Individual reports -------------------------------------------------------------------------

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017

for(ij in fitdirs){

  load(paste0(dirs[ij],"/OMI"))
  out<-M3read(OMDir=dirs[ij])
  OMI@Name<-paste0("#",ij,": ",OMI@Name)
  render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.Rmd"),
         output_file=paste0(dirs[ij],"/Report_",ij,".html"))

}

# Summary reports ------------------------------------------------------------------------------------------------------------------------------------
FreeComp<-TRUE # ignore design grid stuff
custom_cols<-rep(c('black','blue','grey','orange'),each=4)
custom_lwds<-rep(rep(c(1,2),each=2),4)
custom_ltys<-rep(c(1:2),8)
custom_cexs<-0.5

OMdir<-paste0(getwd(),"/objects/OMs/")
dirs<-paste0(OMdir,1:nOMs)


# Lcomp L
nOMs<-nrow(Design$Design_Ref)
introtext<-"A comparison report for reference grid OMs with length composition weighting of 1/20 (factor level L)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,4]=="L"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"New_Lcomp_L_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)

# Lcomp H
nOMs<-nrow(Design$Design_Ref)
introtext<-"A comparison report for reference grid OMs with length composition weighting of 1 (factor level H)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,4]=="H"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"New_Lcomp_H_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)



# Index fit reports -----------------------------------------------------------------------------------------------------------------------------------------------
custom_cols<-rep(c('black','blue','grey','orange'),each=4)
custom_lwds<-rep(rep(c(1,2),each=2),4)
custom_ltys<-rep(c(1:2),8)
custom_cexs<-0.5

CPUEnos<-match(c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_177","MEXUS_GOM_PLL","JPN_LL_West2","CAN GSL","CAN SWNS","US_RR_66_144"),OMI@CPUEnames)
Inos<-match(c("FR_AER_SUV2","MED_LAR_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR"),OMI@Inames)
Iarea<-c(OMI@CPUEobs[match(CPUEnos,OMI@CPUEobs[,4]),3],OMI@Iobs[match(Inos,OMI@Iobs[,5]),3])
Proposed <-c("FR_AER_SUV2","MED_LAR_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR",
             "MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_177","MEXUS_GOM_PLL","JPN_LL_West2","CAN GSL","CAN SWNS","US_RR_66_144")
discont<-c("MED_LAR_SUV","FR_AER_SUV2","GBYP_AER_SUV_BAR")


# Mix I Lcomp L
nOMs<-nrow(Design$Design_Ref)
introtext<-"Index fitting report for reconditioned reference grid OMs with length composition weighting of 1/20 (factor level L)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,4]=="L"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"New_Lcomp_L_Index_Fit.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary_manyOMs.Rmd"),output_file=outfile)

# Mix II Lcomp H
nOMs<-nrow(Design$Design_Ref)
introtext<-"Index fitting report for reconditioned reference grid OMs with length composition weighting 1 (factor level 4)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,4]=="H"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"New_Lcomp_H_Index_Fit.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary_manyOMs.Rmd"),output_file=outfile)




# copy ind reps -------------------------------------------
copyinds<-F
if(copyinds){

  nOMs<-nrow(Design$Design_Ref)
  repdir<-paste0(getwd(),"/Results/Reconditioning_Comparison/Appendices/Appendix B Individual OM Reports")
  for(i in 1:nOMs)  file.copy(paste0(dirs[i],"/Report_",i,".html"),repdir)

}



# --- Make OM objects -------------------------------------------------------------------------------------------------------------------------------------------------------


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


testmode<-F

if(testmode){

  nsim<-2
  proyears<-54
  seed<-1
  MLEonly<-T
  Deterministic=F
  ploty=T
  debug=T
  Snames=c("East","West")
  Recruitment=Recs[[2]]

  OMfolders<-paste(paste(getwd(),"/Objects/OMs/",1:nOMs,sep=""))

  i<-7
  OMd<-paste(getwd(),"/Objects/OMs/",i,sep="")
  j<-Design$Design_Ref[i,1]
  OM<-new('OM',OMd=OMfolders[i],nsim=nsim,proyears=proyears,seed=1,MLEonly=T,Recruitment=Recs[[j]])
  loadABT()
  MSE<-new('MSE',OM,Obs=Good_Obs,MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI")),interval=3,IE="Umax_90",check=T)

  Obs=Good_Obs; MPs=list(c("UMSY","UMSY")); interval=2; IE="Umax_90"
  TAC2016=c(19296000,1912000);TAC2017=c(23155000,2000000);TAC2018=c(28200000,2350000);TAC2019=c(32240000,2350000);TAC2020=c(36000000,2350000)
  Allocation=ABTMSE:::Allocation; MPareas=NA; Fdistyrs=3; maxTAC=c(10,10); MSEparallel=F; check=FALSE

}


OMfolders<-paste(paste(getwd(),"/Objects/OMs/",1:nOMs,sep=""))

#SD_override<-data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR"),
#                        SD=c(  0.45,         0.45,            0.8,          0.45              ))

#AC_override<- data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR","JPN_LL_West2"),
#                         AC=c(  0.2,         0,               0.2,          0.2,                         0))

Yrs_override<-data.frame(Name='MED_LAR_SUV',start=48, end=55)
CPUEinds<-c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_66_144","US_RR_177","MEXUS_GOM_PLL","JPN_LL_West2","CAN GSL","CAN SWNS")
Iinds<-c("FR_AER_SUV2","MED_LAR_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR")


for(i in 1:nOMs){

  print(paste0(i,":  ",OMcodes[i]))
  j<-Design$Design_Ref[i,1]
  OM<-new('OM',OMd=OMfolders[i],nsim=48,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[j]],
          SD_override=NULL, AC_override=NULL, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)

  save(OM,file=paste0(OMfolders[i],'/OM_',i))
  OM<-new('OM',OMd=OMfolders[i],nsim=2,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[j]],
          SD_override=NULL, AC_override=NULL, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds,
          Deterministic=T, Obs = "Perfect_Obs")

  save(OM,file=paste0(OMfolders[i],'/OM_',i,"d"))

}


loadABT()

TAC2019=c(32240000,2350000);TAC2020=c(36000000,2179000);TAC2021=c(36000000,2350000);TAC2022=c(36000000,2350000)
Allocation=ABTMSE:::Allocation;MPareas=NA;Fdistyrs=3;maxTAC=c(10,10);MSEparallel=F; check=FALSE; Reallocate=TRUE
MPs=list(c("UMSY","UMSY"),c("UMSY_PI","UMSY_PI"))

MSE<-new('MSE',OM,MPs=list(c("U3","U3"),c("U5","U5")),check=T)



