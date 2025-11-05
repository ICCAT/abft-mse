
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of reference operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC

# 3rd February 2020

# There are five phases to reference operating model specification:

# (1) Fit the base model (parameterized like the most recent stock assessment)
# (2) Fitting of various natural-mortality rate and maturity rate scenarios (I, II and III) (1, 7, 13)
# (3) Taking the OMs from step 2 and add the  modified abundance scenarios from them (A, B and C)
# (4) Copying the fitted models of 1 and 2 (to expand to the future recruitment scenarios) (1, 2 and 3)
# (5) Create the future recruitment scenarios (1, 2, 3) and build operating model objects

# Basic OM template is a copy of SSBprofile #6 Lcompwt = 0.05
# as before except catch is 1/50 and MED LAR SUV is upwt 10

library(ABTMSE)

setwd("C:/Users/tcar_/Dropbox/abft-mse")
setwd("C:/Users/tcarruth/Dropbox/abft-mse")
wd<-getwd()

load("Objects/OMs/OMI") # Load base OM
OMIt<-OMI

#              1 catch, 2 cpue, 3 FIindex,  4 Lcomp,   5 SOO, 6 PSAT,  7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB, 13 SSBinc, 14 Fmod,  15 R0diff, 16 BSfrac, 17 MICV, 18 SpatPr
OMIt@LHw <-   c(1/50,  1,       2,          0.05,      1,     5,       0,       1,        1,     1,      1,      1,      0,         1,        1,         1,         1,       1)
OMIt@nLHw<-as.integer(18)
OMIt@Cobs[,7]<-1
OMIt@CPUEobs[,9]<-1
OMIt@CLobs[,7]<-1
OMIt@SOOobs[,9]<-1
OMIt@Iobs[,10]<-1
OMIt@Iobs[OMIt@Iobs[,5] == match("GOM_LAR_SUV",OMIt@Inames),10] <- 10   # upweight gom lar suv
OMIt@Iobs[OMIt@Iobs[,5] == match("MED_LAR_SUV",OMIt@Inames),10] <- 10   # upweight med lar suv
OMIt@SpatPr[,6]<-0.02
OMIt@ET_LHF<-1
OMIt@LC_LHF<-2
OMIt@Phases<-rep(1,4) #c(1,2,3,4)
OMIt@MICV<-1
OMIt@verbose<-as.integer(0)

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017
cond<-dat$assessment=='SS'&dat[,3]>=OMIt@years[1]
SSBprior<-cbind(match(dat[cond,1],c("East","West")),dat[cond,3]-OMIt@years[1]+1,dat[cond,4])
OMIt@SSBprior<-SSBprior
OMIt@nSSBprior<-nrow(SSBprior)
OMIt@SSBCV<-0.05
OMIt@BSfrac<-matrix(0.05)
OMIt@BSfracCV<-0.05


packageVersion('ABTMSE')
OMDir<-paste(getwd(),"/M3",sep="")

# --- Set up the MSE design matrix
#                 rec            M/mat        mix               scale           length comp weighting
all_levs<-list(c("1","2","3"), c("A","B"), c("I","II"),c("--","-+","+-","++"),c("L","H"))

all_lnams<-list( c("1: West: h=0.6 to h=0.9 1975+, East:  h=0.98 for 1987- to h=0.98 1988+",
                   "2: West: B-H h=0.6 all years, East: B-H h=0.7 all years",
                   "3: West: post 75+ changes to pre '75 after 10 yrs, East: 88+ to '50-87 after 10 years"),

                 c("A: Younger spawning, High M",
                   "B: Older spawning, Low M"),

                 c("I: Low West Stock Migration",
                   "II: High West Stock Migration"),

                 c("--: mean SSB 15kt West, 200kt East",
                   "-+: mean SSB 15kt West, 400kt East",
                   "+-: mean SSB 50kt West, 200kt East",
                   "++: mean SSB 50kt West, 400kt East"),

                 c("L: Low length composition weight of 1/20",
                   "H: High length composition weight of 1")
                 )
#Mig_Ref, Bmu_Ref, Lcomp_Ref

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


# Copy from previous files ---------------------------------------------
ifcopy=F
if(ifcopy){

  fromlevs<-all_levs
  fromlevs[[1]]<-fromlevs[[1]][1:2]
  fromgrid<-expand.grid(fromlevs[1:4])

  # Low Lcomp weight
  fromdirL<-"C:/Users/tcar_/Dropbox/BFT MSE/Meetings/TT Feb 2020 NEW/Alt_muSSB_5"

  # High Lcomp weight
  fromdirH<-"C:/Users/tcar_/Dropbox/BFT MSE/Meetings/TT Feb 2020 NEW/NewDef"

  for(i in 1:nOMs){

    OMfolder<-paste(getwd(),"/Objects/OMs/",i,sep="")

    if(!dir.exists(OMfolder))dir.create(OMfolder)

    if(Design_Ref[i,1]!=3){  # not recruitment level 3
      cond <- as.character(fromgrid[,1])==as.character(Design_Ref[i,1]) &
              fromgrid[,2]==Design_Ref[i,2] &
              fromgrid[,3]==Design_Ref[i,3] &
              fromgrid[,4]==Design_Ref[i,4]
    }else{                   # recruitment level 3 is a copy of rec level 1
      cond <- fromgrid[,1]==1 &
              fromgrid[,2]==Design_Ref[i,2] &
              fromgrid[,3]==Design_Ref[i,3] &
              fromgrid[,4]==Design_Ref[i,4]
    }

    fromfolderno<-(1:32)[cond]
    print("---")
    print(fromfolderno)
    print(i)

    if(Design_Ref[i,5] == "L"){ # Low Lcomp weight runs (1/20)

      #if(Design_Ref[i,1]==3){
        #load(paste(fromdirL,fromfolderno,"OMI",sep="/"))
        #OMI<-Rec_Ref(OMI,lev=3)
       # save(OMI,file=paste0(OMfolder,"OMI","/"))
      #}else{
        file.copy(paste(fromdirL,fromfolderno,"OMI",sep="/"),OMfolder,overwrite=T)
      #}

      file.copy(paste(fromdirL,fromfolderno,"OMI",sep="/"),OMfolder,overwrite=T)
      #file.copy(paste(fromdirL,fromfolderno,"M3.dat",sep="/"),OMfolder,overwrite=T)
      #file.copy(paste(fromdirL,fromfolderno,"M3.par",sep="/"),OMfolder,overwrite=T)
      #file.copy(paste(fromdirL,fromfolderno,"M3.rep",sep="/"),OMfolder,overwrite=T)

    }else{  # High L comp weight runs (1)

      file.copy(paste(fromdirH,fromfolderno,"OMI",sep="/"),OMfolder,overwrite=T)
      #file.copy(paste(fromdirH,fromfolderno,"M3.dat",sep="/"),OMfolder,overwrite=T)
      #file.copy(paste(fromdirH,fromfolderno,"M3.par",sep="/"),OMfolder,overwrite=T)
      #file.copy(paste(fromdirH,fromfolderno,"M3.rep",sep="/"),OMfolder,overwrite=T)

    }

  }


}else{  # !!!!!!! Note this have never been run so needs checking !!!!!!!!!!!!!!!1111

   # --- Build operating model input objects and write them to folders ------------

  for(i in 1:nOMs){

    OMI<-OMIt
    print(paste(i,"/",nOMs))
    OMcode<-OMcodes[i]
    OMno<-i
    OMfolder <- paste(getwd(),"/Objects/OMs/",OMno,sep="")
    if(!dir.exists(OMfolder))dir.create(OMfolder)

    OMI@Name<-paste0(OMno,"/",nOMs," : ",OMcode)
    OMI@OMfactors<-as.list(LNames_Ref[OMno,])

    # Recruitment scenario # factor 1
    Reclev<-match(Design_Ref[i,1],Design$all_levs[[1]])
    OMI<-Rec_Ref(OMI,lev=Reclev)

    # Maturity M scenario # factor 2
    MatMlev<-match(Design_Ref[i,2],Design$all_levs[[2]])
    MatMlev<-MatMlev^2 # map 1,2 on to 1,4
    OMI<-MatM_Ref2(OMI,lev=MatMlev)

    # Western migration scenario # factor 3
    MigLev<-match(Design_Ref[i,3],Design$all_levs[[3]])
    OMI<-Mig_Ref(OMI, MigLev)

    # Scale scenario # factor 4
    BmuLev<-match(Design_Ref[i,4],Design$all_levs[[4]])
    OMI<-Bmu_Ref(OMI,BmuLev)

    # Scale scenario # factor 5
    LcompLev<-match(Design_Ref[i,5],Design$all_levs[[5]])
    OMI<-Lcomp_Ref(OMI,LcompLev)

    print(paste(i,":",OMcodes[i],Reclev,MatMlev,MigLev,BmuLev,LcompLev))


    file.copy(paste(OMDir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
    file.copy(paste(OMDir,"/stats2.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
    file.copy(paste0(OMDir,"/M3.tpl"),OMfolder,overwrite=T)

    if(Reclev==1)file.copy("C:/Users/tcar_/Dropbox/abft-mse/objects/Pins/Rec1/M3.pin",OMfolder,overwrite=T)
    if(Reclev==2)file.copy("C:/Users/tcar_/Dropbox/abft-mse/objects/Pins/Rec2/M3.pin",OMfolder,overwrite=T)

    M3write(OMI,OMdir=OMfolder)                                        # write the appropriate data file into the temporary folder ready to be run in parallel
    save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

  }
}


setwd(wd)
fitdirs <- (1:nrow(Design$Design_Ref)) [Design$Design_Ref[, 1]!="3"]
OMdir<-"C:/Users/tcarruth/Dropbox/abft-mse/objects/OMs/"
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

# Mix I Lcomp L
nOMs<-nrow(Design$Design_Ref)
introtext<-"A comparison report for reference grid OMs with 1% western biomass mixing (mixing factor level I) and length composition weighting of 1/20 (factor level L)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="L" & Design$Design_Ref[,3] == "I"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_I_Lcomp_L_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)

# Mix II Lcomp L
nOMs<-nrow(Design$Design_Ref)
introtext<-"A comparison report for reference grid OMs with 20% western biomass mixing (mixing factor level II) and length composition weighting of 1/20 (factor level L)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="L" & Design$Design_Ref[,3] == "II"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_II_Lcomp_L_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)

# Mix I Lcomp H
nOMs<-nrow(Design$Design_Ref)
introtext<-"A comparison report for reference grid OMs with 1% western biomass mixing (mixing factor level I) and length composition weighting of 1 (factor level H)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="H" & Design$Design_Ref[,3] == "I"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_I_Lcomp_H_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)

# Mix II Lcomp H
nOMs<-nrow(Design$Design_Ref)
introtext<-"A comparison report for reference grid OMs with 20% western biomass mixing (mixing factor level II) and length composition weighting of 1 (factor level H)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3 & Design$Design_Ref[,5]=="H" & Design$Design_Ref[,3] == "II"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_II_Lcomp_H_summary.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)




# Index fit reports -----------------------------------------------------------------------------------------------------------------------------------------------
custom_cols<-rep(c('black','blue','grey','orange'),each=4)
custom_lwds<-rep(rep(c(1,2),each=2),4)
custom_ltys<-rep(c(1:2),8)
custom_cexs<-0.5

CPUEnos<-match(c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_177","US_GOM_PLL2","JPN_LL_West2","CAN GSL","CAN SWNS"),OMI@CPUEnames)
Inos<-match(c("FR_AER_SUV2","MED_LAR_SUV","CAN_ACO_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR"),OMI@Inames)
Iarea<-c(OMI@CPUEobs[match(CPUEnos,OMI@CPUEobs[,4]),3],OMI@Iobs[match(Inos,OMI@Iobs[,5]),3])
Proposed <-c("GOM_LAR_SUV","US_RR_66_114","JPN_LL_West2","CAN_ACO_SUV",
             "JPN_LL_NEAtl2","MED_LAR_SUV","FR_AER_SUV2","GBYP_AER_SUV_BAR")
discont<-c("MED_LAR_SUV","FR_AER_SUV2","GBYP_AER_SUV_BAR")


# Mix I Lcomp L
nOMs<-nrow(Design$Design_Ref)
introtext<-"Index fitting report for reference grid OMs with 1% western biomass mixing (mixing factor level I) and length composition weighting of 1/20 (factor level L)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="L" & Design$Design_Ref[,3] == "I"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_I_Lcomp_L_Index_Fit_2.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary_manyOMs.Rmd"),output_file=outfile)

# Mix II Lcomp L
nOMs<-nrow(Design$Design_Ref)
introtext<-"Index fitting report for reference grid OMs with 20% western biomass mixing (mixing factor level II) and length composition weighting of 1/20 (factor level L)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="L" & Design$Design_Ref[,3] == "II"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_II_Lcomp_L_Index_Fit_2.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary_manyOMs.Rmd"),output_file=outfile)

# Mix I Lcomp H
nOMs<-nrow(Design$Design_Ref)
introtext<-"Index fitting report for reference grid OMs with 1% western biomass mixing (mixing factor level I) and length composition weighting of 1 (factor level H)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="H" & Design$Design_Ref[,3] == "I"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_I_Lcomp_H_Index_Fit_2.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary_manyOMs.Rmd"),output_file=outfile)

# Mix II Lcomp H
nOMs<-nrow(Design$Design_Ref)
introtext<-"Index fitting report for reference grid OMs with 20% western biomass mixing (mixing factor level II) and length composition weighting of 1 (factor level H)"
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,5]=="H" & Design$Design_Ref[,3] == "II"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
outfile<-paste0(OMdir,"Mix_II_Lcomp_H_Index_Fit_2.html")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary_manyOMs.Rmd"),output_file=outfile)








# copy ind reps -------------------------------------------
copyinds<-F
if(copyinds){

  nOMs<-nrow(Design$Design_Ref)
  repdir<-"C:/temp/OMreps_6_6_18"
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
  TAC2015=c;TAC2016=c(19296000,1912000);TAC2017=c(23155000,2000000);TAC2018=c(28200000,2350000);TAC2019=c(32240000,2350000);TAC2020=c(36000000,2350000)
  Allocation=ABTMSE:::Allocation; MPareas=NA; Fdistyrs=3; maxTAC=c(10,10); MSEparallel=F; check=FALSE

}


OMfolders<-paste(paste(getwd(),"/Objects/OMs/",1:nOMs,sep=""))

SD_override<-data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR"),
                        SD=c(  0.45,         0.45,            0.8,          0.45              ))

AC_override<- data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR","JPN_LL_West2"),
                         AC=c(  0.2,         0,               0.2,          0.2,                         0))

Yrs_override<-data.frame(Name='MED_LAR_SUV',start=48, end=52)
CPUEinds<-c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_177","US_GOM_PLL2","JPN_LL_West2","CAN GSL","CAN SWNS")
Iinds<-c("FR_AER_SUV2","MED_LAR_SUV","CAN_ACO_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR")


for(i in 1:nOMs){

  print(paste0(i,":  ",OMcodes[i]))
  j<-Design$Design_Ref[i,1]
  OM<-new('OM',OMd=OMfolders[i],nsim=48,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[j]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)
  save(OM,file=paste0(OMfolders[i],'/OM_',i))
  OM<-new('OM',OMd=OMfolders[i],nsim=2,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[j]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)
  save(OM,file=paste0(OMfolders[i],'/OM_',i,"d"))

}


