
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
#loadABT()

setwd("C:/Users/tcar_/Dropbox/abft-mse")
setwd("C:/Users/tcarruth/Dropbox/abft-mse")
wd<-getwd()

load("Objects/OMs/OMI") # Load base OM
OMIt<-OMI

#              1 catch, 2 cpue, 3 FIindex,  4 Lcomp,   5 SOO, 6 PSAT,  7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB, 13 SSBinc, 14 Fmod,  15 R0diff, 16 BSfrac, 17 MICV, 18 SpatPr
OMIt@LHw <-   c(1/50,  1,       2,          0.05,         1,     5,       0,       1,        1,     1,      1,      1,      0,         1,        1,         1,         1,       1)
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
OMIt@Phases<-c(1,2,3,4)
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

# --- Source MSE functions and objects -------------------------------------------------------------------------

packageVersion('ABTMSE')
OMDir<-paste(getwd(),"/M3",sep="")

# --- Set up the MSE design matrix
#                 rec            M/mat        mix         scale
all_levs<-list(c("1","2","3"), c("A","B"), c("I","II"),c("-","+"))

all_lnams<-list( c("1: West: h=0.6 to h=0.9 1975+, East:  h=0.98 for 1987- to h=0.98 1988+",
                   "2: West: B-H h=0.6 all years, East: B-H h=0.9 all years",
                   "3: West: post 75+ changes to pre '75 after 10 yrs, East: 88+ to '50-87 after 10 years"),

                 c("A: Younger spawning, High M",
                   "B: Older spawning, Low M"),

                 c("I: Low West Stock Migration",
                   "II: High West Stock Migration"),

                 c("-: Smaller Spawning Stock Biomass",
                   "+: Larger Spawning Stock Biomass")
                 )

Mig_Ref<-function(OMI,code){
  if(code=="I")OMI@BSfrac<-matrix(0.05)
  if(code=="II")OMI@BSfrac<-matrix(0.2)
  OMI
}

Sca_Ref<-function(OMI,code){
  if(code=="-")OMI@SSBprior[,3]<-SSBprior[,3]
  if(code=="+")OMI@SSBprior[,3]<-SSBprior[,3]*2
  OMI
}


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
  OMfolder<-paste(getwd(),"/Objects/OMs/",OMno,sep="")
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
  OMI<-Mig_Ref(OMI, Design_Ref[i,3])

  # Scale scenario # factor 4
  OMI<-Sca_Ref(OMI, Design_Ref[i,4])

  file.copy(paste(OMDir,"/M3.exe",sep=""),OMfolder,overwrite=T)      # copy the latest executable to the temporary
  file.copy(paste(OMDir,"/stats2.cxx",sep=""),OMfolder,overwrite=T)   # copy over the statistics library
  file.copy(paste0(OMDir,"/M3.tpl"),OMfolder,overwrite=T)

  M3write(OMI,OMdir=OMfolder)                                        # write the appropriate data file into the temporary folder ready to be run in parallel
  save(OMI,file=paste(OMfolder,"/OMI",sep=""))                       # save the input object into its home folder

}


setwd(wd)
fitdirs <- (1:nOMs) [Design$Design_Ref[, 1]!="3"]
OMdir<-"C:/Users/tcarruth/Dropbox/abft-mse/objects/OMs/"
dirs<-paste0(OMdir,fitdirs)

sfInit(parallel=T,cpus=10)

system.time({
  sfLapply(fitdirs,runM3p,OMdir=OMdir,mcmc=F,hess=F)
})

for(i in 1:nexp)pin_from_par(dirs[i])


load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017
OMdir<-"C:/Users/tcar_/Dropbox/abft-mse/objects/OMs/"
repdirs<-paste0(OMdir,1:nrow(Design_Ref))


introtexts<-list()

commontext<-"OMs labelled with a 1 or a 2 refer to a historical regime shift (1) and no-regime shift (2). OMs labeled with an A or a B refer to high natural mortality rate,
 low age at maturity (A) and low natural mortality rate and high age at maturity (B). Operating models labelled with an I or an II refer to low western mixing of an average 5% Western
stock in the East area per year under asymptotic unfished conditions (I) or high western mixing of an average 20% western stock in the East area under asymptotic unfished conditions (II)"

introtexts[[1]]<-paste("A comparison report for interim grid reference operating models 1-12. These operating models include a prior on Spawning Stock Biomass
by East / West area that matches the most recent Stock Synthesis assessments for each area (these 'smaller scale' OMs are denoted with a '-' symbol).",commontext)

introtexts[[2]]<-paste("A comparison report for interim grid reference operating models 13-24. These operating models include a prior on Spawning Stock Biomass
by East / West area that is double the most recent Stock Synthesis assessments for each area (these 'larger scale' OMs are denoted with a '+' symbol).",commontext)

jj<-0
for(scale in all_levs[[4]]){

    OMnos<-(1:nrow(Design_Ref))[Design_Ref[,4]==scale & Design_Ref[,1]!=3]
    OMnames <-   paste0(OMnos,sep=": ",OMcodes[OMnos])
    OMdirs  <-   repdirs[OMnos]
    jj<-jj+1
    introtext<-introtexts[[jj]]
    outfile<-paste0(OMdir,"/Ref OMs ",scale,".html")
    render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),
           output_file=outfile)

}

# Individual reports
for(ij in fitdirs){

  load(paste0(repdirs[ij],"/OMI"))
  out<-M3read(OMDir=repdirs[ij])
  render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OMreport_test.Rmd"),
         output_file=paste0(repdirs[ij],"/Report_",ij,".html"))

}



# Comparison fit reports
introtexts[[1]]<-paste("An index fit comparison report for interim grid reference operating models 1-12. These operating models include a prior on Spawning Stock Biomass
by East / West area that matches the most recent Stock Synthesis assessments for each area (these 'smaller scale' OMs are denoted with a '-' symbol).",commontext)

introtexts[[2]]<-paste("A index fit comparison report for interim grid reference operating models 13-24. These operating models include a prior on Spawning Stock Biomass
by East / West area that is double the most recent Stock Synthesis assessments for each area (these 'larger scale' OMs are denoted with a '+' symbol).",commontext)


jj<-0
for(scale in all_levs[[4]]){

  OMnos<-(1:nrow(Design_Ref))[Design_Ref[,4]==scale & Design_Ref[,1]!=3]
  OMnames <-   paste0(OMnos,sep=": ",OMcodes[OMnos])
  OMdirs  <-   repdirs[OMnos]
  jj<-jj+1
  introtext<-introtexts[[jj]]
  outfile<-paste0(OMdir,"/Index Fits Ref OMs ",scale,".html")
  render(input=paste0(getwd(),"/R_package/ABTMSE/inst/Index_fit_summary.Rmd"),
         output_file=outfile)

}



#for(ij in 1:nrow(Design_Ref)){
#  files<-list.files(repdirs[ij],full.names = T)
#  todelete<-files[grepl(".html",files)]
#  file.remove(todelete)
#}





