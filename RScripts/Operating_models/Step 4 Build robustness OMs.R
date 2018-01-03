# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of robustness operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC

# 12th December 2017

# There are four high priority robustness OMs

# (1) 20% overages in both East and West areas
# (2) Undetected increase in catchability of 1% (OM@qinc=1)
# (3) Non-linear index relationships (hyperstability / hyper depletion) (OM@Ibeta_ignore=F)
# (4) Alternative mixing scenario (Frac East stock in West area is halved and vice-versa)

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

nOMs<-4
OMnams<-paste0("ROM_",1:nOMs)
OMcode<-paste0("R",1:nOMs)
OMfolder<-paste(getwd(),"/Objects/OMs/",OMcode,sep="")

# === Step 1: Fit the base operating model (~ 1 hour) =============================================================================================================

if(runinitialOM){
  runM3(OMDir,mcmc=T)                      # Run the base M3 operating model
  pin_from_par(OMDir)               # Record the MLE parameter estimates as initial values
  make_fit_reports(OMDir)           # Make_fit_reports(dirs=paste(getwd(),"/Objects/OMs/",1,sep="")) #make_fit_reports(dirs=paste0(getwd(),"/M3"))
}


# === Step 2: Specify OMs 1-3  ====================================

# --- Build operating model objects and write them to folders ------------
if(OMbuild){

  OMDir1<-paste(getwd(),"/Objects/OMs/1",sep="")

  for(i in 1:nOMs){

    if(!dir.exists(OMfolder[i]))dir.create(OMfolder[i])                 # create the directory if necessary
    file.copy(paste(OMDir1,list.files(OMDir1),sep="/"),OMfolder[i],overwrite=T)  # copy files from 1

    # Update OMI file
    load(file=paste0(getwd(),"/Objects/OMs/",OMcode[i],"/OMI"))       # load the reference operating model input object

    OMI@Name<-paste0("R",i,"/",nOMs," : ",OMcode[i])
    save(OMI,file=paste(OMfolder[i],"/OMI",sep=""))                       # save the input object into its home folder

  }

}

# ROM1 - no change just like OM1 but with 20% overages

# ROM2 - 1% catchability increase for CPUE indices

load(file=paste0(getwd(),"/Objects/OMs/1/OM"))
OM@qinc<-1
save(OM,file=paste0(OMfolder[2],"/OM"))

# ROM3 - non-linear relationship between indices and abundance

load(file=paste0(getwd(),"/Objects/OMs/1/OM"))
OM@Ibeta_ignore=FALSE
save(OM,file=paste0(OMfolder[3],"/OM"))


# ROM 4 - prior on mixing

out<-M3read(OMDir=paste0(getwd(),"/Objects/OMs/1/"))

mov<-out$mov
nages<-out$na
npop<-out$np
nsubyears<-out$ns
nareas<-out$nr

stemp<-array(1/nareas,dim=c(npop,nsubyears,nareas))
movi<-mov[,,nages,,]

for(y in 1:out$nydist){
  for(m in 1:nsubyears){
    if(m==1){
       stemp[,m,]<-apply(array(rep(stemp[,nsubyears,],nareas)*movi[,m,,],c(npop,nareas,nareas)),c(1,3),sum)
    }else{
      stemp[,m,]<-apply(array(rep(stemp[,m-1,],nareas)*movi[,m,,],c(npop,nareas,nareas)),c(1,3),sum)
    }
  }
}

BSfrac<-array(NA,c(npop,nsubyears))
BSfrac[1,]<-apply(stemp[1,,1:4],1,sum)/apply(stemp[1,,],1,sum)
BSfrac[2,]<-apply(stemp[2,,5:nareas],1,sum)/apply(stemp[2,,],1,sum)

load(file=paste0(getwd(),"/Objects/OMs/1/OMI"))
OMI@BSfrac<-BSfrac/2   # !!! HALF THE MIXING !!!
OMI@LHw[15]<-10
save(OMI,file=paste0(OMfolder[4],"/OMI"))                       # save the input object into its home folder
M3write(OMI,OMfolder[4])

if(runM3OM){
  runM3(OMfolder[4],mcmc=T)
  pin_from_par(OMfolder[4])
}

if(OMbuild){

  nsim<-48
  proyears<-50
  seed<-1

  Recs<-new('list')

  Recs[[1]]<-list(# Scenario 1: West - Hockey stick, East - '83+ B-H h=0.98

    proyears=array(c(1,Inf),c(1,2)),                           # Future recruitment follows just a single curve per stock
    years=array(c(1983-OMI@years[1]+1, Inf,                    # East
                  -Inf,                Inf),c(2,2,1)),         # West  years is future recuitment types, stock, historical time period,
    type=array(c('BH_R0','HS'),c(1,2)),                        # East - West - future recuitment follows just a single curve per stock
    fixpar=array(c(0.98, NA),c(1,2)),                          # East - West - future recruitment follows just a single curve per stock
    prob=array(c(1,1),c(1,2))                                  # Probability of flipping recruitment scenarios

  )

  OM<-new('OM',OMd=OMfolder[4],nsim=nsim,proyears=proyears,seed=1,Recruitment=Recs[[1]])
  save(OM,file=paste0(OMfolder[4],"/OM"))

}


# ==== END ==================================================================================================================================

