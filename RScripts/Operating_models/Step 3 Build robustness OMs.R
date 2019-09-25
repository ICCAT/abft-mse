# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of robustness operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC

# 3 February 2018

# There are a set of high priority robustness OMs relating to stock mixing
# reconditioning:
# (1) Half the degree of stock mixing
# (2) Zero western fish in the East
# (3) Artifically increase GOM quarter 2 none in quarter 3
# (4) Brazilian catches in the East not west.

# not reconditioned
# (5) Time-varying mixing zero / 150% alternating every 3 years
# (6) Half goes to 150% after year 10

# Other old high priority robustness OMs

# (7) 20% overages in both East and West areas
# (8) Undetected increase in catchability of 1% (OM@qinc=1)
# (9) Non-linear index relationships (hyperstability / hyper depletion) (OM@Ibeta_ignore=F)

#rm(list=ls(all=TRUE))                       # Remove all existing objects from environment
setwd("C:/Users/tcarruth/Dropbox/abft-mse")
setwd("C:/Users/tcar_/Dropbox/abft-mse")
load(file=paste0(getwd(),"/Objects/OMs/Design"))

# --- Source MSE functions and objects ------

library(ABTMSE)
loadABT()
M3dir<-paste0(getwd(),"/M3")
ROMnams<-paste0("ROM_",1:35)
nsim<-72
proyears<-54
seed<-1

# Make directories -------------------------------

ROMnos<-1:35
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

# Make folders
for(i in 1:length(output_dirs)){
  if(!file.exists(output_dirs[i])) dir.create(output_dirs[i])
  file.copy(paste0(M3dir,"/M3.exe"),output_dirs[i],overwrite=T)      # copy the latest executable to the temporary
  file.copy(paste0(M3dir,"/stats.cxx"),output_dirs[i],overwrite=T)   # copy over the statistics library
  cat(i);cat("-")
}


# Step 1: ROMs 1-3 -------------------------------------------------------------

nOMs<-nrow(Design_Ref)
#OMcodes<-apply(Design_Ref,1,FUN=function(x)paste(x,collapse="-"))
OMnos<-4:6
input_dirs <- paste0(getwd(),"/Objects/OMs/",OMnos)
ROMnos<-1:3
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-ROMnams[ROMnos[i]]
  OMI@IobsCV[5]<-0.15
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(i)

}



# === Step 2: Specify ROMs 4-11  ====================================

OMcopies<-1:2

# --- ROMs #4-5 GOM Q2 increase, GOM Q3 none ------------------------------------------------------------------------------------

OMnos<-4:5
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){

  out<-M3read(OMDir=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/"))
  nages<-out$na
  npop<-out$np
  nsubyears<-out$ns
  nareas<-out$nr
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  #          quarter         area
  cond2<-OMI@Eobs[,2]==2&OMI@Eobs[,3]==1
  cond3<-OMI@Eobs[,3]==2&OMI@Eobs[,3]==1
  OMI@Eobs[cond2,6]<-OMI@Eobs[cond2,6]/30 # lower F to get same catches is higher VB
  OMI@Eobs[cond3,6]<-OMI@Eobs[cond3,6]*30 # higher F to get same catches is lower VB
  OMI@Name<-ROMnams[OMnos[cops]]

  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])

}



# --- ROM #6-7 Brazilian catches in the East not west ------------------------------------------------------------------------------------

OMcopies<-c(1,2)
OMnos<-c(6,7)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

# Get new HCobs
source(paste0(getwd(),"/RScripts/Data processing/Historical catches 2019.r")) # Creates HCobs will all catches south of 20 lat and west of -25 assigned to SEAtl
cond= (1950-Base@Hyears[1]+1) : (1965-Base@Hyears[1])
HCobs_str<-HCobs
HCobs[cond,,,4]<-HCobs[cond,,,4]+HCobs_str[cond,,,3]*0.2
HCobs[cond,,,3]<-HCobs_str[cond,,,3]*0.8

for(cops in 1:length(OMcopies)){

  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))

  OMI@HCobs<-HCobs
  OMI@Name<-ROMnams[OMnos[cops]]

  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])

}


# --- ROMs #8:9 150% mixing ------------------------------------------------------------------------------------

OMcopies<-c(1,2)
OMnos<-8:9
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

#OMlabs<-c(rep("Mixing changes from half to 150% every 3 years",2),rep("Mixing changes from half to 150% after 10 years",2))

for(cops in 1:length(OMcopies)){

  out<-M3read(OMDir=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/"))

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
  BSfrac[1,]<-apply(stemp[1,,1:3],1,sum)/apply(stemp[1,,],1,sum)
  BSfrac[2,]<-apply(stemp[2,,4:nareas],1,sum)/apply(stemp[2,,],1,sum)

  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@BSfrac<-BSfrac*1.5                                                     # !!! 150% THE MIXING !!!
  OMI@LHw[16]<-50
  OMI@Name<-ROMnams[OMnos[cops]]

  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])

}


# ---------- Now run all conditionings -------------------------------------------

ncond<-9 # fitting 1:9
sfInit(parallel=T,cpus=ncond)                                                # Initiate the cluster
foldernos<-1:ncond # foldernos<-c(1,6,8,9,13,17,21,25,29,33) foldernos<-c(3,11,16,20,21,23,27,31,33,35)
system.time(sfLapply(foldernos,runM3p,OMdir=paste0(getwd(),"/Objects/ROMs"),mcmc=T))   # Run the M3 executables in parallel
for(i in ncond)pin_from_par(paste0(getwd(),"/Objects/ROMs/",i))       # Record the MLE parameter estimates as initial values




# --- Construct all OMs 1-11 -------------------------------------------------------------------------

load(file=paste0(getwd(),"/Objects/OMs/1/OMI")) # Load any OMI object to get a real year reference
load(file=paste(getwd(),"/Objects/Recruitment_scenarios/Trial specifications",sep=""))


output_dirs<-paste0(getwd(),"/objects/ROMs/",1:9)
ncond<-9
converged<-rep(FALSE,ncond)

for(i in 1:ncond){
  OMno<-output_dirs[i]
  tryCatch({
    samps<-read.table(paste(OMno,"/nodes.cha",sep=""), sep=" ")
    converged[i]<-TRUE
  },
  error= function(e){
    message(paste0("Operating model ",i,": ",OMno, " did not converge and is currently unavailable"))
  })
}

# ROMs 1-7
recno<-c(1,2,3,1,2,1,2) # the reference recruitment scenarios for ROMs 1-7

for(i in 1:7){
  #if(converged[i]){
    
    OMd<-output_dirs[i]
    
    OM<-new('OM',OMd=OMd,nsim=nsim,proyears=proyears,seed=1,Recruitment=Recs[[recno[i]]])
    save(OM,file=paste0(OMd,'/OM'))
    
    OM<-new('OM',OMd=OMd,nsim=2,proyears=proyears,seed=1,MLEonly=T,Recruitment=Recs[[recno[i]]])
    save(OM,file=paste0(OMd,'/OMd'))
  #}
}


# Second-round robustness OMs =========================================================================

# ROMs 8-11  ------------------------------------------------------------------------------------
# --- Create the variable future movement scenarios ------------------

# ROM8 and ROM9 three year switch from half to 150% movement
# this is tricky - you have to make the OMs for the 8 and 9 fits then use these 150% mixing movements 
# with the movement scenario II ref case movements and either alternate (8-9) or do a shift (10-11)

OMdir<-paste0(getwd(),"/objects/ROMs/",8)
OM8<-new('OM',OMd=OMdir,nsim=nsim,proyears=proyears,seed=1,Recruitment=Recs[[recno[1]]])
OMd8<-new('OM',OMd=OMdir,nsim=2,proyears=proyears,seed=1,MLEonly=T,Recruitment=Recs[[recno[1]]])
OMdir<-paste0(getwd(),"/objects/ROMs/",8)
OM9<-new('OM',OMd=OMdir,nsim=nsim,proyears=proyears,seed=1,Recruitment=Recs[[recno[2]]])
OMd9<-new('OM',OMd=OMdir,nsim=2,proyears=proyears,seed=1,MLEonly=T,Recruitment=Recs[[recno[2]]])

# ROM8 Deterministic ----

load(paste(getwd(),"/objects/OMs/7/OMd",sep=""))
OM@mov[,,,2,,,]<-OMd8@mov[,,,1,,,] # copy over the 150 movement to position 2
ilength<-length(OM@movIndex)
OM@movIndex[(OM@nyears+1):ilength]<-rep(c(1,1,1,2,2,2),100)[1:(ilength-OM@nyears)]
save(OM,file=paste(getwd(),"/objects/ROMs/8/OMd",sep=""))

# ROM8 Stochastic ----

load(paste(getwd(),"/objects/OMs/7/OM",sep=""))
OM@mov[,,,2,,,]<-OM8@mov[,,,1,,,] # copy over the half movement to position 2
OM@movIndex[(OM@nyears+1):ilength]<-rep(c(1,1,1,2,2,2),100)[1:(ilength-OM@nyears)]
save(OM,file=paste(getwd(),"/objects/ROMs/8/OM",sep=""))

# ROM9 Deterministic ----

load(paste(getwd(),"/objects/OMs/8/OMd",sep=""))
OM@mov[,,,2,,,]<-OMd9@mov[,,,1,,,] # copy over the 150 movement to position 2
OM@movIndex[(OM@nyears+1):ilength]<-rep(c(1,1,1,2,2,2),100)[1:(ilength-OM@nyears)]
save(OMd,file=paste(getwd(),"/objects/ROMs/9/OMd",sep=""))

# ROM9 Stochastic ----

load(paste(getwd(),"/objects/OMs/8/OM",sep=""))
OM@mov[,,,2,,,]<-OM9@mov[,,,1,,,] # copy over the half movement to position 2
OM@movIndex[(OM@nyears+1):ilength]<-rep(c(1,1,1,2,2,2),100)[1:(ilength-OM@nyears)]
save(OM,file=paste(getwd(),"/objects/ROMs/9/OM",sep=""))



# ROM10 Deterministic ----

load(paste(getwd(),"/objects/OMs/7/OMd",sep=""))
OM@mov[,,,2,,,]<-OMd8@mov[,,,1,,,] # copy over the 150 movement to position 2
ilength<-length(OM@movIndex)
OM@movIndex[(OM@nyears+11):ilength]<-2
save(OM,file=paste(getwd(),"/objects/ROMs/10/OMd",sep=""))

# ROM10 Stochastic ----

load(paste(getwd(),"/objects/OMs/7/OM",sep=""))
OM@mov[,,,2,,,]<-OM8@mov[,,,1,,,] # copy over the half movement to position 2
OM@movIndex[(OM@nyears+11):ilength]<-2
save(OM,file=paste(getwd(),"/objects/ROMs/10/OM",sep=""))

# ROM11 Deterministic ----

load(paste(getwd(),"/objects/OMs/8/OMd",sep=""))
OM@mov[,,,2,,,]<-OMd9@mov[,,,1,,,] # copy over the 150 movement to position 2
OM@movIndex[(OM@nyears+11):ilength]<-2
save(OMd,file=paste(getwd(),"/objects/ROMs/11/OMd",sep=""))

# ROM11 Stochastic ----

load(paste(getwd(),"/objects/OMs/8/OM",sep=""))
OM@mov[,,,2,,,]<-OM9@mov[,,,1,,,] # copy over the half movement to position 2
OM@movIndex[(OM@nyears+11):ilength]<-2
save(OM,file=paste(getwd(),"/objects/ROMs/11/OM",sep=""))




# Third Round ROMs ============================================================================================================

OMcopies<-1:2
UpFac<-5

# === Scenescence ROMs 12-13 =======================================

OMnos<-12:13
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@Name<-ROMnams[OMnos[cops]]
  OMI@Ma[,26:OMI@na]<-0.47 
  
  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])
  
}


# === Upweight of CPUE indices 14-15 ====================================

OMnos<-14:15
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@Name<-ROMnams[OMnos[cops]]
  OMI@LHw[2]<- OMI@LHw[2]*UpFac
  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])
}


# === Upweight of Fishery Independent indices 16-17 ====================================

OMnos<-16:17
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@Name<-ROMnams[OMnos[cops]]
  OMI@LHw[3]<- OMI@LHw[3]*UpFac
  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])
}


# === Upweight of genetic SOO 18-19 ====================================

OMnos<-18:19
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@Name<-ROMnams[OMnos[cops]]
  OMI@LHw[5]<- OMI@LHw[5]*UpFac
  cond<-OMI@SOOobs[,8]==1 # 1 is microchem, 2 is genetics
  OMI@SOOobs[cond,7]<-10 # CV is huge for microchem
  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])
  
}

# === Upweight of microchem SOO 20-21 ====================================

OMnos<-20:21
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@Name<-ROMnams[OMnos[cops]]
  OMI@LHw[5]<- OMI@LHw[5]*UpFac
  cond<-OMI@SOOobs[,8]==2 # 1 is microchem, 2 is genetics
  OMI@SOOobs[cond,7]<-10 # CV is huge for microchem
  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])
  
}


# === Upweight of Length comp data 22-23 ====================================

OMnos<-22:23
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@Name<-ROMnams[OMnos[cops]]
  OMI@LHw[4]<- OMI@LHw[4]*UpFac
  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])
}



# === Upweight of landings data 24-25 ====================================

OMnos<-24:25
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMI"))
  OMI@Name<-ROMnams[OMnos[cops]]
  OMI@LHw[1]<- OMI@LHw[1]*UpFac
  save(OMI,file=paste0(output_dirs[cops],"/OMI"))                       # save the input object into its home folder
  M3write(OMI,output_dirs[cops])
}



# Conditioning --------------------------------



sfInit(parallel=T,cpus=10)                                                # Initiate the cluster
foldernos<-12:25 
system.time(sfLapply(foldernos,runM3p,OMdir=paste0(getwd(),"/Objects/ROMs"),mcmc=T))   # Run the M3 executables in parallel
for(i in foldernos)pin_from_par(paste0(getwd(),"/Objects/ROMs/",i))       # Record the MLE parameter estimates as initial values




# ======= Make OMs ==================================


load(file=paste0(getwd(),"/Objects/OMs/1/OMI")) # Load any OMI object to get a real year reference
load(file=paste(getwd(),"/Objects/Recruitment_scenarios/Trial specifications",sep=""))


output_dirs<-paste0(getwd(),"/objects/ROMs/",foldernos)

converged<-rep(FALSE,ncond)

for(i in 1:length(output_dirs)){
  OMno<-output_dirs[i]
  tryCatch({
    samps<-read.table(paste(OMno,"/nodes.cha",sep=""), sep=" ")
    converged[i]<-TRUE
  },
  error= function(e){
    message(paste0("Operating model ",i,": ",OMno, " did not converge and is currently unavailable"))
  })
}

# ROMs 12:25
recno<-rep(c(1,2),7) # the reference recruitment scenarios for ROMs 1-7

for(i in 1:length(output_dirs)){
 
  OMd<-output_dirs[i]
  OM<-new('OM',OMd=OMd,nsim=nsim,proyears=proyears,seed=1,Recruitment=Recs[[recno[i]]])
  save(OM,file=paste0(OMd,'/OM'))
  OM<-new('OM',OMd=OMd,nsim=2,proyears=proyears,seed=1,MLEonly=T,Recruitment=Recs[[recno[i]]])
  save(OM,file=paste0(OMd,'/OMd'))

}


# --- Catchability increase of 2% in indices 26-27 -------------------------------

OMnos<-26:27
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OM"))
  OM@qinc<-2
  save(OM,file=paste0(output_dirs[cops],"/OM")) 
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMd"))
  OM@qinc<-2
  save(OM,file=paste0(output_dirs[cops],"/OMd"))                      
}



# --- Catchability decrease of 2% in indices 28-29 ----------------------------------

OMnos<-28:29
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OM"))
  OM@qinc<-(-2)
  save(OM,file=paste0(output_dirs[cops],"/OM")) 
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMd"))
  OM@qinc<-(-2)
  save(OM,file=paste0(output_dirs[cops],"/OMd"))                      
}



# --- Add hyperstability / hyperdepletion to indices 30-31 -------------------------------------

OMnos<-30:31
output_dirs<-paste0(getwd(),"/Objects/ROMs/",OMnos)

for(cops in 1:length(OMcopies)){
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OM"))
  OM@Ibeta_ignore<-FALSE
  save(OM,file=paste0(output_dirs[cops],"/OM")) 
  load(file=paste0(getwd(),"/Objects/OMs/",OMcopies[cops],"/OMd"))
  OM@Ibeta_ignore<-FALSE
  save(OM,file=paste0(output_dirs[cops],"/OMd"))                      
}






# =========== Build the fitting reports ===================

OMfolder<-paste0(getwd(),"/Objects/ROMs/",c(1:9,12:25))
load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017
for(ij in 2:ncond){
  if(converged[ij]){
    input_dir<-OMfolder[ij]
    out<-M3read(input_dir)
    load(paste(input_dir,"/OMI",sep=""))

    lab<-OMcode[ij]
    #render(input=system.file("OMreport.Rmd", package="ABTMSE"), output_file=paste(input_dir,"/Report_",lab,".html",sep=""))
    render(input=system.file("OMreport.Rmd", package="ABTMSE"), output_file=paste(input_dir,"/Report_",lab,".html",sep=""))
  }
}


load(file=paste0(getwd(),"/Objects/OMs/Design.Rdata"))
for(i in 1:length(OMfolder))if(converged[i])file.copy(paste0(OMfolder[i],"/Report_",foldernos[i],".html"),"C:/OMtemp",overwrite=T)      # copy the relevant report








# ==== END ==================================================================================================================================

