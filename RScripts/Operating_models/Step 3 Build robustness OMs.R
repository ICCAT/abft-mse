# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a set of robustness operating models following the trial specifications document that
# are modifications of the base model

# Tom Carruthers UBC

# 3 June 2021

#rm(list=ls(all=TRUE))                       # Remove all existing objects from environment

# === Set up ================================================================

setwd("C:/Users/tcarruth/Dropbox/abft-mse")
setwd("C:/Users/tcar_/Dropbox/abft-mse")

# --- Source MSE functions and objects ------

library(ABTMSE)
loadABT()
M3dir<-paste0(getwd(),"/M3")

OMcodes<-apply(Design$Design_Ref,1,FUN=function(x)paste(x,collapse=" "))

# ROM grids ---------------------------------
FourX<-match(c("1 A -- L","2 A -- L", "1 B -- L", "2 B -- L"),OMcodes) # all ROMs except the regime change "TVregime" and "IntPar"
FourX3<- match(c("3 A -- L","3 B -- L", "3 A -- L", "3 B -- L"),OMcodes)  # TVregime
FourX10<- match(c("1 A -- L","2 A -- L", "1 A -- H", "2 A -- H"),OMcodes)  # for IntPar

ROMlevs<-c("WstGw","Qinc","CatOver","HiWmix","BrzCt","TVmix","NLindex","PChgMix","TVregime","IntPar","ZeroEmix")
ROMcode<-paste(rep(ROMlevs,each=4),rep(c(1:4),length(ROMlevs)),sep="_")
ROMnos<-1:(length(ROMlevs)*4)
nROMs<-length(ROMnos)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)
ROMnams<-paste0("ROM_",1:nROMs)
proyears<-54
seed<-1
input_dirs <- paste0(getwd(),"/Objects/OMs/",FourX)

# Make directories -------------------------------


for(i in 1:length(output_dirs)){
  if(!file.exists(output_dirs[i])) dir.create(output_dirs[i])
  file.copy(paste0(M3dir,"/M3.exe"),output_dirs[i],overwrite=T)      # copy the latest executable to the temporary
  file.copy(paste0(M3dir,"/stats.cxx"),output_dirs[i],overwrite=T)   # copy over the statistics library
  cat(i);cat("-")
}


# === Test 1: ROMS 1-4 # Western Growth ===========================================

ROMnos<-grep(ROMlevs[1],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-ROMcode[ROMnos[i]]
  OMI@Phases<-as.integer(c(1,2,3,4)) # Estimation phases for various M3 components

  OMI@wt_age[1,,]<- OMI@wt_age[2,,]
  OMI@iALK[1,,,]<-OMI@iALK[2,,,]
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(i)

}

# === Test 2: ROMS 5-8 # Qinc ===========================================
# OM only change no reconditioning required

ROMnos<-grep(ROMlevs[2],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  assign("OM",get(paste0('OM_',FourX[i])))
  assign("OMd",get(paste0('OM_',FourX[i],"d")))

  OM@Name<-OMd@Name<-ROMcode[ROMnos[i]]
  OM@qinc<-OMd@qinc<-2

  save(OM,file=paste0(output_dirs[i],"/OM"))
  save(OMd,file=paste0(output_dirs[i],"/OMd"))
  cat(ROMnos[i])

}

# === Test 3: ROMS 9-12 # Unreported overages ===========================================
# OM only change no reconditioning required

ROMnos<-grep(ROMlevs[3],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  assign("OM",get(paste0('OM_',FourX[i])))
  assign("OMd",get(paste0('OM_',FourX[i],"d")))

  OM@Name<-OMd@Name<-ROMcode[ROMnos[i]]
  OM@IE<-OMd@IE<-"Overage_20"
  OM@Obs<-OMd@Obs<-"Unreported_20"

  save(OM,file=paste0(output_dirs[i],"/OM"))
  save(OMd,file=paste0(output_dirs[i],"/OMd"))
  cat(ROMnos[i])

}

# === Test 4: ROMS 13-16 # High western mixing ===========================================

ROMnos<-grep(ROMlevs[4],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-ROMcode[ROMnos[i]]
  OMI@Phases<-as.integer(c(1,2,3,4)) # Estimation phases for various M3 components
  OMI@BSfrac[]<-0.2
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(ROMnos[i])

}


# === Test 5: ROMs 17-20 # Brazilian catch ===============================================

ROMnos<-grep(ROMlevs[5],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

source(paste0(getwd(),"/RScripts/Data processing/Historical catches Brazilian to East.R"))
# !!!! Note you have no done the Cobs update since you have no updated equivalent of "Cobs<-read.csv(paste(getwd(),"/data/ICCAT_2020_3/Cobs_JPLLsplit_BRArobustness.csv",sep=""))

for(i in 1:length(ROMnos)){

  load(paste(input_dirs[i],"/OMI",sep=""))
  OMI@Name<-ROMnams[ROMnos[i]]
  OMI@Phases<-as.integer(c(1,2,3,4)) # Estimation phases for various M3 components
  OMI@HCobs<-HCobs # the adjusted brazilian cathes

  # !!!! Note you have no done the Cobs update since you have no updated equivalent of "Cobs<-read.csv(paste(getwd(),"/data/ICCAT_2020_3/Cobs_JPLLsplit_BRArobustness.csv",sep=""))
    #OMI@Cobs<-as.matrix(Cobs)
  #OMI@nCobs<-nrow(Cobs)

  Catches<-Eobs<-array(0,c(OMI@ny,OMI@ns,OMI@nr,OMI@nf))
  Catches[as.matrix(OMI@Cobs[,1:4])]<-OMI@Cobs[,5]
  Catches<-(Catches/(mean(Catches,na.rm=T)))*0.001
  Catches[is.na(Catches)]<-0
  cond<-Catches>0
  ind<-TEG(c(OMI@ny,OMI@ns,OMI@nr,OMI@nf))[cond,]
  Eobs[ind]<--log((1-Catches[ind]/OMI@RAI[ind[,1:3]]))
  Eobs2<-cbind(ind,ind[,4],Eobs[ind]) # y s r f i cpue/pf
  Eobs2<-Eobs2[!is.na(Eobs2[,6]),]
  mubyfleet<-aggregate(Eobs2[,6],by=list(Eobs2[,4]),FUN=mean)
  Eobs2[,6]<-Eobs2[,6]/mubyfleet[Eobs2[,4],2]*0.2
  Eobs<-Eobs2

  OMI@nE<-OMI@nf
  OMI@nEobs<-nrow(Eobs)
  OMI@Eobs<-Eobs

  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(ROMnos[i])

}


# === Test 6: ROMs 21-24 # Time Varying mixing ===============================================

# !!! Find low high mixing scenarios from fitted "L" scenarios

nOMs<-nrow(Design$Design_Ref)
OMdir<-paste0(getwd(),"/objects/OMs/")
dirs<-paste0(OMdir,1:nOMs)
OMnos <- (1:nOMs)[Design$Design_Ref[,1]!=3& Design$Design_Ref[,4]=="L"]
OMnames <- OMcodes[OMnos]
OMdirs  <-   dirs[OMnos]
nOMs<-length(OMnos)

outread<-function(x,OMdirs)M3read(OMdirs[x])
OMIread<-function(x,OMdirs){
  load(paste0(OMdirs[x],"/OMI"))
  return(OMI)
}

sfInit(parallel=T,cpus=min(nOMs,detectCores()/2))
M3read<-ABTMSE::M3read
sfExport("M3read")

# Lcomp L
outs<-sfLapply(1:nOMs,outread,OMdirs=OMdirs)
OMIs<-sfLapply(1:nOMs,OMIread,OMdirs=OMdirs)

F_E_E<-F_E_W<-F_W_E<-F_W_W<-F_EinW<-F_WinE<-SSB_W<-SSB_E<-D_W<-D_E<-new('list')
ind<-Nind<-TEG(dim(outs[[1]]$N))
indw<-ind[,c(2,4,1)]

for(OM in 1:nOMs){

  B<-SSB<-array(NA,dim(outs[[OM]]$N))

  B[ind]<-outs[[OM]]$N[ind]*outs[[OM]]$wt_age[indw]
  SSB[Nind]<-outs[[OM]]$N[ind]*outs[[OM]]$mat_age[ind[,c(1,4)]]*outs[[OM]]$wt_age[ind[,c(2,4,1)]]

  RAIpP<-apply(B,c(5,3,2,1),sum) # r s y p

  Wareas<-1:3
  Eareas<-4:7

  EinW<-apply(RAIpP[Wareas,,,1],3,sum)
  EinE<-apply(RAIpP[Eareas,,,1],3,sum)
  WinW<-apply(RAIpP[Wareas,,,2],3,sum)
  WinE<-apply(RAIpP[Eareas,,,2],3,sum)

  SSB_E[[OM]]<-apply(SSB[1,,2,,],1,sum)/1E6 #  y spawning season = 2
  SSB_W[[OM]]<-apply(SSB[2,,2,,],1,sum)/1E6 #  y spawning season = 2

  D_E[[OM]]<- SSB_E[[OM]]/SSB_E[[OM]][1]
  D_W[[OM]]<- SSB_W[[OM]]/SSB_W[[OM]][1]

  F_EinW[[OM]]<-EinW/(EinE+EinW)
  F_WinE[[OM]]<-WinE/(WinW+WinE)

  F_E_E[[OM]]<-EinE/(EinE+WinE)
  F_E_W[[OM]]<-EinW/(EinW+WinW)
  F_W_E[[OM]]<-WinE/(EinE+WinE)
  F_W_W[[OM]]<-WinW/(EinW+WinW)

}


Fracs<-unlist(lapply(F_EinW,FUN=function(x)mean(x[1:52])))
mindir<-OMdirs[match(min(Fracs),Fracs)] # 2% East stock biomass in west
maxdir<-OMdirs[match(max(Fracs),Fracs)] # 11% East stock biomass in west

minOM<-OMnos[match(min(Fracs),Fracs)]
maxOM<-OMnos[match(max(Fracs),Fracs)]

mincode<-OMcodes[minOM]
maxcode<-OMcodes[maxOM]

ROMnos<-grep(ROMlevs[6],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  assign("OM",get(paste0('OM_',FourX[i])))
  assign("OMd",get(paste0('OM_',FourX[i],"d")))

  OMmin<-get(paste0("OM_",minOM))
  OMdmin<-get(paste0("OM_",minOM,"d"))
  OMmax<-get(paste0("OM_",maxOM))
  OMdmax<-get(paste0("OM_",maxOM,"d"))

  movmin<-OMmin@mov[,,,1,,,]
  movmind<-OMdmin@mov[,,,1,,,]
  movmax<-OMmax@mov[,,,1,,,]
  movmaxd<-OMdmax@mov[,,,1,,,]
  movhist<-OM@mov[,,,1,,,]
  movhistd<-OMd@mov[,,,1,,,]

  OM@Name<-OMd@Name<-ROMcode[ROMnos[i]]

  OM@mov<-array(NA,c(dim(OM@mov)[1:3],3,dim(OM@mov)[5:7]))
  OMd@mov<-array(NA,c(dim(OMd@mov)[1:3],3,dim(OMd@mov)[5:7]))

  OM@mov[,,,1,,,]<-movhist
  OM@mov[,,,2,,,]<-movmin
  OM@mov[,,,3,,,]<-movmax
  OM@movIndex[OM@nyears+1:OM@proyears]<-rep(rep(c(2,3),each=3),50)[1:OM@proyears]

  OMd@mov[,,,1,,,]<-movhistd
  OMd@mov[,,,2,,,]<-movmind
  OMd@mov[,,,3,,,]<-movmaxd
  OMd@movIndex[OMd@nyears+1:OMd@proyears]<-rep(rep(c(2,3),each=3),50)[1:OMd@proyears]

  save(OM,file=paste0(output_dirs[i],"/OM"))
  save(OMd,file=paste0(output_dirs[i],"/OMd"))
  cat(ROMnos[i])

}




# === Test 8: ROMs 29-32 # Persistent change in mixing ===============================================

# !!! borrow low and high mixing scenarios from fitted "L" scenarios above

ROMnos<-grep(ROMlevs[8],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  assign("OM",get(paste0('OM_',FourX[i])))
  assign("OMd",get(paste0('OM_',FourX[i],"d")))

  OMmin<-get(paste0("OM_",minOM))
  OMdmin<-get(paste0("OM_",minOM,"d"))
  OMmax<-get(paste0("OM_",maxOM))
  OMdmax<-get(paste0("OM_",maxOM,"d"))

  movmin<-OMmin@mov[,,,1,,,]
  movmind<-OMdmin@mov[,,,1,,,]
  movmax<-OMmax@mov[,,,1,,,]
  movmaxd<-OMdmax@mov[,,,1,,,]
  movhist<-OM@mov[,,,1,,,]
  movhistd<-OMd@mov[,,,1,,,]

  OM@Name<-OMd@Name<-ROMcode[ROMnos[i]]

  OM@mov<-array(NA,c(dim(OM@mov)[1:3],3,dim(OM@mov)[5:7]))
  OMd@mov<-array(NA,c(dim(OMd@mov)[1:3],3,dim(OMd@mov)[5:7]))

  OM@mov[,,,1,,,]<-movhist
  OM@mov[,,,2,,,]<-movmin
  OM@mov[,,,3,,,]<-movmax
  OM@movIndex[OM@nyears+1:OM@proyears]<-c(rep(2,10),rep(3,(OM@proyears-10)))

  OMd@mov[,,,1,,,]<-movhistd
  OMd@mov[,,,2,,,]<-movmind
  OMd@mov[,,,3,,,]<-movmaxd
  OMd@movIndex[OMd@nyears+1:OMd@proyears]<-c(rep(2,10),rep(3,(OM@proyears-10)))

  save(OM,file=paste0(output_dirs[i],"/OM"))
  save(OMd,file=paste0(output_dirs[i],"/OMd"))
  cat(ROMnos[i])

}


# === Test 7: ROMS 25-28 # Non linear indices ===========================================

# need to refit and change OM to Obs="Hyperstable"
ROMnos<-grep(ROMlevs[7],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  assign("OM",get(paste0('OM_',FourX[i])))
  assign("OMd",get(paste0('OM_',FourX[i],"d")))

  OM@Name<-OMd@Name<-ROMcode[ROMnos[i]]
  OM@Obs<-OMd@Obs<-"Hyperstable"

  save(OM,file=paste0(output_dirs[i],"/OM"))
  save(OMd,file=paste0(output_dirs[i],"/OMd"))
  cat(ROMnos[i])

  dontdo<-T
  if(!dontdo){
    load(paste(input_dirs[i],"/OMI",sep=""))
    OMI@Name<-ROMcode[ROMnos[i]]
    OMI@Phases<-as.integer(c(1,2,3,4)) # Estimation phases for various M3 components
    OMI@beta<-0.5
    M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
    save(OMI,file=paste0(output_dirs[i],"/OMI"))
    cat(ROMnos[i])
  }

}


# === Test 9: ROMS 33-36 # Varying regime change time ===========================================
# OM only change no reconditioning required

ROMnos<-grep(ROMlevs[9],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)

for(i in 1:length(ROMnos)){

  assign("OM",get(paste0('OM_',FourX3[i])))
  assign("OMd",get(paste0('OM_',FourX3[i],"d")))

  ref1<-OM@Recind[,1]
  ref2<-OM@Recind[,OM@proyears]

  if(i<3){ # 5 East  20 West
    OM@Recind[]<-ref1
    OM@Recind[1,6:OM@proyears]<-ref2[1]
    OM@Recind[2,21:OM@proyears]<-ref2[2]
  }else{  # 20 East 5 West
    OM@Recind[]<-ref1
    OM@Recind[1,21:OM@proyears]<-ref2[1]
    OM@Recind[2,6:OM@proyears]<-ref2[2]
  }

  OMd@Recind<-OM@Recind
  OM@Name<-OMd@Name<-ROMcode[ROMnos[i]]

  save(OM,file=paste0(output_dirs[i],"/OM"))
  save(OMd,file=paste0(output_dirs[i],"/OMd"))
  cat(ROMnos[i])

}


# === Test 10: ROMS 37-40 # Intermediate param values for M growth maturity scale and regime shifts

ROMnos<-grep(ROMlevs[10],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)
Base<-OMI_1

for(i in 1:length(ROMnos)){

  Ma<-(OMI_1@Ma+OMI_4@Ma)/2
  wt_age<-(OMI_1@wt_age+OMI_4@wt_age)/2
  wt_len<-(OMI_1@wt_len+OMI_4@wt_len)/2
  iALK<-(OMI_1@iALK+OMI_4@iALK)/2
  mat<-(OMI_1@mat+OMI_4@mat)/2
  surv<-exp(-t(apply(cbind(c(0,0),Ma[,1:(Base@na-1)]),1,cumsum))) # assumption is that age class 0 have zero natural mortality
  Fec<-mat[,,1]*wt_age[,,1]
  SSBpR<-apply(surv*Fec,1,sum)+surv[,Base@na]*exp(-Ma[,Base@na])/(1-exp(-Ma[,Base@na]))*Fec[,Base@na]

  SSBprior<-OMI@SSBprior
  SSBprior[,3]<-(OMI_1@SSBprior[,3]+OMI_19@SSBprior[,3])/2

  assign('OMI',get(paste0("OMI_",FourX10[i])))
  OMI@Name<-ROMcode[ROMnos[i]]
  OMI@Phases<-as.integer(c(1,2,3,4)) # Estimation phases for various M3 components
  OMI@Ma<-Ma
  OMI@wt_age<-wt_age
  OMI@wt_len<-wt_len
  OMI@iALK<-iALK
  OMI@mat<-mat
  OMI@Fec<-Fec
  OMI@SSBpR<-SSBpR
  OMI@SSBprior<-SSBprior

  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(ROMnos[i])

}


# === Test 11: ROMS 41-44 # No Eastern fish in the west

ROMnos<-grep(ROMlevs[11],ROMcode)
output_dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)
OMI<-OMI_1
apd<-expand.grid(1,1:3,1:4,1:7,2:3)
apd<-cbind(apd,rep(-20,nrow(apd)))
names(apd)<-names(OMI@MovExc)

for(i in 1:length(ROMnos)){

  assign('OMI',get(paste0("OMI_",FourX[i])))
  OMI@Name<-ROMcode[ROMnos[i]]
  OMI@Phases<-as.integer(c(1,2,3,4)) # Estimation phases for various M3 components
  OMI@MovExc<-rbind(OMI@MovExc,apd)
  OMI@nMovExc<-nrow(OMI@MovExc)
  M3write(OMI,OMdir=output_dirs[i])  # Store this base operating model in the M3 directory
  save(OMI,file=paste0(output_dirs[i],"/OMI"))
  cat(ROMnos[i])

}




# ============ Now run all conditionings =============================================================================================

isdat<-function(dir)file.exists(paste0(dir,"/M3.dat"))
output_dirs<-paste0(getwd(),"/Objects/ROMs/",1:44)
fitdirs<-(1:44)[sapply(output_dirs,isdat)]

OMdir<-paste0(getwd(),"/objects/ROMs/")

sfInit(parallel=T,cpus=10)

system.time({
  sfLapply(fitdirs,runM3p,OMdir=OMdir,mcmc=F,hess=F)
})





# === OM building ====================================================================================================================

NLind<-(1:nROMs)[grepl("NLindex",ROMcode)]
Rectype<-rep(c(1,2,1,2),100)[1:length(ROMcode)] # luckily these are all alternating rec type 1,2,1,2 !!! CHECK
OMfolders<-paste(paste(getwd(),"/Objects/ROMs/",1:nROMs,sep=""))

load(file=paste(getwd(),"/Objects/Recruitment_scenarios/Trial specifications",sep="")) # Load Recs

SD_override<-NULL #data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR"),
                      #  SD=c(  0.45,         0.45,            0.8,          0.45              ))

AC_override<- NULL #data.frame(Name=c("MOR_POR_TRAP","JPN_LL_NEAtl2","FR_AER_SUV2","GBYP_AER_SUV_BAR","JPN_LL_West2"),
                        # AC=c(  0.2,         0,               0.2,          0.2,                         0))

Yrs_override<-data.frame(Name='MED_LAR_SUV',start=48, end=55)
CPUEinds<-c("MOR_POR_TRAP","JPN_LL_NEAtl2","US_RR_66_114","US_RR_115_144","US_RR_66_144","US_RR_177","MEXUS_GOM_PLL","JPN_LL_West2","CAN GSL","CAN SWNS")
Iinds<-c("FR_AER_SUV2","MED_LAR_SUV","GOM_LAR_SUV","GBYP_AER_SUV_BAR")


for(i in fitdirs){

  print(paste0(i,":  ",ROMcode[i]))
  j<-Rectype[i]

  OM<-new('OM',OMd=OMfolders[i],nsim=48,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[j]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds)


  OMd<-new('OM',OMd=OMfolders[i],nsim=2,proyears=54,seed=1,MLEonly=T,Recruitment=Recs[[j]],
          SD_override=SD_override, AC_override=AC_override, Yrs_override=Yrs_override, CPUEinds=CPUEinds, Iinds=Iinds,
          Deterministic=T, Obs = "Perfect_Obs")

  if(i%in%NLind)OM@Obs<-OMd@Obs<-"Hyperstable"

  save(OM,file=paste0(OMfolders[i],'/OM'))
  save(OMd,file=paste0(OMfolders[i],'/OMd'))

}


# Checks

isOM<-function(dir)file.exists(paste0(dir,"/OM"))
isOMd<-function(dir)file.exists(paste0(dir,"/OMd"))

output_dirs<-paste0(getwd(),"/Objects/ROMs/",1:44)
OM_dirs<-(1:44)[sapply(output_dirs,isOM)]
OMd_dirs<-(1:44)[sapply(output_dirs,isOMd)]
length(OM_dirs)==44
length(OMd_dirs)==44




# ==== build individual reports =======================================================================

load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017
#dirs<-paste0(getwd(),"/Objects/ROMs/",ROMnos)
isdat<-function(dir)file.exists(paste0(dir,"/M3.dat"))
output_dirs<-paste0(getwd(),"/Objects/ROMs/",1:44)
ROMfitnos<-(1:44)[sapply(output_dirs,isdat)]
dirs<-paste0(getwd(),"/Objects/ROMs/",ROMfitnos)


for(ij in 17:length(dirs)){

  utffile<-paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.utf8.md")
  if(file.exists(utffile))file.remove(utffile)
  load(paste0(dirs[ij],"/OMI"))
  out<-M3read(OMDir=dirs[ij])
  outfile<-paste0(dirs[ij],"/Report_R",ROMfitnos[ij],".html")
  render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OMreport.Rmd"),
         output_file=outfile)

}


alldirs<-paste0(getwd(),"/objects/ROMs/",ROMnos)
isRep<-function(dir)any(grepl("Report_",list.files(dir)))
ROMcopynos<-(1:44)[sapply(output_dirs,isRep)]


copyinds<-F
if(copyinds){

  repdir<-paste0(getwd(),"/Results/Robustness_OMs_2021/Appendices/Appendix A Individual ROM Reports")
  for(i in ROMcopynos)  file.copy(paste0(alldirs[i],"/Report_R",i,".html"),repdir)

}

# ==== build summary reports ==========================================================================


FreeComp<-TRUE # ignore design grid stuff
custom_cols<-rep(c('black','aquamarine3','orange'),each=4)
custom_lwds<-rep(rep(c(1,2),each=2),3)
custom_ltys<-rep(c(1:2),6)
custom_cexs<-0.5
repdir<-paste0(getwd(),"/Results/Robustness_OMs_2021/Appendices/Appendix B Summary ROM Reports")
OMcodes<-apply(Design$Design_Ref,1,FUN=function(x)paste(x,collapse=" "))
FourX<-match(c("1 A -- L","2 A -- L", "1 B -- L", "2 B -- L"),OMcodes) # all ROMs except the regime change "TVregime" and "IntPar"
refdirs<-paste0(getwd(),"/objects/OMs/",FourX)
alldirs<-paste0(getwd(),"/objects/ROMs/",ROMnos)



# First 8
introtext<-"A comparison report for Robustness set OMs including Western growth for the Eastern stock (WstGw), High western stock mixing (HiWmix) and
the Brazilian catch scenario (BrzCt). Four ROMs are fitted for each of these types: (1) 1A--L, (2) 2A--L, (3) 1B--L, (4) 2B-L. So WstGw_2 is reference
grid OM #2 (2A--L) but with eastern growth following that of the western stock"
ROMno <- ROMcopynos[1:8]
OMnames <- c(OMcodes[FourX],ROMcode[ROMno])
OMdirs  <-   c(refdirs,alldirs[ROMno])
OMnos<-c(FourX,ROMno)
outfile<-paste0(repdir,"/ROM Summary WstGw - HiWmix")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)

# Second 8
introtext<-"A comparison report for Robustness set OMs including the Brazilian catch scenario (BrzCt) and Intermediate parameter values (IntPar).
Four ROMs are fitted for each of these types: (1) 1A--L, (2) 2A--L, (3) 1B--L, (4) 2B-L. So WstGw_BrzCt is reference
grid OM #2 (2A--L) but with the Brazilian catch scenario."
ROMno <- ROMcopynos[9:16]
OMnos<-c(FourX,ROMno)
OMnames <- c(OMcodes[FourX],ROMcode[ROMno])
OMdirs  <-   c(refdirs,alldirs[ROMno])
outfile<-paste0(repdir,"/ROM Summary BrzCt - IntPar")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)

# Last  4
introtext<-"A comparison report for Robustness set OMs including and Zero Eastern Mixing (ZeroEmix_1).
Four ROMs are fitted for each of these types: (1) 1A--L, (2) 2A--L, (3) 1B--L, (4) 2B-L. So ZeroEmix_2 is reference
grid OM #2 (2A--L) but with the zero eastern mixing scenario"
ROMno <- ROMcopynos[17:20]
OMnos<-c(FourX,ROMno)
OMnames <- c(OMcodes[FourX],ROMcode[ROMno])
OMdirs  <-   c(refdirs,alldirs[ROMno])
outfile<-paste0(repdir,"/ROM Summary ZeroEmix")
render(input=paste0(getwd(),"/R_package/ABTMSE/inst/OM_Comp.Rmd"),output_file=outfile)






# ==== END ==================================================================================================================================

