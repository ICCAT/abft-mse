# ===================================================================================================================
# ==== MSE operating model modification functions ===================================================================
# ===================================================================================================================

# These functions represent modifications to a Base OMI (operating model input) object.


# Factor 1 Recruitment scenario

Rec_Ref<-function(OMI,lev=NA){

  if(is.na(lev)){

    return(3)

  }else if(lev=='Names'){

    return(c("W: h=0.6 then 0.9, E: h=0.98 then 0.98","W: h=0.6, E:h=0.7", "W/E future shift to first recruitment in level 1"))

  }else if(lev=='LongNames'){

    return(c("Level 1: West BH h=0.6 to h = 0.9 in 1975, East BH h=0.98 R0 before/after 1988",
             "Level 2: West BH h=0.6, East BH h=0.7",
             "Level 3: As level 1 but changes back to first recruitment after 10 years"))

  }else{


    if(lev==1|lev==3){# East,  West
      OMI@nSR=4
      OMI@SRpar<-c(0.98,0.98,0.6,0.9)
      OMI@SRp<-c(1,1,2,2)
      OMI@SRtype<-c("BH","BH","BH","BH")
      OMI@SRminyr<-c(1,24,1,11)
      OMI@SRmaxyr<-c(23,52,10,52)

      OMI@nRDs<-rep(1,4)

      RDts<-array(4,c(OMI@np,OMI@ny))
      RDno<-array(1,c(OMI@np,OMI@ny))
      yblock<-2 # the duration of recruitment deviation blocks
      for(rd in 1:OMI@nSR){
        RDts[OMI@SRp[rd],OMI@SRminyr[rd]:OMI@SRmaxyr[rd]]=rd
        tempvec<-rep(1:100,each=yblock)[1:(OMI@SRmaxyr[rd]-OMI@SRminyr[rd]+1)]
        RDno[OMI@SRp[rd],OMI@SRminyr[rd]:OMI@SRmaxyr[rd]]=tempvec
        OMI@nRDs[rd]<-max(tempvec)

      }
      OMI@RDno=RDno
      OMI@RDts=RDts

    }else if(lev==2){      # East,  West

      OMI@nSR=2
      OMI@SRpar<-c(0.7,0.6)
      OMI@SRp<-c(1,2)
      OMI@SRtype<-c("BH","BH")
      OMI@SRminyr<-c(1,1)
      OMI@SRmaxyr<-c(52,52)
      OMI@nRDs<-rep(1,4)

      RDts<-array(4,c(OMI@np,OMI@ny))
      RDno<-array(1,c(OMI@np,OMI@ny))
      yblock<-2 # the duration of recruitment deviation blocks
      for(rd in 1:OMI@nSR){
        RDts[OMI@SRp[rd],OMI@SRminyr[rd]:OMI@SRmaxyr[rd]]=rd
        tempvec<-rep(1:100,each=yblock)[1:(OMI@SRmaxyr[rd]-OMI@SRminyr[rd]+1)]
        RDno[OMI@SRp[rd],OMI@SRminyr[rd]:OMI@SRmaxyr[rd]]=tempvec
        OMI@nRDs[rd]<-max(tempvec)

      }
      OMI@RDno=RDno
      OMI@RDts=RDts

    }

    return(OMI)

  }

}

# Factor 2 Abundance fitting -----------------------------------

SSB_Dep_Ref<-function(OMI,lev=NA){

  load(system.file("ts2017.Rdata", package="ABTMSE"))
  dat<-ts2017

  if(is.na(lev)){

    return(3)

  }else if(lev=='Names'){

    return(c("Best estimate","As 2017 assessments", "Matches perception of heavy depletion"))

  }else if(lev=='LongNames'){

    return(c("Level 1: Best estimates of the M3 model",
             "Level 2: Strong priors on east-west area SSB from 2017 assessments",
             "Level 3: Prior on depletion to match perception of heavy exploitation"))

  }else{

    if(lev==1){# East,  West

      OMI@nSSBprior=1
      OMI@SSBprior<-matrix(c(1,1,20000),nrow=1)
      OMI@SSBCV<-10
      OMI@nDepprior=1
      OMI@Depprior<-matrix(c(1,1,0.5),nrow=1)
      OMI@DepCV<-10
      OMI@LHw[12:13]<-0
      #OMI@LHw[14]<-25 # the Fmod prior

    }else if(lev==2){      # East,  West

      tempdat<-dat[dat$assessment=="VPA",c(1,3,4)]
      tempdat[,1]<-match(tempdat[,1],c("East","West"))
      tempdat[,2]<-tempdat[,2]-OMI@years[1]+1

      OMI@nSSBprior=nrow(tempdat)
      OMI@SSBprior<-as.matrix(tempdat)
      OMI@SSBCV<-0.1
      OMI@nDepprior=1
      OMI@Depprior<-matrix(c(1,1,0.5),nrow=1)
      OMI@DepCV<-10
      OMI@LHw[12:13]<-c(0.5,0)
      #OMI@LHw[14]<-5 # 2 x base - the Fmod prior

    } else{

      OMI@nSSBprior=1
      OMI@SSBprior<-matrix(c(1,1,20000),nrow=1)
      OMI@SSBCV<-10
      OMI@nDepprior=20
      OMI@Depprior<-cbind(rep(1:2,each=10),rep(41:50,2),rep(0.25,20))
      OMI@DepCV<-0.05
      OMI@LHw[12:13]<-c(0,0.05)
      OMI@LHw[14]<-5 # 5 x base - the Fmod prior
    }

    return(OMI)

  }

}




# Factor 3 Natural mortality rate / M --------------------------

MatM_Ref2<-function(OMI,lev=NA){

  if(is.na(lev)){

    return(4)

  }else if(lev=='Names'){

    return(c("L Mat - L M","L Mat - H M","H Mat - L M", "H Mat - H M"))

  }else if(lev=='LongNames'){

    return(c("Level 1: Younger maturity, high M",
             "Level 2: Younger maturity, low M",
             "Level 3: Older maturity, high M",
             "Level 4: Older maturity, low M"))

  }else{

    matlow<- c(0, 0, 0.25, 0.5, rep(1,OMI@na-4)) # both stocks
    mathighW<-c(0, 0, 0, 0, 0, 0, 0.01, 0.04, 0.19, 0.56, 0.88, 0.98, 1, rep(1,OMI@na-13)) #both stocks
    mathighE<-c(0, 0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, rep(1,OMI@na-8)) #both stocks


    if(lev==1){# East,  West

      tmat<-array(c(matlow,matlow),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]


    }else if(lev==2){      # East,  West

      tmat<-array(c(matlow,matlow),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]
      OMI@Ma<-t(array(c(0.36, 0.27, 0.21, 0.17, 0.14, 0.12, 0.11, 0.10, 0.09, 0.09, 0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.07,rep(0.07,OMI@na-18)),c(OMI@na,OMI@np)))

    }else if(lev==3){

      tmat<-array(c(mathighE,mathighW),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]

    }else{ # East,  West

      tmat<-array(c(mathighE,mathighW),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]
      OMI@Ma<-t(array(c(0.36, 0.27, 0.21, 0.17, 0.14, 0.12, 0.11, 0.10, 0.09, 0.09, 0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.07,rep(0.07,OMI@na-18)),c(OMI@na,OMI@np)))

    }

    surv<-exp(-t(apply(cbind(c(0,0),OMI@Ma[,1:(OMI@na-1)]),1,cumsum)))
    OMI@SSBpR<-apply(surv*OMI@Fec,1,sum)+surv[,OMI@na]*exp(-OMI@Ma[,OMI@na])/(1-exp(-OMI@Ma[,OMI@na]))*OMI@Fec[,OMI@na]

    return(OMI)

  }

}


SSBref<-function(OMI,lev=NA){

  if(is.na(lev)){

    return(3)

  }else if(lev=='Names'){

    return(c("Best estimate","W SSB as VPA","E SSB inc as VPA"))

  }else if(lev=='LongNames'){

    return(c("Level 1: Best estimates of spawning biomass",
             "Level 2: Western SSB like assessment",
             "Level 3: Eastern SSB increases like assessment"))

  }else{


    if(lev==2){      # East,  West

      OMI@SSBfit = 3
      OMI@SSBprior = c(3E+8, 2.7E+7) # tonnes
      OMI@SSBCV=0.01
      OMI@LHw[12]<-100

    }else if(lev==3){# East,  West

      OMI@SSBinc = 5
      OMI@SSBincstock=1
      OMI@SSBy = c(OMI@ny-11,OMI@ny)
      OMI@LHw[13]<-100
      OMI@LHw[8]<-50
      OMI@RDCV<-0.35

    }

    return(OMI)

  }

}



# Steepness ---------------------------------------------

Steep_Ref<-function(OMI,lev=NA){

  if(is.na(lev)){

    return(2)

  }else if(lev=='Names'){

    return(c("Lh","Hh"))

  }else if(lev=='LongNames'){

    return(c("Level 1: Steepness of the Beverton Holt Stock recruitment relationship for both stocks assumed to be 0.7",
             "Level 2: Steepness of the Beverton Holt Stock recruitment relationship for both stocks assumed to be 0.98"))

  }else{

    if(lev==1){
      OMI@steep<-c(0.7,0.7)
    }else{
      OMI@steep<-c(0.98,0.98)
    }

    return(OMI)

  }

}

# Choice of master index for calculating partial Fs -------------------------

Ind_Ref<-function(OMI,lev=NA){

  load(file=paste(getwd(),"/Data/Processed/Conditioning/MI",sep=""))

  if(is.na(lev)){

    return(2)

  }else if(lev=='Names'){

    return(c("I1","I2"))

  }else if(lev=='LongNames'){

    return(c("Level 1: Master abundance index calculated including Spanish CPUE data",
             "Level 2: Master abundance index calculated excluding Spanish CPUE data"))

  }else{

    if(lev==1){
      OMI@RAI<-MI[[1]] # 0% catchability increase per year
    }else{
      OMI@RAI<-MI[[3]] # 0% catchability increase per year
    }
    OMI@D_ini<-c(sum(OMI@RAI[,2,OMI@canspawn[,1]==1][1:3])/sum(OMI@RAI[,2,OMI@canspawn[,1]==1][(OMI@ny-2):OMI@ny]),sum(OMI@RAI[,2,OMI@canspawn[,2]==1][1:3])/sum(OMI@RAI[,2,OMI@canspawn[,2]==1][(OMI@ny-2):OMI@ny]))# just for comparison with simulations

    return(OMI)

  }

}


# Depletion signal of index (could be half of base level) -----------------

Dep_Ref<-function(OMI,lev=NA){

  if(is.na(lev)){

    return(2)

  }else if(lev=='Names'){

    return(c("D1","D2"))

  }else if(lev=='LongNames'){

    return(c("Level 1: Depletion of both stocks as inferred by the unmodified master index",
             "Level 2: Depletion of both stocks inferred by master index declining linearly to a 50% reduction"))

  }else{

    if(lev==2){

      OMI@RAI<-OMI@RAI*seq(1,0.5,length.out=OMI@ny)

    }

    Catches<-CPUEobs<-array(NA,c(OMI@ny,OMI@ns,OMI@nr,OMI@nf))
    Catches[as.matrix(OMI@Cobs[,1:4])]<-OMI@Cobs[,5]
    Catches<-(Catches/(mean(Catches,na.rm=T)))*0.001
    ind<-TEG(c(OMI@ny,OMI@ns,OMI@nr,OMI@nf))
    CPUEobs[ind]<--log((1-Catches[ind]/OMI@RAI[ind[,1:3]]))

    CPUEobs<-cbind(ind,ind[,4],CPUEobs) # y s r f i cpue/pf
    CPUEobs<-CPUEobs[!is.na(CPUEobs[,6]),]
    mubyfleet<-aggregate(CPUEobs[,6],by=list(CPUEobs[,4]),FUN=mean)
    CPUEobs[,6]<-CPUEobs[,6]/mubyfleet[CPUEobs[,4],2]
    OMI@CPUEobs<-CPUEobs
    OMI@nCPUEobs<-nrow(CPUEobs)

    return(OMI)

  }

}



Mat_Ref<-function(OMI,lev=NA){

  if(is.na(lev)){

    return(3)

  }else if(lev=='Names'){

    return(c("W-LSpn_E-LSpn","W-LSpn_E-HSpn","W-HSpn_E-LSpn","W-HSpn_E-HSpn"))

  }else if(lev=='LongNames'){

    return(c("Level 1: West - younger spawning, East - younger spawning",
             "Level 2: West - younger spawning, East - older spawning",
             "Level 3: West - older spawning, East - younger spawning",
             "Level 4: West - older spawning, East - older spawning"))

  }else{

    matlow<- c(0, 0, 0.25, 0.5, rep(1,OMI@na-4)) # both stocks
    mathighW<-c(0, 0, 0, 0, 0, 0.01, 0.04, 0.19, 0.56, 0.88, 0.98, 1, rep(1,OMI@na-12)) #both stocks
    mathighE<-c(0, 0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, rep(1,OMI@na-8)) #both stocks

    if(lev==1){      # East,  West
      tmat<-array(c(matlow,matlow),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]
    }else if(lev==2){# East,  West
      tmat<-array(c(mathighE,matlow),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]
    }else if(lev==3){ # East,  West
      tmat<-array(c(matlow,mathighW),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]
    }else{
      tmat<-array(c(mathighE,mathighW),c(OMI@na,OMI@np))
      OMI@mat<-array(t(tmat),c(OMI@np,OMI@na,OMI@ny))
      OMI@Fec<-OMI@wt_age[,,1]*OMI@mat[,,1]
    }

    return(OMI)

  }
}


Bmu_Ref<-function(OMI,lev){

  if(is.na(lev)){

    return(4)

  }else if(lev=='Names'){

    return(c("--","-+","+-","++"))

  }else if(lev=='LongNames'){

    return(c("Mean biomass of 15kt and 200kt (1965-2016) for the west and east areas",
             "Mean biomass of 15kt and 400kt (1965-2016) for the west and east areas",
             "Mean biomass of 50kt and 200kt (1965-2016) for the west and east areas",
             "Mean biomass of 50kt and 400kt (1965-2016) for the west and east areas"))

  }else{

    SSBmus<-cbind(c(200,400,200,400,700),c(15,15,50,50,90))*1000
    OMI@SSBprior[OMI@SSBprior[,1]==1,3] <- SSBmus[lev,1]
    OMI@SSBprior[OMI@SSBprior[,1]==2,3] <- SSBmus[lev,2]
    return(OMI)

  }
}


Mig_Ref<-function(OMI,lev){

  if(is.na(lev)){

    return(3)

  }else if(lev=='Names'){

    return(c("LowWmix","HighWmix"))

  }else if(lev=='LongNames'){

    return(c("Low mixing of west stock biomass in east areas of 1% from 1965-2016",
             "High mixing of west stock biomass in east areas of 20% from 1965-2016"))

  }else{

    if(lev==1)OMI@BSfrac<-matrix(0.01)
    if(lev==2)OMI@BSfrac<-matrix(0.2)
    return(OMI)

  }

}


Lcomp_Ref<-function(OMI,lev){

  if(is.na(lev)){

    return(5)

  }else if(lev=='Names'){

    return(c("LowLcomp","HighLcomp"))

  }else if(lev=='LongNames'){

    return(c("Low log-likelihood weight on length composition data of 1/20",
             "High log-likelihood weight on length composition data of 1/20"))

  }else{

    if(lev==1){      # East,  West
      OMI@LHw[4]<-1/20
    }else{
      OMI@LHw[4]<-1
    }
    return(OMI)

  }

}
