# ===================================================================================================================
# ==== MSE operating model modification functions ===================================================================
# ===================================================================================================================

# These functions represent modifications to a Base OMI (operating model input) object.

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


# Natural mortality rate / M --------------------------

MatM_Ref<-function(OMI,lev=NA){

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
    mathighW<-c(0, 0, 0, 0, 0, 0.01, 0.04, 0.19, 0.56, 0.88, 0.98, 1, rep(1,OMI@na-12)) #both stocks
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

      OMI@SSBinc = 3
      OMI@SSBincstock=1
      OMI@SSBy = c(OMI@ny-8,OMI@ny)
      OMI@LHw[13]<-100

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
