# ===================================================================================================================
# ==== MSE operating model modification functions ===================================================================
# ===================================================================================================================

# These functions represent modifications to a Base OMI (operating model input) object.

# Natural mortality rate / M --------------------------

MatM_Ref<-function(OMI,lev=NA){
  
  if(is.na(lev)){
    
    return(3)
    
  }else if(lev=='Names'){
    
    return(c("E-HMLMat_W-LMHMat","HMHMat","LMLMat"))
    
  }else if(lev=='LongNames'){
    
    return(c("Level 1: High natural mortality and younger age of maturity for the Eastern stock, Low natural mortality and older age at maturity for the Western stock",
             "Level 2: High natural mortality and older age of maturity for both stocks",
             "Level 3: Low natural mortality and younger age of maturity for both stocks"))
    
  }else{
    
    matlow<- c(0,0,0,0.25,0.5, 1,rep(1,OMI@na-6))
    mathigh<-c(0,0,0,0,0.08,0.15,0.24,0.33,0.41,0.5,0.58,0.65,0.71,0.82,0.86,0.9,rep(1,OMI@na-16))
    Ma<-c(0.8318,0.864)*OMI@wt_age[,,OMI@ny]^-0.288 
    Mlow<-Ma*c(0.8,0.8)
    Mhigh<-Ma*c(1.2,1.2)
    
    if(lev==1){      # East,  West
      OMI@mat[1,,]<-matlow
      OMI@mat[2,,]<-mathigh
      OMI@Ma<-t(array(c(Mhigh[1,],Mlow[2,]),c(OMI@na,OMI@np)))
    }else if(lev==2){# East,  West
      OMI@mat[1,,]<-mathigh
      OMI@mat[2,,]<-mathigh
      OMI@Ma<-Mhigh
    }else{ # East,  West
      OMI@mat[1,,]<-matlow
      OMI@mat[2,,]<-matlow
      OMI@Ma<-Mlow
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
