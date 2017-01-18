# ===========================================================================================================================================================================
# ==== ABT MSE object classes ===============================================================================================================================================
# ===========================================================================================================================================================================

# Operating  model input

setClass("OMI",representation(
  # Description
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",                           
  OMfactors="list",
  
  # other slots
  years="numeric", 
  Hyears="numeric",
  areas='character',
  areanams='character',
  area_defs='list',
  Fleets='list',
  L1='numeric',L2='numeric',K='numeric',p='numeric',
  ageM='numeric',ageMsd='numeric',mat='array',
  lenbins='numeric',
  
  # Inputs to M3 (in order)
  nHy='integer', ny='integer', ns='integer',np='integer',na='integer',nr='integer',nf='integer',
  nl='integer',nRPT='integer',RPTind='matrix',sdur='numeric',nydist='integer',#nZeq='integer',nyeq='integer',
  mulen='numeric',RDblock='numeric',nRD='integer',
  iALK='array',#p y a l (trans)
  lwa='numeric', lwb='numeric',
  len_age='array',# p a y (trans)
  wt_age='array', # p a y (trans)
  Fec='array',#steep='numeric', # p a (wt*mat) (trans)
  spawns='numeric',canspawn='matrix', # p r (trans)
  Ma='array', # p a (trans)
  nCobs='integer',  Cobs='matrix', # nCobs x 5 (y s r f Cobs) (trans)
  nCPUEq='integer', 
  nCPUEobs='integer',  CPUEobs='matrix', # nCPUEobs x 6 (y s r q f index) (trans)
  nE='integer',                  # number of partial f series (basically nfleets but coding seperatly to later account for catchability mirroring)
  nEobs='integer',Eobs='matrix', # nE x 5 (y s r f partial F)
  nCLobs='integer',CLobs='matrix', # nCLobs x 6 (y s r f l N) (trans)
  HCobs='array', # (y x s, x r x a) (trans)
  RAI='array', # r s y (not trans)
  nI='integer',nIobs='integer',Iobs='matrix', # nI x 7 (y s spawn_area pp index(=pp if SSB) type(biomass/ssb) index) (trans)
  nPSAT='integer',PSAT='matrix', # nPSAT x 7 (p a s t fr tr N) (trans)
  nPSAT2='integer',PSAT2='matrix', # nPSAT2 x 5+(np) (a s t fr tr SOOp1 SOOp2) (trans)
  nTag='integer',Tag='matrix', # nTag x 10 (y s r a - y s r f a N)
  nSOOobs ='integer',SOOobs='matrix',# nSOOobs x 6 (p a y s r N) (trans)
  nsel='integer',seltype='numeric',# nsel
  selind='numeric', # nf
  ratiolim='numeric', infleclim='numeric',
  nma='integer',ma='numeric', # na
  nMP='integer',
  nmovind='integer',movind='matrix',# nmovind x 4 (p s r r)
  nmov1='integer',mov1='matrix', # nmov1 x 4 (p s r r)
  movtype='integer',
  CobsCV='numeric',# nfleets
  CPUEobsCV='numeric',# fleets,
  IobsCV='numeric',# nI (np if SSB)
  RDCV='numeric',
  SSBprior='numeric',SSBCV='numeric',
  nLHw='integer',
  LHw='numeric', # 12: 1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB
  muR_ini='numeric',# np
  sel_ini='matrix',# f l (trans)
  selpar_ini='matrix', # f 3 (trans)
  lnF_ini='numeric', # nCobs
  ilnRD_ini='matrix', # na-1
  lnRD_ini='matrix',# ny
  mov_ini='array', # p a s r r (trans)
  qCPUE_ini='numeric', # nf
  qI_ini='numeric',# nI
  D_ini='numeric',# np
  complexRD='integer',
  complexF='integer',
  nF='integer',# nCobs
  nMPind='integer',# number of MP index observations
  MPind="data.frame",# the MP indices
  debug='integer',verbose='integer',datacheck='integer',
 
  # Misc
  CPUEnames='character'
  
))

#setMethod("initialize", "OMI", function(.Object){
  
# .Object

#})
  




setClass("SimSam",representation(
  
  # Description
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",                           
  
  # Dimensions
  nsim="integer",npop="integer",nages="integer",             # MSE dimensions
  nyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
  proyears="integer", targpop="integer", nfleets="integer",    # Proyears, number of management procedures
  interval="integer",nma="integer",ma="array",                                  # Number of movement age classes, age class definitions
  nlen="integer",lenbins="numeric",       # Proyears
  mulen="numeric",
  
  # Observation model 
  Cimp="numeric",Cb="numeric",Cerr="array",
  Iimp="numeric",Ibeta="numeric",Ierr="array",
  nCAAobs="numeric",nCALobs="numeric",Lcv="numeric",
  Mb="numeric",Kb="numeric",t0b="numeric",Linfb="numeric",
  LFCb="numeric",LFSb="numeric",
  FMSYb="numeric",FMSY_Mb="numeric",BMSY_B0b="numeric",
  ageMb="numeric", 
  Dimp="numeric", Db="numeric",Derr="array",
  Btimp="numeric", Btb="numeric",Bterr="array",
  Ftimp="numeric", Ftb="numeric",Fterr="array",
  
  hb="numeric",
  Recbcv="numeric",
  IMSYb="numeric", MSYb="numeric", BMSYb="numeric",
  
  # Management quantities
  C="array",
  D="array",
  B_BMSY="array",
  F_FMSY="array",
  B="array",
  SSB="array",
  TAC="array",
  
  
  simlist="list",
  
  # Performance metrics
  Perf="data.frame",
  POF="array",
  Y="array",
  AAVY="array",
  PB10="array",
  PB50="array",
  PB100="array"
  
)) 


setMethod("initialize", "SimSam", function(.Object,OM,Obs,movtype=2,OMDir="G:/M3",verbose=0,
                                           complexF=0,complexRD=0,M3temp="C:/M3temp/"){
  #.Object})
  #.Object<-new('SimSam')
  # Bias in fraction in spawning area (unfished)
  
  
  # Auto-correlation in recrutiment deviations is currently disabled
  set.seed(OM@seed)
  if(class(OM)!='OM'){
    print(paste('Could not run SimSam:',deparse(substitute(OMd)),'not of class OM'))
    stop()
  }
  if(class(Obs)!='Obs'){
    print(paste('Could not run SimSam:',deparse(substitute(Obs)),'not of class Obs'))
    stop()
  }
  
  
  # copy over dimensions ------
  dimslots<-slotNames(OM)[1:17]
  for(i in 1:17)slot(.Object,dimslots[i])<-slot(OM,dimslots[i])
  
  cat("Constructing arrays")
  cat("\n")
  flush.console()  
  
  # Dimensions  S P A Y M R
  nsim<-OM@nsim
  npop<-OM@npop
  nyears<-OM@nyears
  proyears<-OM@proyears
  nages<-OM@nages
  nsubyears<-OM@nsubyears
  nareas<-OM@nareas
  nfleets<-OM@nfleets
  .Object@nfleets<-nfleets
  targpop<-as.integer(OM@targpop)
  .Object@targpop<-targpop
  allyears<-nyears+proyears
  nlen<-OM@nlen
  lenbins<-OM@lenbins
  mulen<-OM@mulen
  Wt_age<-OM@Wt_age
  nZeq<-OM@nZeq
  nydist<-OM@nydist
  nyeq<-OM@nyeq
  
  
  # Define arrays -----------------------------------------------------------
  
  # Management variables
  # !!!! This a temporary fix for simulation testing- keep maturity constant
  ind2<-ind<-TEG(dim(OM@mat))
  ind2[,4]<-1
  OM@mat[ind]<-OM@mat[ind2]
  OM@Wt_age[ind]<-OM@Wt_age[ind]
  OM@Mmov<-OM@mov
  OM@Recdevs[,,1]<-1
  
  # Run historical simulation ----------------------------------------------
  M<-OM@M
  Mtemp<-array(0,dim(OM@M))
  Mtemp[,,2:nages,]<-OM@M[,,1:(nages-1),]
  
  surv=tomt(exp(-apply(Mtemp[,,,1],2:1,cumsum)))
  surv[,,nages]<-surv[,,nages]*exp(-M[,,nages,1])/(1-exp(-M[,,nages,1]))
  
  N<-SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  SSBA<-array(NA,c(nsim,npop,allyears))
  FD<-array(NA,c(nsim,nfleets,allyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  FM<-VB<-C<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  CA<-array(NA,c(nsim,npop,allyears,nsubyears,nareas))
  
  mref<-c(2:nsubyears,1)  # movement reference
  y<-1
  m<-1
  
  # need to remake all these for OM renewal
  RFL<-array(NA,c(nsim,nfleets,nlen,nyears,nsubyears,nareas))
  indL<-TEG(dim(RFL))
  indE<-indL[,c(1,2,4,5,6)]
  RFL[indL]<-OM@q[indL[,c(1,2)]]*OM@sel[indL[,1:3]]*OM@E[indE]
  
  #got to here! translate RFL (fishing mort by length to fishing mort by age)
  Ftrans<-array(0,c(nsim,nfleets,nyears,nsubyears,nages,nlen,nareas,npop))
  Find<-TEG(dim(Ftrans))
  
  Lind<-Find[,c(1,2,6,3,4,7)] # s f l y m r
  Ftrans[Find]<-OM@iALK[Find[,c(1,8,3,5,6)]]*RFL[Lind]
  RF<-apply(Ftrans,c(1,8,5,3,4,7,2),sum) # s p a y m r f
  
  maxRF<-apply(RF,c(1,2,4,5,6,7),max) # s p y r f 
  
  Rind<-TEG(c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))#as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nareas,1:nfleets))
  sel<-RF
  sel[Rind]<-sel[Rind]/maxRF[Rind[,c(1,2,4,5,6,7)]]
  sel<-sel[,,,nyears,nsubyears,,] # s p a r f # Take this from last year, in future simulations this may be by year so leave this code!
  
  SFAYMR<-as.matrix(expand.grid(1:nsim, 1:nfleets,1:nages,y,m,1:nareas)) # Set up some array indexes
  SFAY<-SFAYMR[,1:4]
  
  SPAYMR<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas))     # Set up some array indexes
  SARP<-SPAYMR[,c(1,3,6,2)]
  SPA<-SPAYMR[,1:3]
  SPR<-SPAYMR[,c(1,2,6)]
  SPMR<-SPAYMR[,c(1,2,5,6)]
  SP<-SPAYMR[,1:2]
  SA<-SPAYMR[,c(1,3)]
  SAR<-SPAYMR[,c(1,3,6)]
  SPAR<-SPAYMR[,c(1:3,6)]
  SPAY<-SPAYMR[,1:4]
  SPAM<-SPAYMR[,c(1:3,5)]
  
  # New model initialization ------------ pay         paymrf
  R0<- OM@R0 
  h<-OM@h
  mat<-OM@mat
  mov<-OM@mov
  Zeq<-array(apply(M[,,,1:nZeq],1:3,mean),c(nsim,npop,nages,nsubyears,nareas))/nsubyears+apply(apply(RF[,,,1:nZeq,,,],1:6,sum),c(1,2,3,5,6),mean)
  SSB0<-apply(surv*array(R0,dim(surv))*Wt_age[,,,1]*mat[,,,1],1:2,sum)
  SSBpR<-SSB0/R0 
  
  stemp<-array(1/nareas,dim=c(nsim,npop,nsubyears,nareas))
  movi<-mov[,,nages,,,]
  
  for(y in 1:nydist){
    
    for(m in 1:nsubyears){
      
      if(m==1){
        
        stemp[,,m,]<-apply(array(rep(stemp[,,nsubyears,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)  
        
      }else{
        
        stemp[,,m,]<-apply(array(rep(stemp[,,m-1,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)  
        
      }     
      
    }
    
  }
  
  indN<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1,nsubyears,1:nareas))#
  N[indN]=R0[indN[,1:2]]*surv[indN[,1:3]]*stemp[indN[,c(1,2,5,6)]]
  
  SSB[,,,1,nsubyears,]<-N[,,,nyears,nsubyears,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)
  
  for(y in 1:nyeq){
    
    for(m in 1:nsubyears){
      
      if(m==1){ # first subyear
        
        N[,,,1,m,]<-exp(-Zeq[,,,nsubyears,])*N[,,,1,nsubyears,]
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)
        
        
      }else if(m==2){ # spawning subyear
        
        N[,,,1,m,]<-exp(-Zeq[,,,m-1,])*N[,,,1,m-1,]
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)
        spawnr<-apply(SSB[,,,1,m,],c(1,2,4),sum)/array(apply(SSB[,,,1,m,],1:2,sum),dim(SSB)[c(1,2,6)])
        SSBt<-apply(SSB[,,,1,m,],1:2,sum)
        N[,,nages,1,m,]<-N[,,nages,1,m,]+N[,,nages-1,1,m,] # plus group
        N[,,2:(nages-1),1,m,]<-N[,,1:(nages-2),1,m,]
        N[,,1,1,m,]<-spawnr*array(((0.8*R0*h*SSBt)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSBt)),dim(spawnr)) 
        #print(sum(N[1,1,1,1,m,]))
        #SSBA[,,1]<-apply(N[,,,1,m,]*array(Wt_age[,,,1]*OM@mat[,,,nyears],dim=c(nsim,npop,nages,nareas)),1:2,sum)
        
        
      }else{   # after spawning subyear
        
        N[,,,1,m,]<-exp(-Zeq[,,,m-1,])*N[,,,1,m-1,]
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)
        
      } # End of if subyear
      
    }   # end of subyear
  }     # end of equlibrium calculation year nyeq
  
  
  bR<-log(5*h)/(0.8*SSB0)                                      # Ricker SR params
  aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params
  
  
  y<-1
  m<-1
  
  SPAYMRF2<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas,1:nfleets))
  SF2<-SPAYMRF2[,c(1,7)]
  SFA2<-SPAYMRF2[,c(1,7,3)]
  SFAR2<-SPAYMRF2[,c(1,7,3,6)]
  SPRFA2<-SPAYMRF2[,c(1,2,6,7,3)]
  SPFR2<-SPAYMRF2[,c(1,2,7,6)]
  SPAY2<-SPAYMRF2[,1:4]
  SFAR2<-SPAYMRF2[,c(1,7,3,6)]
  SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
  SPAYRF2<-SPAYMRF2[,c(1,2,3,4,6,7)]
  SPARF2<-SPAYMRF2[,c(1,2,3,6,7)]
  
  for(m in 1:nsubyears){
    
    SPAYMRF2[,5]<-m
    SPAYMR2<-SPAYMRF2[,1:6]
    SPAYMR[,5]<-m
    VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                    # Calculate vunerable biomass
    #FM[SPAYMRF2]<-RF[SPAYRF2]#*FD[FYMR2]
    Ftot<-apply(RF[,,,y,m,,],1:4,sum)
    Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears
    C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*RF[SPAYMRF2]/Z[SPAYMR2]  # need to add back in mortality rate before C calculation
    #C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*RF[SPAYMRF2]/Z[SPAYMR2]
    
  }
  
  SPAYMR[,5]<-1
  SPAYMRF2[,5]<-1
  SPAYMR2<-SPAYMRF2[,1:6]
  
  cat("Re-running historical simulations")
  cat("\n")
  
  for(y in 2:nyears){
    
    SPAYMR[,4]<-y
    SPAY<-SPAYMR[,1:4]
    SPAYMRF2[,4]<-y
    SPAYRF2[,4]<-y
    SPAY2<-SPAYMRF2[,1:4]
    SFAY2<-SPAYMRF2[,c(1,7,3,4)]
    SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
    SFAR2<-SPAYMRF2[,c(1,7,3,6)]
    
    for(m in 1:nsubyears){
      
      SPAYMR[,5]<-m
      SPAM<-SPAYMR[,c(1:3,5)]
      SPAYMRF2[,5]<-m
      SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
      SPAYMR2<-SPAYMRF2[,1:6]
      
      if(m==1){
        N[,,,y,m,]<-N[,,,y-1,nsubyears,]*exp(-Z[,,,y-1,nsubyears,])
      }else{
        N[,,,y,m,]<-N[,,,y,m-1,]*exp(-Z[,,,y,m-1,])
      }  
      
      # move fish
      N[,,,y,m,]<-domov(N[,,,y,m,],OM@mov[,,,m,,])
      
      
      VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                    # Calculate prop to vunerable biomass
      Ftot<-apply(RF[,,,y,m,,],1:4,sum)
      Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears
      
      # harvest fish
      #C[SPAYMRF2]<-N[SPAYMR2]*(exp(Z[SPAYMR2])-1)*RF[SPAYMRF2]/Z[SPAYMR2]
      C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*RF[SPAYMRF2]/Z[SPAYMR2]
      #  age individuals
      for(pp in 1:npop){
        if(OM@Recsubyr[pp]==m){
          
          # age fish
          SSBA[,pp,y]<-apply(N[,pp,,y-1,m,]*array(Wt_age[,pp,,nyears]*OM@mat[,pp,,nyears],dim=c(nsim,nages,nareas)),1,sum)
          SSBdist<-apply(N[,pp,,y-1,m,]*array(Wt_age[,pp,,nyears]*OM@mat[,pp,,nyears],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]
          
          N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,]
          N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]
          
          # recruit fish
          if(OM@SRrel[pp]==1){    # Beverton-Holt recruitment
            rec<-OM@Recdevs[,pp,y]*(0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)*SSBA[,pp,y]) 
          }else{              # Most transparent form of the Ricker uses alpha and beta params
            rec<-OM@Recdevs[,pp,y]*aR[,pp]*SSBA[,pp,y]*exp(-bR[,pp]*SSBA[,pp,y])
          }   
          
          N[,pp,1,y,m,]<-rec*SSBdist
           
          
        } # if its the right subyear
        
      } # end of pop
      SSB[,,,y,m,]<-N[,,,y,m,]*rep(Wt_age[,,,nyears],nareas)*rep(mat[,,,nyears],nareas)
      
    } # end of subyear
    
  } # end of year  
  
  Bcur<-apply(N[,,,nyears,nsubyears,]*
                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)
  
  #Bcur<-apply(N[ss,1,,nyears,nsubyears,]*
  #             array(Wt_age[ss,1,,nyears]*OM@mat[ss,1,,nyears],c(nages,nareas)),1:2,sum)
  
  #Bcur<-sum(array(N[targpop,,nyears,nsubyears,],c(length(targpop),nages,nareas))*
  #            array(Wt_age[targpop,,nyears]*mat[targpop,,nyears],c(length(targpop),nages,nareas)))
  
  
  SSBall<-N*array(Wt_age,dim(N))*array(OM@mat,dim(N))
  RAI<-apply(SSBall,c(1,4,5,6),sum)
  RAI<-RAI[,1:nyears,,]
  RAI<-RAI/array(apply(RAI,1,mean),dim(RAI))
  
  
  D<-Bcur/SSB0 # Check against OM@D (remember only targetpop is matched)
  
  # Generate observation errors ---------------------------------------------
  
  .Object@Cimp<-runif(nsim,Obs@Ccv[1],Obs@Ccv[2])
  .Object@Cb<-trlnorm(nsim,1,Obs@Cbcv)
  .Object@Cerr<-array(trlnorm(nsim*allyears,rep(.Object@Cb,allyears),rep(.Object@Cimp,allyears)),c(nsim,allyears)) 
  
  .Object@Iimp<-runif(nsim,Obs@Icv[1],Obs@Icv[2])
  .Object@Ierr<-array(trlnorm(nsim*allyears,1,rep(.Object@Iimp,allyears)),c(nsim,allyears)) 
  .Object@Ibeta<-exp(runif(nsim,log(Obs@Ibeta[1]),log(Obs@Ibeta[2])))
  
  .Object@Btimp<-runif(nsim,Obs@Btcv[1],Obs@Btcv[2])
  .Object@Btb<-trlnorm(nsim,1,Obs@Btbcv)
  .Object@Bterr<-array(trlnorm(nsim*allyears,rep(.Object@Btb,allyears),rep(.Object@Btimp,allyears)),c(nsim,allyears)) 
  
  .Object@Mb<-trlnorm(nsim,1,Obs@Mbcv)
  .Object@Kb<-trlnorm(nsim,1,Obs@Kbcv)
  .Object@Linfb<-trlnorm(nsim,1,Obs@Linfbcv)
  .Object@t0b<-rep(1,nsim)
  
  
  # Generate data ------------------------------------------------
  datfile<-paste(OMDir,"/M3.dat",sep="")
  cat("\n")
  cat("Generating data")
  cat("\n")
  
  
  #sof<-apply(array(OM@E[,,nyears]*OM@q,c(nsim,nfleets,nages))*sel,c(1,3),sum)
  #sof<-sof/apply(sof,1,max)
  SFAY1<-SFAY2
  Find<-as.matrix(expand.grid(1:nsim,1:npop,1:nareas,1:nfleets))[,c(1,2,4,3)]
  FindSF<-Find[,c(1,3)]
  FindSPR<-Find[,c(1,2,4)]
  SPFR3<-as.matrix(expand.grid(1:nsim,1:npop,1:nfleets,1:nareas))
  SPR3<-SPFR3[,c(1,2,4)]
  
  # Age-length key --
  #contour(OM@iALK[1,1,1,,])
  
  # Spawning --
  spawnr<-array(NA,c(nsim,npop,nareas))
  
  for(pp in 1:npop){
    m<-OM@Recsubyr[pp]
    spawnr[,pp,]<-apply(SSN[,pp,,1,m,]*array(Wt_age[,pp,,1],dim=c(nsim,nages,nareas)),c(1,3),sum)/SSBA[,pp,y]
  }
  
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nareas))
  sums<-apply(spawnr,1:2,sum)
  sind<-ind[,1:2]
  spawnr[ind]<-spawnr[ind]/sums[sind]
  
  # Fishery data -------------
  
  
  # Catch
  mult<-nyears*nsubyears*nareas*nfleets
  #Cerr<-array(trlnorm(nsim*mult,rep(.Object@Cb,mult),rep(.Object@Cimp,mult)),c(nsim,nyears,nsubyears,nareas,nfleets)) 
  Cerr<-array(trlnorm(nsim*mult,rep(1,mult),rep(.Object@Cimp,mult)),c(nsim,nyears,nsubyears,nareas,nfleets)) 
  Cobsta<-array(NA,c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nfleets))
  Cobsta[ind]<-C[ind]*Wt_age[ind[,1:4]]
  Cobst<-apply(Cobsta,c(1,4:7),sum,na.rm=T)*Cerr
  Cobsta<-apply(Cobsta,c(1,3:7),sum,na.rm=T)
  # CPUE
  Ierr<-array(trlnorm(nsim*mult,1,rep(.Object@Iimp,mult)),c(nsim,nyears,nsubyears,nareas,nfleets)) 
  Ibeta<-exp(runif(nsim,log(Obs@Ibeta[1]),log(Obs@Ibeta[2])))
  
  if(complexF==1){
    # SYMRF                             SPAY M R F2
    Iobst<-apply(VB[,,,1:nyears,,,],c(1, 4:7),sum)#^Ibeta
    Isum<-apply(Iobst,c(1,5),mean)
    ind<-as.matrix(expand.grid(1:nsim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
    #Iobst[ind]<-Ierr*(Iobst[ind]/Isum[ind[,c(1,5)]])
    Iobst[ind]<-Iobst[ind]/Isum[ind[,c(1,5)]]
  }else{
    #Iobst<-Ierr*apply(VB[,,,1:nyears,,,],c(1,4,5,6,7),sum)#^Ibeta
    apicalFage<-apply(OM@sel,1:2,which.max)
    Iobst<-array(NA,dim=c(nsim,nyears,nsubyears,nareas,nfleets))
    ind<-as.matrix(expand.grid(1:nsim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
    VBsum<-apply(VB[,,,1:nyears,,,],c(1,3:7),sum) # sum over pops
    VBind<-cbind(ind[,1],apicalFage[ind[,c(1,5)]],ind[,2:5]) # add apical age to VBindex
    #Iobst[ind]<--log(1-(Cobsta[VBind]/VBsum[VBind]))
    Iobst[ind]<-OM@E[ind[,c(1,5,2,3,4)]]#(OMd@nsim,OMd@nfleets,OMd@nyears,OMd@nsubyears,OMd@nareas))
    #Isum<-apply(Iobst,c(1,5),mean)
    #Iobst[ind]<-(Iobst[ind]/Isum[ind[,c(1,5)]])
  }
  
  debugR<-F
  if(debugR){
    simo<-1
    p<-1
    age<-13
    m<-1
    f<-1
    r<-1
    ys<-1:25
    
    test<-as.data.frame(cbind(Cobst[simo,ys,m,r,f],apply(VB[simo,,,ys,m,r,f],3,sum),Iobst[simo,ys,m,r,f],Cobst[simo,ys,m,r,f]/Iobst[simo,ys,m,r,f],FM[ss,p,8,ys,m,r,f],OM@E[ss,f,ys]))
    test<-test/rep(apply(test,2,mean),each=nrow(test))
    names(test)<-c("Cobs","VulnB","vBindex","Cobs/vBindex","FM","effort")
    test
  }
  
  
  # Length composition
  CALm<-array(NA,c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets,nlen))
  ind<-TEG(dim(CALm))
  ALKind<-ind[,c(1,2,4,3,8)]
  Cind<-ind[,1:7]
  CALm[ind]<-C[Cind]*OM@iALK[ALKind]
  
  
  # You were here simulating fishery independent SSB in the spawning area
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,OM@Recsubyr,1:nareas))
  SSBtemp<-array(NA,c(nsim,npop,nages,nyears,nareas))
  SSBtemp[ind[,c(1,2,3,4,6)]]<-N[ind]*Wt_age[ind[,1:4]]*OM@mat[ind[,1:4]]
  SSBtemp<-apply(SSBtemp,c(1,2,4,5),sum)
  SpawnA<-apply(SSBtemp,1:3,which.max)                         
  FIobst<-array(NA,c(nsim,npop,nyears))
  ind<-TEG(c(nsim,npop,nyears))
  FIobst[ind]<-SSBtemp[cbind(ind,SpawnA[ind])]
  meanFI<-apply(FIobst,1:2,mean)
  FIobst[ind]<-FIobst[ind]/meanFI[ind[,1:2]]
  FIerr<-array(trlnorm(nsim*mult,1,rep(.Object@Iimp,mult)),c(nsim,npop,nyears)) 
  FIobst<-FIobst*FIerr
 
  # Tagging data ---
  nRPT<-2 # maximum number of timesteps that a tag may be recaptured in (n subyears)
  temp<-rep(1:nsubyears,ceiling(nRPT/nsubyears)+nsubyears)
  RPTind<-array(NA,c(nsubyears,nRPT))
  for(ss in 1:nsubyears)RPTind[ss,]<-temp[ss:(ss+nRPT-1)]
 
  
  for(sim in 1:nsim){ # Now loop over simulations, create data and write M3 files for parallel processing
    
    simfolder<-paste(M3temp,sim,sep="")
    if(!file.exists(simfolder))dir.create(simfolder)
    
    file.copy(paste(OMDir,"/M3.exe",sep=""),simfolder,overwrite=T)
    file.copy(paste(OMDir,"/M3.pin",sep=""),simfolder,overwrite=T)
    datfile<-paste(simfolder,"/M3.dat",sep="")
    #datfile<-"G:/M3/M3.dat"
 # }
    
    #sim<-1
    print(sim)
    print(Sys.time())
    #datfile<-paste(OMDir,"/M3.dat",sep="")
    cat("\n")
    cat("Write data")
    cat("\n")
    
    
    # -- Model Dimensions --
    
    write("# ny number of years",datfile,1,append=F)
    write(nyears,datfile,1,append=T)
    
    write("# ns number of subyears",datfile,1,append=T)
    write(nsubyears,datfile,1,append=T)
    
    write("# np number of populations/stocks",datfile,1,append=T)
    write(npop,datfile,1,append=T)
    
    write("# na number of age classes",datfile,1,append=T)
    write(nages,datfile,1,append=T)
    
    write("# nr number of regions/areas",datfile,1,append=T)
    write(nareas,datfile,1,append=T)
    
    write("# nf number of fleets",datfile,1,append=T)
    write(nfleets,datfile,1,append=T)
    
    write("# nl number of length classes",datfile,1,append=T)
    write(nlen,datfile,1,append=T)
    
    write("# nRPT maximum number of time steps that a PSAT can be recaptured",datfile,1,append=T)
    write(nRPT,datfile,1,append=T)
    
    write("# RPtind correct subyear recapture index",datfile,1,append=T)
    write(t(RPTind),datfile,nRPT,append=T)
    
    write("# sdur the duration of the various subyears (sums to 1)",datfile,1,append=T)
    write(rep(1/nsubyears,nsubyears),datfile,nsubyears,append=T)
    
    write("# nZeq: number of years at the start of the model to calculate equilibrium Z from",datfile,1,append=T)
    write(nZeq,datfile,1,append=T)
    
    write("# nydist: number of years over which initial stock distribution is calculated (prior to spool up)",datfile,1,append=T)
    write(nydist,datfile,1,append=T)
    
    write("# nyeq: number of spool-up years over which the stock is subject to nZeq, used to define equilibrium conditions",datfile,1,append=T)
    write(nyeq,datfile,1,append=T)
    
    write("# ml the mean length of the length categories",datfile,1,append=T)
    write(mulen,datfile,nlen,append=T)
    
    yblock<-5
    RDblock<-rep(1:100,each=yblock)[1:nyears]
    
    write("# RDblock the RD parameter for each year",datfile,1,append=T)
    write(RDblock,datfile,nyears,append=T)
    
    write("# nRD the number of estimated recruitment strengths",datfile,1,append=T)
    if(complexRD==0)write(max(RDblock),datfile,nyears,append=T)
    if(complexRD==1)write(nyears,datfile,nyears,append=T)
    
    # -- Growth --
    
    write("# iALK the age-length key by population and year p y a l",datfile,1,append=T)
    write(tomt(OM@iALK[sim,,,,]),datfile,nlen,append=T)      
    
    write("# lwa weight-length parameter a w=al^ b",datfile,1,append=T)
    write(OM@a,datfile,npop,append=T)
    
    write("# lwa weight-length parameter b w=al^ b",datfile,1,append=T)
    write(OM@b,datfile,npop,append=T)
    
    write("# len_age (pay)",datfile,1,append=T)
    write(OM@Len_age[sim,,,1:nyears],datfile,nyears,append=T)
    
    write("# wt_age (pay)",datfile,1,append=T)
    write(OM@Wt_age[sim,,,1:nyears],datfile,nyears,append=T)
    
    
    
    # -- Maturity --
    
    write("# Fec, fecundity at age, SSB at age",datfile,1,append=T)
    write(t(Wt_age[sim,,,nyears]*OM@mat[sim,,,nyears]),datfile,nages,append=T)      
    
    write("# steep, steepness of the Bev-Holt SR relationship",datfile,1,append=T)
    write(OM@h[sim,],datfile,npop,append=T)
    
    
    # -- Spawning --
    
    write("# spawns, the subyear in which the stock spawns",datfile,1,append=T)
    write(OM@Recsubyr,datfile,npop,append=T)
    
    #write("# spawnr, the fracton of recruits in each area",datfile,1,append=T)
    #write(t(spawnr[sim,,]),datfile,nareas,append=T)
    
    
    # -- Natural Mortality rate --
    
    write("# Ma, instantaneous natural mortality rate at age",datfile,1,append=T)
    write(t(M[sim,,,1]),datfile,nages,append=T)      
    
    
    # -- Fishery data --
    
    # Catches / F init
    if(complexF==1){
      
      allobsbelow<-0.02 # level of catches at cumulative 2%
      Cobs_cutoff<- min(Cobst[order(as.vector(Cobst))][cumsum(Cobst[order(as.vector(Cobst))])/sum(Cobst)>allobsbelow])
      
      ind<-as.matrix(expand.grid(sim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
      cond<-Cobst[ind]>Cobs_cutoff
      nind<-ind[cond,]
      rat<-sum(Cobst[ind])/sum(Cobst[nind])
      Cobs<-cbind(nind[,2:5],Cobst[nind])
      Cobs[,5]<-Cobs[,5]*rat
      
    }else{
      
      ind<-as.matrix(expand.grid(sim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
      Cobs<-cbind(ind[,2:5],Cobst[ind])
      
    }  
    
    #plot(density(Cobst[nind]))
    #lines(density(Cobst[ind]),col='red')
    #legend('topright',legend=c(round(rat,4),nrow(Cobs)))
    
    write("# nCobs, the number of catch weight observations y s r f CW",datfile,1,append=T)
    write(nrow(Cobs),datfile,1,append=T)
    
    write("# Cobs, catch weight observations y s r f C(weight)",datfile,1,append=T)
    write(t(Cobs),datfile,5,append=T)
    
    # CPUE
    ind<-as.matrix(expand.grid(sim,1:nyears,1:nsubyears,1:nareas,1:nfleets))
    CPUEobs<-cbind(ind[,c(2:5,5)],Iobst[sim,,,,]) # fleet is index number
    
    write("# nCPUE, the number of CPUE series",datfile,1,append=T)
    write(nfleets,datfile,1,append=T) # in this simulation this is the same as the number of fleets
    
    write("# nCPUEobs, the number of CPUE observations y s r f CPUE(weight)",datfile,1,append=T)
    write(nrow(CPUEobs),datfile,1,append=T)
    
    write("# CPUEobs, CPUE observations y s r f CPUE(weight)",datfile,1,append=T)
    write(t(CPUEobs),datfile,6,append=T)
    
    # Length composition
    
    CALt<-CALm[sim,,,,,,,]  # p a y s m f l
    CALsum<-ceiling(apply(CALt,3:7,sum,na.rm=T))  # y s m f l
    #CALtot<-apply(CALsum)
    
    
    #par(mfrow=c(1,2))
    #plot(CALsum[2,1,2,1,]/max(CALsum[2,1,2,1,]))
    #lines(CALsum[2,1,2,2,]/max(CALsum[2,1,2,2,]),col='red')
    #plot(OM@sel[sim,1,])
    #lines(OM@sel[sim,2,],col='red')
    
    ind<-as.matrix(expand.grid(1:nyears,1:nsubyears,1:nareas,1:nfleets,1:nlen))
    cond<-CALsum>0
    CLobs<-cbind(ind[cond,],CALsum[cond])              
    #CLobs<-cbind(ind,CALsum[ind])
    sum(is.na(CLobs))
    
    
    write("# nCLobs, the number of catch-at-length observations y s r f l N",datfile,1,append=T)
    write(nrow(CLobs),datfile,1,append=T)
    
    write("# CLobs, catch-at-length observations y s r f l N",datfile,1,append=T)
    write(t(CLobs),datfile,6,append=T)
    
    # The real relative abundance index RAI (y, s, r) !!! need to change this to real values
    write("# RAI, Relative Abundance index r x s x y",datfile,1,append=T)
    write(RAI[sim,,,],datfile,nyears,append=T)
    
    # Fishery-independent indices y s r pp i type(biomass/ssb) index
    ind<-as.matrix(expand.grid(sim,1:npop,1:nyears))
    Iobs<-as.matrix(cbind(ind[,3],OM@Recsubyr[ind[,2]],SpawnA[ind],ind[,2],ind[,2],rep(2,nrow(ind)),FIobst[ind])) # type SSB 
    
    write("# nI, the number of fishery independent indices series",datfile,1,append=T)
    write(npop,datfile,1,append=T) # in this simulation this is the same as the number of populations
    
    write("# nIobs, the number of fishery independent observations y s r i type(biomass/ssb) index",datfile,1,append=T)
    write(nrow(Iobs),datfile,1,append=T)
    
    write("# Iobs, fishery independent observations y s r i type(biomass/ssb) index",datfile,1,append=T)
    write(t(Iobs),datfile,7,append=T)
    
    
    # PSAT tagging --
    nPSATs<-10000
    PSATdist<-apply(C[sim,,,,,,],c(1,2,4,5),sum,na.rm=T)^0.01
    PSATdist<-PSATdist/apply(PSATdist,1,sum)
    PSATdist<-ceiling(PSATdist/sum(PSATdist)*nPSATs)
    nPSATs<-sum(PSATdist)
    track<-array(NA,c(nPSATs,nRPT))
    sy<-rep(NA,nPSATs)
    SOO<-array(NA,c(nPSATs,npop))
    nT<-1+ceiling(runif(nPSATs)*(nRPT-1)) # nT is the number of timesteps for recapture, this is set to 2 here,nRPT is the maximum number of timesteps that a tag may be recaptured
    
    PSAT<-c(1,1,3,1,9,9)
    PSAT2<-c(1,1,1,1,1,0.05,0.95)
    
    j<-0
    mov<-OM@mov[sim,,,,,]
    
    for(pp in 1:npop){
      for(aa in 1:nages){
        for(ss in 1:nsubyears){
          for(rr in 1:nareas){
            if(PSATdist[pp,aa,ss,rr]>0){
              for(i in 1:PSATdist[pp,aa,ss,rr]){
                j<-j+1
                SOO[j,]<-apply(C[sim,,aa,ceiling(nyears*0.7),ss,rr,],1,sum)/sum(C[sim,,aa,ceiling(nyears*0.7),ss,rr,]) #SPAYMRF
                track[j,1]<-rr
                sy[j]<-ss
                
                #for(rpt in 2:nT[j]){
                rpt<-2
                m<-RPTind[ss,rpt]
                
                track[j,rpt]<-(1:nareas)[rmultinom(1,1,mov[pp,aa,mref[m],track[j,rpt-1],])==1] # 
                SOO[j,]<-SOO[j,]*apply(C[sim,,aa,ceiling(nyears*0.7),m,track[j,rpt],],1,sum)/sum(C[sim,,aa,ceiling(nyears*0.7),m,track[j,rpt],])
                #} # track length
                SOO[j,]<-SOO[j,]/sum(SOO[j,])
                #if(1%in%SOO[j,]){
                # for(rpt in 2:nT[j]){
                #m<-RPTind[ss,rpt]
                PSAT<-rbind(PSAT,c(pp,OM@ma[aa,pp],ss,2,track[j,(rpt-1):rpt]))
                #}
                #}else{
                # for(rpt in 2:nT[j]){
                #  #m<-RPTind[ss,rpt]
                #  PSAT2<-rbind(PSAT2,c(ss,2,track[j,(rpt-1):rpt],SOO[j,]))
                #}
                #}
                
              } # tags
            }
          } # areas pp
        } # ages
      } # subyears 
    } # pops
    
    PSAT<-PSAT[2:nrow(PSAT),]
    PSAT<-aggregate(rep(1,nrow(PSAT)),by=list(PSAT[,1],PSAT[,2],PSAT[,3],PSAT[,4],PSAT[,5],PSAT[,6]),sum)
    #testPSAT<-array(0,c(npop,OM@nma,nsubyears,nareas,nareas))
    #testPSAT[as.matrix(PSAT[,c(1,2,3,5,6)])]<-PSAT[,7]
    
    #PSAT2<-PSAT2[2:nrow(PSAT2),]
    
    write("# nPSAT, PSATs data of known stock of origin p a s t fr tr N",datfile,1,append=T)
    write(nrow(PSAT),datfile,1,append=T)
    
    write("# PSAT data of known stock of origin p a s t fr tr N",datfile,1,append=T)
    write(t(PSAT),datfile,7,append=T)
    
    write("# nPSAT2, PSATs data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
    write(1,datfile,1,append=T)
    #write(nrow(PSAT2),datfile,1,append=T)
    
    write("# PSAT2 data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
    write(t(PSAT2),datfile,5+npop,append=T)
    
    
    # Placeholder for conventional tags
    Tag<-array(c(2,1,1,1,2,2,1,1,1,1),c(1,10))
    
    write("# nTag, number of conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
    write(nrow(Tag),datfile,1,append=T)
    
    write("# Tag, conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
    write(t(Tag),datfile,10,append=T)
    
    
    # Stock of origin
    NSOO<-min(ceiling(nyears*nsubyears*nareas/2),500) # number of data points in time and space
    muSOO<-10  # mean number of observations at those points
    SOO<-apply(C[sim,,,1:nyears,,,],1:5,sum,na.rm=T)
    #Csum<-apply(SOO,2:4,sum)
    rat<-mean(SOO,na.rm=T)/muSOO
    SOO<-SOO/rat
    ind<-expand.grid(1:nages,1:nyears,1:nsubyears,1:nareas)[sample(1:(nages*nyears*nsubyears*nareas),NSOO),]
    ind<-as.matrix(cbind(rep(1:npop,rep=NSOO),ind[rep(1:NSOO,each=npop),]))
    SOOobs<-as.matrix(cbind(ind,SOO[ind]))
    SOOobs<-SOOobs[SOOobs[,6]>0,] # remove zeros 
    
    write("# nSOOobs, number of stock of origin observations p aa y s r N",datfile,1,append=T)
    write(nrow(SOOobs),datfile,1,append=T)
    
    write("# SOOobs, stock of origin observations p aa y s r N",datfile,1,append=T)
    write(t(SOOobs),datfile,6,append=T)
    
    
    # -- Selectivity controls
    write("# nsel, number of estimated selectivities",datfile,1,append=T)
    write(nfleets,datfile,1,append=T) # same as number of fleets
    
    write("# seltype, 2:logistic, 3:Thompson",datfile,1,append=T)
    write(c(2,3),datfile,nfleets,append=T) # first fleet is logistic
    
    write("# selind, which selectivity is assigned to each fleet",datfile,1,append=T)
    write(c(1,2),datfile,nfleets,append=T) # same as fleets
    
    write("# ratiolim, limits on logistic slope parameter relative to inflection point",datfile,1,append=T)
    write(c(0.1,1),datfile,nfleets,append=T) # same as fleets
    
    write("# infleclim, limits on model selectivity",datfile,1,append=T)
    write(c(4,15),datfile,nfleets,append=T) # same as fleets
    
    
    # -- Movement estimation
    
    mov<-array(NA,c(npop,OM@nma,nsubyears,nareas,nareas))
    #mov[as.matrix(PSAT[,c(1,2,4,5)])]<-1
    
    movind<-mov1<-c(1,1,1,1,1)
    maclassfind<-match(1:OM@nma,OM@ma[,1])
    mov<-OM@mov[sim,,maclassfind,,,] # p ma s fr tr
    mov[mov>0]<-1
    notanarea<-apply(mov,c(1,2,3,5),sum) # p ma s tr
    notanarea<-array(as.integer(notanarea>0),dim(notanarea))
    can<-apply(mov,c(1,5),sum)  # can a movement happen from this area for this stock?
    can<-array(as.integer(can>0),dim(can))
    
    ind<-TEG(dim(mov))
    mov[ind]<-mov[ind]*notanarea[ind[,c(1,2,3,4)]]
    
    for(pp in 1:npop){
      for(ma in 1:OM@nma){
        for(ss in 1:nsubyears){
          for(rr in 1:nareas){
            np<-sum(mov[pp,ma,ss,rr,],na.rm=T)
            if(np>0){
              fR<-match(1,mov[pp,ma,ss,rr,])
              mov1<-rbind(mov1,c(pp,ma,ss,rr,fR))
              if(np>1){
                oR<-grep(1,mov[pp,ma,ss,rr,])
                oR<-oR[oR!=fR]
                for(i in 1:length(oR)){
                  movind<-rbind(movind,c(pp,ma,ss,rr,oR[i])) 
                }   
              }  
            }  
          }
        }
      }
    }
    
    movind<-movind[2:nrow(movind),]
    mov1<-mov1[2:nrow(mov1),]
    
    
    if(movtype==1){ # if a gravity formulation these indices are for the to area that should be estimated by season
      
      firstr<-apply(can,1,which.max)
      mov1<-TEG(c(npop,OM@nma,nsubyears))
      mov1<-cbind(mov1,firstr[mov1[,1]],rep(999,nrow(mov1)))
      #mov1<-cbind(rep(1:npop,each=nsubyears),rep(1:nsubyears,npop),firstr[rep(1:npop,each=nsubyears)],rep(999,nsubyears*npop))
      can2<-can
      can2[cbind(1:npop,firstr)]<-0
      can2<-t(can2)
      nrest<-apply(can2,1,sum)
      indr<-array(1:nareas,c(nareas,npop))
      indp<-array(rep(1:npop,each=nareas),c(nareas,npop))
      rs<-indr[can2==1]
      ps<-indp[can2==1]
      movindo<-cbind(rep(ps,each=nsubyears),rep(1:nsubyears,length(rs)),rep(rs,each=nsubyears),rep(999,length(rs)*nsubyears))
      
      movind<-array(rep(movindo,each=OM@nma),dim=c(nrow(movindo)*OM@nma,4))
      movind<-cbind(movind[,1],rep(1:OM@nma,nrow(movindo)),movind[,2:4])
      
    }
    
    write("# nMP, number of estimated movement parameters",datfile,1,append=T)
    if(movtype==1)write(nrow(movind)+nsubyears*OM@nma*npop,datfile,1,append=T)
    if(movtype==2)write(nrow(movind),datfile,1,append=T)
    
    write("# nma, number of estimated movement age classes",datfile,1,append=T)
    write(OM@nma,datfile,1,append=T)
    
    write("# ma, assignment of age classes to age",datfile,1,append=T)
    write(OM@ma,datfile,nages,append=T)
    
    write("# nmovind, number of estimated movement parameters minus viscosity",datfile,1,append=T)
    write(nrow(movind),datfile,1,append=T)
    
    write("# movind, the location of estimated movement parameters p s r r",datfile,1,append=T)
    write(t(movind),datfile,5,append=T)
    
    write("# nmov1, number of initial non-estimated movement parameters",datfile,1,append=T)
    write(nrow(mov1),datfile,1,append=T)
    
    write("# mov1, the location of initial non-estimated movement parameters p s r r",datfile,1,append=T)
    write(t(mov1),datfile,5,append=T)
    
    write("# movtype, the type of movement parameterization 1: gravity 2:markov matrix",datfile,1,append=T)
    write(movtype,datfile,1,append=T)
    
    
    # -- Observation errors
    
    write("# CobsCV, lognormal CV of the observed catches",datfile,1,append=T)
    write(rep(0.2,nfleets),datfile,nfleets,append=T)
    
    write("# CPUEobsCV, lognormal CV of the CPUE indices",datfile,1,append=T)
    write(rep(0.2,nfleets),datfile,nfleets,append=T) # CPUE index for each fleet
    
    write("# IobsCV, lognormal CV of the fishery independent indices",datfile,1,append=T)
    write(rep(0.2,npop),datfile,npop,append=T) # SSB index for each population
    
    
    # -- Priors 
    
    write("# RDCV, lognormal penalty on recruitment deviations",datfile,1,append=T)
    write(2,datfile,1,append=T) # SSB index for each population
    
    write("# nLHw, number of likelihood weights",datfile,1,append=T)
    write(10,datfile,1,append=T) # SSB index for each population
    
    write("# LHw, likelihood weights (1 catch, 2 cpue, 3 FIindex,   4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel)",datfile,1,append=T)
    write(c(                          10,       1,     1/100000000, 1/1000,  1/10,  1/100,  1,       1,        1,      2),datfile,10,append=T) # SSB index for each population
    
    # -- Initial values
    write("# R0_ini, initial values for log R0",datfile,1,append=T)
    write(OM@R0[sim,],datfile,npop,append=T) # Simulated R0 for each population
    
    write("# sel_ini, initial values for selectivity",datfile,1,append=T)
    write(t(OM@sel[sim,,]),datfile,nlen,append=T) # Actual selectivity
    
    write("# selpar_ini, initial values for selectivity parameters",datfile,1,append=T)
    write(t(OM@selpars[sim,,]),datfile,3,append=T) # Actual selectivity
    
    #RFL<-array(NA,c(nsim,nfleets,nlen,nyears,nsubyears,nareas))
    Fsub<-array(NA,c(nyears,nsubyears,nareas,nfleets))
    indt<-as.matrix(expand.grid(sim,1:nfleets,1:nyears,1:nsubyears,1:nareas))
    Fsub[indt[,c(3,4,5,2)]]<-OM@E[indt]*OM@q[indt[,1:2]]
    
    # old-----
    #Fsum<-RF[sim,1,,,,,]
    #ind<-TEG(c(nyears,nsubyears,nareas,nfleets))
    #ind<-as.matrix(cbind(maxv[ind[,4]],ind[]))
    #Fsub<-array(Fsum[ind],c(nyears,nsubyears,nareas,nfleets))
    #-----
    
    if(complexF==0)lnFini<-log(as.vector(Fsub))
    if(complexF==1)lnFini<-log(Fsub[nind[,2:5]])
    
    write("# lnF_ini, initial values for log F",datfile,1,append=T)
    write(lnFini,datfile,nrow(Cobs),append=T) # log apical F
    
    write("# ilnRD_ini, initial recruitment deviations y=1 a=2:nages",datfile,1,append=T)
    write(array(0,c(nages-1,npop)),datfile,nages-1,append=T) # Initial recruitment deviations
    
    write("# lnRD_ini, initial recruitment deviations y=1:nyears",datfile,1,append=T)
    write(log(t(OM@Recdevs[sim,,1:nyears])),datfile,nyears,append=T) # Recruitment deviations
    
    write("# mov_ini, simulated movement p s a r r",datfile,1,append=T) # this is a pain: M3 is p s a r r, OM@mov is p a s r r (oh well)
    movt<-OM@mov[sim,,,,,] # p a s r r
    mov<-array(NA,dim(movt)[c(1,3,2,4,5)]) # p s a r r
    ind<-TEG(dim(movt))
    mov[ind[,c(1,3,2,4,5)]]<-movt[ind]
    write(tomt(mov),datfile,nareas,append=T) # Movement probabilities
    
    write("# qCPUE_ini, initial values for CPUE catchability nCPUE",datfile,1,append=T)
    
    if(complexF==1)write(log(1/Isum[sim,]),datfile,nfleets,append=T) # CPUE catchabilities I=qVB
    if(complexF==0){
      #apicalF<-apply(FM[sim,,,1:nyears,,,],c(1,3,4,5,6),max,na.rm=T)
      write(log(OM@q[sim,]),datfile,nfleets,append=T) # CPUE catchabilities I=qVB
    }
    
    write("# qI_ini, initial values for fishery independent catchability nI",datfile,1,append=T)
    write(log(rep(1,nfleets)),datfile,nfleets,append=T) # Catchabilities I=qSSB or I=qB
    
    write("# D_ini, simulated depletion SSB/SSB0",datfile,1,append=T)
    write(D[sim,],datfile,nfleets,append=T) # Catchabilities I=qSSB or I=qB
    
    write("# complexRD 1= run with full estimation of all recruitment deviations by year",datfile,1,append=T)
    write(complexRD,datfile,1,append=T) # debug switch
    
    write("# complexF 1= run with full estimation of all F's by year, subyear, fleet, region",datfile,1,append=T)
    write(complexF,datfile,1,append=T) # debug switch
    
    write("# nF either nCobs or 1 if complexF=0",datfile,1,append=T)
    if(complexF==0)write(1,datfile,1,append=T)
    if(complexF==1)write(nrow(Cobs),datfile,1,append=T)
    
    write("# debug 1= run with initial values",datfile,1,append=T)
    write(0,datfile,1,append=T) # debug switch
    
    write("# verbose 1= run with printouts",datfile,1,append=T)
    write(verbose,datfile,1,append=T) # debug switch
    
    write("# datacheck",datfile,1,append=T)
    write(991199,datfile,1,append=T) # datacheck
    
    #system(paste(OMDir,"M3.exe -est",sep="/"),wait=T,show.output.on.console = F) # run the exe
    
    #if(sim==1)pin_from_par(file=paste(OMDir,"/M3",sep=""))
    # Store results
    
    #out[[sim]]<-M3read(OMDir)
  }  
  
  
 
  spawnr=c(4,1)
  B0<-apply(N[,,,1,1,]*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),c(1:2,4),sum)
  B0<-B0/array(apply(B0,1:2,sum),dim(B0))
  
  Bind<-expand.grid(1:nsim,1:npop)
  Bfrac<-matrix(B0[as.matrix(cbind(Bind,spawnr[Bind[,2]]))],ncol=npop)
  
  SSB1<-apply(N[,,,1,1,]*
                  array(Wt_age[,,,1]*OM@mat[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)
  
  SSBcur<-apply(N[,,,nyears,nsubyears,]*
                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)
  
  Bcur<-apply(N[,,,nyears,nsubyears,]*
                  array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)
  
  Cobsta<-array(NA,c(nsim,npop,nages,nsubyears,nareas,nfleets))
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,nyears,1:nsubyears,1:nareas,1:nfleets))
  Cobsta[ind[,c(1:3,5:7)]]<-C[ind]*Wt_age[ind[,1:4]]
  Cobsta<-apply(Cobsta,c(1,2,6),sum)
  Ct<-apply(Cobsta,1:2,sum)
  Urat<-Cobsta/array(Ct,dim(Cobsta))
  
  U<-Ct/(apply(Bcur,1,sum)+Ct)
  B0t<-apply(N[,,,1,1,]*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),c(1:2),sum)

  ratB0<-B0t/apply(B0t,1,sum)
  ratBcur<-Bcur/apply(Bcur,1,sum)
  
  
  
  .Object@simlist<-list(SSB0=SSB0,D=SSBcur/SSB0,D1=SSBcur/SSB1,B0=B0t,Bfrac=Bfrac,Bcur=Bcur,Urat=Urat,U=U,ratB0=ratB0,ratBcur=ratBcur)
  
  
  #Bfracp<-t(sapply(1:nsim,getBfrac,out,spawnr=spawnr))
  #Bfracbias<-(Bfracp-Bfrac)/Bfrac
  
  # Bias in current depletion (SSB)
  #Dp<-t(sapply(1:nsim,getdep,out))
  #Dbias<-(Dp-D)/D
  
  # Bias in current SSB (absolute)
  #SSBp<-t(sapply(1:nsim,getSSBnow,out))
  #SSBbias<-(SSBp-Bcur)/Bcur
  
  #Perf<-data.frame(Dbias,SSBbias,Bfracbias)
  #names(Perf)<-c(paste("Dbias",1:npop,sep="_"),paste("SSBtbias",1:npop,sep="_"),paste("Bfracbias",1:npop,sep="_"))
  #.Object@Perf<-Perf
  
  #.Object@C[MP,,,]<-apply(C[,,,1:allyears,,,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets)),c(1,2,4),sum)
  #SSB<-apply(SSN[,,,1:allyears,4,]*array(Wt_age[,,,1:allyears],c(nsim,npop,nages,allyears,nareas)),c(1,2,4),sum)
  #.Object@D[MP,,,]<-SSB/apply(SSB0,1,sum)
  #B<-apply((SSN[,,,1:allyears,4,]+NSN[,,,1:allyears,4,])*array(Wt_age,c(nsim,npop,nages,allyears,nareas)),c(1:2,4),sum)
  #Bthen<-apply((SSN[,,,1,4,]+NSN[,,,1,4,])*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)
  #.Object@B_BMSY[MP,,]<-apply(array(B[,targpop,],dim=c(nsim,length(targpop),allyears)),c(1,3),sum)/OM@BMSY
  #U<-apply(array(.Object@C[MP,,targpop,],c(nsim,length(targpop),allyears)),c(1,3),sum)/
  #apply(array(VBA[,targpop,,1:allyears,4,],c(nsim,length(targpop),nages,allyears,nareas)),c(1,4),sum)
  #.Object@F_FMSY[MP,,]<-U/OM@UMSY
  
  #cat("\n")
  #  #.Object@MPs<-MPs
  
  .Object
  
})  


    
 


# Operating model definition object ---------------------------------------------------------------------------------------------------------------------
setClass("OMd",representation(
               
               # Description
               Name="character",Date="character",Author="character",
               Notes="character",PrimarySource="character",
               
               # Dimensions
               nsim="integer",npop="integer",nages="integer",             # MSE dimensions
               nyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
               proyears="integer",nlen="integer",lenbins="numeric",       # Projected years
               interval="integer",                                        # Update interval
               nma="integer",ma="array",                                  # Number of movement age classes, age class definitions
               
               # Parameter ranges / simulation sample distributions
               Magemu="array",Mrange="array",Msd="array",Mgrad="array",   # Mean natural mortality rate at age, sample range, interannual variability and gradient % yr-1
               SRrel="integer",h="array",recgrad="array",                 # Stock-recruitment relationship type, steepness, underlying gradient % yr-1
               Reccv="array",AC="array", Recsubyr="integer",              # CV of recruitment deviations and recruitment auto-correlation
               Linf="array",K="array",t0="numeric",                       # Mean growth parameters
               Ksd="array",Kgrad="array",Linfsd="array",Linfgrad="array", # Interannual variability in growth and mean trajectory % yr-1
               a="numeric",b="numeric",                                   # Weight - Length conversion W=aL^b 
               ageM="array",ageMsd="array",ageMgrad="array",              # Age-at-maturity, interannual variability and gradient % yr-1
               D="array",R0="array",                                      # Current stock depletion, abundance                                 
               Size_area="array",mov="array",                             # Size of regions, Markov movement matrix for all fish and mature fish
               movvar="numeric",movsd="array",movgrad="array",              # Inter-simulation variability in movement, interannual-variability in movement, gradient changes in area gravity weights
               excl="array",                                              # Exclusion matrix [0,1] depending on whether the stock can go there
               
               # Fleet specifications
               nfleets="integer",                                         # Number of fleets,
               L05="array",VmaxL="array", LFS="array",                    # Length at 5% vulnerability, vulnerability of largest fish, length at full selection                  
               Fsd="array",Fgrad="array", Frat="numeric",                 # Interannual variability in F, Final gradient in F yr-1
               Spat_targ="array",                                         # Spatial targetting parameter F =prop= V^Spat_targ
               Area_names="character", Area_defs="list",                  # Area definitions (polygons)
               targpop="numeric",                                         # The target population for calculation of MSY and depletion reference points
               #nZeq="integer",                                            # The number of initial years to calculation equilibrium F
               nydist="integer",                                          # The number of years (iterations) taken to find equilibrium spatial distribution
               #nyeq="integer",                                            # The number of years (iterations) taken to find equilibrium F
               
               # Observation properties relevant to trial specifications
               Cbias="numeric",
               
               # Misc
               seed="numeric"                                             # Random seed for the generation of the OM

))                                           
               
# Operating model definition object ---------------------------------------------------------------------------------------------------------------------
setClass("OM",representation(
              
              # Description
              Name="character",Date="character",Author="character",
              Notes="character",PrimarySource="character",
              
              # Dimensions
              nsim="integer",npop="integer",nages="integer",             # MSE dimensions
              nyears="integer",nHyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
              proyears="integer",nlen="integer",lenbins="numeric",       # Proyears
              interval="integer",                                        # Update interval
              nma="integer",ma="array",                                  # Number of movement age classes, age class definitions
              mulen="numeric",                                           # Mean length of each length bin
              
              # Sampled parameters 
              Magemu="array",Mrange="array",Msd="array",Mgrad="array",   # Mean natural mortality rate at age, interannual variability and gradient % yr-1
              Rectype="array",h="array",recgrad="array",                 # Stock-recruitment relationship type, steepness, underlying gradient % yr-1
              Recind="array",                                            # Proyear array determining which SR curve in what proyear nsim x nstock x proyears 
              Recpars="array",                                           # Parameters of various SR curves nsim x nstock x rectypes x 2
              Reccv="array",AC="array",                                  # CV of recruitment deviations and recruitment auto-correlation
              Recsubyr="integer",                                        # Sub-year in which recruitment occurs, area in which recruitment occurs
              Linfmu="array",Kmu="array",t0="array",                     # Mean growth parameters
              Ksd="array",Kgrad="array",Linfsd="array",Linfgrad="array", # Interannual variability in growth and mean trajectory % yr-1
              a="numeric",b="numeric",                                   # Weight - Length conversion W=aL^b 
              Len_age="array",Wt_age="array",                            # Growth 
              ageMmu="array",ageM="array",ageMsd="array",ageMgrad="array", # Age-at-maturity, interannual variability and gradient % yr-1
              D="array",SSBcur="array",                                  # Current stock depletion, abundance                                 
              Size_area="array",mov="array",Mmov="array",                # Size of regions, Markov movement matrix for all fish and mature fish
              movvar="matrix",movsd="array",movgrad="array",             # Inter-simulation variability in movement, interannual-variability in movement, gradient changes in area gravity weights
              excl="array",                                              # Exclusion matrix [0,1] depending on whether the stock can go there
              nfleets="integer",                                         # Number of fleets,
              L05="array",VmaxL="array", LFS="array",                    # Length at 5% vulnerability, vulnerability of largest fish, length at full selection                  
              iALK="array",                                              # Inverse Age-Length-Key
              Fsd="array",Fgrad="array", Frat="array",                   # Interannual variability in F, Final gradient in F yr-1
              Area_names="character", Area_defs="list",                  # Area definitions (polygons)
              Spat_targ="array",                                         # Spatial targetting parameter F =prop= V^Spat_targ
              hZ="array",
              HCobs="array",
              Cobs="array",
              
              # Simulation data -------
              E="array",dFfinal="array",
              qE="array",qI="array",qCPUE="array",            # Catchability of effort, fishery ind indices and fishery CPUE indices
              sel="array",selpars="array",
              mat="array",
              Recdevs="array", R0="array",  muR="array",      # Recruitment deviations, unfished recruitment, mean historical recruitment
              FM="array",M="array",Z="array",                 # Fishing, natural and total instantaneous mortality rate. 
              B="array",SSB="array",NSB="array",              # Biomass, spawning stock biomass
              N="array",SSN="array",NSN="array",              # Numbers, spawning stock numbers
              C="array",CAA="array",                          # Catches taken, catch at age taken
              MSY="array",BMSY="array", VBMSY="array",        # Maximum sustainable yield, biomass at maximum sustainable yield
              SSBMSY="array",
              FMSY="array",UMSY="array",FMSYa="array",         # Fishing mortality rate at maximum sustainable yield
              SSBMSY_SSB0="array",
              IMSY="numeric",                                  # Relative abundance index at maximum sustainable yield
              Linf="array",K="array",
              Idist="array",                                   # for plotting OM unfished spatial dist
              targpop="numeric",                               # What populations are to be used in MSY / depletion optimization
              #nZeq="integer",                                 # The number of initial years to calculation equilibrium F
              nydist="integer",                                # The number of years (iterations) taken to find equilibrium spatial distribution
              #nyeq="integer",                                 # The number of years (iterations) taken to find equilibrium F
              
              Snames="character",                              # Name of the stocks
              
              # Observation properties relevant to trial specifications
              Cbias="numeric",
              
              seed="numeric"                                   # Random seed from which this object was made
              ))
         
setMethod("initialize", "OM", function(.Object,OMd="C:/M3",nsim=32,proyears=30,seed=1,
                                       targpop=NA,Recruitment=NULL,Snames=c("East","West")){
  # .Object})
  # .Object<-new('OM',OMd="C:/M3")
  #sfExport(list=c("popdyn","domov2","TEG"))
 
  .Object@Snames=Snames
  
  if(class(OMd)=='OMd'){
  
    .Object@targpop<-OMd@targpop
    
    # copy over dimensions ------
    set.seed(OMd@seed)
    .Object@seed<-OMd@seed
    dimslots<-slotNames(OMd)[1:17]
    for(i in 1:17)slot(.Object,dimslots[i])<-slot(OMd,dimslots[i])
   
    .Object@nZeq=OMd@nZeq                           # The number of initial years to calculation equilibrium F
    .Object@nydist=OMd@nydist                       # The number of years (iterations) taken to find equilibrium spatial distribution
    .Object@nyeq=OMd@nyeq                           # The number of years (iterations) taken to find equilibrium F
    
    cat("Generating random variables / operating model parameters")
    cat("\n")
  
    # --- set up M -------
    Mmu<-array(rep(OMd@Magemu,each=OMd@nsim),c(OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears))
    Mrng<-array(runif(OMd@nsim*OMd@npop,rep(t(OMd@Mrange)[1,],each=OMd@nsim),rep(t(OMd@Mrange)[2,],each=OMd@nsim)),c(OMd@nsim,OMd@npop))
    out<-gettempvar2(array(1,c(OMd@npop,2)),OMd@Msd,OMd@Mgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
    gradi<-out[[4]]
    ind<-as.matrix(expand.grid(1:OMd@nsim,1:OMd@npop,1:OMd@nages,1:OMd@nyears+OMd@proyears))  
    ind2<-ind[,c(1,2,4)]
    ind3<-ind[,c(1,2)]
    M<-Mmu
    M[ind]<-Mmu[ind]*gradi[ind2]*Mrng[ind3]
    
    .Object@M<-M
    .Object@Msd<-out[[2]]
    .Object@Mgrad<-out[[3]]
    .Object@Mrange<-Mrng
    
    # ---- Stock-recruit relationships -------
    .Object@SRrel<-OMd@SRrel
    .Object@h<-gettempvar3(OMd@h,OMd@nsim,OMd@npop)
     out<-gettempvar2(array(1,c(OMd@npop,2)),OMd@Reccv,OMd@recgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
    .Object@Recdevs<-out[[4]]
    .Object@Reccv<-out[[2]]
    .Object@recgrad<-out[[3]]
    .Object@AC<-gettempvar3(OMd@AC,OMd@nsim,OMd@npop)
    .Object@Recsubyr<-OMd@Recsubyr
    .Object@t0<-OMd@t0
    out<-gettempvar2(OMd@K,OMd@Ksd,OMd@Kgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
    .Object@Kmu<-out[[1]]
    .Object@Ksd<-out[[2]]
    .Object@Kgrad<-out[[3]]
    .Object@K<-out[[4]]
    out<-gettempvar2(OMd@Linf,OMd@Linfsd,OMd@Linfgrad,OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears)
    .Object@Linfmu<-out[[1]]
    .Object@Linfsd<-out[[2]]
    .Object@Linfgrad<-out[[3]]
    .Object@Linf<-out[[4]]
    .Object@a<-OMd@a
    .Object@b<-OMd@b
    .Object@ageMmu<-array(NA,c(OMd@nsim,OMd@npop))
    .Object@ageMsd<-array(NA,c(OMd@nsim,OMd@npop))
    .Object@ageMgrad<-array(NA,c(OMd@nsim,OMd@npop))
    .Object@ageM<-array(NA,c(OMd@nsim,OMd@npop,OMd@nyears+OMd@proyears))
    .Object@mat<-array(NA,c(OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears))
    
    for(pp in 1:OMd@npop){
      
      .Object@ageMmu[,pp]<-runif(OMd@nsim,OMd@ageM[pp,1],OMd@ageM[pp,2])   # now predicted by a log-linear model
      .Object@ageMsd[,pp]<-runif(OMd@nsim,OMd@ageMsd[pp,1],OMd@ageMsd[pp,2])
      .Object@ageMgrad[,pp]<-runif(OMd@nsim,OMd@ageMgrad[pp,1],OMd@ageMgrad[pp,2])
      .Object@ageM[,pp,]<-gettempvar(.Object@ageMmu[,pp],rep(0.001,OMd@nsim),
                                               .Object@ageMgrad[,pp],OMd@nyears+OMd@proyears,OMd@nsim)   # creates a time series per simulation that has gradient grad and random normal walk wiht sigma
      
      for(yy in 1:(OMd@nyears+OMd@proyears)){     
        ageMarray<-array(.Object@ageM[,pp,yy],dim=c(OMd@nsim,OMd@nages)) # Age at maturity array
        Agearray<-array(rep(1:OMd@nages,each=OMd@nsim),dim=c(OMd@nsim,OMd@nages))   # Age array
        .Object@mat[,pp,,yy]<-1/(1+exp((ageMarray-(Agearray))/(ageMarray*.Object@ageMsd[,pp])))  # Maturity at age array
      }
      
    }
    
    .Object@D<-gettempvar3(OMd@D,OMd@nsim,OMd@npop)
    .Object@Size_area<-OMd@Size_area
    
    selpars<-array(NA,c(OMd@nsim,OMd@nfleets,3))
    sel<-array(NA,c(OMd@nsim,OMd@nfleets,OMd@nlen))
    L05<-array(NA,c(OMd@nsim,OMd@nfleets))
    LFS<-array(NA,c(OMd@nsim,OMd@nfleets))
    VmaxL<-array(NA,c(OMd@nsim,OMd@nfleets))
    mulen<-(OMd@lenbins[1:OMd@nlen]+OMd@lenbins[2:(OMd@nlen+1)])/2
    mulena<-array(rep(mulen,each=OMd@nsim),c(OMd@nsim,OMd@nlen))
    
    #par(mfrow=c(4,4),mai=c(0.1,0.1,0.01,0.01))
    for(ff in 1:OMd@nfleets){
      
      L05[,ff]<-runif(OMd@nsim,OMd@L05[ff,1],OMd@L05[ff,2])
      L05[,ff]<-apply((mulena-L05[,ff])^2,1,which.min)
      LFS[,ff]<-runif(OMd@nsim,OMd@LFS[ff,1],OMd@LFS[ff,2])
      LFS[,ff]<-apply((mulena-LFS[,ff])^2,1,which.min)
      VmaxL[,ff]<-runif(OMd@nsim,OMd@VmaxL[ff,1],OMd@VmaxL[ff,2])
      seltemp<-fitThompson(LFS=LFS[,ff],L05=L05[,ff],VmaxL=VmaxL[,ff],nlen=OMd@nlen,nsim=OMd@nsim,ploty=F)
      sel[,ff,]<-seltemp[[1]]
      selpars[,ff,]<-seltemp[[2]]
        #getDNvulnS_L(LFS[,ff],L05[,ff],VmaxL[,ff],OMd@nlen,OMd@nsim,OMd@lenbins)
    }
  
    .Object@nfleets<-OMd@nfleets
    .Object@L05<-L05
    .Object@LFS<-LFS
    .Object@VmaxL<-VmaxL
    .Object@sel<-sel
    .Object@selpars<-selpars
    .Object@mulen<-mulen
    
    agearray<-array(rep(1:OMd@nages,each=OMd@npop*OMd@nsim),c(OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears))
    Len_age<-Wt_age<-array(NA,c(OMd@nsim,OMd@npop,OMd@nages,OMd@nyears+OMd@proyears))
    ind<-as.matrix(expand.grid(1:OMd@nsim,1:OMd@npop,1:OMd@nages,1:(OMd@nyears+OMd@proyears)))
    Len_age[ind]<-.Object@Linf[ind[,c(1,2,4)]]*(1-exp(-.Object@K[ind[,c(1,2,4)]]*(agearray[ind]-.Object@t0[ind[,2]])))
    .Object@Len_age<-Len_age
    Wt_age[ind]<-OMd@a[ind[,2]]*Len_age[ind]^OMd@b[ind[,2]]
    .Object@Wt_age<-Wt_age
    
    # Age-length key --
    LenCV<-0.1
    iALK<-array(NA,c(OMd@nsim,OMd@npop,OMd@nyears,OMd@nages,OMd@nlen))
    ind<-as.matrix(expand.grid(1:OMd@nsim,1:OMd@npop,1:OMd@nyears,1:OMd@nages,1:OMd@nlen))
    Lind<-ind[,c(1,2,4,3)]
    iALK[ind]<-dnorm(mulen[ind[,5]],Len_age[Lind],Len_age[Lind]*LenCV)
    sums<-apply(iALK,1:4,sum)
    sind<-ind[,1:4]
    iALK<-iALK/sums[sind]
    .Object@iALK<-iALK
 
    .Object@Fsd=gettempvar3(OMd@Fsd,OMd@nsim,OMd@npop)
  
    # Interannual variability in F, Final gradient in F yr-1
    .Object@E<-array(NA,c(OMd@nsim,OMd@nfleets,OMd@nyears,OMd@nsubyears,OMd@nareas))
    E<-array(NA,c(OMd@nsim,OMd@nfleets,OMd@nyears,OMd@nareas)) # in this simulation effort is constant over subyears in each year, by fleet and area
    .Object@dFfinal<-array(NA,c(OMd@nsim,OMd@nfleets))   
      
    for(ff in 1:OMd@nfleets){    
      
      deriv<-getFhist(.Object@nsim,.Object@Fsd[,ff],OMd@nyears,dFmin=OMd@Fgrad[ff,1],dFmax=OMd@Fgrad[ff,2],bb=2)     # Calculate fishing mortality rate
      E[,ff,,]<-rep(deriv[[1]],OMd@nareas)*exp(rnorm(OMd@nsim*OMd@nyears*OMd@nareas,0,0.3))    # Calculate fishing mortality rate
      .Object@dFfinal[,ff]<-deriv[[2]]  # Final gradient in F yr-1 
    
    }
    
    ind<-TEG(dim(.Object@E))
    indE<-ind[,c(1,2,3,5)]
    .Object@E[ind]<-E[indE] # s f y m r
    .Object@Frat<-array(rep(OMd@Frat,each=OMd@nsim),c(OMd@nsim,OMd@nfleets))
    .Object@Spat_targ<-gettempvar3(OMd@Spat_targ,OMd@nsim,OMd@nfleets)       # Spatial targetting parameter F =prop= V^Spat_targ
    .Object@mov<-simmov4(OMd)   # simmov3 just copies movement into each simulation
    .Object@excl<-OMd@excl  
    .Object@Area_names<-OMd@Area_names
    .Object@Area_defs<-OMd@Area_defs
    
    # Set up arrays ---------------
    nsim<-OMd@nsim
    npop<-OMd@npop
    nages<-OMd@nages
    nyears<-OMd@nyears
    nsubyears<-OMd@nsubyears
    nareas<-OMd@nareas
    nfleets<-OMd@nfleets
    mdim<-c(nsim,npop,nages,nyears,nsubyears,nareas) 
    fdim<-c(nsim,npop,nfleets,nages,nyears,nsubyears,nareas)
    .Object@SSB<-.Object@NSB<-.Object@SSN<-.Object@NSN<-.Object@N<-.Object@B<-array(NA,mdim)
    .Object@CAA<-.Object@FM<-array(NA,fdim)
    .Object@R0<-gettempvar3(OMd@R0,OMd@nsim,OMd@npop)
    .Object@Recsubyr<-OMd@Recsubyr
     
    .Object@targpop<-OMd@targpop
  
    cat("Optimizing for user-specified depletion")
    cat("\n")
    
    qopt<-t(sfSapply(1:nsim,getF4dep,.Object,toly=0.05))#test<-cbind(qopt[,2],apply(.Object@D,1,mean))
    #out<-sapply(1,getF4dep,.Object)
    # qopt returns objective function, sim depletion, target depletion, ratio of F fleet 1, ratio of F fleet 2
    totF<-exp(qopt[,1])
    relF<-matrix(qopt[,4:(1+nfleets)],ncol=nfleets-1)
    relF<-cbind(relF,1-apply(relF,1,sum))
    .Object@q<-totF*relF # right now F is distributed evenly over subyears
    
  }else if(dir.exists(OMd)){
     
    if(!file.exists(paste0(OMd,"/M3.dat"))){
      print(paste('Could not build operating model: M3 output file ',paste0(OMd,"/M3.dat"),' not found'))
      stop()
    }  
    
    #if(!file.exists(paste0(OMd,"/OMI"))){
      #print(paste('Could not build operating model: Operating model input object ',paste0(OMd,"/OMI"),' not found'))
      
      #stop()
    #}
    
    cat(paste("Loading operating model input object :",OMd))
    cat("\n")#.Object@targpop=targpop
    load(file=paste0(OMd,"/OMI"))
    
    cat(paste("Reading operating model fit data from directory:",OMd))
    cat("\n")#.Object@targpop=targpop
    out<-M3read(OMd)
    
    set.seed(seed)
    .Object@seed<-seed
    
    .Object@Name=paste("From",OMd)
    .Object@Date=as.character(Sys.time())
    .Object@Author="NA"
    .Object@Notes="Made from an empirical fit of the M3 model"
    .Object@PrimarySource="NA"
    
    # Dimensions
    
    npop<-np<-out$np
    nages<-na<-out$na
    nyears<-out$ny
    nHyears<-out$nHy
    nsubyears<-out$ns
    nareas<-out$nr
    nlen<-out$nl
    nfleets<-out$nf
    nma<-out$nma
    
    .Object@nsim<-as.integer(nsim)
    .Object@npop<-as.integer(out$np)
    .Object@nages<-as.integer(out$na)
    .Object@nyears<-as.integer(out$ny)
    .Object@nHyears<-as.integer(out$nHy)
    .Object@nsubyears<-as.integer(out$ns)
    .Object@nareas<-as.integer(out$nr)
    .Object@proyears<-as.integer(proyears)
    .Object@nlen<-as.integer(out$nl)
    .Object@nma<-as.integer(out$nma)
    .Object@ma<-ma<-array(rep(c(rep(1,4),rep(2,4),rep(3,na-8)),each=2),c(np,na)) # for some reason this doesn't yet exist. 
    
    .Object@Wt_age<-out$wt_age
    .Object@Len_age<-out$len_age
    
    .Object@Kmu<-.Object@t0<-.Object@Linfmu<-array(0,c(nsim,npop))
    .Object@a<-.Object@b<-rep(0,npop)
    
    for(pp in 1:npop){
      
      opt<-calcVBpars(.Object@Len_age[1,,pp],plot=T)
      .Object@Kmu[,pp]<-opt$K
      .Object@t0[,pp]<-opt$t0
      .Object@Linfmu[,pp]<-opt$Linf
      
      opt<-calcABpars(La=.Object@Len_age[1,,pp],Wa=.Object@Wt_age[1,,pp],plot=T)
      .Object@a[pp]<-opt$a
      .Object@b[pp]<-opt$b
      
    }
    
    .Object@Cobs<-array(0,c(nyears,nsubyears,nareas,nfleets))
    .Object@Cobs[out$Cobs[,1:4]]<-out$Cobs[,5]
    
    .Object@lenbins<-rep(0,out$nl+1)
    .Object@mulen<-out$ml
    .Object@nfleets<-as.integer(out$nf)
    .Object@M<-array(rep(out$M_age,each=nsim),c(nsim,npop,nages,nyears+proyears))
    
    .Object@hZ<-out$hZ
    .Object@HCobs<-out$HCobs
    
    # ---- Stock-recruit relationships -------
    
    if(is.null(Recruitment)){
      
      Recruitment<-list(
        
        proyears=array(c(1,Inf),c(1,2)),                         # Future recruitment follows just a single curve per stock
        years=array(c(-Inf,Inf),c(2,np,1)),                      # historical time period, 
        type=array('BH',c(1,np)),                                # future recuitment follows just a single curve per stock
        h=array(NA,c(1,np)),                                     # future recruitment follows just a single curve per stock
        prob=array(1,c(1,np))                                    # Probability of flipping recruitment scenarios
        
      )
      
    }
   
    .Object@Rectype<-Recruitment$type                                            # time period x stock
    ntypes<-nrow(Recruitment$type)
    yvec<-rep(1,proyears)
    if(ntypes>1)for(i in 2:ntypes)yvec[Recruitment$proyears[i,1]:min(Recruitment$proyears[i,2],proyears)]<-i
    .Object@Recind<-array(rep(yvec,each=nsim),c(nsim,proyears))
    .Object@Recpars<-array(NA,c(nsim,np,ntypes,2))
    .Object@Reccv<-array(NA,c(nsim,np))         # sigma R (sd of lognormal rec devs)
    .Object@AC<-array(NA,c(nsim,np))
    
    for(pp in 1:np){
      for(tt in 1:ntypes){
        type=strsplit(Recruitment$type[tt,pp],split="_")[[1]][1]
        just_R0<-grepl("R0",Recruitment$type[tt,pp])
        opt<-SRopt(out,plot=T,quiet=F,years=Recruitment$years[,pp,tt],
                   type=type,just_R0=just_R0,h=Recruitment$fixpar[tt,pp])
        
        if(just_R0){ # only second parameter is subject to error
          .Object@Recpars[,pp,tt,1]<-Recruitment$fixpar[tt,pp] # either logit h or logit inflection
          .Object@Recpars[,pp,tt,2]<-exp(rnorm(nsim,opt$lnR0[pp],opt$VC[[pp]]^0.5)) # always lnR0
        }else{
          .Object@Recpars[,pp,tt,]<-rmvnorm(nsim,mean=c(opt$par1[pp],opt$lnR0[pp]),sigma=opt$VC[[pp]])
          if(type=="BH").Object@Recpars[,pp,tt,1]<-0.2+1/(1+exp(-.Object@Recpars[,pp,tt,1]))*0.8
          if(type=="HS").Object@Recpars[,pp,tt,1]<-1/(1+exp(-.Object@Recpars[,pp,tt,1]))
          .Object@Recpars[,pp,tt,2]<-exp(.Object@Recpars[,pp,tt,2])
        }
      
        if(tt==1){ # some properties of recruitment deviations
          recpred<-opt$resid[[pp]]$rec-opt$resid[[pp]]$devs
          .Object@Reccv[,pp]<-sd(opt$resid[[pp]]$devs/recpred)         # sigma R (sd of lognormal rec devs)
          .Object@AC[,pp]<-acf(opt$resid[[pp]]$devs,plot=F)$acf[2,1,1] # lag 1 autocorrelation
        }
        
      } # end of types per stock
    }   # end of stocks
    
   
    # ---- Get covariance matrix and sample correlated parameters -------
    
    vcv<-read.fit(paste(OMd))
    #if(is.na(vcv$cov[1,1])){ # no valid variance-covariance matrix from which to sample parameter values
      print(paste('You specified a directory',OMd,'that does not contain a valid M3.cor file. The M3.cor file is generated when the convergence
                  criterion of a positive definite hessian, is met.'))
      samps<-as.data.frame(matrix(rep(vcv$est[1:vcv$nopar],each=nsim)+rnorm(vcv$nopar*nsim,0,0.1),nrow=nsim))
      names(samps)<-vcv$names[1:vcv$nopar]
    #}else{
    #  samps<-as.data.frame(mvrnorm(nsim,vcv$est,vcv$cov))
    #  names(samps)<-vcv$names
    #}
    
      
    # you were here and have no idea what this is all about: !  
    # ---- Get recruitment deviations -----
    Rdind<-grep("lnRD",vcv$names)
    nrest<-length(Rdind)/npop
    Rdest<-t(exp(matrix(vcv$est[Rdind],nrow=nrest)))
    Rdest<-Rdest/array(apply(Rdest,1,mean),dim(Rdest))
    test<-samps[,Rdind]
    Rdsamps<-exp(array(as.matrix(samps[,Rdind]),c(nsim,nrest,npop)))#nsim nrest
    Rdmu<-apply(Rdsamps,c(1,3),mean)
    ind<-TEG(dim(Rdsamps))
    indmu<-ind[,c(1,3)]
    Rdsamps[ind]<-Rdsamps[ind]/Rdmu[indmu]
  
    #out$RDblock<-rep(1:11,each=5)
    ind<-TEG(c(nsim,npop,nyears))
    indest<-cbind(ind[,1],out$RDblock[ind[,3]],ind[,2])
    
    .Object@Recdevs<-array(NA,dim=c(nsim,npop,nyears))
    .Object@Recdevs[ind]<-Rdsamps[indest]
    .Object@Reccv<-apply(Rdsamps,c(1,3),sd)*sqrt(nyears/nrest) # convert from std err to st dev 
    .Object@Recsubyr<-as.integer(out$spawns)
    
    .Object@muR<-as.matrix(exp(samps[,grep("lnmuR",vcv$names)]))
    .Object@D<-array(NA,dim(.Object@R0)) # don't need this for empirical fit
    .Object@mat<-array(rep(out$mat_age,each=nsim),c(nsim,npop,nages,nyears))
   
    .Object@ageM<-array(rep(apply((out$mat_age-0.51)^2+rep(1:out$na,each=out$np)/10000,1,which.min),each=nsim),c(nsim,npop))
    
    # calcmovements 
    movs<-samps[,grep("movest",vcv$names)]
    # movs[1,]<-vcv$est[grep("movest",vcv$names)] 
    # used in MSE with indexing spaymrr but think we dropped age varying movement so spamrr
    # send to get MSYrefs as mov=.Object@mov[ss,,,,,]
    # goes to domov2 as mov[,,m,,]
    # intepretted by domov2 as parr
    # points to OM@mov[sim,,,m,,]
    
    mov<-array(NA, c(nsim,npop,nages,nsubyears,nareas,nareas)) # spamrr
    movcalc<-array(-10,c(nsim,npop,nma,nsubyears,nareas,nareas))
    for(ss in 1:nsim){
      
      if(out$movtype==1){   # gravity model
        # areas that gravities are fixed to zero for (gravities are applied by row so column 4 is now 'to area')
        # p ma s tr
        ind<-cbind(rep(ss,nareas*out$nmov1),  # sim
                   rep(out$mov1[,1],each=nareas), # pop
                   rep(out$mov1[,2],each=nareas), # age class
                   rep(out$mov1[,3],each=nareas), # subyear
                   rep(1:nareas,out$nmov1),       # from area
                   rep(out$mov1[,4],each=nareas)) # to area
        movcalc[ind]<-0
        
        refind<-cbind(rep(ss,out$nmovind*nareas),rep((1:out$nmovind),each=nareas)+npop*nsubyears*nma)
        ind<-cbind(rep(ss,nareas*out$nmovind),
                   rep(out$movind[,1],each=nareas),
                   rep(out$movind[,2],each=nareas),
                   rep(out$movind[,3],each=nareas),
                   rep(1:nareas,out$nmovind),
                   rep(out$movind[,4],each=nareas))
        movcalc[ind]<-movs[refind]
        
        ind<-TEG(c(nareas,nsubyears,nma,npop))[,c(4:1,1)]
        ind0<-TEG(c(nsubyears,nma,npop))[,3:1]
        refno<-tomt(array(1:(nma*nsubyears*npop),c(nsubyears,nma,npop)))
        
        refind<-cbind(rep(ss,nrow(ind)),refno[ind[,1:3]])
        ind<-cbind(rep(ss,nrow(ind)),ind)
        movcalc[ind]<-movcalc[ind]+exp(movs[refind]/12)
        
        
      }else{            # full markov matrix
        
        ind<-cbind(rep(ss,out$nmov1), out$mov1[,1:4])
        movcalc[ind]<-0
        ind<-cbind(rep(ss,out$nmovind),out$movind[,1:4])
        movcalc[ind]<-movs[ss,]
          
      }
   
    }    # loop over sims
    
    movcalc[movcalc>5]<-5 # set some kind of upper bound for sampled movement parameters (or else get Infs)
     #nsim,npop,nsubyears,nareas,nareas
    movcalc<-exp(movcalc)
    movsum<-apply(movcalc,1:5,sum)
    movcalc<-movcalc/array(movsum,dim(movcalc))
    #nsim,npop,nages,nsubyears,nareas,nareas)
    movind<-movindc<-TEG(dim(mov))
    movindc[,3]<-ma[movind[,2:3]]
    mov[movind]<-movcalc[movindc]   
    .Object@mov<-mov    
      
    #          nsim,npop,nages,nyears+proyears
    .Object@Wt_age<-tomt(array(out$wt_age,c(dim(out$wt_age),nsim)))
    .Object@iALK<-array(rep(out$iALK,each=nsim),c(nsim,dim(out$iALK)))
    
    .Object@sel<-array(NA,c(nsim,nfleets,nlen))
    
    sela<-array(NA,c(nsim,out$nsel,nlen))
    sels<-samps[,grep("selpar",vcv$names)]
    ml<-out$ml
    
    sc<-1
    
    for(ss in 1:out$nsel){
      
      spars<-sels[,sc:(sc+out$seltype[ss]-1)]
      
      if(out$seltype[ss]==2){
        spar2<-ml[nlen]*(0.2+0.5*exp(spars[,2])/(1+exp(spars[,2])));       #// Inflection point (2) as a fraction of largest length I(0.1|0.8)
        spar1<-ml[nlen]*(0.02+0.08*(exp(spars[,1])/(1+exp(spars[,1]))));  # // Logistic slope (1) as fraction of inflection point (2) I(0.01|0.5)
        ind<-as.matrix(expand.grid(1:nsim,ss,1:nlen))  
        sela[ind]<-1/(1+exp((spar2[ind[,1]]-ml[ind[,3]])/spar1[ind[,1]]))
        sela[,ss,]<-sela[,ss,]/apply(sela[,ss,],1,max)
      }else if(out$seltype[ss]==3){
        spar1<-0.01+((spars[,1]+2)/5)^3;                     # // Dome-shape parameter I(0|0.2)
        spar2<-0.01+0.01*(spars[,2]+2)^3;               #// Precision as the ratio of the inflection point I(0.1|0.7)
        spar3<-ml[nlen]*(0.1+0.8*(spars[,3]+2)/4);   #// Inflection point as a fraction of largest length I(0.15|0.9)
        ind<-as.matrix(expand.grid(1:nsim,ss,1:nlen))  
        inds<-ind[,1]
        indl<-ind[,3]
        sela[ind]<-(1/(1-spar1[inds]))*((1-spar1[inds])/spar1[inds])^spar1[inds] * exp(spar2[inds]*spar1[inds]*(spar3[inds]-ml[indl]))/(1+exp(spar2[inds]*(spar3[inds]-ml[indl])));		
        sela[,ss,]<-sela[,ss,]/apply(sela[,ss,],1,max)
      }
      sc<-sc+out$seltype[ss]
    }
    
    matplot(t(sela[1,,]),type='l')
    
    for(ff in 1:nfleets).Object@sel[,ff,]<-sela[,out$selind[ff],]
    
    .Object@qE<-as.matrix(exp(samps[,grep("lnqE",vcv$names)]))
    .Object@qI<-as.matrix(exp(samps[,grep("lnqI",vcv$names)]))
    .Object@qCPUE<-as.matrix(exp(samps[,grep("lnqCPUE",vcv$names)]))
    
    maxF<-apply(out$FL,1:4,max)
    E<-maxF/array(rep(exp(out$lnqE),each=prod(dim(maxF)[1:3])),dim(maxF))
    .Object@E<-array(NA,c(nsim,nfleets,nyears,nsubyears,nareas))
    ind<-TEG(dim(.Object@E))
    indE<-ind[,c(3,4,5,2)]
    .Object@E[ind]<-E[indE]
    
    .Object@Spat_targ<-array(1,c(nsim,nfleets)) #NA
    .Object@Frat <-.Object@qE/apply(.Object@qE,1,sum) # NA
    .Object@targpop<-1
    #.Object@nZeq<-as.integer(out$nZeq)
    .Object@nydist<-as.integer(out$nydist)
    #.Object@nyeq<-as.integer(out$nyeq)
     
    
  }else{ # Efit !is.na and Efit directory does not exist
    
    print(paste('You specified a directory',OMd,'that does not exist or an object',OMd, 'that does not exist'))
    stop()
    
  }
 
  cat("Calculating MSY reference points")
  cat("\n") 
  #MSYrefs<-matrix(9999,ncol=7)
  # Edit this to make a flexible npop array MSYrefs
  # MSY refs return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSYa,SSBMSY_B0))
  
  res<-array(NA,c(nsim,np,8)) # MSY, FMSYa, UMSY, BMSY, SSBMSY, BMSY/B0, SSBMSY/SSB0, RMSY/R0
  
  #FML s, m, r, f, l
  FMLs<-array(NA,c(nsim,nsubyears,nareas,nfleets,nlen))
  Find<-TEG(dim(FMLs))
  Eind<-cbind(Find[,c(1,4)],rep(nyears,nrow(Find)),Find[,c(2,3)])
  FMLs[Find]<-.Object@E[Eind]*.Object@qE[Find[,c(1,4)]]*.Object@sel[Find[,c(1,4,5)]]
 
  for(i in 1:nsim){

    res[i,,]<-as.matrix(MSY_FAST(FML=FMLs[i,,,,], iALK=out$iALK[,nyears,,], N=out$N[,out$ny,,,],
                wt_age=t(out$wt_age[out$ny,,]), M_age=out$M_age, mat_age=out$mat_age, 
                R0s=.Object@Recpars[i,,1,2], fixpars=.Object@Recpars[i,,1,1]),
                SRtypes=.Object@Rectype[1,]) # recruitment curve assumed to be first of the future assumed types
    
    #res[i,,]<-MSY_FAST(FML=out$FL[out$ny,,,,], iALK=out$iALK[,out$ny,,], N=out$N[,out$ny,,,],
    #                   wt_age=t(out$wt_age[out$ny,,]), M_age=out$M_age, mat_age=out$mat_age, R0s=out$R0, hs=out$h)
  }
  #                 UMSY, BMSY, VBMSY, SSBMSY, FMSYa, SSBMSY/SSB0
  #MSYrefs1<-res[,1,c(3,4,4,5,2,7)]#t(sfSapply(1:nsim,getMSYrefs,.Object,nyears=60,toly=0.1))
  #MSYrefs2<-res[,2,c(3,4,4,5,2,7)]#t(sfSapply(1:nsim,getMSYrefs,.Object,nyears=60,toly=1))
  
  .Object@targpop<-1:2
  #out<-sapply(1,getMSYrefs,.Object)
  #names(MSYrefs)<-c("MSY","BMSY","VBMSY","SSBMSY","UMSY","SSBMSY_B0")
  #c("MSY","FMSYap","UMSY","BMSY","SSBMSY","BMSY_B0","SSBMSY_SSB0","RMSY_R0")
  .Object@MSY<-res[,,1]#cbind(MSYrefs1[,1],MSYrefs2[,1])
  .Object@BMSY<-res[,,4]#cbind(MSYrefs1[,2],MSYrefs2[,2])
  .Object@VBMSY<-array(NA,c(nsim,npop)) # not calculated
  .Object@SSBMSY<-res[,,5]#cbind(MSYrefs1[,4],MSYrefs2[,4])
  .Object@UMSY<-res[,,3]#cbind(MSYrefs1[,5],MSYrefs2[,5])
  .Object@FMSYa<-res[,,2]#cbind(MSYrefs1[,6],MSYrefs2[,6])
  .Object@SSBMSY_SSB0<-res[,,7]#cbind(MSYrefs1[,7],MSYrefs2[,7])

  # Add statistical properties of recruitment
 
  .Object

})


             

# Observation model definition object -------------------------------------------------------------------------------------------------------------------  
setClass("Obs",representation(Name="character",                           
               Ccv="numeric",Cbcv="numeric",                                  # Observation error and bias in total annual catches
               nCAAobs="numeric",nCALobs="numeric", Lcv="numeric",            # Number of annual catch at age (CAA) and catch-at-length (CAL) observations 
               Ibeta="numeric", Icv="numeric",                                # Hyperstability parameter I^beta and observation error in relative abundance indices
               Mbcv="numeric",                                                # Bias in observation of natural mortality rate
               Kbcv="numeric",t0bcv="numeric",Linfbcv="numeric",              # Bias in estimation of growth parameters
               LFCbcv="numeric", LFSbcv="numeric",                            # Bias in observation of length at first capture (LFC) and length at full selection (LFS)
               FMSYbcv="numeric",FMSY_Mbcv="numeric",BMSY_B0bcv="numeric",    # Bias in observaton of FMSY, ratio of FMSY/M, BMSY/B0
               ageMbcv="numeric",                                             # Bias in observation of age at 50% maturity and 
               Dbcv="numeric",Dcv="numeric",                                  # Bias and imprecision in observation of current stock depletion
               Btbcv="numeric",Btcv="numeric",                                # Bias and imprecision in observation of current stock biomass
               Ftbcv="numeric",Ftcv="numeric",                                # Bias and imprecision in observation of current fishing mortality rate
               hbcv="numeric",                                                # Bias in observation of steepness
               Recbcv="numeric",IMSYbcv="numeric",                            # Bias in observation of recent recrutiment, target CPUE (CPUE @ MSY)     
               MSYbcv="numeric",BMSYbcv="numeric",                            # Bias in observation of target catch and biomass (MSY and BMSY)
               MPind="data.frame",MPind_stats="list"                          # Indices that may be used in MPs
)) 


setClass("MSE",representation(
  
  # Description
  Name="character",Date="character",Author="character",
  Notes="character",PrimarySource="character",                           
  
  # Dimensions
  nsim="integer",npop="integer",nages="integer",             # MSE dimensions
  nyears="integer",nHyears="integer",nsubyears="integer",nareas="integer",     # MSE dimensions
  proyears="integer", nlen="integer",lenbins="numeric",       # Proyears
  interval="integer",                                        # Update interval
  nma="integer",ma="array",                                  # Number of movement age classes, age class definitions
  mulen="numeric",                                           # Mean length of each length bin
  
  # Observation model 
  Cimp="numeric",Cb="numeric",Cerr="array",
  Iimp="numeric",Ibeta="numeric",Ierr="array",
  nCAAobs="numeric",nCALobs="numeric",Lcv="numeric",
  Mb="numeric",Kb="numeric",t0b="numeric",Linfb="numeric",
  LFCb="numeric",LFSb="numeric",
  FMSYb="numeric",FMSY_Mb="numeric",BMSY_B0b="numeric",
  ageMb="numeric", 
  Dimp="numeric", Db="numeric",Derr="array",
  Btimp="numeric", Btb="numeric",Bterr="array",
  Ftimp="numeric", Ftb="numeric",Fterr="array",
  
  hb="numeric",
  Recbcv="numeric",
  IMSYb="numeric", MSYb="numeric", BMSYb="numeric",
  
  # Management quantities
  C="array",
  D="array",
  B_BMSY="array",
  F_FMSY="array",
  B="array",
  SSB="array",
  SSB0="array",
  SSB0proj="array",
  TAC="array",
  
  # Performance metrics
  Perf="data.frame",
  POF="array",
  Y="array",
  AAVY="array",
  PB10="array",
  PB50="array",
  PB100="array",
  
  nfleets="integer",    
  targpop="integer",
  nMPs="integer",
  Snames="character",
  
  MPs="list"
  
)) 

setMethod("initialize", "MSE", function(.Object,OM,Obs,MPs=list(c("UMSY","UMSY")),interval=3,IE="Umax",
                                        curTAC=c(13500000,2000000),Allocation=NA,Fdistyrs=3){
  #.Object})
  #.Object<-new('MSE')
  .Object@Snames<-OM@Snames
  # Auto-correlation in recrutiment deviations is currently disabled
  set.seed(OM@seed)
  if(class(OM)!='OM'){
    print(paste('Could not run MSE:',deparse(substitute(OMd)),'not of class OM'))
    stop()
  }
  if(class(Obs)!='Obs'){
    print(paste('Could not run MSE:',deparse(substitute(Obs)),'not of class Obs'))
    stop()
  }
  if(class(get(MPs[[1]][1]))!='ABT_MP'){
    print(paste('Could not run MSE:',deparse(substitute(MPs[[1]][1])),'not of class ABT_MP'))
    stop()
  }
  if(class(get(IE))!='ABT_IE'){
    print(paste('Could not run MSE:',deparse(substitute(IE)),'not of class ABT_IE'))
    stop()
  }
 
  
  MPs<-append(list(c("ZeroC","ZeroC")),MPs)
  
  # copy over dimensions ------
  dimslots<-slotNames(OM)[1:18]
  for(i in 1:18)slot(.Object,dimslots[i])<-slot(OM,dimslots[i])
  #.Object})
  #.Object<-new('MSE',OM,Obs)
  # -------------------------------------------------------------------------
  
  cat("Constructing arrays")
  cat("\n")
  flush.console()  
  
  # Dimensions  S P A Y M R
  nsim<-OM@nsim
  npop<-OM@npop
  nyears<-OM@nyears
  nHyears<-OM@nHyears
  proyears<-OM@proyears
  nages<-OM@nages
  nsubyears<-OM@nsubyears
  nareas<-OM@nareas
  nfleets<-OM@nfleets
  targpop<-as.integer(OM@targpop)
  .Object@targpop<-targpop
  allyears<-nyears+proyears
  nMPs<-length(MPs)
  .Object@nMPs<-nMPs
  nlen<-OM@nlen
  lenbins<-OM@lenbins
  mulen<-OM@mulen
  Wt_age<-OM@Wt_age
  nydist<-OM@nydist
  #R0<-OM@Recpars[,,1,2]
  mat<-OM@mat
  mov<-OM@mov
  h<-OM@h
  Recsubyr<-OM@Recsubyr
  Recdevs<-OM@Recdevs
  
  HCobs<-OM@HCobs
  
  # Run historical simulation ----------------------------------------------
  
  
  cat("Running historical simulations")
  cat("\n")
  
  
  M<-OM@M
  Mtemp<-array(0,dim(OM@M))
  Mtemp[,,2:nages,]<-OM@M[,,1:(nages-1),]
  
  surv=tomt(exp(-apply(Mtemp[,,,1],2:1,cumsum)))
  
  N<-SSN<-NSN<-SSB<-VBA<-Z<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  #SSBA<-array(NA,c(nsim,npop,allyears))
  FD<-array(NA,c(nsim,nfleets,allyears,nsubyears,nareas))              # Fishing distribution
  Fdist<-array(NA,c(nsim,npop,nfleets,nareas))
  VB<-C<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  CA<-array(NA,c(nsim,npop,allyears,nsubyears,nareas))
  
  hFAT<-array(NA,c(nsim,nHyears,nsubyears,nages,nareas))
  hZ<-array(NA,c(nsim,npop,nages,nHyears,nsubyears,nareas))
  hZind<-TEG(dim(hZ))
  hFATind<-hZind[,c(1,3,4,5,6)]
  
  mref<-c(2:nsubyears,1)  # movement reference
  y<-1
  m<-1
  
  RFL<-array(NA,c(nsim,nfleets,nlen,nyears,nsubyears,nareas))
  indL<-TEG(dim(RFL))
  RFL[indL]<-OM@qE[indL[,c(1,2)]]*nsubyears*OM@sel[indL[,1:3]]*OM@E[indL[,c(1,2,4,5,6)]]
  
  Ftrans<-array(0,c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets,nlen))
  
  for(i in 1:nsim){
    
    Find<-as.matrix(expand.grid(i,1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nfleets,1:nlen))
    Lind<-Find[,c(1,7,8,4,5,6)]
    Ftrans[Find]<-OM@iALK[Find[,c(1,2,4,3,8)]]*RFL[Lind]
    
  }
  
  FM<-array(NA,c(nsim,npop,nages,allyears,nsubyears,nareas,nfleets))
  FM[,,,1:nyears,,,]<-apply(Ftrans,1:7,sum) # s p a y m r f
  maxRF<-apply(FM,c(1,2,4,5,6,7),max)
  Rind<-TEG(c(nsim,npop,nages,nyears,nsubyears,nareas,nfleets))
  sel<-FM
  sel[Rind]<-sel[Rind]/maxRF[Rind[,c(1,2,4,5,6,7)]]
  sel<-sel[,,,nyears,nsubyears,,] # Take this from last year, in future simulations this may be by year so leave this code!
  sel[is.na(sel)]<-0
  
  SFAYMR<-as.matrix(expand.grid(1:nsim, 1:nfleets,1:nages,y,m,1:nareas)) # Set up some array indexes
  SFAY<-SFAYMR[,1:4]
  
  SPAYMR<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas))     # Set up some array indexes
  SARP<-SPAYMR[,c(1,3,6,2)]
  SPA<-SPAYMR[,1:3]
  SPR<-SPAYMR[,c(1,2,6)]
  SPMR<-SPAYMR[,c(1,2,5,6)]
  SP<-SPAYMR[,1:2]
  SA<-SPAYMR[,c(1,3)]
  SAR<-SPAYMR[,c(1,3,6)]
  SPAR<-SPAYMR[,c(1:3,6)]
  SPAY<-SPAYMR[,1:4]
  SPAM<-SPAYMR[,c(1:3,5)]
  
  # New model initialization -----------------------
  
  Rec<-array(array(OM@muR,c(nsim,npop,nyears))*OM@Recdevs,c(nsim,npop,nyears))
  Rec1<-Rec[,,1]
  
  #Zeq<-apply(apply(FM[,,,1:nZeq,,,],1:6,sum),c(1,2,3,5),mean)+ 
  #Zeq<-apply(FM[,,,1:nZeq,,,],c(1,2,3,5,6),mean)+ 
   # array(apply(M[,,,1:nZeq],1:3,mean),c(nsim,npop,nages,nsubyears,nareas))/nsubyears
  #SSB0<-apply(surv*array(R0,dim(surv))*Wt_age[,,,1]*mat[,,,1],1:2,sum)
  #SSBpR<-SSB0/R0 
  
  stemp<-array(1/nareas,dim=c(nsim,npop,nsubyears,nareas))
  movi<-mov[,,nages,,,]
  
  for(y in 1:nydist){
    
    for(m in 1:nsubyears){
      
      if(m==1){
        
        stemp[,,m,]<-apply(array(rep(stemp[,,nsubyears,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)  
        
      }else{
        
        stemp[,,m,]<-apply(array(rep(stemp[,,m-1,],nareas)*movi[,,m,,],c(nsim,npop,nareas,nareas)),c(1,2,4),sum)  
        
      }     
      
    }
    
  }
  
  indN<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1,1:nsubyears,1:nareas))
  N[indN]=Rec1[indN[,1:2]]*surv[indN[,1:3]]*stemp[indN[,c(1,2,5,6)]]
  N[,,nages,1,,]<-N[,,nages,1,,]+N[,,nages,1,,]*array(exp(-M[,,nages,1])/(1-exp(-M[,,nages,1])),c(nsim,npop,nsubyears,nareas)) # plus group
 
  SSB[,,,1,,]<-N[,,,1,,]*rep(Wt_age[,,,1],nareas*nsubyears)*rep(mat[,,,1],nareas*nsubyears)
  
  sdur<-1/nsubyears
  canspawn<-array(rep(c(0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0),each=nsim),c(nsim,npop,nareas))
  hM<-array(M,c(nsim,npop,nages,nareas))
  
  for(y in 2:nHyears){
    
    for(m in 1:nsubyears){
      
      if(m==1){ # first subyear
        
        Ntemp<-apply(N[,,,1,nsubyears,]*exp(-hM*sdur/2),c(1,3,4),sum,na.rm=T)
        Ntemp[Ntemp==0]<-tiny # to avoid zero catches divided by zero numbers
        Htemp<-array(rep(HCobs[y-1,nsubyears,,],each=nsim),c(nsim,nages,nareas))/Ntemp
        Htemp[Htemp>0.9]<-0.9
        hFAT[,y-1,nsubyears,,]<--log(1-Htemp)
        hZind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y-1,nsubyears,1:nareas))
        hFATind<-hZind[,c(1,4,5,3,6)]
        hMind<-hZind[,c(1,2,3,6)]
        hZ[hZind]<-hFAT[hFATind]+hM[hMind]*sdur
        N[,,,1,m,]<-N[,,,1,nsubyears,]*exp(-hZ[,,,y-1,nsubyears,])
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)
        
      }else if(m==2){ # spawning subyear
        
        Ntemp<-apply(N[,,,1,m-1,]*exp(-hM*sdur/2),c(1,3,4),sum,na.rm=T)
        Ntemp[Ntemp==0]<-tiny
        Htemp<-array(rep(HCobs[y,m-1,,],each=nsim),c(nsim,nages,nareas))/Ntemp
        Htemp[Htemp>0.9]<-0.9
        hFAT[,y,m-1,,]<--log(1-Htemp)
        hZind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m-1,1:nareas))
        hFATind<-hZind[,c(1,4,5,3,6)]
        hMind<-hZind[,c(1,2,3,6)]
        hZ[hZind]<-hFAT[hFATind]+hM[hMind]*sdur
        N[,,,1,m,]<-N[,,,1,m-1,]*exp(-hZ[,,,y,m-1,])
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)
        
        SSBtemp<-apply(SSB[,,,1,m,],c(1,2,4),sum)*canspawn # viable spawning biomass
        spawnr<-SSBtemp/array(apply(SSBtemp,1:2,sum),dim(SSBtemp))
        SSBt<-apply(SSB[,,,1,m,],1:2,sum)
        N[,,nages,1,m,]<-N[,,nages,1,m,]+N[,,nages-1,1,m,] # plus group
        N[,,2:(nages-1),1,m,]<-N[,,1:(nages-2),1,m,]
        N[,,1,1,m,]<-spawnr*array(Rec1,dim(spawnr)) # Initial recruitment distributed by viable spawning biomass
        
      }else{   # after spawning subyear
        
        Ntemp<-apply(N[,,,1,m-1,]*exp(-hM*sdur/2),c(1,3,4),sum,na.rm=T)
        Ntemp[Ntemp==0]<-tiny
        Htemp<-array(rep(HCobs[y,m-1,,],each=nsim),c(nsim,nages,nareas))/Ntemp
        Htemp[Htemp>0.9]<-0.9
        hFAT[,y,m-1,,]<--log(1-Htemp)
        hZind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m-1,1:nareas))
        hFATind<-hZind[,c(1,4,5,3,6)]
        hMind<-hZind[,c(1,2,3,6)]
        hZ[hZind]<-hFAT[hFATind]+hM[hMind]*sdur
        N[,,,1,m,]<-N[,,,1,m-1,]*exp(-hZ[,,,y,m-1,])
        N[,,,1,m,]<-domov(N[,,,1,m,],mov[,,,m,,])
        SSB[,,,1,m,]<-N[,,,1,m,]*rep(Wt_age[,,,1],nareas)*rep(mat[,,,1],nareas)
        
      }# End of if subyear
    }  # end of subyear
  }    # end of SRA year
  
  #test
  #OM@hZ[1,90,2,,1]
  #hZ[1,1,,90,2,1]
  
  #bR<-log(5*h)/(0.8*SSB0)                                      # Ricker SR params
  #aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params
  
  y<-1
  m<-1
  
  SPAYMRF2<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,y,m,1:nareas,1:nfleets))
  SPAYRF2<-SPAYMRF2[,c(1:4,6,7)]
  SF2<-SPAYMRF2[,c(1,7)]
  SPAY2<-SPAYMRF2[,1:4]
  SFAY2<-SPAYMRF2[,c(1,7,3,4)]
  SFYMR2<-SPAYMRF2[,c(1,7,4,5,6)]
  SPAYMR2<-SPAYMRF2[,1:6]
  SFA2<-SPAYMRF2[,c(1,7,3)]
  SFAR2<-SPAYMRF2[,c(1,7,3,6)]
  SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
  SPARF2<-SPAYMRF2[,c(1:3,6,7)]
  SPRFA2<-SPAYMRF2[,c(1,2,6,7,3)] #
  SPR2<-SPAYMRF2[,c(1,2,6)] # for projections only
  SRF2<-SPAYMRF2[,c(1,6,7)] 
  SPAL<-cbind(SPAYMRF2[,c(1:3)],rep(nyears,nrow(SPAYMRF2)))
  
  for(m in 1:nsubyears){
    
    SPAYMRF2[,5]<-m
    SPAYMR2<-SPAYMRF2[,1:6]
    SPAYMR[,5]<-m
    VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                    # Calculate vunerable biomassp a y m r f
    Ftot<-apply(FM[,,,y,m,,],1:4,sum)
    Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears
    
    C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*(FM[SPAYMRF2]/Z[SPAYMR2]) # Calculate catches
    
  }
  
  SPAYMR[,5]<-1
  SPAYMRF2[,5]<-1
  SPAYMR2<-SPAYMRF2[,1:6]
  
  for(y in 2:nyears){
    
    SPAYMR[,4]<-y
    SPAY<-SPAYMR[,1:4]
    SPAYMRF2[,4]<-y
    SPAY2<-SPAYMRF2[,1:4]
    SFAY2<-SPAYMRF2[,c(1,7,3,4)]
    SFAYR2<-SPAYMRF2[,c(1,7,3,4,6)]
    SPAYRF2<-SPAYMRF2[,c(1:4,6,7)]
    
    for(m in 1:nsubyears){
      
      SPAYMR[,5]<-m
      SPAM<-SPAYMR[,c(1:3,5)]
      SPAYMRF2[,5]<-m
      SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
      SPAYMR2<-SPAYMRF2[,1:6]
      
      if(m==1){
        N[,,,y,m,]<-N[,,,y-1,nsubyears,]*exp(-Z[,,,y-1,nsubyears,])
      }else{
        N[,,,y,m,]<-N[,,,y,m-1,]*exp(-Z[,,,y,m-1,])
      }  
      
      # move fish
      
      N[,,,y,m,]<-domov(N[,,,y,m,],mov[,,,m,,])
      
      VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAY2]*sel[SPARF2]                     # Calculate vunerable biomass
      Ftot<-apply(FM[,,,y,m,,],1:4,sum)
      Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears
      
      C[SPAYMRF2]<-N[SPAYMR2]*(1-exp(-Z[SPAYMR2]))*(FM[SPAYMRF2]/Z[SPAYMR2]) # Calculate catches
      
      for(pp in 1:npop){
        
        SSB[,pp,,y,m,]<-N[,pp,,y,m,]*array(Wt_age[,pp,,y]*mat[,pp,,y],dim=c(nsim,nages,nareas))
        
        if(Recsubyr[pp]==m){
          
          # new
          SSBtemp<-apply(SSB[,pp,,y,m,],c(1,3),sum)*canspawn[,pp,] # viable spawning biomass
          spawnr<-SSBtemp/array(apply(SSBtemp,1,sum),dim(SSBtemp))
          #SSBt<-apply(SSB[,pp,,y,m,],1:2,sum)
          N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,] # plus group
          N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]
          N[,pp,1,y,m,]<-spawnr*array(Rec[,pp,y],dim(spawnr)) # Initial recruitment distributed by viable spawning biomass
          
          # old
          #spawnr<-apply(SSB[,pp,,y,m,],c(1,3),sum)/apply(SSB[,pp,,y,m,],1,sum)
          #SSBt<-apply(SSB[,pp,,y,m,],1,sum)
          #N[,pp,2:nages,y,m,]<-N[,pp,1:(nages-1),y,m,] # age fish
          
          #if(SRrel[pp]==1){    # Beverton-Holt recruitment
            #N[,pp,1,y,m,]<-Recdevs[,pp,y]*spawnr*((0.8*R0[,pp]*h[,pp]*SSBt)/(0.2*SSBpR[,pp]*R0[,pp]*(1-h[,pp])+(h[,pp]-0.2)*SSBt))
          #}else{              # Most transparent form of the Ricker uses alpha and beta params
           # N[,pp,1,y,m,]<-Recdevs[,pp,y]*spawnr*aR[,pp]*SSBt*exp(-bR[,pp]*SSBt)
          #}
          
        } # if its the right subyear
      } # end of pop
    } # end of subyear  
  } # end of year  
  
  Bcur<-apply(N[,,,nyears,nsubyears,]*
                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1:2,sum)
  
  Biomass<-array(0,c(nsim,npop,nages,allyears,nsubyears,nareas))
  
  Biomass[,,,1:nyears,,]<-N[,,,1:nyears,,]*array(Wt_age,c(nsim,npop,nages,nyears,nsubyears,nareas))
  
  
  SSBall<-N*array(Wt_age,dim(N))*array(OM@mat,dim(N))
  RAI<-apply(SSBall,c(1,4,5,6),sum)
  RAI<-RAI[,1:nyears,,]
  RAI<-RAI/array(apply(RAI,1,mean),dim(RAI))
  
  
  #D<-Bcur/SSB0 # Check against OM@D (remember only targetpop is matched)
  
  # Generate observation errors ---------------------------------------------
  
  .Object@Cimp<-runif(nsim,Obs@Ccv[1],Obs@Ccv[2])
  .Object@Cb<-trlnorm(nsim,1,Obs@Cbcv)
  .Object@Cerr<-array(trlnorm(nsim*allyears,rep(.Object@Cb,allyears),rep(.Object@Cimp,allyears)),c(nsim,allyears)) 
  
  .Object@Iimp<-runif(nsim,Obs@Icv[1],Obs@Icv[2])
  .Object@Ierr<-array(trlnorm(nsim*allyears,1,rep(.Object@Iimp,allyears)),c(nsim,allyears)) 
  .Object@Ibeta<-exp(runif(nsim,log(Obs@Ibeta[1]),log(Obs@Ibeta[2])))
  
  .Object@Btimp<-runif(nsim,Obs@Btcv[1],Obs@Btcv[2])
  .Object@Btb<-trlnorm(nsim,1,Obs@Btbcv)
  .Object@Bterr<-array(trlnorm(nsim*allyears,rep(.Object@Btb,allyears),rep(.Object@Btimp,allyears)),c(nsim,allyears)) 
  
  .Object@Mb<-trlnorm(nsim,1,Obs@Mbcv)
  .Object@Kb<-trlnorm(nsim,1,Obs@Kbcv)
  .Object@Linfb<-trlnorm(nsim,1,Obs@Linfbcv)
  .Object@t0b<-rep(1,nsim)
  
  .Object@MSYb<-trlnorm(nsim,1,Obs@MSYbcv)
  .Object@BMSYb<-trlnorm(nsim,1,Obs@BMSYbcv)
  .Object@IMSYb<-trlnorm(nsim,1,Obs@IMSYbcv)
  .Object@FMSYb<-trlnorm(nsim,1,Obs@FMSYbcv)
  .Object@FMSY_Mb<-trlnorm(nsim,1,Obs@FMSY_Mbcv)
  
  .Object@nCAAobs<-ceiling(runif(nsim,Obs@nCAAobs[1],Obs@nCAAobs[2]))
 
  .Object@C<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@B_BMSY<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@F_FMSY<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@D<-array(NA,c(nMPs,nsim,npop,allyears))
  .Object@ageMb<-trlnorm(nsim,1,Obs@ageMbcv)
  .Object@SSB<-array(NA,c(nMPs,nsim,npop,allyears))
  
  # Run projections ------------------------------------------------
  cat("\n")
  cat("Running projections")
  cat("\n")
  
  sfExport(list=c("XSA","DD","DD_R","UMSY","Cooke_DD","tiny"))
  upyrs<-nyears+(0:(floor(OM@proyears/interval)-1))*interval  # the years in which there are updates (every three years)
 
  testC<-array(NA,c(nsim,npop,nfleets,nareas))
  CAdist<-array(NA,c(nsim,npop,nareas,nfleets,nages))
  CAA<-sampCatch(apply(C[,,,1:(nyears-1),,,],c(1,3,4),sum,na.rm=T),.Object@nCAAobs)
  inc<-OM@mulen[2]-OM@mulen[1]
  CAL<-makeCAL2(CAA,OM@iALK)
  CAL_bins<-c(OM@mulen,OM@mulen[OM@nlen]+inc)-0.5*inc 
  
  # Allocation / assessment vector -------------------------
  Assess_areas<-c(rep(2,4),rep(1,nareas-4))
  nAss<-max(Assess_areas)
  
  if(is.na(Allocation)[1]){
    Allocation<-array(0,c(nAss,nfleets))
    Cdist<-apply(OM@Cobs[(nyears-2):nyears,,,],3:4,sum)
    for(a in 1:nAss)Allocation[a,]<-apply(Cdist[Assess_areas==a,],2,sum)/sum(Cdist[Assess_areas==a,])
  }
  
  if(ncol(Allocation)!=nfleets)stop("You need to specify an allocation array with  (OM@nfleets) columns")
  if(length(Assess_areas)!=nareas)stop("You need to specify an Assess_areas array with  (OM@nareas) columns")
  
  Assess_data<-array(rep(Assess_areas,each=nAss)==rep(1:nAss,nareas),c(nAss,nareas)) # logical array for later calculations
  .Object@TAC<-array(NA,c(nsim,nMPs,nAss,proyears))
   
  # MP index properties -------------------------------------
  
  nind<-max(Obs@MPind$No)
  Index_areas<-array(FALSE,c(nind,nareas))
  Istats<-array(NA,c(nsim,nind,4)) # beta, AC, sd, cor
  Itype<-rep(1,nind)
  Ilev<-array(NA,c(nsim,nyears+proyears,nareas,2))
  Ilev[,1:nyears,,1]<-apply(Biomass[,,,1:nyears,,],c(1,4,6),sum)
  Ilev[,1:nyears,,2]<-apply(SSB[,,,1:nyears,,],c(1,4,6),sum)
  Isim<-array(NA,c(nsim,nind,nyears+proyears,2))
   
   
  for(i in 1:nind){
     
    id<-subset(Obs@MPind,Obs@MPind$No==i & Obs@MPind$Year<(OM@nyears+1))
    yrs<-yrs2<-id$Year
    if(!nyears%in%yrs)yrs2<-c(yrs,nyears)
    Itype[i]<-id$Type[1]  # Biomass (1) or spawning biomass (2)
    aind<-as.numeric(strsplit(as.character(id$Areas[1]),"-")[[1]])
    Index_areas[i,aind]<-TRUE
    
    for(s in 1:nsim){
    
      IB<-Ilev[s,yrs,Index_areas[i,],Itype[i]]
      if(class(IB)=="matrix")IB<-apply(IB,1,sum)
      
      IB2<-Ilev[s,yrs2,Index_areas[i,],Itype[i]]
      if(class(IB2)=="matrix")IB2<-apply(IB2,1,sum)
      
      Isim[s,i,yrs2,1]<-IB2 # these also have to include the latest model year nyears yrs2
      
      fitout<-indfit(SSB=IB,ind=id$Index,Year=yrs,sim=F,plot=F)
      
      Istats[s,i,]<-as.numeric(fitout[[1]])
      Isim[s,i,yrs,2]<-fitout$mult
      procmu <- -0.5*(Istats[s,i,3])^2 # adjusted log normal mean
     
      Perr<-rnorm(proyears+1,procmu, Istats[s,i,3])
      Perr<-Istats[s,i,2]*Perr+Perr*(1-Istats[s,i,2]*Istats[s,i,2])^0.5#2#AC*Perr[,y-1]+(1-AC)*Perr[,y] # apply a pseudo AR1 autocorrelation to rec devs (log space)
      Isim[s,i,nyears:(proyears+nyears),2] <-exp(Perr*Istats[s,i,3]/sd(Perr)) # normal 
      #Isim[s,i,nyears,1]<-sum(Ilev[s,nyears,Index_areas[i,],Itype[i]])
    }  
    
  }
  
  
  Itemp<-Isim[,,1:nyears,1]^array(Istats[,,1],c(nsim,nind,nyears))                 # add hyperstability / hyper depletion
  Itemp2<-Itemp[,,1:nyears]/array(apply(Itemp[,,1:nyears],1:2,mean,na.rm=T),dim(Itemp))*Isim[,,1:nyears,2] # normalize to mean 1 pre autocorrelated residuals
  Iobs<-Itemp2/array(apply(Itemp2,1:2,mean,na.rm=T),dim(Itemp2))                # normalize to mean 1 post residual error
  # !! CHECK !! These historical indices should be identical among simulations (they are the backward recreation of the statistical fits)
  
  # you need the fraction of total F by fleet and area pseudocode
  # tempcat<-TAC * Allocation * Ffrac
  # maxF(tempcat)
  
  
  #Fdist<-apply(FM[,,,nyears,,,],1:5,sum)# sum over fleets
  #Fdist<-apply(Fdist,c(1,2,4,5),max)# get max F
  #Fdist<-Fdist/array(apply(Fdist,1:2,sum),c(nsim,npop,nsubyears,nareas)) # spatial /subyear F distribution
  
  Fdist<-apply(FM[,,,(nyears-Fdistyrs+1):nyears,,,],c(1,5,6,7),sum) # F is the same for both stocks so summing makes no difference
  Fdistsum<-apply(Fdist,c(1,4),sum)
  Fdistind<-TEG(dim(Fdist))
  Fdist[Fdistind]<-Fdist[Fdistind]/Fdistsum[Fdistind[,c(1,4)]]
  Fdist[is.na(Fdist)]<-0   # gets rid of zero divided by zero NaN values
  
  testC<-array(NA,c(nsim,nsubyears,nareas,nfleets,nAss))
  testCind<-TEG(dim(testC))
  
  #include check for Fdist and Allocation
  
  Regime<-OM@Recind[,y-nyears]
  Rectype<-OM@Rectype[Regime,pp] # rec changes at the same time for both stocks
  proccv<-rep(OM@Reccv,allyears)
  procmu <- -0.5*(proccv)^2
  Pe<-array(exp(rnorm(nsim*npop*allyears,procmu,proccv)),c(nsim,npop,allyears))
  
  pset<-new('list')
  
  for(MP in 1:nMPs){
    
    cat(paste0(paste0(MP,"/",nMPs," Running MSE for: "),paste0(MPs[[MP]]," (",.Object@Snames,")",collapse="  ")))  # print a progress report
    cat("\n")
    flush.console()                                                  # update the console
    
    TAC<-array(rep(curTAC,each=nsim),c(nsim,nAss)) 
    
    for(y in nyears:(nyears+proyears)){
      
      cat(".")
      
      if(y%in%upyrs){# Operate MP S P A Y M R
        
        # Simulate indices ----------------------------------------------
        # if new data are required
        if(y!=nyears){  
          
          Ilev[,(y-interval+1):y,,1]<-apply(Biomass[,,,(y-interval+1):y,,],c(1,4,6),sum)
          Ilev[,(y-interval+1):y,,2]<-apply(SSB[,,,(y-interval+1):y,,],c(1,4,6),sum)
          
          Itemp<-array(NA,c(nsim,nind,y,nareas))
          Itemp_ind<-TEG(dim(Itemp))
          Ilev_ind<-cbind(Itemp_ind[,c(1,3,4)],Itype[Itemp_ind[,2]])
          Itemp[Itemp_ind]<-Ilev[Ilev_ind]*Index_areas[Itemp_ind[,c(2,4)]]
          Isim[,,1:y,1]<-apply(Itemp[,,1:y,],1:3,sum)
            
          Itemp<-Isim[,,1:y,1]^array(Istats[,,1],c(nsim,nind,y))                 # add hyperstability / hyper depletion
          Itemp2<-Itemp/array(apply(Itemp,1:2,mean,na.rm=T),dim(Itemp))*Isim[,,1:y,2] # normalize to mean 1 pre autocorrelated residuals
          Iobs<-Itemp2/array(apply(Itemp2,1:2,mean,na.rm=T),dim(Itemp2))                # normalize to mean 1 post residual error
         
          # if additional data are required
          nuy<-(upyrs[match(y,upyrs)-1]):(y-1)
          nCAA<-sampCatch(apply(C[,,,nuy,,,],c(1,3,4),sum),.Object@nCAAobs)
          CAA<-abind(CAA,nCAA,along=3)
          CAL<-abind(CAL,makeCAL3(nCAA,OM@iALK[,,nyears,,]),along=3)
        }
        for(AS in 1:nAss){
          #SPAYMRF
          AA<-Assess_data[AS,]
          nA<-sum(AA)
          pset[[AS]]<-list("Cobs"=apply(C[,,,1:(y-1),,AA,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,y-1,nsubyears,nA,nfleets)),c(1,4),sum)*.Object@Cerr[,1:(y-1)],
                   "Iobs"=Iobs,
                   "K"=OM@Kmu[,1]*.Object@Kb,        # for now these assume same growth by stock
                   "Linf"=OM@Linfmu[,1]*.Object@Kb,  # for now these assume same growth by stock
                   "t0"=OM@t0[,1],                   # no error in t0
                   "M"=OM@M[,AS,,(y-1)]*.Object@Mb,  # assume AS is same as stock
                   "Bt"=apply(N[,,,y-1,nsubyears,AA]*
                                array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nA)),1,sum)*.Object@Bterr[,(y-1)],#apply(VBA[,,,(y-1),4,],1,sum)*.Object@Bterr[,(y-1)], # you were here
                   "MSY"=OM@MSY[,AS]*.Object@MSYb,
                   "BMSY"=OM@BMSY[,AS]*.Object@BMSYb,
                   "UMSY"=OM@UMSY[,AS]*.Object@FMSYb,   
                   "a"=rep(OM@a,nsim),
                   "b"=rep(OM@b,nsim),
                   "nages"=OM@nages,
                   "ageM"=OM@ageM[,AS]*.Object@ageMb,
                   "Mat"=OM@mat[,AS,,nyears],
                   "Bt_PI"=apply(N[,,,y-1,nsubyears,]*
                                   array(Wt_age[,,,nyears]*OM@mat[,,,nyears],c(nsim,npop,nages,nareas)),1,sum),
                   "UMSY_PI"=apply(array(OM@UMSY[,AS],c(nsim,AS)),1,mean),
                   "CAA"=CAA,
                   "CAL"=CAL,
                   "CAL_bins"=CAL_bins,
                   "MPrec"=TAC[,AS])
        
          assign("pset",pset,envir=globalenv()) # debugging
          #sfExport("pset")
          #if(MPs[MP]=="XSA")
            TAC[,AS]<-sapply(1:nsim,get(MPs[[MP]][AS]),pset[[AS]])
          #if(MPs[MP]!="XSA")TAC[,AS]<-sfSapply(1:nsim,get(MPs[AS,MP]),pset[[AS]])
        #print(TAC)
          if(y<allyears).Object@TAC[,MP,AS,y-nyears+1]<-TAC[,AS]
        }
        
      } # end of upyrs
      
      # you were herey<-1
      
      # now you have to allocate to fleet and then distribute according to Fdist
      
      
      SPAYMR[,4]<-y
      SPAYMRF2[,4]<-y
      SPAY<-SPAYMR[,1:4]
      SPAY2<-SPAYMRF2[,1:4]
      SFAY2<-SPAYMRF2[,c(1,7,3,4)]
     
      testC[testCind]<-TAC[testCind[,c(1,5)]]*Fdist[testCind[,c(1:4)]]*Allocation[testCind[,c(5,4)]] # predicted catch by 
      aggC<-apply(testC,1:4,sum)
        
      for(m in 1:nsubyears){
        
        SPAYMR[,5]<-m
        SPAM<-SPAYMR[,c(1:3,5)]
        SPAYMRF2[,5]<-m
        SFYMR2<-SPAYMRF2[,c(1,7,4:6)]
        SPAYMR2<-SPAYMRF2[,1:6]
        
        if(m==1){
          N[,,,y,m,]<-N[,,,y-1,nsubyears,]*exp(-Z[,,,y-1,nsubyears,])
        }else{
          N[,,,y,m,]<-N[,,,y,m-1,]*exp(-Z[,,,y,m-1,])
        }  
        
        # move fish spaymrr
        
        N[,,,y,m,]<-domov(N[,,,y,m,],mov[,,,m,,])
        
        Biomass[,,,y,m,]<-N[,,,y,m,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas))
        
        VB[SPAYMRF2]<-N[SPAYMR2]*Wt_age[SPAL]*sel[SPARF2]                    # Calculate vunerable biomass
        
        Btemp<-apply(N[,,,y,m,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,nareas)),c(1,4),sum)
        
        testU<-aggC[,m,,]/array(Btemp,dim(aggC[,m,,])) # implied harvest rate
        Fp<-(-log(1-(do.call(IE,list(testU))))) # subject to implementation error
        testC2<-(1-exp(-Fp))*array(Btemp,dim(aggC[,m,,])) 
        
        CAdist[SPRFA2]<-N[SPAYMR2]*Wt_age[SPAL]*sel[SPARF2] # predicted magnitude of catch in each strata
        CAdistsum<-apply(CAdist,c(1,3,4),sum)                # total in each sim, region and fleet
        CAdist[SPRFA2]<-CAdist[SPRFA2]/CAdistsum[SPRFA2[,c(1,3,4)]] # fraction in each stock and age class per sim region and fleet
        CAdist[is.na(CAdist)]<-0
       
        C[SPAYMRF2]<-testC2[SRF2]*CAdist[SPRFA2]
        C[SPAYMRF2][is.na(C[SPAYMRF2])]<-0
        C[SPAYMRF2]<-C[SPAYMRF2]/Wt_age[SPAL] # divide by weight to get numbers
        Up<-array(C[SPAYMRF2]/N[SPAYMR2],c(nsim,npop,nages,nareas,nfleets)) # additional check on maximum / minimum U
        Up[is.na(Up)|Up<0.00001]<-0.00001   # otherwise you can't generate some of the automatic fishery data
        Up[Up>0.9]<-0.9
        FM[SPAYMRF2]<--log(1-Up[SPARF2])
        
        Ftot<-apply(FM[,,,y,m,,],1:4,sum)
        Z[SPAYMR]<-Ftot[SPAR]+M[SPAY]/nsubyears
        
        for(pp in 1:npop){
          
          SSB[,pp,,y,m,]<-N[,pp,,y,m,]*array(Wt_age[,pp,,nyears]*mat[,pp,,nyears],dim=c(nsim,nages,nareas))
          
          if(Recsubyr[pp]==m){
             
            SSBtemp<-apply(SSB[,pp,,y,m,],c(1,3),sum)*canspawn[,pp,] # viable spawning biomass
            spawnr<-SSBtemp/array(apply(SSBtemp,1,sum),dim(SSBtemp))
            #SSBt<-apply(SSB[,pp,,y,m,],1:2,sum)
            N[,pp,nages,y,m,]<-N[,pp,nages,y,m,]+N[,pp,nages-1,y,m,] # plus group
            N[,pp,2:(nages-1),y,m,]<-N[,pp,1:(nages-2),y,m,]
             
            #spawnr<-apply(SSB[,pp,,y,m,],c(1,3),sum)/apply(SSB[,pp,,y,m,],1,sum)
            #N[,pp,2:nages,y,m,]<-N[,pp,1:(nages-1),y,m,] # age fish
            
            if(y==nyears){
              #use Rec
              N[,pp,1,y,m,]<-spawnr*array(Rec[,pp,y],dim(spawnr)) # Initial recruitment distributed by viable spawning biomass
              
            }else{
              
              SSBt<-apply(SSB[,pp,,y,m,],1,sum)
              Regime<-OM@Recind[,y-nyears]
              Rectype<-OM@Rectype[Regime,pp] # rec changes at the same time for both stocks
              #procmu <- -0.5*(OM@Reccv[,pp])^2
              #Pe<-exp(rnorm(nsim,procmu,OM@Reccv[,pp]))
              R0<-OM@Recpars[cbind(1:nsim,rep(pp,nsim),Regime,rep(2,nsim))]
              SSBpR<-apply(surv[,pp,]*OM@mat[,pp,,nyears]*OM@Wt_age[,pp,,nyears],1,sum) # This R0 dependent so needs updating for varying future R0s
              SSB0<-R0*SSBpR
              
              if(Rectype[1]=="BH"){ # currently rectypes change together
                h<-OM@Recpars[cbind(1:nsim,rep(pp,nsim),Regime,rep(1,nsim))]
                N[,pp,1,y,m,]<-Pe[,pp,y]*spawnr*(         (0.8*R0*h*SSBt) /
                                            (0.2*SSBpR*R0*(1-h) + (h-0.2)*SSBt))
              }else if(Rectype[1]=="HS"){
                inflect<-OM@Recpars[cbind(1:nsim,rep(pp,nsim),Regime,rep(1,nsim))]
                N[,pp,1,y,m,]<-Pe[,pp,y]*spawnr*R0
                cond<-SSBt<(SSB0*inflect)
                N[cond,pp,1,y,m,]<-N[cond,pp,1,y,m,]*SSBt[cond]/(SSB0[cond]*inflect[cond])
              }
            }  
          } # if its the right subyear
        } # end of pop
      } # end of subyear  
    } # end of year  
    
   
    
    # Store results
    
    SSBmu<-apply(SSB,c(1:4,6),mean)
    .Object@SSB[MP,,,]<-apply(SSBmu,c(1:2,4),sum)
    
    .Object@C[MP,,,]<-apply(C[,,,1:allyears,,,]*array(Wt_age[,,,nyears],dim(C[,,,1:allyears,,,])),c(1,2,4),sum)
    
    SSB2<-apply(N[,,,1:allyears,4,]*array(mat[,,,nyears]*Wt_age[,,,nyears],c(nsim,npop,nages,allyears,nareas)),c(1,2,4),sum)
    .Object@D[MP,,,]<-SSB2/array(SSB2[,,1],dim(SSB2))
    
    B<-apply(N[,,,1:allyears,4,]*array(Wt_age[,,,nyears],c(nsim,npop,nages,allyears,nareas)),c(1:2,4),sum)
    #Bthen<-apply((SSN[,,,1,4,]+NSN[,,,1,4,])*array(Wt_age[,,,1],c(nsim,npop,nages,nareas)),1:2,sum)
    .Object@B_BMSY[MP,,,]<-B/array(OM@BMSY,dim(B))
     
    U<-.Object@C[MP,,,]/(.Object@C[MP,,,]+B)
    .Object@F_FMSY[MP,,,]<-U/apply(array(OM@UMSY[,],c(nsim,length(targpop))),1,mean)
    
    cat("\n")
  } # end of MP
  
  .Object@SSB0<-apply(array(OM@muR,dim(surv))*surv*Wt_age[,,,nyears]*mat[,,,nyears],1:2,sum)
  
  # This is all about calculating the equilibrium unfished SSB0 for the various recruitment types in the future
  SSB0proj<-array(NA,c(nsim,npop,proyears))
  SSB0ind<-TEG(dim(SSB0proj))
  parind<-cbind(SSB0ind[,1:2],OM@Recind[SSB0ind[,c(1,3)]],rep(2,nrow(SSB0ind)))
  SSB0proj[SSB0ind]<-OM@Recpars[parind]*apply(surv*Wt_age[,,,nyears]*mat[,,,nyears],1:2,sum)[SSB0ind[,1:2]]
  .Object@SSB0proj<-SSB0proj
  
  .Object@MPs<-MPs
  
  .Object
  
})

    
  
 

cv<-function(x)  sd(x)/mean(x)
sdconv<-function(m,sd)(log(1+((sd^2)/(m^2))))^0.5        # get log normal standard deviation from transformed space mean and standard deviation
mconv<-function(m,sd)log(m)-0.5*log(1+((sd^2)/(m^2)))    # get log normal mean from transformed space mean and standard deviation
alphaconv<-function(m,sd)m*(((m*(1-m))/(sd^2))-1)
betaconv<-function(m,sd)(1-m)*(((m*(1-m))/(sd^2))-1)
trlnorm<-function(reps,mu,cv)return(rlnorm(reps,mconv(mu,mu*cv),sdconv(mu,mu*cv)))

sampCatch<-function(Csamp,nSamp){
  out<-array(NA,dim(Csamp)) 
  nsim<-dim(Csamp)[1]
  nages<-dim(Csamp)[2]
  nyears<-dim(Csamp)[3]
  for(ss in 1:nsim){
    for(yy in 1:nyears){
      
      Csampo<-Csamp[ss,,yy]
      #assign("Csampot",Csampo,envir=globalenv()) # debugging
      #assign("nsampt",nSamp[ss],envir=globalenv()) # debugging
      if(sum(Csampo)==0)Csampo<-rep(1/nages,nages)
      out[ss,,yy]<-ceiling(rmultinom(1,size=nSamp[ss],Csampo)*sum(Csampo)/nSamp[ss])
      
  }}
  out
}

makeCAL<-function(CAA,Linf,K,t0,CAL_bins,CALsd=0.05){
  ny<-dim(CAA)[3]
  na<-dim(CAA)[2]
  ns<-dim(CAA)[1]
  CALmu<--0.5*CALsd^2 
  nCALbins<-length(CAL_bins)-1
  CAL<-array(NA,dim=c(ns,nCALbins,ny))
  for(i in 1:ns){
    for(j in 1:ny){
      ages<-rep(1:na,CAA[i,,j])+runif(sum(CAA[i,,j]),-0.5,0.5)
      lengths<-Linf[i,j]*(1-exp(-K[i,j]*(ages-t0)))*exp(rnorm(sum(CAA[i,,j]),CALmu,CALsd))
      CAL[i,,j]<-hist(lengths,CAL_bins,plot=F)$counts
    }
  }
  CAL
} 

#install.packages("grImport")
#library(grImport)
