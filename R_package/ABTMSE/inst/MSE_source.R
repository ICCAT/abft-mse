# ===================================================================================================================
# ==== MSE source code ==============================================================================================
# ===================================================================================================================

tiny=1E-10
gettempvar<-function(targ,targsd,targgrad,nyears,nsim){   # creates a time series per simulation that has gradient grad and random normal walk wiht sigma
  mutemp<--0.5*targsd^2
  temp<-array(1,dim=c(nsim,nyears))
  for(i in 2:nyears){
    temp[,i]<-temp[,i]*exp(rnorm(nsim,mutemp,targsd))
  }
  yarray<-array(rep((1:nyears)-1,each=nsim),dim=c(nsim,nyears))
  temp<-temp*(1+targgrad/100)^yarray
  targ*temp/apply(temp,1,mean)
}

gettempvar2<-function(mur,sdr,gradr,nsim,npop,nyears){   # creates a time series per simulation that has gradient grad and random normal walk wiht sigma

  out<-new('list')
  out1<-array(1,dim=c(nsim,npop,nyears))
  targ<-array(NA,c(nsim,npop))
  targsd<-array(NA,c(nsim,npop))
  targgrad<-array(NA,c(nsim,npop))
  for(pp in 1:npop){
    targ[,pp]<-runif(nsim,mur[pp,1],mur[pp,2])
    targsd[,pp]<-runif(nsim,sdr[pp,1],sdr[pp,2])
    targgrad[,pp]<-runif(nsim,gradr[pp,1],gradr[pp,2])
    mutemp<--0.5*targsd[,pp]^2
    temp<-array(1,dim=c(nsim,nyears))
    for(i in 2:nyears)temp[,i]<-temp[,i]*exp(rnorm(nsim,mutemp,targsd[,pp]))
    yarray<-array(rep((1:nyears)-1,each=nsim),dim=c(nsim,nyears))
    temp<-temp*(1+targgrad[,pp]/100)^yarray
    out1[,pp,]<-targ[,pp]*temp/apply(temp,1,mean)
  }
  out[[1]]<-targ
  out[[2]]<-targsd
  out[[3]]<-targgrad
  out[[4]]<-out1?c
  out

}

gettempvar3<-function(bounds,nsim,npop) array(runif(nsim*npop,rep(t(bounds)[1,],each=nsim),rep(t(bounds)[2,],each=nsim)),c(nsim,npop))


densnormasc<-function(sd1,age_05,mody){
  (0.05-(dnorm(age_05,mody,sd1)/dnorm(mody,mody,sd1)))^2
}

getsdasc<-function(sm,age05,mod){
  optimize(densnormasc,interval=c(0.5,100),age_05=age05[sm],mody=mod[sm])$minimum
}

densnormdesc<-function(sd2,V_maxage,maxy,mody){
  (V_maxage-(dnorm(maxy,mody,sd2)/dnorm(mody,mody,sd2)))^2
}

getsddesc<-function(sm,Vmaxage,maxage,mod){
  optimize(densnormdesc,interval=c(0.5,10000),V_maxage=Vmaxage[sm],maxy=maxage,mody=mod[sm])$minimum
}

getDNvulnS<-function(mod,age05,Vmaxage,maxage,nsim){
  sd_asc<-sapply(1:nsim,getsdasc,age05=age05,mod=mod)
  sd_desc<-sapply(1:nsim,getsddesc,Vmaxage=Vmaxage,maxage=maxage,mod=mod)
  V<-array(NA,dim=c(nsim,maxage))
  for(i in 1:nsim){
    V[i,1:ceiling(mod[i])]<-dnorm(1:ceiling(mod[i]),mod[i],sd_asc[i])
    V[i,(1+ceiling(mod[i])):maxage]<-dnorm((1+ceiling(mod[i])):maxage,mod[i],sd_desc[i])
    V[i,(1+ceiling(mod[i])):maxage]<-V[i,(1+ceiling(mod[i])):maxage]/V[i,1+ceiling(mod[i])]#/V[i,floor(mod[i])+1]
    V[i,1:ceiling(mod[i])]<-V[i,1:ceiling(mod[i])]/dnorm(mod[i],mod[i],sd_asc[i])#,mod[i],sd_asc[i])#V[i,floor(mod[i])]

  }
  #outy<-new('list')
  #outy[[1]]<-V
  #outy[[2]]<-mod-1.18*sd_asc
  V
}

#ff<-1
#LFS<-LFS[,ff]
#L05<-L05[,ff]
#VmaxL<-VmaxL[,ff]
#nlen<-OMd@nlen
#nsim<-OMd@nsim
#lenbins<-OMd@lenbins

getDNvulnS_L<-function(LFS,L05,VmaxL,nlen,nsim,lenbins){
  sd_asc<-sapply(1:nsim,getsdasc,L05,LFS)
  sd_desc<-sapply(1:nsim,getsddesc,VmaxL,nlen,LFS)
  V<-array(NA,dim=c(nsim,nlen))
  mulen<-(lenbins[1:nlen]+lenbins[2:(nlen+1)])/2
  mulena<-array(rep(mulen,each=nsim),c(nsim,nlen))
  bkp<-apply((mulena-LFS)^2,1,which.min)

  for(i in 1:nsim){
    V[i,1:bkp[i]]<-dnorm(mulen[1:bkp[i]],LFS[i],sd_asc[i])
    V[i,1:bkp[i]]<-V[i,1:bkp[i]]/max(V[i,1:bkp[i]])
    V[i,(bkp[i]+1):nlen]<-dnorm(mulen[(bkp[i]+1):nlen],LFS[i],sd_desc[i])
    V[i,(bkp[i]+1):nlen]<-V[i,(bkp[i]+1):nlen]/dnorm(mulen[bkp[i]],LFS[i],sd_desc[i])#/V[i,floor(mod[i])+1]
  }
  #outy<-new('list')
  #outy[[1]]<-V
  #outy[[2]]<-mod-1.18*sd_asc
  V
}


fitThompson<-function(LFS,L05,VmaxL,nlen,nsim,ploty=F){

  V<-array(NA,dim=c(nsim,nlen))
  parstore<-array(NA,c(nsim,3))
  for(i in 1:nsim){

    #opt<-optim(par=c(0.1,0.2),Thompsonfit,
    #           lower=c(0.1,0.1),
    #           upper=c(0.3,0.6),
    #           method="L-BFGS-B",LFSc=LFS[i],
    #           L05c=L05[i],VmaxLc=VmaxL[i],nlen=nlen)


    opt<-optim(par=c(0.1,0.2,LFS[i]),Thompsonfit,
              LFSc=LFS[i],
              L05c=L05[i],VmaxLc=VmaxL[i],nlen=nlen)
    sel<-thompson(c(opt$par,LFS[i]),nlen)
    if(ploty){

     plot(sel)
     abline(h=0.05,col='green')
     abline(v=L05[i],col='green')
     abline(h=1,col='red')
     abline(v=LFS[i],col='red')
     abline(h=VmaxL[i],col='blue')
    }

    V[i,]<-sel
    parstore[i,]<-opt$par
  }
  out<-new('list')
  out[[1]]<-V
  out[[2]]<-parstore
  out
}


Thompsonfit<-function(par,LFSc,L05c,VmaxLc,nlen){

  sel<-thompson(pars=par,na=nlen)
  obj<-(sel[L05c]-0.05)^2+(sel[LFSc]-1)^2+(sel[nlen]-VmaxLc)^2
  return(obj)
}

thompson<-function(pars,na){ # pars(dome, rprecision, peak) na is maximum age
  (
    (1/(1-pars[1]))*
      ((1-pars[1])/pars[1])^pars[1]
  )*(
    exp(pars[2]*pars[1]*(pars[3]-(1:na)))
    /(1+exp(pars[2]*(pars[3]-(1:na))))
  )
}


getFhist<-function(nsim,Esd,nyears,dFmin,dFmax,bb){

  ne<-nsim*3                                                         # Number of simulated effort datasets
  dEfinal<-runif(ne,dFmin,dFmax)#(exp(rnorm(ne,mean=demu,sd=desd))-1)*6               # Sample the final gradient in effort
  a<-(dEfinal-bb)/nyears                                         # Derive slope to get there from intercept
  a<-array(a,dim=c(ne,nyears))                                  # Slope array
  bb<-array(bb,dim=c(ne,nyears))                                  # Intercept array
  x<-array(rep(1:nyears,each=ne),dim=c(ne,nyears))              # Year array
  dE<-a*x+bb                                                     # Change in effort
  E<-array(NA,dim=c(ne,nyears))                                 # Define total effort array
  E[,1]<-dE[,1]
  for(y in 2:nyears){
    E[,y]<-apply(dE[,1:y],1,sum)
  }
  E<-E/array(apply(E,1,mean),dim=c(ne,nyears))                  # Standardise Effort to average 1
  cond<-apply(E,1,min)>0
  pos<-(1:ne)[cond]
  pos<-pos[1:nsim]
  #environment("dEfinal")<-asNamespace('DLMtool')#assign("dFfinal",dEfinal[pos],envir=.GlobalEnv)

  E<-E[pos,]                                 # Sample only those without negative effort
  Emu<--0.5*Esd^2
  Eerr<-array(exp(rnorm(nyears*nsim,rep(Emu,nyears),rep(Esd,nyears))),c(nsim,nyears))
  outy<-new('list')
  outy[[1]]<-E*Eerr
  outy[[2]]<-dEfinal[pos]
  outy
}

simmov<-function(mov,movvar,movsd,movgrad,nsim,npop,nages,nyears,nsubyears,nareas){

  #nsim<-OMd@nsim
  #mov<-OMd@mov
  #movvar<-.Object@movvar
  #movsd<-.Object@movsd
  #movgrad<-.Object@movgrad
  #npop<-.Object@npop
  #nages<-.Object@nages
  #nyears<-.Object@nyears
  #nsubyears<-.Object@nsubyears
  #nareas<-.Object@nareas

  vara<-array(rnorm(nsim*npop*nages*nsubyears*nareas*nareas,0,rep(movvar,nages*nsubyears*nareas*nareas)),dim=c(nsim,npop,nages,nsubyears,nareas,nareas))
  sda<-array(rnorm(nsim*prod(dim(mov)),0,rep(movsd,nages*nyears*nsubyears*nareas*nareas)),dim=c(nsim,npop,nages,nyears,nsubyears,nareas,nareas))
  smov<-array(NA,c(nsim,dim(mov)))
  tmov<-log(mov/(1-mov)) # logit transform
  tmov[tmov==Inf]<-0
  ind<-as.matrix(expand.grid(1:nsim,1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nareas))
  indv<-ind[,c(1,2,3,5,6,7)]
  indm<-ind[,2:7]
  smov[ind]<-tmov[indm]+vara[indv]+sda[ind]
  ssum<-apply(exp(smov),1:6,sum)
  exp(smov)/array(ssum,c(dim(ssum),nareas))

}


simmov2<-function(mov,movvar,nsim,npop,nages,nsubyears,nareas){

  #mov<-array(NA,c(npop,nages,nsubyears,nareas,nareas))
  vara<-array(rnorm(npop*nsim,1,movvar),dim=c(nsim,npop,nages,nsubyears,nareas,nareas))
  smov<-array(NA,c(nsim,dim(mov)))
  tmov<-log(mov/(1-mov)) # logit transform
  tmov[tmov==Inf]<-0
  ind<-TEG(c(nsim,npop,nages,nsubyears,nareas,nareas))
  indm<-ind[,2:6]
  smov[ind]<-tmov[indm]*vara[ind]
  ssum<-apply(exp(smov),1:5,sum)
  exp(smov)/array(ssum,c(dim(ssum),nareas))

}

simmov3<-function(mov,nsim){ # replicate movement by nsims

 array(rep(mov,each=nsim),c(nsim,dim(mov)))

}

simmov4<-function(OMd){ # replicate movement by nsims

  vara<-array(rnorm(OMd@npop*OMd@nsim,1,rep(OMd@movvar,each=OMd@nsim)),dim=c(OMd@nsim,OMd@npop,OMd@nages,OMd@nsubyears,OMd@nareas,OMd@nareas))
  smov<-array(NA,c(OMd@nsim,dim(OMd@mov)))
  tmov<-log(OMd@mov/(1-OMd@mov)) # logit transform
  tmov[tmov==Inf]<-0
  ind<-TEG(c(OMd@nsim,OMd@npop,OMd@nages,OMd@nsubyears,OMd@nareas,OMd@nareas))
  indm<-ind[,2:6]
  smov[ind]<-tmov[indm]*vara[ind]
  ssum<-apply(exp(smov),1:5,sum)
  exp(smov)/array(ssum,c(dim(ssum),OMd@nareas))
}



domov<-function(Ntemp,movtemp){ # S P A R  x  S P A R R
  #Ntemp<-Idist
  #movtemp<-.Object@mov[,,,1,1,,]
  nareas<-dim(movtemp)[5]
  apply(array(Ntemp, c(dim(Ntemp),nareas))*movtemp,c(1,2,3,5),sum)
}

domov2<-function(Ntemp,movtemp){ # P A R  x  P A R R
  #Ntemp<-Idist
  #movtemp<-.Object@mov[,,,1,1,,]
  nareas<-dim(movtemp)[4]
  apply(array(Ntemp, c(dim(Ntemp),nareas))*movtemp,c(1,2,4),sum)
}

domov3<-function(Ntemp,movtemp){ # S P R  x  S P R R
  #Ntemp<-Idist
  #movtemp<-.Object@mov[,,,1,1,,]
  nareas<-dim(movtemp)[3]
  apply(array(Ntemp, c(dim(Ntemp),nareas))*movtemp,c(1,2,4),sum)
}


assignsomevals<-function(){
  .Object<-OM
  mode=1
  toly=1e-1
  ss<-1
  npop=.Object@npop
  nages=.Object@nages
  nlen=.Object@nlen
  nyears=.Object@nyears
  nsubyears=.Object@nsubyears
  nareas=.Object@nareas
  nfleets=.Object@nfleets
  R0=.Object@R0[ss,]
  targD=.Object@D[ss,]
  M=.Object@M[ss,,,]
  mat=.Object@mat[ss,,,]
  #Idist=.Object@Idist[ss,,,]
  Wt_age=.Object@Wt_age[ss,,,]
  iALK=.Object@iALK[ss,,,,]
  sel=.Object@sel[ss,,]
  E=.Object@E[ss,,,,]
  Spat_targ=.Object@Spat_targ[ss,]
  mov=.Object@mov[ss,,,,,]
  Recsubyr=.Object@Recsubyr
  h=.Object@h[ss,]
  Recdevs=.Object@Recdevs[ss,,]
  SRrel=.Object@SRrel
  targpop=1
  targdep=.Object@D[ss,.Object@targpop]
  ratF=.Object@Frat[ss,]
  MSYyear=NA
  nZeq=.Object@nZeq
  nydist=.Object@nydist
  nyeq=.Object@nyeq
  tol=toly

  par=log(0.05)
  qs<-.Object@q[ss,]

  # For getMSYrefs
  par=log(sum(.Object@q[ss,]))
  nyears<-20
  mode<-1
  MSYyear<-.Object@nyears
  targpop<-1

}



getF4dep<-function(ss,.Object,toly=1e-3){
  #system.time({
  #method="L-BFGS-B",lower=c(log(0.01),rep(-4,.Object@nfleets-1)),  upper=c(log(2),rep(4,.Object@nfleets-1)),

  test<-optimize(popdyn,log(c(0.01,4)),mode=3,npop=.Object@npop,nages=.Object@nages,nlen=.Object@nlen,nyears=.Object@nyears,
              nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
              R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
              Wt_age=.Object@Wt_age[ss,,,],iALK=.Object@iALK[ss,,,,],
              sel=.Object@sel[ss,,], E=.Object@E[ss,,,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
              Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
              Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=.Object@D[ss,.Object@targpop],
              ratF=.Object@Frat[ss,],nZeq=.Object@nZeq,nydist=.Object@nydist,nyeq=.Object@nyeq,MSYyear=NA,tol=toly)
  #})
  test2<-popdyn(test$minimum,mode=6,npop=.Object@npop,nages=.Object@nages,nlen=.Object@nlen,nyears=.Object@nyears,
                nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
                R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
                Wt_age=.Object@Wt_age[ss,,,],iALK=.Object@iALK[ss,,,,],
                sel=.Object@sel[ss,,], E=.Object@E[ss,,,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
                Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
                Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=.Object@D[ss,.Object@targpop],
                ratF=.Object@Frat[ss,],nZeq=.Object@nZeq,nydist=.Object@nydist,nyeq=.Object@nyeq,MSYyear=NA)

  c(test$minimum,test2)
}

getMSYrefs<-function(ss,.Object,nyears=40,toly=1e-3){
  #system.time({
  #method="L-BFGS-B",lower=c(log(0.01),rep(-4,.Object@nfleets-1)),  upper=c(log(2),,rep(4,.Object@nfleets-1))
  test<-optimize(popdyn,log(c(0.0001,0.5)),mode=1,npop=.Object@npop,nages=.Object@nages,nlen=.Object@nlen,nyears=nyears,
              nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
              R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
              Wt_age=.Object@Wt_age[ss,,,],iALK=.Object@iALK[ss,,,,],
              sel=.Object@sel[ss,,], E=.Object@E[ss,,,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
              Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
              Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=NA,
              ratF=.Object@Frat,nZeq=.Object@nZeq,nydist=.Object@nydist,nyeq=.Object@nyeq,MSYyear=.Object@nyears,tol=toly)
  #})

  popdyn(test$minimum,mode=4,npop=.Object@npop,nages=.Object@nages,nlen=.Object@nlen,nyears=nyears,
         nsubyears=.Object@nsubyears,nareas=.Object@nareas,nfleets=.Object@nfleets,
         R0=.Object@R0[ss,], targD=.Object@D[ss,],M=.Object@M[ss,,,],mat=.Object@mat[ss,,,],
         Wt_age=.Object@Wt_age[ss,,,],iALK=.Object@iALK[ss,,,,],
         sel=.Object@sel[ss,,], E=.Object@E[ss,,,,], Spat_targ=.Object@Spat_targ[ss,], mov=.Object@mov[ss,,,,,],
         Recsubyr=.Object@Recsubyr, h=.Object@h[ss,],
         Recdevs=.Object@Recdevs[ss,,],SRrel=.Object@SRrel,targpop=.Object@targpop,targdep=NA,
         ratF=.Object@Frat,nZeq=.Object@nZeq,nydist=.Object@nydist,nyeq=.Object@nyeq,MSYyear=.Object@nyears)
}

popdyn<-function(par,.Object,mode,npop,nages,nlen,nyears,nsubyears,nareas,nfleets,R0,targD,M,mat,
                Wt_age,iALK,sel, E, Spat_targ, mov, Recsubyr, h, Recdevs,SRrel,
                targpop=NA,targdep=0.2,ratF=0.5,nZeq=5,nydist=10,nyeq,MSYyear=1){

  totF<-exp(par)

  #cat<-rep(NA,10)
  #totFs<-seq(0.1,0.2,length.out=10)
  #for(tF in 1:10){
  #totF<-totFs[tF]

  #ratF<-exp(par[2:length(par)])/(1+sum(exp(par)[2:length(par)]))
  qs<-as.numeric(totF*ratF)  # make the optimizer find equivalent annual F (to avoid mistakes and aid in initializaton)


  agearray<-array(rep(1:nages,each=npop),c(npop,nages))
  #Nfrac<-surv*mat[,,1]
  #Len_age<-Wt_age<-array(NA,c(npop,nages,nyears))
  ind<-indo<-as.matrix(expand.grid(1:npop,1:nages,1:nyears))

  if(mode%in%c(1,2,4,5)){# if MSY refs

    indo[,3]<-MSYyear
    M<-array(M[indo],c(npop,nages,nyears))
    mat<-array(mat[indo],c(npop,nages,nyears))
    Recdevs<-array(1,c(npop,nyears))
    Wt_age<-array(Wt_age[,,MSYyear],c(npop,nages,nyears))

  }

  Mtemp<-cbind(rep(0,npop),M[,1:(nages-1),1])
  surv=t(exp(-apply(Mtemp,1,cumsum)))
  surv[,nages]<-surv[,nages]*exp(-M[,nages,1])/(1-exp(-M[,nages,1]))# this adds the indefinite integral to the final survival plus group

  N<-SSB<-Z<-array(NA,c(npop,nages,nyears,nsubyears,nareas)) # only need aggregated catch for these purposes
  FD<-array(NA,c(nfleets,nyears,nsubyears,nareas))              # Fishing distribution
  FM<-VB<-C<-array(NA,c(npop,nages,nyears,nsubyears,nareas,nfleets))

  mref<-c(2:nsubyears,1)  # The correct subyear to get for each mov post
  y<-1
  m<-1

  RFL<-array(NA,c(nfleets,nlen,nyears,nsubyears,nareas))
  indL<-indLo<-TEG(dim(RFL))
  if(mode%in%c(1,2,4,5))indLo[,3]<-MSYyear # if MSY refs
  RFL[indL]<-qs[indLo[,1]]*sel[indLo[,1:2]]*E[indLo[,c(1,3,4,5)]]

  Ftrans<-array(0,c(npop,nages,nyears,nsubyears,nareas,nfleets,nlen))
  Find<-Find2<-TEG(dim(Ftrans))

  if(mode%in%c(1,2,4,5)){ # if MSY refs
    Find2[,3]<-MSYyear
  }

  Lind<-Find[,c(6,7,3,4,5)]
  Ftrans[Find]<-iALK[Find2[,c(1,3,2,7)]]*RFL[Lind]
  FM<-apply(Ftrans,1:6,sum) # p a y m r f

  maxRF<-apply(FM,c(1,3,4,5,6),max)
  Rind<-as.matrix(expand.grid(1:npop,1:nages,1:nyears,1:nareas,1:nfleets))
  Rind<-TEG(c(npop,nages,nyears,nsubyears,nareas,nfleets))
  sel2<-FM
  sel2[Rind]<-sel2[Rind]/maxRF[Rind[,c(1,3,4,5,6)]]
  sel2<-sel2[,,nyears,nsubyears,,] # Take this from last year, in future simulations this may be by year so leave this code!
  sel2[is.na(sel2)]<-0

  FAYMR<-as.matrix(expand.grid(1:nfleets,1:nages,y,m,1:nareas)) # Set up some array indexes
  FAY<-FAYMR[,1:3]

  PAYMR<-as.matrix(expand.grid(1:npop,1:nages,y,m,1:nareas))    # Set up some array indexes
  PA<-PAYMR[,1:2]
  PR<-PAYMR[,c(1,5)]
  PMR<-PAYMR[,c(1,4,5)]
  P<-PAYMR[,1]
  PAR<-PAYMR[,c(1,2,5)]
  PAY<-PAYMR[,1:3]
  PAM<-PAYMR[,c(1,2,4)]


  # New model initialization -----------------------
  #      pay         paymrf
  Zeq<-array(apply(M[,,1:nZeq],1:2,mean),c(npop,nages,nsubyears,nareas))/nsubyears+apply(apply(FM[,,1:nZeq,,,],1:5,sum),c(1,2,4,5),mean)
  SSB0<-apply(surv*R0*Wt_age[,,nyears]*mat[,,nyears],1,sum)
  #SSB0<-SSB0+(exp(-M[,nages,1])/(1-exp(-M[,nages,1]))*R0*surv[,nages]*Wt_age[,nages,1]*mat[,nages,1])
    # validate indefinite integral: apply(exp(-M[,nages,1])^array(rep(1:100,each=npop),dim=c(npop,100)),1,sum)*(R0*surv[,nages]*Wt_age[,nages,1]*mat[,nages,1])
  SSBpR<-SSB0/R0

  stemp<-array(1/nareas,dim=c(npop,nsubyears,nareas))
  movi<-mov[,nages,,,]

  for(y in 1:nydist){

    for(m in 1:nsubyears){

      if(m==1){

        stemp[,m,]<-apply(array(rep(stemp[,nsubyears,],nareas)*movi[,m,,],c(npop,nareas,nareas)),c(1,3),sum)

      }else{

        stemp[,m,]<-apply(array(rep(stemp[,m-1,],nareas)*movi[,m,,],c(npop,nareas,nareas)),c(1,3),sum)

      }

    }

  }

  indN<-as.matrix(expand.grid(1:npop,1:nages,1,nsubyears,1:nareas))
  N[indN]=R0[indN[,1]]*surv[indN[,1:2]]*stemp[indN[,c(1,4,5)]]
  SSB[,,1,nsubyears,]<-N[,,1,nsubyears,]*rep(Wt_age[,,nyears],nareas)*rep(mat[,,nyears],nareas)

  for(y in 1:nyeq){

    for(m in 1:nsubyears){

      if(m==1){ # first subyear

        N[,,1,m,]<-exp(-Zeq[,,nsubyears,])*N[,,1,nsubyears,]
        N[,,1,m,]<-domov2(N[,,1,m,],mov[,,m,,])
        SSB[,,1,m,]<-N[,,1,m,]*rep(Wt_age[,,nyears],nareas)*rep(mat[,,nyears],nareas)

      }else if(m==2){ # spawning subyear

        N[,,1,m,]<-exp(-Zeq[,,m-1,])*N[,,1,m-1,]
        N[,,1,m,]<-domov2(N[,,1,m,],mov[,,m,,])
        SSB[,,1,m,]<-N[,,1,m,]*rep(Wt_age[,,nyears],nareas)*rep(mat[,,nyears],nareas)
        spawnr<-apply(SSB[,,1,m,],c(1,3),sum)/apply(SSB[,,1,m,],1,sum)

        SSBt<-apply(SSB[,,1,m,],1,sum)
        N[,nages,1,m,]<-N[,nages,1,m,]+N[,nages-1,1,m,]
        N[,2:(nages-1),1,m,]<-N[,1:(nages-2),1,m,]
        N[,1,1,m,]<-spawnr*((0.8*R0*h*SSBt)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSBt))

      }else{   # after spawning subyear

        N[,,1,m,]<-exp(-Zeq[,,m-1,])*N[,,1,m-1,]
        N[,,1,m,]<-domov2(N[,,1,m,],mov[,,m,,])
        SSB[,,1,m,]<-N[,,1,m,]*rep(Wt_age[,,nyears],nareas)*rep(mat[,,nyears],nareas)

      }# End of if subyear
    }  # end of subyear
  }    # end of equlibrium calculation year nyeq

  bR<-log(5*h)/(0.8*SSB0)                                     # Ricker SR params
  aR<-exp(bR*SSB0)/SSBpR                                       # Ricker SR params

  if(mode%in%c(1,2,4,5)){ # Start MSY optimization at approx 50% B0

    N<-N*0.5
    SSB<-SSB*0.5

  }

  PAYMRF2<-as.matrix(expand.grid(1:npop,1:nages,y,m,1:nareas,1:nfleets))
  PAYRF2<-PAYMRF2[,c(1,2,3,5,6)]
  F2<-PAYMRF2[,6]
  PAY2<-PAYMRF2[,c(1,2,3)]
  FAY2<-PAYMRF2[,c(6,2,3)]
  FYMR2<-PAYMRF2[,c(6,3,4,5)]
  PAYMR2<-PAYMRF2[,1:5]
  FA2<-PAYMRF2[,c(6,2)]
  FAR2<-PAYMRF2[,c(6,2,5)]
  FAYR2<-PAYMRF2[,c(6,2,3,5)]
  PARF2<-PAYMRF2[,c(1,2,5,6)]

  for(m in 1:nsubyears){

    PAYMRF2[,4]<-m
    PAYMR2<-PAYMRF2[,1:5]
    PAYMR[,4]<-m
    VB[PAYMRF2]<-N[PAYMR2]*Wt_age[PAY2]*sel2[PARF2]                    # Calculate vunerable biomassp a y m r f
    Ftot<-apply(FM[,,y,m,,],c(1,2,3),sum)
    Z[PAYMR]<-Ftot[PAR]+M[PAY]/nsubyears

  }

  PAYMR[,4]<-1
  PAYMRF2[,4]<-1
  PAYMR2<-PAYMRF2[,1:5]

  for(y in 2:nyears){

    PAYMR[,3]<-y
    PAY<-PAYMR[,1:3]
    PAYMRF2[,3]<-y
    PAY2<-PAYMRF2[,c(1,2,3)]
    FAY2<-PAYMRF2[,c(6,2,3)]
    FAYR2<-PAYMRF2[,c(6,2,3,5)]
    PAYRF2<-PAYMRF2[,c(1,2,3,5,6)]

    for(m in 1:nsubyears){

      PAYMR[,4]<-m
      PAM<-PAYMR[,c(1,2,4)]
      PAYMRF2[,4]<-m
      FYMR2<-PAYMRF2[,c(6,3,4,5)]
      PAYMR2<-PAYMRF2[,1:5]

      if(m==1){
        N[,,y,m,]<-N[,,y-1,nsubyears,]*exp(-Z[,,y-1,nsubyears,])
      }else{
        N[,,y,m,]<-N[,,y,m-1,]*exp(-Z[,,y,m-1,])
      }

      # move fish
      if(mode%in%c(1,2,4,5)){# if MSY refs might have to change this is movement is time-varying
        N[,,y,m,]<-domov2(N[,,y,m,],mov[,,m,,])
      }else{
        N[,,y,m,]<-domov2(N[,,y,m,],mov[,,m,,])
      }

      VB[PAYMRF2]<-N[PAYMR2]*Wt_age[PAY2]*sel2[PARF2]                    # Calculate vunerable biomass
      Ftot<-apply(FM[,,y,m,,],c(1,2,3),sum)
      Z[PAYMR]<-Ftot[PAR]+M[PAY]/nsubyears

      C[PAYMRF2]<-N[PAYMR2]*(1-exp(-Z[PAYMR2]))*(FM[PAYMRF2]/Z[PAYMR2]) # Calculate catches

      for(pp in 1:npop){
        if(Recsubyr[pp]==m){
          spawnr<-apply(SSB[pp,,y-1,m,],2,sum)/sum(SSB[pp,,y-1,m,]) # lag by one year
          SSBt<-sum(SSB[pp,,y-1,m,])# lag by one year
          N[pp,nages,y,m,]<-N[pp,nages,y,m,]+N[pp,nages-1,y,m,]
          N[pp,2:(nages-1),y,m,]<-N[pp,1:(nages-2),y,m,] # age fish

          if(SRrel[pp]==1){    # Beverton-Holt recruitment
            N[pp,1,y,m,]<-Recdevs[pp,y]*spawnr*((0.8*R0[pp]*h[pp]*SSBt)/(0.2*SSBpR[pp]*R0[pp]*(1-h[pp])+(h[pp]-0.2)*SSBt))
          }else{              # Most transparent form of the Ricker uses alpha and beta params
            N[pp,1,y,m,]<-Recdevs[pp,y]*spawnr*aR[pp]*SSBt*exp(-bR[pp]*SSBt)
          }
          SSB[pp,,y,m,]<-N[pp,,y,m,]*array(Wt_age[pp,,nyears]*mat[pp,,nyears],dim=c(nages,nareas))

        } # if its the right subyear
      } # end of pop

    } # end of subyear
  } # end of year


  if(mode==1)return(-log(sum(
    array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
    array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)))))
  if(mode==2)return(-log(sum(
    array(C[,,nyears,,,],c(npop,nages,nsubyears,nareas,nfleets))*
    array(Wt_age[,,nyears],c(npop,nages,nsubyears,nareas,nfleets)))))
  if(mode==3){
    Bcur<-sum(array(N[targpop,,nyears,nsubyears,],c(length(targpop),nages,nareas))*
              array(Wt_age[targpop,,nyears]*mat[targpop,,nyears],c(length(targpop),nages,nareas)))
    D<-Bcur/sum(SSB0[targpop])
    #print("---")
    #print(par)
    #print(exp(par))
    #print(D)
    #Crat<-apply(C[targpop,,nyears,,,]*array(Wt_age[targpop,,nyears],c(nages,nsubyears,nareas,nfleets)),4,sum)
    #Crat<-(Crat/sum(Crat))[1:(nfleets-1)]
    #print(paste(D,Crat))
    obj<-((log(D)-log(mean(targdep)))^2)
    #for(i in 1:(nfleets-1))obj<-obj+(log(Crat[i])-log(targC[i]))^2
    return(obj)
  }
  if(mode==4){
    MSY<-sum(array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
             array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)))
    BMSY<-sum(
      array(N[targpop,,nyears,1,],c(length(targpop),nages,nareas))*
      array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))

   # sof<-array(NA,c(nsubyears,nfleets,nages,nareas))
    #ind<-TEG(c(nsubyears,nfleets,nages,nareas))
    #indE<-cbind(ind[,2],rep(MSYyear,nrow(ind)),ind[,1],ind[,4])
    #sind<-cbind(rep(targpop,nrow(ind)),ind[,c(3,4,2)])
    #sof[ind]<-E[indE]*sel2[sind]*qs[ind[,2]]
    #sof<-apply(sof,3:4,sum)

    #sof<-array(NA,c(nfleets,nages,nareas))
    #ind<-TEG(c(nfleets,nages,nareas))
    #indE<-cbind(ind[,1],rep(MSYyear,nrow(ind)),ind[,3])
    #sind<-cbind(rep(targpop,nrow(ind)),ind[,c(2,3,1)])
    #sof[ind]<-E[indE]*sel2[sind]*qs*4

    #sof<-apply(sof,2:3,sum)
    #FMSYa<-max(sof)
    #sof<-sof/rep(apply(sof,2,max),each=nages)

    VBMSY<-NA#sum(array(sof,c(1,dim(sof)))*
      #array((SSN[targpop,,nyears,1,]+NSN[targpop,,nyears,1,]),c(length(targpop),nages,nareas))*
      #  array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))

    SSBMSY<-sum(array(N[targpop,,nyears,1,],c(length(targpop),nages,nareas))*array(Wt_age[targpop,,nyears]*mat[targpop,,nyears],c(length(targpop),nages,nareas)))
    UMSY<-MSY/BMSY
    FMSYa=-log(1-UMSY)
    SSBMSY_B0<-SSBMSY/sum(SSB0[targpop])
    return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSYa,SSBMSY_B0))
  }
  if(mode==5){
    MSY<-sum(array(C[,,nyears,,,],c(npop,nages,nsubyears,nareas,nfleets))*
            array(Wt_age[,,nyears],c(npop,nages,nsubyears,nareas,nfleets)))
    BMSY<-sum(
          array(N[,,nyears,1,],c(npop,nages,nareas))*
          array(Wt_age[,,nyears],c(npop,nages,nareas)))

   #sof<-array(NA,c(nfleets,nages,nareas))
   #ind<-TEG(c(nfleets,nages,nareas))
   #indE<-cbind(ind[,1],rep(MSYyear,nrow(ind)),ind[,3])
   #sind<-cbind(rep(targpop,nrow(ind)),ind[,c(2,3,1)])
   #sof[ind]<-E[indE]*sel2[sind]*qs*4

   #sof<-apply(sof,2:3,sum)
   #FMSYa<-max(sof)
   #sof<-sof/rep(apply(sof,2,max),each=nages)

    VBMSY<-NA#sum(array(rep(sof,each=npop),c(npop,nages,nareas))*
              #   array((SSN[,,nyears,1,]+NSN[,,nyears,1,]),c(npop,nages,nareas))*
               #  array(Wt_age[,,nyears],c(npop,nages,nareas)))

    SSBMSY<-sum(array(N[,,nyears,nsubyears,],c(npop,nages,nareas))*
                array(Wt_age[,,nyears]*mat[,,nyears],c(npop,nages,nareas)))
    UMSY<-MSY/BMSY
    FMSYa<--log(1-UMSY)
    SSBMSY_B0<-SSBMSY/sum(SSB0)
    return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSYa,SSBMSY_B0))
  }
  if(mode==6){
    Bcur<-sum(array(N[targpop,,nyears,nsubyears,],c(length(targpop),nages,nareas))*
                array(Wt_age[targpop,,nyears]*mat[targpop,,nyears],c(length(targpop),nages,nareas)))
    D<-Bcur/sum(SSB0[targpop])
    #Crat<-apply(array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
    #              array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)),5,sum)
    #Crat<-(Crat/sum(Crat))[1:(nfleets-1)]
    #print(paste(D,Crat))
    return(c(D,targdep,ratF))
  }
}#


dummyfunc<-function(){
  ss<-1
  par<-log(0.01)
  mode<-3
  npop<-.Object@npop
  nages<-.Object@nages
  nyears<-.Object@nyears
  #nyears<-.Object100
  nsubyears<-.Object@nsubyears
  nareas<-.Object@nareas
  nfleets<-.Object@nfleets
  R0<-.Object@R0[ss,]
  targD<-.Object@D[ss,]
  M=.Object@M[ss,,,]
  mat=.Object@mat[ss,,,]
  Idist=.Object@Idist[ss,,,]
  MIdist=.Object@MIdist[ss,,,]
  t0=.Object@t0
  Linf=.Object@Linf[ss,,]
  K=.Object@K[ss,,]
  a=.Object@a
  b=.Object@b
  sel=.Object@sel[ss,,]
  E=.Object@E[ss,,]
  Spat_targ=.Object@Spat_targ[ss,]
  mov<-.Object@mov[ss,,,,]
  Recsubyr<-.Object@Recsubyr
  h<-.Object@h[ss,]
  Recdevs=.Object@Recdevs[ss,,]
  SRrel=.Object@SRrel
  targpop<-1:3
  targdep<-0.1
  MSYyear<-43

  ratF=0.5
}

invent_mov<-function(gravs, visc,notmat,excl,nages,nyears){
  movs<-new('list') # mov and Mmov (non-mature and Mature movement)
  npop<-dim(gravs)[1]
  nareas<-dim(gravs)[3]
  nsubyears<-dim(gravs)[2]
  mov<-array(NA,c(npop,nages,nyears,nsubyears,nareas,nareas))
  ind<-as.matrix(expand.grid(1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nareas))
  mov[ind]<-gravs[ind[,c(1,4,6)]]
  mov[ind[,c(1,2,3,4,5,5)]]<-mov[ind[,c(1,2,3,4,5,5)]]+visc[ind[,c(1,4)]]
  mov<-exp(mov)
  mov[ind]<-mov[ind]*excl[ind[,c(1,6)]]
  Mmov<-mov/array(apply(mov,1:5,sum),dim(mov))
  mov[ind]<-mov[ind]*notmat[ind[,c(1,4,6)]]
  mov<-mov/array(apply(mov,1:5,sum),dim(mov))
  movs[[1]]<-mov
  movs[[2]]<-Mmov
  movs
}


invent_age_mov<-function(gravs,visc,nma,ma,excl,nages,nyears){
  npop<-dim(gravs)[1]
  nma<-dim(gravs)[2]
  nareas<-dim(gravs)[4]
  nsubyears<-dim(gravs)[3]
  mov<-array(NA,c(npop,nages,nyears,nsubyears,nareas,nareas))
  ind<-as.matrix(expand.grid(1:npop,1:nages,1:nyears,1:nsubyears,1:nareas,1:nareas))
  ind2<-ind
  ind2[,2]<-ma[ind2[,2]]
  mov[ind]<-gravs[ind2[,c(1,2,4,6)]]
  mov[ind[,c(1,2,3,4,5,5)]]<-mov[ind[,c(1,2,3,4,5,5)]]+visc[ind2[,c(1,2,4)]]
  mov<-exp(mov)
  mov[ind]<-mov[ind]*excl[ind[,c(1,6)]]
  mov<-mov/array(apply(mov,1:5,sum),dim(mov))
  mov
}


invent_mov2<-function(gravs, visc,notmat,excl,nages){
  movs<-new('list') # mov and Mmov (non-mature and Mature movement)
  npop<-dim(gravs)[1]
  nareas<-dim(gravs)[3]
  nsubyears<-dim(gravs)[2]
  mov<-array(NA,c(npop,nages,nsubyears,nareas,nareas))
  ind<-as.matrix(expand.grid(1:npop,1:nages,1:nsubyears,1:nareas,1:nareas))
  mov[ind]<-gravs[ind[,c(1,3,5)]]
  mov[ind[,c(1,2,3,4,4)]]<-mov[ind[,c(1,2,3,4,4)]]+visc[ind[,c(1,3)]]
  mov<-exp(mov)
  mov[ind]<-mov[ind]*excl[ind[,c(1,5)]]
  Mmov<-mov/array(apply(mov,1:4,sum),dim(mov))
  mov[ind]<-mov[ind]*notmat[ind[,c(1,3,5)]]
  mov<-mov/array(apply(mov,1:4,sum),dim(mov))
  movs[[1]]<-mov
  movs[[2]]<-Mmov
  movs
}


invent_mov3<-function(gravs, visc,OMd){

  # gravs (nma,npop,nsubyears,nareas)
  # visc (nma,npop,nsubyears)
  # OMd@excl (npop, nareas)

  npop<-OMd@npop
  nareas<-OMd@nareas
  nsubyears<-OMd@nsubyears
  nareas<-OMd@nareas
  nages<-OMd@nages
  nma<-OMd@nma

  mov<-array(NA,c(npop,nages,nsubyears,nareas,nareas))
  gind<-ind<-as.matrix(expand.grid(1:npop,1:nages,1:nsubyears,1:nareas,1:nareas))
  gind[,2]<-OMd@ma[ind[,c(2,1)]]


  mov[ind]<-gravs[gind[,c(2,1,3,5)]]
  mov[ind[,c(1,2,3,4,4)]]<-mov[ind[,c(1,2,3,4,4)]]+visc[gind[,c(2,1,3)]]
  mov<-exp(mov)
  mov[ind]<-mov[ind]*OMd@excl[ind[,c(1,5)]]
  mov<-mov/array(apply(mov,1:4,sum),dim(mov))
  mov # p a m r r

}


tomt<-function(arr){
  dim<-new('list')
  dims<-dim(arr)
  ndims<-length(dims)
  for(i in 1:ndims)dim[[i]]<-1:dims[i]
  ind<-as.matrix(expand.grid(dim))
  out<-array(NA,dims[ndims:1])
  out[ind[,ndims:1]]<-arr[ind]
  out
}

TEG<-function(arr){ # make index for list calculation
  dim<-new('list')
  ndims<-length(arr)
  for(i in 1:ndims)dim[[i]]<-1:arr[i]
  as.matrix(expand.grid(dim))
}

sdensplot<-function(dens,areadefs,ncolgrad=200,colpal='heat'){

  xlimy<-c(-95,40)
  ylimy<-c(-25,60)
  plot(xlimy,ylimy,col="white",axes=F,xlab="",ylab="")
  nbins<-ncolgrad
  bins<-seq(min(dens),max(dens),length.out=nbins+1)
  y<-ceiling((dens-min(dens))/(max(dens)-min(dens))*nbins)
  y[y==0]<-1
  if(colpal=="heat")cols<-heat.colors(ncolgrad,alpha=1)[ncolgrad:1]
  if(colpal=="gray")cols<-gray.colors(ncolgrad, start = 0.25, end = 0.985, gamma = 2.2, alpha = NULL)[ncolgrad:1]

  for(i in 1:length(OMd@Area_names)){
    polygon(OMd@Area_defs[[i]],col=cols[y[i]],border='white',density=NA)
    #text(mean(OMd@Area_defs[[i]]$x),mean(OMd@Area_defs[[i]]$y),OMd@Area_names[i],col='white',font=2,cex=0.8)
  }
  map(xlim=xlimy,ylim=ylimy,add=T,fill=T,col="light grey")
}

exportFLR<-function(){
  assign("FLIndex", FLIndex, envir=globalenv())
  assign("FLQuant",FLQuant,envir=globalenv())
  assign("FLStock",FLStock,envir=globalenv())
  assign("FLXSA.control",FLXSA.control,envir=globalenv())
  assign("FLIndices",FLIndices,envir=globalenv())
  assign("FLXSA",FLXSA,envir=globalenv())
  sfExport(list=c("FLIndex","FLQuant","FLStock","FLXSA.control","FLIndices","FLXSA","getrefs","getMSYrefs2"))
}

ADMBrep<-function(repfile,st,ADMBdim,quiet=T)  tomt(array(scan(repfile,skip=st,nlines=prod(ADMBdim[1:(length(ADMBdim)-1)]),quiet=quiet),ADMBdim[length(ADMBdim):1]))


read.fit<-function(file="C:/M3"){
  # ! Borrowing some of Anders Nielsen's code !
  # Function to read a basic AD Model Builder fit.
  # Use for instance by:
  # simple.fit <- read.fit('c:/admb/examples/simple')
  # M3.fit<-read.fit("C:/M3/M3")
  # Then the object 'simple.fit' is a list containing sub?objects
  # 'names', 'est', 'std', 'cor', and 'cov' for all model
  # parameters and sdreport quantities.
  #
  ret<-list()
  #parfile<-as.numeric(scan(paste(file,'.par', sep=''),
  #                         what='', n=16, quiet=TRUE)[c(6,11,16)])
  parf<-paste(file,"M3.par",sep="/")
  parfile<-as.numeric(scan(parf, what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nlogl<-parfile[2]
  ret$maxgrad<-parfile[3]
  cfile<-paste(file,'M3.cor', sep='/')
  if(file.exists(cfile)){

    lin<-readLines(cfile)
    ret$npar<-length(lin)-2
    ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
    sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
    ret$names<-unlist(lapply(sublin,function(x)x[2]))
    ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
    ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
    ret$cor<-matrix(NA, ret$npar, ret$npar)
    corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
    ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
    ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
    ret$cov<-ret$cor*(ret$std%o%ret$std)

  }else{
    print(paste0("No .cor file: ",cfile," is not available, storing MLE parameter estimates only"))
    lin<-readLines(parf)
    sublin<-lapply(strsplit(lin, ' '),function(x)x[x!=''&x!="#"])
    parnams<-list()
    parind<-list() # indexing of where parnames are stored so values can be taken in the interim
    est<-list()
    j=0

    for(i in 1:length(sublin)){

      if(grepl(":",sublin[[i]][1])){

        j<-j+1
        parnams[[j]]<-unlist(strsplit(sublin[[i]][1],":"))
        firstvals=TRUE

      }else if(j>0){

        if(firstvals){
          est[[j]]<-as.numeric(sublin[[i]])
        }else{
          est[[j]]<-c(est[[j]],as.numeric(sublin[[i]]))
        }
        firstvals=FALSE

      }

    }

    ret$names<-rep(unlist(parnams),lapply(est,function(x)length(x)))
    ret$est<-unlist(est)
    ret$cov<-NA

  }
    #fileo<-strsplit(file,"/")
  #ret$hes<-read.hessian(file)#read.hessian(paste(c(fileo[[1]][1:(length(fileo[[1]])-1)],""),collapse='/'))
  return(ret)
}

pin_from_cor<-function(file="C:/M3/M3",pinfile=NA){
  #pin_from_cor(file="C:/M3/M3")
  ret<-read.fit(file)
  est<-matrix(ret$est,ncol=1)
  if(is.na(pinfile))write.table(est,file=paste(file,".pin",sep=""),col.names=F,row.names=F)
  if(!is.na(pinfile))write.table(est,file=pinfile,col.names=F,row.names=F)
}

pin_from_par<-function(file="C:/M3/M3",pinfile=NA){
  #pin_from_cor(file="C:/M3/M3")
  f <- file(paste(file,".par",sep=""), open="rb")
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
    nlines <- nlines + sum(chunk == as.raw(10L))
  }

  parfile<-paste(file,".par",sep="")
  est<-numeric()
  for(l in 2:nlines){
    test<-read.table(parfile,skip=l-1,nrows=1,comment.char="")
    if(test[1]!="#"){
      est<-c(est,as.numeric(test))
    }
  }
  close(f)
  est<-matrix(est,ncol=1)
  if(is.na(pinfile))write.table(est,file=paste(file,".pin",sep=""),col.names=F,row.names=F)
  if(!is.na(pinfile))write.table(est,file=pinfile,col.names=F,row.names=F)
}

read.hessian<-function(file="C:/M3"){

  filen <- file(paste(file,"admodel.hes",sep="/"), "rb")
  nopar <- readBin(filen, what = "integer", n = 1)
  hes <- readBin(filen, what = "double", n = nopar * nopar)
  hes <- matrix(hes, byrow = TRUE, ncol = nopar)
  close(filen)

  f <- file(paste(file,"M3.par",sep="/"), open="rb")
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
    nlines <- nlines + sum(chunk == as.raw(10L))
  }

  parfile<-paste(file,"M3.par",sep="/")
  parname<-character()
  for(l in 2:nlines){
    test<-read.table(parfile,skip=l-1,nrows=1,comment.char="")
    if(test[1]=="#"){
      partext<-as.character(test[1,2])
    }else{
      parname<-c(parname,paste(partext,1:length(test)))
    }
  }

  close(f)
  colnames(hes) <- rownames(hes) <- parname
  hes

}


M3read<-function(OMDir="C:/M3",quiet=T){

  out<-list()

  repfile<-paste(OMDir,"/M3.rep",sep="")
  out$np<-scan(repfile,skip=1,nlines=1,quiet=quiet)
  out$nHy<-scan(repfile,skip=3,nlines=1,quiet=quiet)
  out$ny<-scan(repfile,skip=5,nlines=1,quiet=quiet)
  out$ns<-scan(repfile,skip=7,nlines=1,quiet=quiet)
  out$nr<-scan(repfile,skip=9,nlines=1,quiet=quiet)
  out$nf<-scan(repfile,skip=11,nlines=1,quiet=quiet)
  out$na<-scan(repfile,skip=13,nlines=1,quiet=quiet)
  out$nl<-scan(repfile,skip=15,nlines=1,quiet=quiet)
  out$nma<-scan(repfile,skip=17,nlines=1,quiet=quiet)

  np<-out$np
  nHy<-out$nHy
  ny<-out$ny
  ns<-out$ns
  nr<-out$nr
  nf<-out$nf
  na<-out$na
  nl<-out$nl
  nma<-out$nma

  st<-19
  #out$ma<-ADMBrep(repfile,st,ADMBdim=c(np,na),quiet=quiet)
  #st<-st+1+np
  out$SSB<-ADMBrep(repfile,st,ADMBdim=c(np,ny,ns),quiet=quiet)
  st<-st+1+np*ny
  out$hSSB<-ADMBrep(repfile,st,ADMBdim=c(np,nHy,ns),quiet=quiet)
  st<-st+1+np*nHy
  out$FL<-ADMBrep(repfile,st,c(ny,ns,nr,nf,nl),quiet=quiet)
  st<-st+1+ny*ns*nr*nf
  out$HCobs<-ADMBrep(repfile,st,c(nHy,ns,na,nr),quiet=quiet)
  st<-st+1+nHy*ns*na
  out$nCobs<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$Cobs<-ADMBrep(repfile,st,c(out$nCobs,5),quiet=quiet)
  st<-st+1+out$nCobs
  out$Cpred<-ADMBrep(repfile,st,c(ny,ns,nr,nf),quiet=quiet)
  st<-st+1+ny*ns*nr
  out$nCLobs<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$CLobs<-ADMBrep(repfile,st,c(out$nCLobs,6),quiet=quiet)
  st<-st+1+out$nCLobs
  out$CLtotpred<-ADMBrep(repfile,st,c(ny,ns,nr,nf,nl),quiet=quiet)
  st<-st+1+ny*ns*nr*nf
  out$ma<-ADMBrep(repfile,st,c(np,na),quiet=quiet)
  st<-st+1+np
  out$mov<-ADMBrep(repfile,st,c(np,ns,na,nr,nr),quiet=quiet)
  st<-st+1+np*ns*na*nr
  out$sel<-ADMBrep(repfile,st,c(nf,nl),quiet=quiet)
  st<-st+1+nf
  out$RAI<-ADMBrep(repfile,st,c(nr,ns,ny),quiet=quiet)
  st<-st+1+nr*ns
  out$ml<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$VB<-ADMBrep(repfile,st,c(ny,ns,nr,nf),quiet=quiet)
  st<-st+1+ny*ns*nr
  out$B<-ADMBrep(repfile,st,c(ny,ns,nr),quiet=quiet)
  st<-st+1+ny*ns
  out$N<-ADMBrep(repfile,st,c(np,ny,ns,na,nr),quiet=quiet)
  st<-st+1+np*ny*ns*na
  out$lwa<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lwb<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$len_age<-ADMBrep(repfile,st,c(ny,na,np),quiet=quiet)
  st<-st+1+ny*na
  out$wt_age<-ADMBrep(repfile,st,c(ny,na,np),quiet=quiet)
  st<-st+1+ny*na
  out$nMP<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$nmovind<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$movind<-ADMBrep(repfile,st,c(out$nmovind,5),quiet=quiet)
  st<-st+1+out$nmovind
  out$nmov1<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$mov1<-ADMBrep(repfile,st,c(out$nmov1,5),quiet=quiet)
  st<-st+1+out$nmov1
  out$movtype<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$M_age<-ADMBrep(repfile,st,c(np,na),quiet=quiet)
  st<-st+1+np
  #out$h<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  #st<-st+2
  out$RDblock<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  fec<-ADMBrep(repfile,st,c(np,na),quiet=quiet)
  out$mat_age<-fec/t(out$wt_age[1,,]) # for some UNEXPLAINED reason this also assigns this to out$ma (pointer?)
  st<-st+1+np
  out$nsel<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$seltype<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$selind<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$spawns<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$iALK<-ADMBrep(repfile,st,c(np,ny,na,nl),quiet=quiet)
  st<-st+1+np*ny*na
  out$lnqE<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnqI<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnqCPUE<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$hZ<-ADMBrep(repfile,st,c(np,nHy,ns,na,nr))
  st<-st+1+np*nHy*ns*na
  out$Ipred<-ADMBrep(repfile,st,c(ny,ns,nr,np))
  st<-st+1+ny*ns*nr
  #out$nZeq<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  #st<-st+2
  out$nydist<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  #out$nyeq<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  #st<-st+2
  out$SSB0<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$muR<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$nRD<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$lnRD<-ADMBrep(repfile,st,c(np,out$nRD))
  st<-st+1+np
  out$D_ini<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$D<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$Dt<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$SSBnow<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$datacheck<-scan(repfile,skip=st,nlines=1,quiet=quiet)

  opt<-SRopt(out,plot=T)
  out$h<-opt$h
  out$R0<-opt$R0
  out$logith_lnR0_VC<-opt$VC
  #out$h_logit<-h_est[,2:3]
  out
}

plotM3fit<-function(out,outdir="G:/ABT-MSE/Results/OM_fits/Test/",
                    firstyr=1960,
                    fleetnams=c("PS","TP","LL","OTH"),
                    areanams=c("GOM","WATL","GSL","CATL","EATL","NEATL","WMED","EMED"),
                    subyrnams=c("Jan-Mar","Apr-Jun","Jul-Sept","Oct-Dec"),
                    popnams=c("East","West"),
                    forpres=F){

  if(substr(outdir,nchar(outdir),nchar(outdir))!="/")outdir<-paste(outdir,"/",sep="")

  if (!dir.exists(outdir)){
    dir.create(outdir)
  }

  fcols<-c("#99999980","#ff000080","#00ff0080","#0000ff80","#ffff0080")
  pcols<-c("#ff000080","#0000ff80","#00ff0080","#ffff0080")

  pch<-19
  cex<-0.8
  lwd=1.5

  np<-out$np
  ny<-out$ny
  ns<-out$ns
  nr<-out$nr
  nf<-out$nf
  na<-out$na
  nl<-out$nl

  yrs<-firstyr:(firstyr+ny-1)

  # Fit to catches --------------------------

  CobsA<-array(NA,c(ny,ns,nr,nf))
  CobsA[out$Cobs[,1:4]]<-out$Cobs[,5]

  Co<-CobsA/1000
  Cp<-out$Cpred/1000

  jpeg(paste(outdir,"Catch_fit.jpg",sep=""),res=300,height=7,width=9,units='in')
  par(mfcol=c(nr,ns),mai=c(0.05,0.15,0.01,0.01),omi=c(0.4,0.5,0.4,0.05))
  ylim=c(0,max(Co,na.rm=T))
  ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))

  for(s in 1:ns){

    for(r in 1:nr){

      plot(yrs,Co[,s,r,1],col=fcols[1],pch=pch,ylim=ylim,axes=F)
      lines(yrs,Cp[,s,r,1],col=fcols[1],lwd=lwd)
      if(r==nr)axis(1,xs,xs)
      if(r<nr)axis(1,xs,NA)
      if(s==1)axis(2,ys,ys)
      if(s<(ns+1))axis(2,ys,NA)
      if(s==1)mtext(areanams[r],2,line=2.75)
      if(r==1)mtext(subyrnams[s],3,line=-0.5)
      if(nf>1){
        for(f in 2:nf){

          points(yrs,Co[,s,r,f],col=fcols[f],pch=pch)
          lines(yrs,Cp[,s,r,f],col=fcols[f],lwd=lwd)

        }
      }

      if(r==1&s==ns)legend('topright',legend=fleetnams,text.font=2,text.col=fcols[1:nf],bty='n')

    }

  }

  mtext("Year",1,outer=T,line=2)
  mtext("Predicted (line) versus observed (point) catches of Atlantic bluefin (tonnes)",3,line=1.5,outer=T)
  dev.off()



  # log catches ---------------------


  jpeg(paste(outdir,"Catch_log_fit.jpg",sep=""),res=300,height=7,width=9,units='in')

  Co<-log(Co)
  Cp<-log(Cp)

  par(mfcol=c(nr,ns),mai=c(0.05,0.15,0.01,0.01),omi=c(0.4,0.5,0.4,0.05))
  ylim=c(0,max(Co,na.rm=T))
  ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))

  for(s in 1:ns){

    for(r in 1:nr){

      plot(yrs,Co[,s,r,1],col=fcols[1],pch=pch,ylim=ylim,axes=F)
      lines(yrs,Cp[,s,r,1],col=fcols[1],lwd=lwd)
      if(r==nr)axis(1,xs,xs)
      if(r<nr)axis(1,xs,NA)
      if(s==1)axis(2,ys,ys)
      if(s<(ns+1))axis(2,ys,NA)
      if(s==1)mtext(areanams[r],2,line=2.75)
      if(r==1)mtext(subyrnams[s],3,line=-0.5)
      if(nf>1){
        for(f in 2:nf){

          points(yrs,Co[,s,r,f],col=fcols[f],pch=pch)
          lines(yrs,Cp[,s,r,f],col=fcols[f],lwd=lwd)

        }
      }

      if(r==1&s==ns)legend('topright',legend=fleetnams,text.font=2,text.col=fcols[1:nf],bty='n')

    }

  }

  mtext("Year",1,outer=T,line=2)
  mtext("Predicted (line) versus observed (point) catches of Atlantic bluefin (log tonnes)",3,line=1.5,outer=T)

  dev.off()



  # Aggregate catch comp fit annual catch comp by area to pred comp (residual fit is below)

  CLobsA<-array(NA,c(ny,ns,nr,nf,nl))
  CLobsA[out$CLobs[,1:5]]<-out$CLobs[,6]

  CLo<-apply(CLobsA,c(1,4,5),sum,na.rm=T)
  CLo[CLo==0]<-NA
  CLotot<-array(apply(CLo,1:2,sum,na.rm=T),dim(CLo))
  CLo<-CLo/CLotot
  CLp<-apply(out$CLtotpred,c(1,4,5),sum,na.rm=T)#*CLotot
  CLp<-CLp/array(apply(CLp,1:2,sum,na.rm=T),dim(CLp))

  for(f in 1:nf){

    jpeg(paste(outdir,"Catch_comp_fit_",fleetnams[f],".jpg",sep=""),res=300,height=7,width=8,units='in')

    yrswd<-unique(array(1:ny,dim(CLo[,f,]))[!is.na(CLo[,f,])])
    yrswd<-yrswd[order(yrswd)]
    nplotyrs<-length(yrswd)

    ncs<-6
    nrs<-ceiling(nplotyrs/ncs)

    par(mfrow=c(nrs,ncs),mai=c(0.15,0.25,0.01,0.01),omi=c(0.5,0.15,0.4,0.05))

    ylim=c(0,0.4)
    ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
    xs<-pretty(seq(1,out$nl,length.out=8))

    for(i in 1:nplotyrs){

      y<-yrswd[i]
      if(i>(nplotyrs-ncs)){
        barplot(CLo[y,f,],ylim=ylim,names.arg=out$ml,border=NA,col='orange')
      }else{
        barplot(CLo[y,f,],ylim=ylim,border=NA,col='orange')
      }
      lines(CLp[y,f,],col="#0000ff90",lwd=1.5)
      legend('topleft',legend=y+firstyr-1,bty='n')
      legend('topright',legend=paste("n=",round(sum(CLobsA[y,,,f,],na.rm=T)/1000,1),"k",sep=""),bty='n')
      abline(v=xs,col="#99999980")
    }


    mtext("Length (cm)",1,outer=T,line=2)
    mtext(paste("Predicted (line) versus observed (point) catch at length for the ",fleetnams[f]," fleet (n=",floor(sum(CLobsA[,,,f,],na.rm=T)/1000),"k)",sep=""),3,line=1.25,outer=T)

    dev.off()

  }


  # Fit to relative abundance index -------------------------------------

  B<-array(NA,dim(out$N))
  ind<-TEG(dim(out$N))
  indw<-ind[,c(2,4,1)]
  B[ind]<-out$N[ind]*out$wt_age[indw]

  RAIp<-apply(B,c(5,3,2),sum) # r s y
  RAIo<-out$RAI
  muRAIp<-mean(RAIp)
  RAIp<-RAIp/muRAIp
  RAIo<-RAIo/mean(RAIo)

  RAIpP<-apply(B,c(5,3,2,1),sum) # r s y p
  RAIpP<-RAIpP/muRAIp


  jpeg(paste(outdir,"Relative abundance fit.jpg",sep=""),res=300,height=7,width=9,units='in')

  par(mfcol=c(nr,ns),mai=c(0.05,0.15,0.01,0.01),omi=c(0.4,0.5,0.4,0.05))
  ylim=c(0,max(RAIo,RAIp,na.rm=T))
  ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))

  for(s in 1:ns){

    for(r in 1:nr){

      plot(yrs,RAIo[r,s,],col="#99999980",pch=pch,ylim=ylim,axes=F)
      lines(yrs,RAIp[r,s,],col="#ff00ff80",lwd=2)
      for(p in 1:np){
        lines(yrs,RAIpP[r,s,,p],col=pcols[p],lwd=1.5)
      }

      if(r==nr)axis(1,xs,xs)
      if(r<nr)axis(1,xs,NA)
      if(s==1)axis(2,ys,ys)
      if(s<(ns+1))axis(2,ys,NA)
      if(s==1)mtext(areanams[r],2,line=2.75)
      if(r==1)mtext(subyrnams[s],3,line=-0.5)
      if(r==1&s==ns)legend('topright',legend=popnams,text.font=2,text.col=pcols[1:np],bty='n')

    }

  }

  mtext("Year",1,outer=T,line=2)
  mtext("Predicted (line) versus observed (point) relative abundance of Atlantic bluefin",3,line=1.5,outer=T)

  dev.off()


  # rescaled by area

  jpeg(paste(outdir,"Relative abundance fit rescaled by area.jpg",sep=""),res=300,height=7,width=9,units='in')

  par(mfcol=c(nr,ns),mai=c(0.05,0.15,0.01,0.01),omi=c(0.4,0.5,0.4,0.05))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))

  for(s in 1:ns){

    for(r in 1:nr){

      ylim=c(0,max(RAIo[r,,],RAIp[r,,],na.rm=T))
      ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))

      plot(yrs,RAIo[r,s,],col="#99999980",pch=pch,ylim=ylim,axes=F)
      lines(yrs,RAIp[r,s,],col="#ff00ff80",lwd=2)
      for(p in 1:np){
        lines(yrs,RAIpP[r,s,,p],col=pcols[p],lwd=1.5)
      }

      if(r==nr)axis(1,xs,xs)
      if(r<nr)axis(1,xs,NA)
      if(s==1)axis(2,ys,ys)
      if(s<(ns+1))axis(2,ys,NA)
      if(s==1)mtext(areanams[r],2,line=2.75)
      if(r==1)mtext(subyrnams[s],3,line=-0.5)
      if(r==1&s==ns)legend('topright',legend=popnams,text.font=2,text.col=pcols[1:np],bty='n')

    }

  }

  mtext("Year",1,outer=T,line=2)
  mtext("Predicted (line) versus observed (point) relative abundance of Atlantic bluefin",3,line=1.5,outer=T)

  dev.off()

  # log relative abundance fit ------------------------

  RAIo<-log(RAIo)
  RAIp<-log(RAIp)
  RAIpP<-log(RAIpP)

  jpeg(paste(outdir,"Relative abundance fit log.jpg",sep=""),res=300,height=7,width=9,units='in')

  par(mfcol=c(nr,ns),mai=c(0.05,0.15,0.01,0.01),omi=c(0.4,0.5,0.4,0.05))
  ylim=range(RAIo,RAIp,na.rm=T)
  ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))

  for(s in 1:ns){

    for(r in 1:nr){

      plot(yrs,RAIo[r,s,],col="#99999980",pch=pch,ylim=ylim,axes=F)
      lines(yrs,RAIp[r,s,],col="#ff00ff80",lwd=2)
      for(p in 1:np){
        lines(yrs,RAIpP[r,s,,p],col=pcols[p],lwd=1.5)
      }

      if(r==nr)axis(1,xs,xs)
      if(r<nr)axis(1,xs,NA)
      if(s==1)axis(2,ys,ys)
      if(s<(ns+1))axis(2,ys,NA)
      if(s==1)mtext(areanams[r],2,line=2.75)
      if(r==1)mtext(subyrnams[s],3,line=-0.5)
      if(r==1&s==ns)legend('topright',legend=popnams,text.font=2,text.col=pcols[1:np],bty='n')

    }

  }

  mtext("Year",1,outer=T,line=2)
  mtext("Predicted (line) versus observed (point) relative abundance of Atlantic bluefin (log)",3,line=1.5,outer=T)

  dev.off()

}


getM3res<-function(out,outdir="G:/ABT-MSE/Results/OM_res/Test/",firstyr=1960,
                   fleetnams=c("PS","TP","LL","OTH"),
                   areanams=c("GOM","WATL","GSL","CATL","EATL","NEATL","WMED","EMED"),
                   subyrnams=c("Jan-Mar","Apr-Jun","Jul-Sept","Oct-Dec"),
                   popnams=c("East","West")){

  pch<-19
  cex<-0.8
  lwd=1.5

  fcols<-c("#99999980","#ff000080","#00ff0080","#0000ff80","#ffff0080")
  pcols<-c("#ff000080","#0000ff80","#00ff0080","#ffff0080")

  if(substr(outdir,nchar(outdir),nchar(outdir))!="/")outdir<-paste(outdir,"/",sep="")

  if (!dir.exists(outdir)){
    dir.create(outdir)
  }

  np<-out$np
  ny<-out$ny
  ns<-out$ns
  nr<-out$nr
  nf<-out$nf
  na<-out$na
  nl<-out$nl
  yrs<-firstyr:(firstyr+ny-1)

  B<-array(NA,dim(out$N))
  ind<-TEG(dim(out$N))
  indw<-ind[,c(2,4,1)]
  B[ind]<-out$N[ind]*out$wt_age[indw]

  Depall<-sum(out$SSB[,ny,ns])/sum(out$SSB[,1,1])
  Depp<-out$SSB[,ny,ns]/out$SSB[,1,1]

  yeval<-c(pretty(yrs)[pretty(yrs)<yrs[ny]],yrs[ny])
  yind<-yeval-firstyr+1
  SSBp<-out$SSB[,yind,ns]
  SSBt<-apply(out$SSB[,yind,ns],2,sum)
  SSB0<-out$SSB[,1,1]

  Btot<-apply(B,c(1:3,5),sum) # p y s r
  Ball<-Umu<-Uwt<-Upred<-array(NA,c(np,dim(out$Cpred))) # p y s r f
  ind<-TEG(dim(Upred))
  indC<-ind[,2:5]
  indB<-ind[,1:4]
  Ball[ind]<-Btot[indB]
  Upred[ind]<-out$Cpred[indC]/(out$Cpred[indC]+Ball[ind])
  Uwt<-Upred*Ball # weighted by biomass
  Uwtot<-apply(Uwt,c(1,2,5),sum) # total U weight summed
  Bwtot<-apply(Ball,c(1,2,5),sum) # total biomass summed
  Umu<-Uwtot/Bwtot # weighted average (weighted by biomass) p y f

  Unow<-Umu[,yind,]

  outtab<-array(NA,c(1000,1000))
  rn<-1
  outtab[rn:(rn+np-1),2]<-round(Depp,3)
  outtab[rn:(rn+np-1),1]<-paste("SSB depln.",popnams)
  rn<-rn+np
  outtab[rn,1:length(yind)+1]<-yind+firstyr-1
  outtab[rn+1,1:length(yind)+1]<-SSBt/1000000
  outtab[(rn+2):(rn+np+1),1:length(yind)+1]<-SSBp/1000000
  outtab[(rn):(rn+np+1),1]<-c("Year","Total SSB (kt)",paste(popnams, "SSB (kt)"))
  rn<-rn+2+np

  for(pp in 1:np){

    outtab[rn:(rn+nf-1),1:length(yind)+1]<-round(t(Unow[pp,,])*ns*100,2)
    outtab[rn:(rn+nf-1),1]<-paste("U(%):",popnams[pp],fleetnams)
    rn<-rn+nf

  }

  resfile<-paste(outdir,"Results.csv",sep="")
  write.csv(outtab[1:(rn-1),1:8],file=resfile)

  jpeg(paste(outdir,"Predicted U by year.jpg",sep=""),res=300,height=3.5,width=np*3.5,units='in')

  par(mfrow=c(1,np),mai=c(0.05,0.15,0.01,0.01),omi=c(0.6,0.5,0.4,0.05))
  ylim=c(0,max(Umu,na.rm=T))
  ys<-pretty(seq(ylim[1],ylim[2]*1.2,length.out=4))
  xs<-pretty(seq(yrs[1],yrs[ny],length.out=6))

  for(p in 1:np){

    plot(yrs,Umu[p,,1],col=fcols[1],lwd=2,type='l',ylim=ylim,axes=F)
    axis(1,xs,xs)
    if(p==1)axis(2,ys,ys)
    if(p>1)axis(2,ys,NA)
    legend('top',legend=popnams[p],bty='n')

    for(f in 2:nf){

      lines(yrs,Umu[p,,f],col=fcols[f],lwd=lwd)
      if(p==np)legend('topright',legend=fleetnams,text.font=2,text.col=fcols[1:nf],bty='n')

    }

  }

  mtext("Year",1,outer=T,line=2)
  mtext("Harvest rate",2,outer=T,line=1.5)
  mtext("Predicted mean subyear harvest rate by fleet",3,line=0.8,outer=T)

  dev.off()


  # selectivity by fleet

  jpeg(paste(outdir,"Predicted selectivities.jpg",sep=""),res=300,height=4.5,width=4.5,units='in')

  par(mai=c(0.05,0.15,0.01,0.01),omi=c(0.66,0.5,0.1,0.05))

  plot(out$ml[c(1,nl)],c(0,1),col='white',lwd=2,type='l')

  for(f in 1:nf){

    lines(out$ml,out$sel[f,],col=fcols[f],lwd=2)

  }
  legend('topright',legend=fleetnams,text.font=2,text.col=fcols[1:nf],bty='n')

  mtext("Length (cm)",1,outer=T,line=2)
  mtext("Selectivity",2,outer=T,line=1.5)

  dev.off()

}

# Some functions for obtaining sim sam estimates from a list of M3 outputs
getdep<-function(x,out)out[[x]]$D
getdepsim<-function(x,out)out[[x]]$D_ini


getSSBnow<-function(x,out)out[[x]]$SSB[,out[[x]]$ny,out[[x]]$ns]

getSSB0<-function(x,out)out[[x]]$SSB0

getBnow<-function(x,out){
  B<-array(NA,dim(out[[x]]$N))
  ind<-TEG(dim(out[[x]]$N))
  indw<-ind[,c(2,4,1)]
  B[ind]<-out[[x]]$N[ind]*out[[x]]$wt_age[indw]
  Bcur<-apply(B[,out[[x]]$ny,out[[x]]$ns,,],1,sum)
  Bcur
}

getUnow<-function(x,out){
  B<-array(NA,dim(out[[x]]$N))
  ind<-TEG(dim(out[[x]]$N))
  indw<-ind[,c(2,4,1)]
  B[ind]<-out[[x]]$N[ind]*out[[x]]$wt_age[indw]
  Bcur<-sum(B[,out[[x]]$ny,out[[x]]$ns,,])

  Cpred<-apply(out[[x]]$Cpred[out[[x]]$ny,out[[x]]$ns,,],2,sum)
  Cpred/(Cpred+Bcur)
}

getBfrac<-function(x,out,spawnr){
  B<-array(NA,dim(out[[x]]$N))
  ind<-TEG(dim(out[[x]]$N))
  indw<-ind[,c(2,4,1)]
  B[ind]<-out[[x]]$N[ind]*out[[x]]$wt_age[indw]
  B0f<-apply(B[,1,1,,],c(1,3),sum)
  B0f<-B0f/apply(B0f,1,sum)
  B0f[as.matrix(cbind(1:out[[x]]$np,spawnr))]
}

copyOMinfo<-function(from,to){

 file.copy(paste(from,"/M3.dat",sep=""),paste(to,"/M3.dat",sep=""),overwrite=T)
 file.copy(paste(from,"/M3.rep",sep=""),paste(to,"/M3.rep",sep=""),overwrite=T)
 file.copy(paste(from,"/M3.par",sep=""),paste(to,"/M3.par",sep=""),overwrite=T)
 file.copy(paste(from,"/admodel.hes",sep=""),paste(to,"/admodel.hes",sep=""),overwrite=T)
 file.copy(paste(from,"/M3.cor",sep=""),paste(to,"/M3.cor",sep=""),overwrite=T)
 pin_from_par(file=paste(from,"/M3",sep=""),pinfile=paste(to,"/M3.pin",sep=""))

}

plotareas<-function(OMd){

  cols<-rep(c("#ff000060","#00000030","#00ff0060","#0000ff60","#00000060","#ff00ff40"),4)
  map(xlim=c(-100,45),ylim=c(-35,70),col='white')
  polygon
  abline(v=(-20:20)*5,col='#0000ff10')
  abline(h=(-20:20)*5,col='#0000ff10')
  abline(v=0,col="red")
  abline(h=0,col="red")

  for(i in 1:length(OMd@Area_names)){
    polygon(OMd@Area_defs[[i]],col=cols[i],border=F)
  }

  map(xlim=c(-100,45),ylim=c(-35,70),fill=T,col='white',border=NA,add=T)
  map(xlim=c(-100,45),ylim=c(-35,70),col='white',add=T,lwd=2)

  for(i in 1:length(OMd@Area_names)){
    #lines(OMd@Area_defs[[i]])
    text(mean(OMd@Area_defs[[i]]$x),mean(OMd@Area_defs[[i]]$y),OMd@Area_names[i],col='black',font=2,cex=0.8)
  }

}


runM3<-function(x,OMdir='C:/M3temp'){
  setwd(paste(OMdir,x,sep="/"))
  system("M3.exe",wait=T,show.output.on.console = F)
  return(999)
}

musmooth<-function(vec){
  vec2<-c(NA,NA,vec,NA,NA)
  pos<-3:(length(vec2)-2)
  val<-cbind(vec2[pos-2],vec2[pos-1],vec2[pos],vec2[pos+1],vec2[pos+2])
  apply(val,1,mean,na.rm=T)
}

M3write<-function(OMI,datfile="C:/M3/M3.dat"){

  if(!dir.exists(unlist(strsplit(datfile,split="M3.dat"))))stop("You must specify a valid M3 directory")

  #write("# name",datfile,1,append=F)
  #write(OMI@Name,datfile,1,append=T)

  #write("# nOMfactors",datfile,1,append=T)
  #write(length(OMI@OMfactors),datfile,1,append=T)

  #write("# OMfactors",datfile,1,append=T)
  #write(unlist(OMI@OMfactors),datfile,length(OMI@OMfactors),append=T)

  write("# nHy number of historical years for SRA",datfile,1,append=F)
  write(OMI@nHy,datfile,1,append=T)

  write("# ny number of years",datfile,1,append=T)
  write(OMI@ny,datfile,1,append=T)

  write("# ns number of subyears",datfile,1,append=T)
  write(OMI@ns,datfile,1,append=T)

  write("# np number of populations/stocks",datfile,1,append=T)
  write(OMI@np,datfile,1,append=T)

  write("# na number of age classes",datfile,1,append=T)
  write(OMI@na,datfile,1,append=T)

  write("# nr number of regions/areas",datfile,1,append=T)
  write(OMI@nr,datfile,1,append=T)

  write("# nf number of fleets",datfile,1,append=T)
  write(OMI@nf,datfile,1,append=T)

  write("# nl number of length classes",datfile,1,append=T)
  write(OMI@nl,datfile,1,append=T)

  write("# nRPT maximum number of time steps that a PSAT can be recaptured",datfile,1,append=T)
  write(OMI@nRPT,datfile,1,append=T)

  write("# RPtind correct subyear recapture index",datfile,1,append=T)
  write(t(OMI@RPTind),datfile,OMI@nRPT,append=T)

  write("# sdur the duration of the various subyears (sums to 1)",datfile,1,append=T)
  write(OMI@sdur,datfile,OMI@ns,append=T)

  #write("# nZeq Number of years at the start of the model to calculate equilibrium Z from (mean of nZep years)",datfile,1,append=T)
  #write(OMI@nZeq,datfile,OMI@ns,append=T)

  write("# nydist Number of years over which initial stock distribution is calculated (prior to spool up)",datfile,1,append=T)
  write(OMI@nydist,datfile,OMI@ns,append=T)

  #write("# nyeq Number of spool-up years over which the stock is subject to nZeq, used to define equilibrium conditions",datfile,1,append=T)
  #write(OMI@nyeq,datfile,OMI@ns,append=T)

  write("# ml the mean length of the length categories",datfile,1,append=T)
  write(OMI@mulen,datfile,OMI@nl,append=T)

  write("# RDblock the RD parameter for each year",datfile,1,append=T)
  write(OMI@RDblock,datfile,OMI@ny,append=T)

  write("# nRD the number of estimated recruitment strengths",datfile,1,append=T)
  write(OMI@nRD,datfile,1,append=T)

  # -- Growth --

  write("# iALK the age-length key by population and year p y a l",datfile,1,append=T)
  write(tomt(OMI@iALK),datfile,OMI@nl,append=T)

  write("# lwa weight-length parameter a w=al^ b",datfile,1,append=T)
  write(OMI@lwa,datfile,OMI@np,append=T)

  write("# lwa weight-length parameter b w=al^ b",datfile,1,append=T)
  write(OMI@lwb,datfile,OMI@np,append=T)

  write("# len_age (pay)",datfile,1,append=T)
  write(OMI@len_age,datfile,OMI@ny,append=T)

  write("# wt_age (pay)",datfile,1,append=T)
  write(OMI@wt_age,datfile,OMI@ny,append=T)

  # -- Maturity --

  write("# Fec, fecundity at age, SSB at age",datfile,1,append=T)
  write(t(OMI@Fec),datfile,OMI@na,append=T)

  #write("# steep, steepness of the Bev-Holt SR relationship",datfile,1,append=T)
  #write(OMI@steep,datfile,OMI@np,append=T)

  # -- Spawning --

  write("# spawns, the subyear in which the stock spawns",datfile,1,append=T)
  write(OMI@spawns,datfile,OMI@np,append=T)

  write("# canspawn, areas in which the stock spawns",datfile,1,append=T)
  write(OMI@canspawn,datfile,OMI@nr,append=T)

  # -- Natural Mortality rate --

  write("# Ma, instantaneous natural mortality rate at age",datfile,1,append=T)
  write(t(OMI@Ma),datfile,OMI@na,append=T)

  # -- Fishery data --

  write("# nCobs, the number of catch weight observations y s r f CW",datfile,1,append=T)
  write(OMI@nCobs,datfile,1,append=T)

  write("# Cobs, catch weight observations y s r f C(weight)",datfile,1,append=T)
  write(t(OMI@Cobs),datfile,5,append=T)

  # CPUE

  write("# nCPUEq, the number of CPUE series",datfile,1,append=T)
  write(OMI@nCPUEq,datfile,1,append=T) # in this simulation this is the same as the number of fleets

  write("# nCPUEobs, the number of CPUE observations y s r f CPUE(weight)",datfile,1,append=T)
  write(OMI@nCPUEobs,datfile,1,append=T)

  write("# CPUEobs, CPUE observations y s r cpueindex f CPUE(weight)",datfile,1,append=T)
  write(t(OMI@CPUEobs),datfile,ncol(OMI@CPUEobs),append=T)

  # Partial F's (standardized effort)

  write("# nE, the number of effort series (typically nF but could be mirroring)",datfile,1,append=T)
  write(OMI@nE,datfile,1,append=T) # in this simulation this is the same as the number of fleets

  write("# nEobs, the number of effort observations ",datfile,1,append=T)
  write(OMI@nEobs,datfile,1,append=T)

  write("# Eobs, CPUE observations y s r f eindex CPUE(weight)",datfile,1,append=T)
  write(t(OMI@Eobs),datfile,ncol(OMI@Eobs),append=T)

  # Length composition

  write("# nCLobs, the number of catch-at-length observations y s r f l N",datfile,1,append=T)
  write(OMI@nCLobs,datfile,1,append=T)

  write("# CLobs, catch-at-length observations y s r f l N",datfile,1,append=T)
  write(t(OMI@CLobs),datfile,ncol(OMI@CLobs),append=T)

  # Historical catches

  write("# HCobs, the historical catch at age observations by Hy x s x r x a",datfile,1,append=T)
  write(tomt(OMI@HCobs),datfile,OMI@nHy,append=T)

  # The real relative abundance index RAI (y, s, r) (pass through for model fitting)
  write("# RAI, Relative Abundance index r x s x y",datfile,1,append=T)
  write(OMI@RAI,datfile,OMI@ny,append=T)

  # Fishery-independent indices y s r i type(biomass/ssb) index

  write("# nI, the number of fishery independent indices series",datfile,1,append=T)
  write(OMI@nI,datfile,1,append=T) # in this simulation this is the same as the number of populations

  write("# nIobs, the number of fishery independent observations y s r pp i type(biomass/ssb) index",datfile,1,append=T)
  write(OMI@nIobs,datfile,1,append=T)

  write("# Iobs, fishery independent observations y s r p i type(biomass/ssb) index",datfile,1,append=T)
  write(t(OMI@Iobs),datfile,7,append=T)

  # PSAT tagging --

  write("# nPSAT, PSATs data of known stock of origin p a s t fr tr N",datfile,1,append=T)
  write(OMI@nPSAT,datfile,1,append=T)

  write("# PSAT data of known stock of origin p a s t fr tr N",datfile,1,append=T)
  write(t(OMI@PSAT),datfile,7,append=T)

  write("# nPSAT2, PSATs data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
  write(t(OMI@nPSAT2),datfile,1,append=T)

  write("# PSAT2 data of unknown stock of origin a s t fr tr SOO(npop)",datfile,1,append=T)
  write(OMI@PSAT2,datfile,5+OMI@np,append=T)

  # Placeholder for conventional tags

  write("# nTag, number of conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
  write(OMI@nTag,datfile,1,append=T)

  write("# Tag, conventional tag observations y s r a - y s r f a N",datfile,1,append=T)
  write(t(OMI@Tag),datfile,10,append=T)

  # Stock of origin

  write("# nSOOobs, number of stock of origin observations p a y s r N",datfile,1,append=T)
  write(nrow(OMI@SOOobs),datfile,1,append=T)

  write("# SOOobs, stock of origin observations p a y s r N",datfile,1,append=T)
  write(t(OMI@SOOobs),datfile,6,append=T)

  # -- Selectivity controls

  write("# nsel, number of estimated selectivities",datfile,1,append=T)
  write(OMI@nsel,datfile,1,append=T) # same as number of fleets

  write("# seltype, 2:logistic, 3:Thompson",datfile,1,append=T)
  #       PS,TP,LL,OTH
  write(OMI@seltype,datfile,OMI@nsel,append=T) # LL fleet is logistic

  write("# selind, which selectivity is assigned to each fleet",datfile,1,append=T)
  write(OMI@selind,datfile,OMI@nf,append=T) # same as fleets

  write("# ratiolim, limits on logistic slope parameter relative to inflection point",datfile,1,append=T)
  write(OMI@ratiolim,datfile,2,append=T) # same as fleets

  write("# infleclim, limits on model selectivity",datfile,1,append=T)
  write(OMI@infleclim,datfile,2,append=T) # same as fleets

  # -- Movement estimation

  write("# nMP, number of estimated movement parameters",datfile,1,append=T)
  write(OMI@nMP,datfile,1,append=T)

  write("# nma, max number of age classes",datfile,1,append=T)
  write(OMI@nma,datfile,1,append=T)

  write("# ,ma, age class assignment by age ",datfile,1,append=T)
  write(array(OMI@ma,c(OMI@na,OMI@np)),datfile,1,append=T)

  write("# nmovind, number of estimated movement parameters minus viscosity",datfile,1,append=T)
  write(OMI@nmovind,datfile,1,append=T)

  write("# movind, the location of estimated movement parameters p s r r",datfile,1,append=T)
  write(t(OMI@movind),datfile,5,append=T)

  write("# nmov1, number of initial non-estimated movement parameters",datfile,1,append=T)
  write(OMI@nmov1,datfile,1,append=T)

  write("# mov1, the location of initial non-estimated movement parameters p s r r",datfile,1,append=T)
  write(t(OMI@mov1),datfile,5,append=T)

  write("# movtype, the type of movement parameterization 1: gravity 2:markov matrix",datfile,1,append=T)
  write(OMI@movtype,datfile,1,append=T)


  # -- Observation errors

  write("# CobsCV, lognormal CV of the observed catches",datfile,1,append=T)
  write(OMI@CobsCV,datfile,OMI@nf,append=T)

  write("# CPUEobsCV, lognormal CV of the CPUE indices",datfile,1,append=T)
  write(OMI@CPUEobsCV,datfile,OMI@nf,append=T) # CPUE index for each fleet

  write("# IobsCV, lognormal CV of the fishery independent indices",datfile,1,append=T)
  write(OMI@IobsCV,datfile,OMI@np,append=T) # SSB index for each population


  # -- Priors

  write("# RDCV, lognormal penalty on recruitment deviations",datfile,1,append=T)
  write(OMI@RDCV,datfile,1,append=T)

  write("# SSBprior, prior on current SSB",datfile,1,append=T)
  write(OMI@SSBprior,datfile,1,append=T) # Absolute tonnage of SSB in current model year

  write("# SSBCV, lognormal penalty on recruitment deviations",datfile,1,append=T)
  write(OMI@SSBCV,datfile,1,append=T) # CV on SSB prior in current model year


  # -- Likelihood weights

  write("# nLHw, number of likelihood components",datfile,1,append=T)
  write(OMI@nLHw,datfile,1,append=T)

  write("# LHw,  likelihood components",datfile,1,append=T)
  write(OMI@LHw,datfile,OMI@nLHw,append=T) # Likelihood weights (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA penalty)

  # -- Initial values

  write("# muR_ini, initial values for mean historical recruitment",datfile,1,append=T)
  write(OMI@muR_ini,datfile,OMI@np,append=T) # Simulated R0 for each population

  write("# sel_ini, initial values for selectivity",datfile,1,append=T)
  write(t(OMI@sel_ini),datfile,OMI@nl,append=T) # Actual selectivity

  write("# selpar_ini, initial values for selectivity parameters",datfile,1,append=T)
  write(t(OMI@selpar_ini),datfile,3,append=T) # Actual selectivity

  write("# lnF_ini, initial values for log F",datfile,1,append=T)
  write(OMI@lnF_ini,datfile,OMI@nCobs,append=T) # log apical F

  write("# lnRD_ini, recruitment deviations y=1:nyears",datfile,1,append=T)
  write(t(OMI@lnRD_ini),datfile,OMI@ny,append=T) # Recruitment deviations

  write("# mov_ini, simulated movement p s r r",datfile,1,append=T)
  write(OMI@mov_ini,datfile,OMI@nr,append=T) # Movement probabilities

  write("# qCPUE_ini, initial values for CPUE catchability nCPUE",datfile,1,append=T)
  write(log(OMI@qCPUE_ini),datfile,OMI@nCPUEq,append=T)

  write("# lnqI_ini, initial values for fishery independent catchability nI",datfile,1,append=T)
  write(log(OMI@qI_ini),datfile,OMI@nI,append=T) # Catchabilities I=qSSB or I=qB

  write("# D_ini, reference (simulated) depletion",datfile,1,append=T)
  write(OMI@D_ini,datfile,OMI@np,append=T) # Catchabilities I=qSSB or I=qB

  # -- Misc

  write("# complexRD 1= run with full estimation of all recruitment deviations by year",datfile,1,append=T)
  write(OMI@complexRD,datfile,1,append=T) # debug switch

  write("# complexF 1= run with full estimation of all F's by year, subyear, fleet, region",datfile,1,append=T)
  write(OMI@complexF,datfile,1,append=T) # debug switch

  write("# nF either nCobs or 1 if complexF=0",datfile,1,append=T)
  write(OMI@nF,datfile,1,append=T) # debug switch

  #write("# nMPind",datfile,1,append=T)
  #write(nrow(OMI@MPind),datfile,1,append=T)

  #write("# MPind",datfile,1,append=T)
  #write(t(OMI@MPind),datfile,7,append=T)

  write("# debug 1= run with initial values",datfile,1,append=T)
  write(OMI@debug,datfile,1,append=T) # debug switch

  write("# verbose 1= run with printouts",datfile,1,append=T)
  write(OMI@verbose,datfile,1,append=T) # debug switch

  write("# datacheck",datfile,1,append=T)
  write(OMI@datacheck,datfile,1,append=T) # datacheck

}



storeoutputs<-function(fromdir,todir){

  nOMs<-length(fromdir)

  for(i in 1:nOMs){

    file.copy(paste(fromdir[i],"/M3.rep",sep=""),paste(todir[i],"/M3.rep",sep=""),overwrite=T)
    file.copy(paste(fromdir[i],"/M3.par",sep=""),paste(todir[i],"/M3.par",sep=""),overwrite=T)
    file.copy(paste(fromdir[i],"/M3.cor",sep=""),paste(todir[i],"/M3.cor",sep=""),overwrite=T)

    out<-M3read(todir[i])
    save(out,file=paste(todir[i],"/out",sep=""))

  }

}

make_summary_report<-function(dir){

  if(!file.exists(dir)){
    print(paste('Directory:',dir,'does not exist'))
    stop()
  }

  designdir<-paste0(dir,"/Design.Rdata")
  if(!file.exists(designdir)){
    print(paste('Object:',designdir,'is required to build the summary table'))
    stop()
  }

  load(designdir)

  OMdirs<-list.dirs(dir)
  OMdirs<-OMdirs[2:length(OMdirs)]
  nOMs<-length(OMdirs)
  fileind<-rep(NA,nOMs)
  foldnams<-strsplit(OMdirs,"/")
  lastfolder<-length(foldnams[[1]])
  lfnams<-unlist(lapply(foldnams,FUN=function(X)X[lastfolder]))
  OMdirs<-OMdirs[order(as.numeric(lfnams))]
  nOMs<-length(OMdirs)
  render(input="Source/OMsummary.Rmd", output_file=paste(dir,"/Summary report.pdf",sep=""))


}


make_fit_reports<-function(dirs="C:/M3") {

  nOMs<-length(dirs)
  load(paste0(getwd(),"/Objects/Observation models/Bad_Obs"))
  for(i in 1:nOMs){

    input_dir<-dirs[i]

    if(!file.exists(input_dir)){
      print(paste('Directory:',input_dir,'does not exist'))
      stop()
    }

    out<-M3read(input_dir)
    load(paste(input_dir,"/OMI",sep=""))
    #dat<-read.csv(paste0(getwd(),"/Data/Previous assessments/2015 VPA SSB rec.csv"))
    #dat<-read.csv(paste0(getwd(),"/Data/Previous assessments/2014VPAs.csv"))
    load(paste0(getwd(),"/Data/Previous assessments/ts.Rdata"))
    dat<-subset(ts,catchScenario=="Reported")

    #getM3res(out,outdir=input_dir,firstyr=1960,
    #                 fleetnams=c(OMI@Fleets$name,'ALL OTH'),
    #                 areanams=OMI@areanams)


    #plotM3fit(out,outdir=input_dir,firstyr=1960,
    #                  fleetnams=c(OMI@Fleets$name,'ALL OTH'),
    #                  areanams=OMI@areanams)

    render(input="Source/OMreport.Rmd", output_file=paste(input_dir,"/Report.pdf",sep=""))
    print(paste0("Report written: ", paste(input_dir,"/Report.pdf",sep="")))

  }
}


indfit<-function(SSB,ind,Year,sim=F,plot=T,lcex=0.8){

  if(sim){
    AC<-0.6
    beta<-0.7
    n<-30
    SSB<-exp(runif(1)*2+sin(runif(1)*5+seq((1:n))/3)*rnorm(n,1,0.3))
    ind<-SSB^beta
    Res<-rnorm(n,1,0.2)
    for(y in 2:n)Res[y]<-AC*Res[y-1]+Res[y]*(1-AC*AC)^0.5
    ind<-ind*Res
    Year<-1981:(1981+n-1)
  }

  SSB<-SSB/mean(SSB)
  ind<-ind/mean(ind)

  if(plot){
    par(mfrow=c(1,2),mai=c(0.7,0.5,0.05,0.01),omi=c(0.01,0.2,0.01,0.01))
    plot(SSB,ind,xlab="",ylab="",pch=19,col=rgb(0,0,0,0.5))
    mtext("Model estimate",1,line=2.2)
    mtext("Index",2,outer=T,line=0)
  }

  getbeta<-function(beta,x,y)sum((y-x^beta)^2)
  opt<-optimize(getbeta,x=SSB,y=ind,interval=c(0.1,10))
  res<-ind-(SSB^opt$minimum)
  ac<-acf(res,plot=F)$acf[2,1,1] # lag-1 autocorrelation


  if(plot){
    SSBseq<-seq(min(SSB),max(SSB),length.out=1000)
    lines(SSBseq,SSBseq^opt$minimum,col='#0000ff90',pch=19)
    legend('bottomright',legend=round(c(sum((ind-SSB)^2),opt$objective),3),text.col=c("black","blue"),bty='n',title="SSQ",cex=lcex)
    legend('topleft',legend=round(opt$minimum,3),text.col="blue",bty='n',title='Hyper-stability, beta',cex=lcex)
    legend('left',legend=round(cor(SSB,ind),3),bty='n',title='Correlation',cex=lcex)

    plot(Year,SSB,ylab="",xlab="",ylim=range(c(ind,SSB)),type="l")
    mtext("Year",1,line=2.2)
    points(Year,ind,col='#ff000090',pch=19)
    legend('topleft',legend=round(ac,3),text.col="red",bty='n',title="Lag 1 autocorrelation",cex=lcex)
    legend('bottomleft',legend=round(sd(res),3),text.col="red",bty='n',title="Residual StDev",cex=lcex)
    legend('topright',legend=c("Model estimate","Index"),text.col=c("black","red"),bty='n',cex=lcex)
  }

  list(stats=data.frame(beta=opt$minimum,AC=ac,sd=sd(ind/(SSB^opt$minimum)),cor=cor(SSB,ind)),mult=ind/(SSB^opt$minimum))

}


vmovplot<-function(vec,N,mov,anam){
  ns<-dim(mov)[1]
  nrr<-dim(mov)[2]

  sref<-(1:ns)[c(2:ns,1)]
  v2<-apply(N,c(1,3),sum)

  xlim<-c(0,(4+nrr)*ns)

  ncol<-100
  vcol<-rainbow(ncol,s=1,v=1,start=0,end=0.45,alpha=1)[ncol:1]
  for(i in 1:50)vcol[i]<-makeTransparent(vcol[i],i/5*10)
  #plot(1:100,pch=19,cex=2,col=vcol)

  colsv<-array(vcol[ceiling((vec/max(vec))*ncol)],dim(vec))
  colsN<-array(vcol[ceiling((N/max(N))*ncol)],dim(N))
  colsv2<-array(vcol[ceiling((v2/max(v2))*ncol)],dim(v2))

  par(mai=rep(0.01,4),omi=rep(0.01,4))
  plot(c(-4,ns*(nrr+4)),c(0,nrr+3.5),col="white",axes=F)

  for(s in 1:ns){

    sc<-(s-1)*(nrr+4)

    for(r in 1:nrr){

      polygon(x=c(sc+1+nrr,sc+2+nrr,sc+2+nrr,sc+1+nrr),y=c(r-1,r-1,r,r),col=colsv[sref[s],r],border="white")

      for(r2 in 1:nrr){

        polygon(x=c(sc+(nrr-r2),sc+1+(nrr-r2),sc+1+(nrr-r2),sc+(nrr-r2)),y=c(r-1,r-1,r,r),col=colsN[s,r,r2],border='white')

      }

      #polygon(x=c(sc+2+(nrr-r),sc+3+(nrr-r),sc+3+(nrr-r),sc+2+(nrr-r)),y=c(0,0,1,1),col=colsv2[s,r],border='white')

    }

    for(r in 1:nrr){
      text(-2.5,r-0.5,anam[r],cex=0.75)
      polygon(x=c(sc+(nrr-r),sc+1+(nrr-r),sc+1+(nrr-r),sc+(nrr-r)),y=c(r-1,r-1,r,r),col=NA,border='light grey')

    }

    text(sc+nrr/2,nrr+0.75,paste0("Season ",s),font=1,cex=0.8)

    arrows(x0=sc+nrr+2.5,x1=sc+nrr+3.5,y0=nrr/2,y1=nrr/2,length=0.1,lwd=2)

  }

}


plot_mov_dist<-function(out,OMI,pnam=c("Eastern stock","Western stock")){

  its<-10
  acs<-c(1,6,12)

  for(pp in 1:out$np){
  for(ac in 1:out$nma){

    movE<-out$mov[pp,,acs[ac],,]

    nr<-out$nr
    ns<-out$ns

    vecE<--array(NA,c(its,ns,nr))
    NE<-array(NA,c(its,ns,nr,nr))

    for(i in 1:its){

      for(j in 1:ns){

        if(i==1&j==1){
           vecE[1,1,]<-rep(1/nr,nr)
        }else if(j==1){
           vecE[i,1,]<-apply(NE[i-1,ns,,],2,sum)
        }else{
           vecE[i,j,]<-apply(NE[i,j-1,,],2,sum)
        }

        NE[i,j,,]<-vecE[i,j,]*movE[j,,]

      }

    }

    vmovplot(vecE[its,,],NE[its,,,],movE,anam=OMI@areanams)
    legend('topleft',legend=paste0(pnam[pp],", Age class ",ac),bty='n',cex=0.9)
  }
  }

}


#MSY_FAST(FML=out$FL[out$ny,,,,], iALK=out$iALK[,out$ny,,], N=out$N[,out$ny,,,],
#         wt_age=t(out$wt_age[out$ny,,]), M_age=out$M_age, mat_age=out$mat_age,
#         R0s=out$R0, hs=out$h)


meanFs<-function(FML,iALK,N,wt_age,M_age,mat_age,R0s,hs,toly=1e-3,rnams=c("East","West")){
  # FML                                    # s, r, f, l
  # iALK                                   # p, a, l
  np<-dim(wt_age)[1]
  Ftot<-array(NA,c(dim(iALK)[1],dim(FML),dim(iALK)[2])) # p, s, r, f, l, a
  Find<-TEG(dim(Ftot))
  Ftot[Find]<-FML[Find[,2:5]]*iALK[Find[,c(1,6,5)]] # p s r f a x p a l
  FM<-apply(Ftot,c(1,2,3,4,6), sum) # p, s, r, f, a    (sum over lengths)
  Nr<-N/apply(N,1,mean)             # p, s, a, r      (normalized to mean 1)
  wFM<-array(NA,dim(FM))            # p, s, r, f, a (weighted fishing mortality rate at age)
  FMind<-TEG(dim(FM))
  wFM[FMind]<-FM[FMind]*Nr[FMind[,c(1,2,5,3)]]
  wFM2<-apply(wFM,c(1,2,4,5),mean)
  wFM2<-apply(wFM2,c(1,4),sum) #what is the F at age profile?
  matplot(t(wFM2),type='l',xlab="Age",ylab="F")
  legend('topright',legend=rnams,bty='n',text.col=c("black","red"))

}


#FML=out$FL; iALK=out$iALK; N=out$N; wt_age=t(out$wt_age[out$ny,,]); M_age=out$M_age; mat_age=out$mat_age;R0s=out$R0; hs=out$h

timeFs<-function(FML,iALK,N,wt_age,M_age,mat_age,R0s,hs,toly=1e-3,rnams=c("East","West")){
  # FML                                    # y, s, r, f, l
  # iALK                                   # p, y, a, l
  # N                                      # p, y, s, a, r
  np<-dim(wt_age)[1]
  ny<-dim(FML)[1]
  ns<-dim(FML)[2]
  nr<-dim(FML)[3]
  nf<-dim(FML)[4]
  nl<-dim(FML)[5]
  na<-dim(iALK)[3]
  Ftot<-array(NA,c(ny,np,ns,nr,nf,nl,na)) # y, p, s, r, f, l, a
  Find<-TEG(dim(Ftot))
  Ftot[Find]<-FML[Find[,c(1,3:6)]]*iALK[Find[,c(2,1,7,6)]] # y p s r f a x p a l
  FM<-apply(Ftot,c(1:5,7), sum) # y, p, s, r, f, a    (sum over lengths)
  Cat<-array(NA,dim(FM))            # y, p, s, r, f, a (weighted fishing mortality rate at age)
  FMind<-TEG(dim(FM))
  Cat[FMind]<-(1-exp(-FM[FMind]))*N[FMind[,c(2,1,3,6,4)]] # y, p, s, r, f, a
  sumCat<-apply(Cat,c(2,1,3,6),sum) # p, y, s, a
  sumN<-apply(N,1:4,sum)            # p, y, s, a
  UbyS<-sumCat/sumN
  muU<-apply(UbyS,c(1,2,4),mean)   # p, y, a # not ideal mean U across seasons but very close to best
  -log(1-apply(muU,1:2,max))  # p, y

}

timeF25<-function(FML,iALK,N,wt_age,M_age,mat_age,R0s,hs,toly=1e-3,rnams=c("East","West")){
  # FML                                    # y, s, r, f, l
  # iALK                                   # p, y, a, l
  # N                                      # p, y, s, a, r
  np<-dim(wt_age)[1]
  ny<-dim(FML)[1]
  ns<-dim(FML)[2]
  nr<-dim(FML)[3]
  nf<-dim(FML)[4]
  nl<-dim(FML)[5]
  na<-dim(iALK)[3]
  Ftot<-array(NA,c(ny,np,ns,nr,nf,nl,na)) # y, p, s, r, f, l, a
  Find<-TEG(dim(Ftot))
  Ftot[Find]<-FML[Find[,c(1,3:6)]]*iALK[Find[,c(2,1,7,6)]] # y p s r f a x p a l
  FM<-apply(Ftot,c(1:5,7), sum) # y, p, s, r, f, a    (sum over lengths)
  Cat<-array(NA,dim(FM))            # y, p, s, r, f, a (weighted fishing mortality rate at age)
  FMind<-TEG(dim(FM))
  Cat[FMind]<-(1-exp(-FM[FMind]))*N[FMind[,c(2,1,3,6,4)]] # y, p, s, r, f, a
  sumCat<-apply(Cat,c(2,1,3,6),sum) # p, y, s, a
  sumN<-apply(N,1:4,sum)            # p, y, s, a
  UbyS<-sumCat/sumN
  muU<-apply(UbyS[,,,2:5],c(1,2,4),mean)   # p, y, a # not ideal mean U across seasons but very close to best
  -log(1-apply(muU,1:2,mean))  # p, y

}


MSY_FAST<-function(FML,iALK,N,wt_age,M_age,mat_age,R0s,fixpars,toly=1e-3,rnams=c("East","West"),SRtypes=c('BH','BH')){
  # FML                                    # s, r, f, l
  # iALK                                   # p, a, l
  np<-dim(wt_age)[1]
  Ftot<-array(NA,c(dim(iALK)[1],dim(FML),dim(iALK)[2])) # p, s, r, f, l, a
  Find<-TEG(dim(Ftot))
  Ftot[Find]<-FML[Find[,2:5]]*iALK[Find[,c(1,6,5)]] # p s r f a x p a l
  FM<-apply(Ftot,c(1,2,3,4,6), sum) # p, s, r, f, a    (sum over lengths)
  Nr<-N/apply(N,1,mean)             # p, s, a, r      (normalized to mean 1)
  wFM<-array(NA,dim(FM))            # p, s, r, f, a (weighted fishing mortality rate at age)
  FMind<-TEG(dim(FM))
  wFM[FMind]<-FM[FMind]*Nr[FMind[,c(1,2,5,3)]]
  wFM2<-apply(wFM,c(1,2,4,5),mean)
  wFM2<-apply(wFM2,c(1,4),sum) #what is the F at age profile?
  # matplot(t(wFM2),type='l',xlab="Age",ylab="F"); legend('topright',legend=c("East","West"),bty='n',text.col=c("black","red"))


  res<-array(NA,dim=c(np,8)) # FMSY, UMSY, MSY, BMSY, SSBMSY, BMSY/B0, SSBMSY/SSB0, RMSY/R0

  for(pp in 1:np){
    opt<-optimize(getMSYfast,c(-6,6),tol=toly,Fa=wFM2[pp,],Ma=M_age[pp,],Wa=wt_age[pp,],mat=mat_age[pp,],R0=R0s[pp],fixpar=fixpars[pp],SRtype=SRtypes[pp],mode=1)
    res[pp,]<-getMSYfast(opt$minimum,Fa=wFM2[pp,],Ma=M_age[pp,],Wa=wt_age[pp,],mat=mat_age[pp,],R0=R0s[pp],fixpar=fixpars[pp],SRtype=SRtypes[pp],mode=2)
  }

  res<-as.data.frame(res)
  names(res)<-c("MSY","FMSYap","UMSY","BMSY","SSBMSY","BMSY_B0","SSBMSY_SSB0","RMSY_R0")
  row.names(res)<-rnams
  res
}

getMSYfast<-function(lnq,Fa,Ma,Wa,mat,R0,fixpar,SRtype,mode=1,nits=150){

  if(grepl("BH",SRtype)){
    h=0.2+1/(1+exp(-fixpar))*0.8
  }else if(grepl("HS",SRtype)){
    inflect=1/(1+exp(-fixpar))
  }

  q<-exp(lnq)
  na<-length(Ma)

  N0<-exp(-(cumsum(Ma)-Ma/2))*R0
  B0<-sum(N0*Wa)
  SSB0<-sum(N0*Wa*mat)
  SSBpR<-SSB0/R0

  Z<-q*Fa+Ma
  surv<-exp(-(cumsum(Z)-Z/2))
  Rtemp<-R0/3

  # Rs<-Bs<-rep(NA,nits)

  for(i in 1:nits){ # run to equilibrium recruitment conditions

    N<-surv*Rtemp
    SSBMSY<-sum(N*Wa*mat)
    BMSY<-sum(N*Wa)

    if(grepl("BH",SRtype)){
      Rtemp<-((0.8*R0*h*SSBMSY)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSBMSY))
    }else if(grepl("HS",SRtype)){
      SSBref<-SSB0*inflect
      if(SSBMSY<SSBref){
        Rtemp=R0*SSBMSY/SSBref
      }else{
        Rtemp=R0
      }
    }

    #Bs[i]<-SSBMSY/SSB0;
    #Rs[i]<-Rtemp/R0
  }

  MSY<-sum(Wa*N*exp(Z/2)*(1-exp(-Z))*(q*Fa)/Z)
  #MSY<-sum(Wa*N*(1-exp(-Z))*(q*Fa)/Z)

  #print(c(max(Fa),MSY/BMSY,MSY,BMSY/B0))

  if(mode==1){
    return(-MSY)
  }else{
    return(c(MSY,max(q*Fa),MSY/BMSY,BMSY,SSBMSY,BMSY/B0,SSBMSY/SSB0,Rtemp/R0))
  }

}

SRopt<-function(out,plot=F,quiet=F,years=NULL,type="BH",just_R0=F,h=0.98){

  blocksize<-out$ny/max(out$RDblock)

  yrs1<-yrs<-match(1:max(out$RDblock),out$RDblock)+floor(blocksize/2)
  if(!is.null(years))yrs<-yrs[yrs1>=years[1]&yrs1<=years[2]]
  paryrs<-match(yrs,yrs1)

  opt<-new('list')
  resid<-new('list') #array(NA,c(out$np,3))
  pnam<-c("East","West")

  if(plot)par(mfrow=c(1,out$np),mai=c(0.4,0.5,0.1,0.05),omi=c(0.5,0.5,0.01,0.01))

  for(pp in out$np:1){

    R0temp<-mean(exp(out$lnRD[pp,1:2])*out$muR[pp]) # have a guess at R0 for initializing nlm

    SSBpR=sum(exp(-cumsum(c(0,out$M_age[1:(out$na-1)])))*out$mat_age[pp,]*out$wt_age[out$ny,,pp]) #SSBpR based on M, mat and growth
    SSB=out$SSB[pp,yrs,out$spawns[pp]]
    rec=out$muR[pp]*exp(out$lnRD[pp,paryrs])

    #fscale<-getSteepness(pars,SSB=SSB,rec=rec, SSBpR=SSBpR,mode=1,plot=F)
    #opt<-nlm(getSR,p=pars,typsize=c(0.5,log(R0temp)),fscale=fscale,hessian=T,print.level=2,

    if(type=="BH"){

      if(just_R0){
        pars<-log(R0temp) # guess / starting values
        opt[[pp]]<-optim(pars,getBH_R0,method="L-BFGS-B",lower=log(R0temp/50),upper=log(R0temp*50),hessian=T,
                         SSB=SSB,rec=rec,SSBpR=SSBpR,h=h,mode=1,plot=F)
        devs<-getBH_R0(opt[[pp]]$par,SSB,rec,SSBpR,h=h,mode=2,plot=plot)
        resid[[pp]]<-data.frame(yrs=yrs,SSB=SSB,rec=rec,devs=devs)

      }else{
        pars<-c(0.5,log(R0temp)) # guess / starting values
        opt[[pp]]<-optim(pars,getBH,method="L-BFGS-B",lower=c(-6.,log(R0temp/50)),upper=c(6.,log(R0temp*50)), hessian=T,
                  SSB=SSB,rec=rec,SSBpR=SSBpR,mode=1, plot=F)

        devs<-getBH(opt[[pp]]$par,SSB,rec,SSBpR,mode=2,plot=plot)
        resid[[pp]]<-data.frame(yrs=yrs,SSB=SSB,rec=rec,devs=devs)
      }

    }else if(type=="HS"){

      if(just_R0){

        pars<-log(R0temp) # guess / starting values
        opt[[pp]]<-optim(pars,getHS_R0,method="L-BFGS-B",lower=log(R0temp/50),upper=log(R0temp*50), hessian=T,
                         SSB=SSB,rec=rec,SSBpR=SSBpR,h=h,mode=1, plot=F)

        devs<-getHS_R0(opt[[pp]]$par,SSB,rec,SSBpR,h=h,mode=2,plot=plot)
        resid[[pp]]<-data.frame(yrs=yrs,SSB=SSB,rec=rec,devs=devs)

      }else{

        pars<-c(-0.5,log(R0temp)) # guess / starting values
        opt[[pp]]<-optim(pars,getHS,method="L-BFGS-B",lower=c(-6.,log(R0temp/50)),upper=c(6.,log(R0temp*50)), hessian=T,
                         SSB=SSB,rec=rec,SSBpR=SSBpR,mode=1, plot=F)

        devs<-getHS(opt[[pp]]$par,SSB,rec,SSBpR,mode=2,plot=plot)
        resid[[pp]]<-data.frame(yrs=yrs,SSB=SSB,rec=rec,devs=devs)

      }

    }

    if(plot)legend('topleft',legend=pnam[pp],bty='n')

  }

  if(plot)  mtext("Spawning Biomass (kg)",1,line=0.8,outer=T);mtext("Recruits (n)",2,line=0.8,outer=T)

  if(type=="BH"){

    if(just_R0){

      logith<-rep(-log(1/((h-0.2)/0.8)-1),out$np)
      lnR0<-sapply(opt,FUN=function(x)x$par[1])
      VC<-lapply(opt,FUN=function(x)solve(x$hessian))

      if(!quiet)return(list(type=rep(type,out$np),par1=logith,lnR0=lnR0,VC=VC,resid=resid))

    }else{

      logith<-sapply(opt,FUN=function(x)x$par[1])#0.2+1/(1+exp(-sapply(opt,FUN=function(x)x$par[1])))*0.8
      lnR0<-sapply(opt,FUN=function(x)x$par[2])
      VC<-lapply(opt,FUN=function(x)solve(x$hessian))

      if(!quiet)return(list(type=rep(type,out$np),par1=logith,lnR0=lnR0,VC=VC,resid=resid))

    }
  }else if(type=="HS"){

    if(just_R0){

      inflect<-0.2/rep(h,out$np)
      logitinflect<--log(1/inflect-1)
      lnR0<-sapply(opt,FUN=function(x)x$par[1])
      VC<-lapply(opt,FUN=function(x)solve(x$hessian))

      if(!quiet)return(list(type=rep(type,out$np),par1=logitinflect,lnR0=lnR0,VC=VC,resid=resid))

    }else{

      logitinflect<-sapply(opt,FUN=function(x)x$par[1])
      lnR0<-sapply(opt,FUN=function(x)x$par[2])
      VC<-lapply(opt,FUN=function(x)solve(x$hessian))

      if(!quiet)return(list(type=rep(type,out$np),par1=logitinflect,lnR0=lnR0,VC=VC,resid=resid))

    }

  }

}


getBH<-function(pars,SSB,rec,SSBpR,mode=1,plot=F){

  h<-0.2+1/(1+exp(-pars[1]))*0.8
  R0<-exp(pars[2])

  recpred<-((0.8*R0*h*SSB)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB))

  if(plot){
    ord<-order(SSB)
    plot(SSB[ord],rec[ord],ylim=c(0,max(rec,R0)),xlim=c(0,max(SSB,R0*SSBpR)),xlab="",ylab="")
    SSB2<-seq(0,R0*SSBpR,length.out=500)
    recpred2<-((0.8*R0*h*SSB2)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB2))
    lines(SSB2,recpred2,col='blue')
    abline(v=c(0.2*R0*SSBpR,R0*SSBpR),lty=2,col='red')
    abline(h=c(R0,R0*h),lty=2,col='red')
    legend('topright',legend=c(paste0("h = ",round(h,3)),paste0("lnR0 = ",round(log(R0),3))),bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    return(-sum(dnorm(log(recpred)-log(rec),0,0.5,log=T))-dnorm(pars[1],0,6,log=T)) # add a vague prior on h = 0.6
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }

}

getBH_R0<-function(pars,SSB,rec,SSBpR,h,mode=1,plot=F){

  R0<-exp(pars[1])

  recpred<-((0.8*R0*h*SSB)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB))

  if(plot){
    ord<-order(SSB)
    plot(SSB[ord],rec[ord],ylim=c(0,max(rec,R0)),xlim=c(0,max(SSB,R0*SSBpR)),xlab="",ylab="")
    SSB2<-seq(0,R0*SSBpR,length.out=500)
    recpred2<-((0.8*R0*h*SSB2)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB2))
    lines(SSB2,recpred2,col='blue')
    abline(v=c(0.2*R0*SSBpR,R0*SSBpR),lty=2,col='red')
    abline(h=c(R0,R0*h),lty=2,col='red')
    legend('topright',legend=c(paste0("h = ",round(h,3)),paste0("lnR0 = ",round(log(R0),3))),bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    return(-sum(dnorm(log(recpred)-log(rec),0,0.5,log=T)))
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }

}

getHS<-function(pars,SSB,rec,SSBpR,mode=1,plot=F){

  inflect<-exp(pars[1])/(1+exp(pars[1]))
  R0<-exp(pars[2])
  SSB0<-R0*SSBpR

  recpred<-rep(R0,length(SSB))
  cond<-SSB<inflect*SSB0
  recpred[cond]<-R0*SSB[cond]/(SSB0*inflect)


  if(plot){
    ord<-order(SSB)
    plot(SSB[ord],rec[ord],ylim=c(0,max(rec,R0)),xlim=c(0,max(SSB,R0*SSBpR)),xlab="",ylab="")
    SSB2<-seq(0,R0*SSBpR,length.out=500)
    recpred2<-rep(R0,length(SSB2))
    cond<-SSB2<inflect*SSB0
    recpred2[cond]<-R0*SSB2[cond]/(SSB0*inflect)

    lines(SSB2,recpred2,col='blue')
    abline(v=c(0.2*SSB0,inflect*SSB0,SSB0),lty=2,col='red')
    h<-0.2/inflect
    R0h<-R0*h
    abline(h=c(R0,R0h),lty=2,col='red')

    legend('topright',legend=c(paste0("Inflec. = ",round(inflect,3)),paste0("lnR0 = ",round(log(R0),3)),paste0("eqiv h = ",round(h,3))),bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    return(-sum(dnorm(log(recpred)-log(rec),0,0.5,log=T))-dnorm(pars[1],0,4,log=T))
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }

}

getHS_R0<-function(pars,SSB,rec,SSBpR,h=0.98,mode=1,plot=F){

  inflect<-0.2/h#exp(pars[1])/(1+exp(pars[1]))
  R0<-exp(pars[1])
  SSB0<-R0*SSBpR

  recpred<-rep(R0,length(SSB))
  cond<-SSB<inflect*SSB0
  recpred[cond]<-R0*SSB[cond]/(SSB0*inflect)


  if(plot){
    ord<-order(SSB)
    plot(SSB[ord],rec[ord],ylim=c(0,max(rec,R0)),xlim=c(0,max(SSB,R0*SSBpR)),xlab="",ylab="")
    SSB2<-seq(0,R0*SSBpR,length.out=500)
    recpred2<-rep(R0,length(SSB2))
    cond<-SSB2<inflect*SSB0
    recpred2[cond]<-R0*SSB2[cond]/(SSB0*inflect)

    lines(SSB2,recpred2,col='blue')
    abline(v=c(0.2*SSB0,inflect*SSB0,SSB0),lty=2,col='red')
    h<-0.2/inflect
    R0h<-R0*h
    abline(h=c(R0,R0h),lty=2,col='red')

    legend('topright',legend=c(paste0("Inflec. = ",round(inflect,3)),paste0("lnR0 = ",round(log(R0),3)),paste0("eqiv h = ",round(h,3))),bty='n')
  }

  if(mode==1){
    return(sum(((recpred-rec)/10000)^2))
    #return(-sum(dnorm(log(recpred)-log(rec),0,0.5,log=T)))
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }

}


DesignEffect<-function(Sres1,Sres2,Design){

 nfac<-length(Design$all_levs)

 par(mfrow=c(nfac,4),mai=c(0.2,0.4,0.5,0.01),omi=c(0.5,0.4,0.4,0.05))
 cols=c("#ff000050","#0000ff50","#00ff0050")
 lcols<-c('red','blue','green')

 facnam<-paste("-----",c("Mort. / Mat.","Steepness","Index type","Depletion"),"-----")

 nams<-c("UMSY","MSY","D","OFL")
 toplabline<-3.5
 cexp<-1

 for(i in 1:nfac){

   col<-cols[Design$Design_Ref[,i]]

   plot(Sres1[,4],Sres2[,4],col=col,pch=19,cex=1,xlab="",ylab="")
   legend('topleft',legend=Design$all_nams[[i]],text.col=lcols[1:max(Design$all_levs[[i]])],bty='n',cex=0.8)

   if(i==1)mtext("UMSY",3,outer=F,line=toplabline)

   plot(Sres1[,2],Sres2[,2],col=col,pch=19,cex=1,xlab="",ylab="")
   if(i==1)mtext("MSY",3,outer=F,line=toplabline)

   mtext(facnam[i],3,line=1,adj=0.8,cex=0.9)

   plot(Sres1[,11],Sres2[,11],col=col,pch=19,cex=1,xlab="",ylab="")
   if(i==1)mtext("Depln.",3,outer=F,line=toplabline)

   plot(Sres1[,13],Sres2[,13],col=col,pch=19,cex=1,xlab="",ylab="")
   if(i==1)mtext("OFL",3,outer=F,line=toplabline)

 }

 mtext("Eastern stock",1,outer=T,line=1.6)
 mtext("Western stock",2,outer=T,line=0.8)

}


build_OMs<-function(dir="C:/M3",nsim=32,proyears=30,seed=1,targpop=NA){

  if(!file.exists(dir)){
    print(paste('Directory:',dir,'does not exist'))
    stop()
  }

  OMdirs<-list.dirs(dir)

  if(length(OMdirs)>1){
    OMdirs<-OMdirs[2:length(OMdirs)]
    nOMs<-length(OMdirs)
    fileind<-rep(NA,nOMs)
    foldnams<-strsplit(OMdirs,"/")
    lastfolder<-length(foldnams[[1]])
    lfnams<-unlist(lapply(foldnams,FUN=function(X)X[lastfolder]))
    OMdirs<-OMdirs[order(as.numeric(lfnams))]
  }else{
    nOMs<-1
  }

  for(i in 1:nOMs){
    OM<-new('OM',OMd=OMdirs[i],nsim=nsim, proyears=proyears,seed=seed,targpop=targpop)
    save(OM,file=paste0(OMdirs[i],"/OM"))
  }
#
}

makeCAL2<-function(CAA,iALK){
  ny<-dim(CAA)[3]
  na<-dim(CAA)[2]
  ns<-dim(CAA)[1]
  nl<-dim(iALK)[5]
  CAL<-array(NA,dim=c(ns,nl,ny))
  for(i in 1:ns){
    for(j in 1:ny){
      CAL[i,,j]<-apply(CAA[i,,j]*iALK[i,1,j,,],2,sum)
    }
  }
  CAL
}

makeCAL3<-function(CAA,iALK){
  ny<-dim(CAA)[3]
  na<-dim(CAA)[2]
  ns<-dim(CAA)[1]
  nl<-dim(iALK)[4]
  CAL<-array(NA,dim=c(ns,nl,ny))
  for(i in 1:ns){
    for(j in 1:ny){
      CAL[i,,j]<-apply(CAA[i,,j]*iALK[i,1,,],2,sum) # currently
    }
  }
  CAL
}

calcVBpars<-function(La,plot=F){

  pars<-c(0,log(La[1]/La[length(La)]),log(max(La)))
  opt<-optim(pars,getVBpars,method="L-BFGS-B",
             lower=c(-3.,log(0.05),log(max(La))),
             upper=c(1,log(0.8),log(max(La))*1.2), hessian=T,
             La=La,plot=F)
  if(plot)getVBpars(opt$par,La,plot=T)
  list(t0=opt$par[1],K=exp(opt$par[2]),Linf=exp(opt$par[3]))

}

getVBpars<-function(pars,La,plot=F){

  t0<-pars[1]
  K<-exp(pars[2])
  Linf<-exp(pars[3])
  Lapred<-Linf*(1-exp(-K*((1:length(La))-t0)))
  if(plot){
    plot(La)
    lines(Lapred,col='green')
  }
  return(sum((Lapred-La)^2))

}


calcABpars<-function(La,Wa,plot=F){

  na<-length(Wa)
  ga<-Wa[na]/La[na]^3
  pars<-c(log(ga),log(3))


  opt<-optim(pars,getABpars,method="L-BFGS-B",
             lower=c(log(ga/10),log(2.7)),
             upper=c(log(ga*10),log(3.3)), hessian=T,
             La=La,Wa=Wa,plot=F)
  if(plot)getABpars(opt$par,La,Wa,plot=T)
  list(a=exp(opt$par[1]),b=exp(opt$par[2]))

}

getABpars<-function(pars,La,Wa,plot=F){

  Wapred<-exp(pars[1])*La^exp(pars[2])
  if(plot){
    plot(Wa)
    lines(Wapred,col='green')
  }
  return(sum((Wapred-Wa)^2))

}
getclass <- function(x,classy) inherits(get(x),classy)

avail<-function(classy){

    #return(unique(c(ls('package:ABT-MSE')[unlist(lapply(ls('package:ABT-MSE'),
    #                                                    getclass,classy=classy))], ls(envir=.GlobalEnv)[unlist(
    #                                                      lapply(ls(envir=.GlobalEnv),getclass,classy=classy))])))
  return(c(ls(envir=.GlobalEnv)[unlist(lapply(ls(envir=.GlobalEnv),getclass,classy=classy))]))

}



# End of source file
