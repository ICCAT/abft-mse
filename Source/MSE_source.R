# ===================================================================================================================
# ==== MSE source code ==============================================================================================
# ===================================================================================================================

cat("Installing and loading libraries")
cat("\n")
if(!require('snowfall'))install.packages('snowfall',repos='http://cran.stat.sfu.ca')
if(!require('maps'))install.packages('maps',repos='http://cran.stat.sfu.ca')
if(!require('mapdata'))install.packages('mapdata',repos='http://cran.stat.sfu.ca')
if(!require('wordcloud'))install.packages('wordcloud',repos='http://cran.stat.sfu.ca')
if(!require('abind'))install.packages('abind',repos='http://cran.stat.sfu.ca')
if(!require('SDMTools'))install.packages('SDMTools',repos='http://cran.stat.sfu.ca')
if(!require('PBSmapping'))install.packages('PBSmapping',repos='http://cran.stat.sfu.ca')
if(!require('MASS'))install.packages('MASS',repos='http://cran.stat.sfu.ca')
if(!require('shiny'))install.packages('shiny',repos='http://cran.stat.sfu.ca')
if(!require('parallel'))install.packages('parallel',repos='http://cran.stat.sfu.ca')
if(!require('lubridate'))install.packages('lubridate',repos='http://cran.stat.sfu.ca')
if(!require('rmarkdown'))install.packages('rmarkdown',repos='http://cran.stat.sfu.ca')

library(snowfall)
library(maps)
library(mapdata)
library(wordcloud)
library(abind)
library(SDMTools)
library(PBSmapping)
library(MASS)
library(shiny)
library(parallel)
library(lubridate)
library(rmarkdown)

#library(FLCore)
#library(FLAssess)
#library(FLXSA)
cat("\n")
cat("Loading objects")
cat("\n")
source("Source/Objects.r")
cat("Loading methods")
cat("\n")
source("Source/Methods.r")
cat("Loading management procedures")
cat("\n")
source("Source/MPs.r")
cat("Loading implementation error models")
cat("\n")
source("Source/IE_models.r")
cat("Loading diagnostics")
cat("\n")
source("Source/Data_formatting.r")
cat("Loading data formatting functions")
cat("\n")
source("Source/Plotting.r")
cat("Loading plotting functions")
cat("\n")
source("Source/Diagnostics.r")

tiny=1E-15

load("Data/Processed/Area definitions/AreaDefs")
load("Data/Processed/Area definitions/AreaNames")

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
  mode=3
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
  nZeq=OMd@nZeq
  nydist=OMd@nydist
  nyeq=OMd@nyeq
  tol=toly
  
  par=log(0.05)
  qs<-OM@q[ss,]
  
  # For getMSYrefs
  par=log(sum(.Object@q[ss,]))
  nyears<-70
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
  
  if(mode%in%c(1,2,4,5)){ # Start MSY optimization at approx 30% B0
    
    N<-N*0.3
    SSB<-SSB*0.3
    
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
  # ! Anders Nielsen's code !
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
  out$ny<-scan(repfile,skip=3,nlines=1,quiet=quiet)
  out$ns<-scan(repfile,skip=5,nlines=1,quiet=quiet)
  out$nr<-scan(repfile,skip=7,nlines=1,quiet=quiet)
  out$nf<-scan(repfile,skip=9,nlines=1,quiet=quiet)
  out$na<-scan(repfile,skip=11,nlines=1,quiet=quiet)
  out$nl<-scan(repfile,skip=13,nlines=1,quiet=quiet)
  out$nma<-scan(repfile,skip=15,nlines=1,quiet=quiet)
  
  np<-out$np   
  ny<-out$ny
  ns<-out$ns
  nr<-out$nr
  nf<-out$nf
  na<-out$na
  nl<-out$nl
  nma<-out$nma
  
  st<-17
  #out$ma<-ADMBrep(repfile,st,ADMBdim=c(np,na),quiet=quiet)
  #st<-st+1+np
  out$SSB<-ADMBrep(repfile,st,ADMBdim=c(np,ny,ns),quiet=quiet)
  st<-st+1+np*ny
  out$FL<-ADMBrep(repfile,st,c(ny,ns,nr,nf,nl),quiet=quiet)
  st<-st+1+ny*ns*nr*nf
  out$NCobs<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$Cobs<-ADMBrep(repfile,st,c(out$NCobs,5),quiet=quiet)
  st<-st+1+out$NCobs
  out$Cpred<-ADMBrep(repfile,st,c(ny,ns,nr,nf),quiet=quiet)
  st<-st+1+ny*ns*nr
  out$nCLobs<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$CLobs<-ADMBrep(repfile,st,c(out$nCLobs,6),quiet=quiet)
  st<-st+1+out$nCLobs
  out$CLtotpred<-ADMBrep(repfile,st,c(ny,ns,nr,nf,nl),quiet=quiet)
  st<-st+1+ny*ns*nr*nf
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
  out$h<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$RDblock<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  fec<-ADMBrep(repfile,st,c(np,na),quiet=quiet)
  out$mat_age<-fec/t(out$wt_age[1,,])
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
  out$lnqs<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$nZeq<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$nydist<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$nyeq<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$SSB0<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$B0<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$D_ini<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$D<-scan(repfile,skip=st,nlines=1,quiet=quiet)
  st<-st+2
  out$datacheck<-scan(repfile,skip=st,nlines=1,quiet=quiet)
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


runM3<-function(x,M3temp='C:/M3temp'){
  setwd(paste(M3temp,x,sep="/"))
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
  
  write("# ny number of years",datfile,1,append=F)
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
  
  write("# nZeq Number of years at the start of the model to calculate equilibrium Z from (mean of nZep years)",datfile,1,append=T)
  write(OMI@nZeq,datfile,OMI@ns,append=T)
  
  write("# nydist Number of years over which initial stock distribution is calculated (prior to spool up)",datfile,1,append=T)
  write(OMI@nydist,datfile,OMI@ns,append=T)
  
  write("# nyeq Number of spool-up years over which the stock is subject to nZeq, used to define equilibrium conditions",datfile,1,append=T)
  write(OMI@nyeq,datfile,OMI@ns,append=T)
  
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
  
  write("# steep, steepness of the Bev-Holt SR relationship",datfile,1,append=T)
  write(OMI@steep,datfile,OMI@np,append=T)
  
  # -- Spawning --
  
  write("# spawns, the subyear in which the stock spawns",datfile,1,append=T)
  write(OMI@spawns,datfile,OMI@np,append=T)
  
  write("# canspawn, areas in which the stock spawns",datfile,1,append=T)
  write(OMI@canspawn,datfile,OMI@np,append=T)
  
  # -- Natural Mortality rate --
  
  write("# Ma, instantaneous natural mortality rate at age",datfile,1,append=T)
  write(t(OMI@Ma),datfile,OMI@na,append=T)      
  
  # -- Fishery data --
  
  write("# nCobs, the number of catch weight observations y s r f CW",datfile,1,append=T)
  write(OMI@nCobs,datfile,1,append=T)
  
  write("# Cobs, catch weight observations y s r f C(weight)",datfile,1,append=T)
  write(t(OMI@Cobs),datfile,5,append=T)
  
  # CPUE
  
  write("# nCPUE, the number of CPUE series",datfile,1,append=T)
  write(OMI@nCPUE,datfile,1,append=T) # in this simulation this is the same as the number of fleets
  
  write("# nCPUEobs, the number of CPUE observations y s r f CPUE(weight)",datfile,1,append=T)
  write(OMI@nCPUEobs,datfile,1,append=T)
  
  write("# CPUEobs, CPUE observations y s r f cpueindex, CPUE(weight)",datfile,1,append=T)
  write(t(OMI@CPUEobs),datfile,ncol(OMI@CPUEobs),append=T)
  
  # Length composition
  
  write("# nCLobs, the number of catch-at-length observations y s r f l N",datfile,1,append=T)
  write(OMI@nCLobs,datfile,1,append=T)
  
  write("# CLobs, catch-at-length observations y s r f l N",datfile,1,append=T)
  write(t(OMI@CLobs),datfile,ncol(OMI@CLobs),append=T)
  
  # The real relative abundance index RAI (y, s, r)
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
  
  write("# nMP, age class assignment by age ",datfile,1,append=T)
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
  write(OMI@RDCV,datfile,1,append=T) # SSB index for each population
  
  # -- Likelihood weights
  
  write("# nLHw, number of likelihood components",datfile,1,append=T)
  write(OMI@nLHw,datfile,1,append=T) 
  
  write("# LHw,  likelihood components",datfile,1,append=T)
  write(OMI@LHw,datfile,OMI@nLHw,append=T) # Likelihood weights (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel)
  
  # -- Initial values
  
  write("# R0_ini, initial values for log R0",datfile,1,append=T)
  write(OMI@R0_ini,datfile,OMI@np,append=T) # Simulated R0 for each population
  
  write("# sel_ini, initial values for selectivity",datfile,1,append=T)
  write(t(OMI@sel_ini),datfile,OMI@nl,append=T) # Actual selectivity
  
  write("# selpar_ini, initial values for selectivity parameters",datfile,1,append=T)
  write(t(OMI@selpar_ini),datfile,3,append=T) # Actual selectivity
  
  write("# lnF_ini, initial values for log F",datfile,1,append=T)
  write(OMI@lnF_ini,datfile,OMI@nCobs,append=T) # log apical F
  
  write("# ilnRD_ini, initial recruitment deviations y=1 a=2:nages",datfile,1,append=T)
  write(t(OMI@ilnRD_ini),datfile,OMI@na-1,append=T) # Initial recruitment deviations
  
  write("# lnRD_ini, recruitment deviations y=1:nyears",datfile,1,append=T)
  write(t(OMI@lnRD_ini),datfile,OMI@ny,append=T) # Recruitment deviations
  
  write("# mov_ini, simulated movement p s r r",datfile,1,append=T)
  write(OMI@mov_ini,datfile,OMI@nr,append=T) # Movement probabilities
  
  write("# qCPUE_ini, initial values for CPUE catchability nCPUE",datfile,1,append=T)
  write(log(OMI@qCPUE_ini),datfile,OMI@nCPUE,append=T)
  
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
  
  write("# debug 1= run with initial values",datfile,1,append=T)
  write(OMI@debug,datfile,1,append=T) # debug switch
  
  write("# verbose 1= run with printouts",datfile,1,append=T)
  write(OMI@verbose,datfile,1,append=T) # debug switch
  
  write("# datacheck",datfile,1,append=T)
  write(OMI@datacheck,datfile,1,append=T) # datacheck
  
}


# Trial specification functions to modify operating model input files

# Natural mortality rate / M

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
    Ma<-3.0*OMI@wt_age[,,OMI@ny]^-0.288
    Ma<-Ma/(apply(Ma[,5:ncol(Ma)],1,mean))  
    Mlow<-Ma*c(0.1,0.1)
    Mhigh<-Ma*c(0.15,0.15)
  
    if(lev==1){      # East,  West
      OMI@mat<-array(c(matlow,mathigh),c(OMI@np,OMI@na,OMI@ny))
      OMI@Ma<-array(c(Mhigh[1,],Mlow[2,]),c(OMI@np,OMI@na))
    }else if(lev==2){# East,  West
      OMI@mat<-array(c(mathigh,mathigh),c(OMI@np,OMI@na,OMI@ny))
      OMI@Ma<-Mhigh
    }else{ # East,  West
      OMI@mat<-array(c(matlow,matlow),c(OMI@np,OMI@na,OMI@ny))
      OMI@Ma<-Mlow
    }
 
    return(OMI)
    
  }  
 
}

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

make_fit_reports<-function(dirs="C:/M3") {
  
  nOMs<-length(dirs)
  
  for(i in 1:nOMs){
  
    input_dir<-dirs[i]
    out<-M3read(input_dir)
    load(paste(input_dir,"/OMI",sep=""))
    
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









