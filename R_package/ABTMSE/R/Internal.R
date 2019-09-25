#' Transpose an array
#'
#' @param arr An array
#' @return the transposition of\code{arr}
#' @examples
#' tomt(array(1:120,c(4,5,6)))
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


#' Calculate coefficient of variance
#'
#' @param x a vector of length 2 or more
#' @return the coefficient of variation of \code{arr}
#' @examples
#' cv(runif(10))
cv<-function(x)  sd(x)/mean(x)

#' Calculate the lognormal standard deviation from logspace mean and standard deviations
#'
#' @param m a positive real number
#' @param sd a positive real number
#' @return the lognormal standard deviation corresponding to \code{m, sd}
#' @examples
#' sdconv(0.5,0.2)
sdconv<-function(m,sd)(log(1+((sd^2)/(m^2))))^0.5        # get log normal standard deviation from transformed space mean and standard deviation

#' Calculate the lognormal mean from logspace mean and standard deviations
#'
#' @param m a positive real number
#' @param sd a positive real number
#' @return the lognormal mean corresponding to \code{m, sd}
#' @examples
#' mconv(0.5,0.2)
mconv<-function(m,sd)log(m)-0.5*log(1+((sd^2)/(m^2)))    # get log normal mean from transformed space mean and standard deviation

#' Calculate the beta distribution parameter alpha from transformed space mean and standard deviations
#'
#' @param m a fraction
#' @param sd a positive real number
#' @return the beta distribution parameter alpha corresponding to \code{m, sd}
#' @examples
#' alphaconv(0.25,0.1)
alphaconv<-function(m,sd)m*(((m*(1-m))/(sd^2))-1)

#' Calculate the beta distribution parameter beta from transformed space mean and standard deviations
#'
#' @param m a fraction
#' @param sd a positive real number
#' @return the beta distribution parameter alpha corresponding to \code{m, sd}
#' @examples
#' betaconv(0.25,0.1)
betaconv<-function(m,sd)(1-m)*(((m*(1-m))/(sd^2))-1)

#' A log-normal probabilty density function specified according to the transformed mean and standar deviation
#' @param reps an integer value representing the number of samples
#' @param mu the desired mean of the random variable
#' @param cv the desired coefficient of variation (StDev/mean) of the random variable
#' @return a random sample produced by \code{reps, mu, cv}
#' @examples
#' x<-trlnorm(1000,2,0.1)
#' mean(x)
#' sd(x)/mean(x)
trlnorm<-function(reps,mu,cv)return(rlnorm(reps,mconv(mu,mu*cv),sdconv(mu,mu*cv)))


sampCatch<-function(Csamp,nSamp){
  out<-array(NA,dim(Csamp))
  nsim<-dim(Csamp)[1]
  nages<-dim(Csamp)[2]
  nyears<-dim(Csamp)[3]
  for(ss in 1:nsim){
    for(yy in 1:nyears){

      Csampo<-Csamp[ss,,yy]
      Csampo[Csampo<0]<-0.00000001
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


SampCatch2<-function(C,nCAAobs){
  # sim, year, subyear, area, fleet, age
  dims<-dim(C)
  nsim<-dims[1]
  nages<-dims[3]
  nyears<-dims[4]
  nsubyears<-dims[5]
  nareas<-dims[6]
  nfleets<-dims[7]

  out<- array(NA,c(nsim,nyears,nsubyears,nareas,nfleets,nages))

  for(ss in 1:nsim){
    for(yy in 1:nyears){
      for(suby in 1:nsubyears){
        for(rr in 1:nareas){
          for(ff in 1:nfleets){

            Csampo<-apply(C[ss,,,yy,suby,rr,ff],2,sum,na.rm=T)
            Csampo[Csampo<0]<-0.00000001
            #assign("Csampot",Csampo,envir=globalenv()) # debugging
            #assign("nsampt",nSamp[ss],envir=globalenv()) # debugging
            if(sum(Csampo)==0)Csampo<-rep(1/nages,nages)
            out[ss,yy,suby,rr,ff,]<-ceiling(rmultinom(1,size=nCAAobs[ss],Csampo))

          }
        }
      }
    }
  }
  out
}


makeCAL4<-function(CAA,iALK){
  nsim<-dim(CAA)[1]
  ny<-dim(CAA)[2]
  ns<-dim(CAA)[3]
  nr<-dim(CAA)[4]
  nf<-dim(CAA)[5]
  na<-dim(CAA)[6]
  nl<-dim(iALK)[5]
  CAL<-array(NA,dim=c(nsim,ny,ns,nr,nf,nl))
  for(ss in 1:nsim){
    for(yy in 1:ny){
      for(suby in 1:ns){
        for(rr in 1:nr){
          for(ff in 1:nf){
            CAL[ss,yy,suby,rr,ff,]<-apply(CAA[ss,yy,suby,rr,ff,]*iALK[ss,1,yy,,],2,sum) # currently
          }
        }
      }
    }
  }
  CAL
}

# year, subyear, area, fleet, length category, N


makeTrans<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
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


ADMBrep<-function(repfile,st,ADMBdim,quiet=T)  tomt(array(scan(repfile,skip=st,nlines=prod(ADMBdim[1:(length(ADMBdim)-1)]),quiet=quiet),ADMBdim[length(ADMBdim):1]))


MSY_FAST<-function(FML,iALK,N,wt_age,M_age,mat_age,R0s,fixpars,toly=1e-3,rnams=c("East","West"),SRtypes=c('BH','HS')){
  # FML                                    # s, r, f, l
  # iALK                                   # p, a, l

  np<-dim(wt_age)[1]

  Fprof<-meanFs(FML, iALK, N,wt_age)
  wFM2<-Fprof/apply(Fprof,1,max)
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
    h=fixpar
  }else if(grepl("HS",SRtype)){
    inflect=fixpar
  }

  q<-exp(lnq)
  na<-length(Ma)

  surv<-exp(-cumsum(Ma))
  N0<-surv*R0
  B0<-sum(N0*Wa)
  B0<-B0+surv[na]*exp(-Ma[na])/(1-exp(-Ma[na]))*Wa[na]
  SSB0<-sum(N0*Wa*mat)
  SSB0<-SSB0+N0[na]*exp(-Ma[na])/(1-exp(-Ma[na]))*Wa[na]*Ma[na]

  SSBpR<-SSB0/R0

  Z<-q*Fa+Ma
  surv<-exp(-cumsum(Z))
  Rtemp<-R0/3

  # Rs<-Bs<-rep(NA,nits)

  for(i in 1:nits){ # run to equilibrium recruitment conditions

    N<-surv*Rtemp
    SSBMSY<-sum(N*Wa*mat)
    BMSY<-sum(N*Wa)
    N[na]<-N[na]+N[na]*exp(-Z[na])/(1-exp(-Z[na]))


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

  MSY<-sum(Wa*N*exp(Z)*(1-exp(-Z))*(q*Fa)/Z) # need to add back in losses due to Z
  #MSY<-sum(Wa*N*(1-exp(-Z))*(q*Fa)/Z)

  #print(c(max(Fa),MSY/BMSY,MSY,BMSY/B0))

  if(mode==1){
    return(-MSY)
  }else{
    return(c(MSY,max(q*Fa),MSY/(BMSY+MSY),BMSY,SSBMSY,BMSY/B0,SSBMSY/SSB0,Rtemp/R0))
  }

}


DesignEffect<-function(Sres1,Sres2,Design){

  nfac<-length(Design$all_levs)

  par(mfrow=c(nfac,4),mai=c(0.2,0.4,0.5,0.01),omi=c(0.5,0.4,0.4,0.05))
  cols=c("#ff000050","#0000ff50","#00ff0050","#99999995")
  lcols<-c('red','blue','green','black')

  facnam<-paste("-----",c("Factor 1: Future recruitment","Factor 2: Abundance","Factor 3: Maturity / M"),"-----")

  nams<-c("UMSY","MSY","D","OFL")
  toplabline<-3.5
  cexp<-1
  OMind<-as.integer(Sres1[,1])

  for(i in 1:nfac){

    col<-cols[Design$Design_Ref[OMind,i]]

    plot(Sres1[,4],Sres2[,4],col=col,pch=19,cex=1,xlab="",ylab="")
    legend('topleft',legend=Design$all_levs[[i]],text.col=lcols[1:length(Design$all_levs[[i]])],bty='n',text.font=2)

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

musmooth<-function(vec){
  vec2<-c(NA,NA,vec,NA,NA)
  pos<-3:(length(vec2)-2)
  val<-cbind(vec2[pos-2],vec2[pos-1],vec2[pos],vec2[pos+1],vec2[pos+2])
  apply(val,1,mean,na.rm=T)
}


meanFage<-function(FML,iALK,N,wt_age,rnams=c("East","West")){
  # FML                                    # s, r, f, l
  # iALK                                   # p, a, l
  np<-dim(wt_age)[1]
  Ftot<-array(NA,c(dim(iALK)[1],dim(FML),dim(iALK)[2])) # p, s, r, f, l, a
  Find<-TEG(dim(Ftot))
  Ftot[Find]<-FML[Find[,2:5]]*iALK[Find[,c(1,6,5)]] # p s r f a x p a l
  FM<-apply(Ftot,c(1,3,6), sum) # p, s, r, f, a    (sum over lengths)
  wFM2<-apply(FM,c(1,3),mean)
  #matplot(t(wFM2),type='l',xlab="Age",ylab="F")
  #legend('topright',legend=rnams,bty='n',text.col=c("black","red"))

}


meanFslen<-function(FML,iALK,N,wt_age,rnams=c("East","West")){
  # FML                                    # s, r, f, l
  # iALK                                   # p, a, l
  np<-dim(wt_age)[1]
  FM<-array(NA,c(dim(iALK)[1],dim(FML))) # p, s, r, f, l
  Find<-TEG(dim(FM))
  FM[Find]<-FML[Find[,2:5]]
  sumN<-apply(N,1:2,sum)          # p, s       (mean over areas)
  Nind<-TEG(dim(N))
  #Nr<-array(0,dim(N))
  #Nr[Nind]<-N[Nind]/muN[Nind[,1:2]]             # p, s, a, r      (normalized to mean 1)
  wFM<-array(NA,dim(FM))            # p, s, r, f, l (weighted fishing mortality rate at length)
  FMind<-TEG(dim(FM))
  wFM[FMind]<-FM[FMind]*N[FMind[,c(1,2,5,3)]]/sumN[FMind[,1:2]]
  wFM2<-apply(wFM,c(1,2,4,5),mean)
  wFM3<-apply(wFM2,c(1,4),sum) #what is the F at length profile?
  #matplot(OMI@mulen,t(wFM3),type='l',xlab="Length",ylab="F")
  #legend('topright',legend=rnams,bty='n',text.col=c("black","red"))

}

meanFs<-function(FML,iALK,N,wt_age,rnams=c("East","West")){
  # FML                                    # s, r, f, l
  # iALK                                   # p, a, l
  np<-dim(wt_age)[1]
  nr<-dim(N)[4]
  Ftot<-array(NA,c(dim(iALK)[1],dim(FML),dim(iALK)[2])) # p, s, r, f, l, a
  Find<-TEG(dim(Ftot))
  Ftot[Find]<-FML[Find[,2:5]]*iALK[Find[,c(1,6,5)]] # p s r f a x p a l
  #FM<-apply(Ftot,c(1,2,3,6), sum) # p, s, r, a    (sum over lengths and fleets)
  #wFM2<-apply(Ftot,c(1,6),sum)/nr
  #wFM2

  np<-dim(iALK)[1]
  na<-dim(iALK)[2] # number of ages
  nl<-dim(iALK)[3] # number of length classes
  ns<-dim(FML)[1] # number of seasons
  nr<-dim(FML)[2] # number of areas
  nf<-dim(FML)[3] # number of fleets
  WFM2<-array(NA,c(np,na))

  for(pp in 1:np){

    iALKp<-iALK[pp,,]
    Np<-N[pp,,,]

    FMLA<-array(NA,c(ns,nr,nf,nl,na))
    FMLAind<-TEG(dim(FMLA))
    FMLind<-FMLAind[,1:4]  # ns, nr, nf, nl
    iALKind<-FMLAind[,5:4] # na, nl
    FMLA[FMLAind]<-FML[FMLind]*iALKp[iALKind]
    FMA<-apply(FMLA,c(1,5,2),sum) # sum over fleets and lengths  ns, na, nr

    # smooth (weighted by season x area)
    NSR<-apply(Np,c(1,3),sum) # number by season and area  ns, nr
    FMAind<-TEG(dim(FMA))
    NSRind<-FMAind[,c(1,3)]
    wt<-array(NA,dim(FMA)) # weighted Fs by area and season
    wt[FMAind]<-FMA[FMAind]*NSR[NSRind] # ns, na, nr
    sel1<-apply(wt,1:2,sum)/apply(NSR,1,sum)
    WFM2[pp,]<-apply(sel1,2,sum)

  }
  WFM2
}


Fageprof<-function(FML,iALK,N,wt_age){

  np<-dim(wt_age)[1]
  ns<-dim(FML)[1]
  nr<-dim(FML)[2]
  nf<-dim(FML)[3]
  nl<-dim(FML)[4]
  na<-dim(iALK)[2]

  Fprof<-array(NA,c(np,na))

  for(pp in 1:np){ # loops due to memory issues in sim test


      Ftot<-array(NA,c(ns,nr,nf,nl,na))

      Find<-TEG(dim(Ftot))
      alkind<-cbind(rep(pp,nrow(Find)),Find[,5:4])
      Ftot[Find]<-FML[Find[,1:4]]*iALK[alkind]

      FM<-apply(Ftot,c(2,5), sum) # s, r, a    (sum over lengths)

      Nw<-apply(N[pp,,,]*rep(wt_age[pp,],each=ns),3:2,sum)
      Fw<-apply(FM*Nw,2,sum)/apply(Nw,2,sum)
      Fprof[pp,]<-Fw

  }

  Fprof

}


timeFs<-function(FML,iALK,N,wt_age){
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

  Fap<-array(NA,c(np,ny))

  for(pp in 1:np){ # loops due to memory issues in sim test
    for(yy in 1:ny){

      Ftot<-array(NA,c(ns,nr,nf,nl,na))

      Find<-TEG(dim(Ftot))
      FMLind<-cbind(rep(yy,nrow(Find)),Find[,c(1:4)])
      alkind<-cbind(rep(pp,nrow(Find)),rep(yy,nrow(Find)),Find[,5:4])

      Ftot[Find]<-FML[FMLind]*iALK[alkind] # y p s r f a x p a l
      FM<-apply(Ftot,c(2,5), sum) # s, r, a    (sum over lengths)

      Nw<-apply(N[pp,yy,,,],3:2,sum)
      Fw<-apply(FM*Nw,2,sum)/apply(Nw,2,sum)
      Fap[pp,yy]<-max(Fw)


    }
  }

  Fap

  #Ftot<-array(NA,c(ny,np,ns,nr,nf,nl,na)) # y, p, s, r, f, l, a
  #Find<-TEG(dim(Ftot))
  #Ftot[Find]<-FML[Find[,c(1,3:6)]]*iALK[Find[,c(2,1,7,6)]] # y p s r f a x p a l
  #FM<-apply(Ftot,c(1:5,7), sum) # y, p, s, r, f, a    (sum over lengths)
  #Cat<-array(NA,dim(FM))            # y, p, s, r, f, a (weighted fishing mortality rate at age)
  #FMind<-TEG(dim(FM))
  #Cat[FMind]<-(1-exp(-FM[FMind]))*N[FMind[,c(2,1,3,6,4)]] # y, p, s, r, f, a
  #sumCat<-apply(Cat,c(2,1,3,6),sum) # p, y, s, a
  #sumN<-apply(N,1:4,sum)            # p, y, s, a
  #UbyS<-sumCat/sumN
  #muU<-apply(UbyS,c(1,2,4),mean)   # p, y, a # not ideal mean U across seasons but very close to best
  #-log(1-apply(muU,1:2,max))  # p, y

}



getBH<-function(pars,SSB,rec,SSBpR,mode=1,plot=F,R0){

  h<-pars

  recpred<-((0.8*R0*h*SSB)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB))

  if(plot){
    recentrec<-rec[length(rec)]
    ord<-order(SSB)
    cols=rep("#0000ff95",length(rec))
    cols[length(rec)]<-"#ff000095"
    plot(SSB[ord],rec[ord],ylim=c(0,max(rec,R0)),xlim=c(0,max(SSB,R0*SSBpR)),xlab="",ylab="",col=cols[ord],pch=19)
    SSB2<-seq(0,max(R0*SSBpR,max(SSB)),length.out=500)
    recpred2<-((0.8*R0*h*SSB2)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB2))
    lines(SSB2,recpred2,col='blue')
    abline(v=c(0.2*R0*SSBpR,R0*SSBpR),lty=2,col='red')
    abline(h=c(R0,R0*h),lty=2,col='red')
    legend('topright',legend=c(paste0("h = ",round(h,3)),paste0("lnR0 = ",round(log(R0),3))),bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    return(-sum(dnorm(log(recpred)-log(rec),0,0.5,log=T))-dnorm(pars[1],0,2,log=T)) # add a vague prior on h = 0.8
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }

}


getHS<-function(pars,SSB,rec,SSBpR,mode=1,plot=F,R0){

  inflect<-pars
  SSB0<-R0*SSBpR

  recpred<-rep(R0,length(SSB))
  cond<-SSB<inflect*SSB0
  recpred[cond]<-R0*SSB[cond]/(SSB0*inflect)


  if(plot){
    ord<-order(SSB)
    plot(SSB[ord],rec[ord],ylim=c(0,max(rec,R0)),xlim=c(0,max(SSB,R0*SSBpR)),xlab="",ylab="",col="#0000ff95",pch=19)
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


getHS_BFT<-function(inflect,ind_inflect,SSB,rec,SSBpR,mode=1,plot=F,R0){

  recpred<-rep(R0,length(SSB))
  SSB0<-R0*SSBpR
  cond<-SSB<inflect*SSB0
  recpred[cond]<-R0*SSB[cond]/(SSB0*inflect)


  if(plot){
    ord<-order(SSB)
    plot(SSB[ord],rec[ord],ylim=c(0,max(rec,R0)),xlim=c(0,max(SSB,R0*SSBpR)),xlab="",ylab="",col="#0000ff95",pch=19)
    points(SSB[ind_inflect],rec[ind_inflect],col="red",pch=19)
    SSB2<-seq(0,R0*SSBpR,length.out=500)
    recpred2<-rep(R0,length(SSB2))
    cond<-SSB2<inflect*SSB0
    recpred2[cond]<-R0*SSB2[cond]/(SSB0*inflect)

    lines(SSB2,recpred2,col='blue')
    abline(v=c(inflect*SSB0,SSB0),lty=2,col='red')
    #h<-0.2/inflect
    #R0h<-R0*h
    abline(h=c(R0),lty=2,col='red')
    legend('topright',legend=c(paste0("Inflec. = ",round(inflect,3)),paste0("lnR0 = ",round(log(R0),3))),bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    return(-sum(dnorm(log(recpred)-log(rec),0,0.5,log=T))-dnorm(pars[1],0,4,log=T))
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }

}


SRopt2<-function(out,plot=F,quiet=F,years=NULL,type=c("BH","BH")){


  blocksize<-sum(out$RDblock==1)

  yrs1<-match(1:max(out$RDblock),out$RDblock)+floor(blocksize/2)
  yrs1[yrs1>out$ny]<-out$ny
  yrs<-yrs1
  if(!is.null(years))yrs<-yrs[yrs1>=years[1]&yrs1<=years[2]]
  paryrs<-match(yrs,yrs1)

  opt<-new('list')
  resid<-new('list') #array(NA,c(out$np,3))
  pnam<-c("East","West")

  Rec<-apply(out$N[,,out$spawns[1],1,],1:2,sum)*exp(out$M_age[,1])# got to add back in mortality from end of time step

  if(plot)par(mfrow=c(1,out$np),mai=c(0.4,0.5,0.1,0.05),omi=c(0.5,0.5,0.01,0.01))

  opt<-new('list')
  par<-rep(NA,2)
  VC<-new('list')

  for(pp in out$np:1){

    surv<-exp(-cumsum(c(0,out$M_age[pp,1:(out$na-1)])))
    SSBpR=sum(surv*out$mat_age[pp,]*out$wt_age[out$ny,,pp]) #SSBpR based on M, mat and growth
    SSBpR=SSBpR+surv[out$na]*exp(-out$M_age[pp,out$na])/(1-exp(-out$M_age[pp,out$na]))*out$mat_age[pp,out$na]*out$wt_age[out$ny,out$na,pp]
    SSB=out$SSB[pp,yrs,out$spawns[pp]]
    yr_nam<-(OMI@years[1]:OMI@years[2])[yrs]
    R0<-out$R0[pp]#SSB0[pp]/SSBpR

    rec<-Rec[pp,yrs]

    if(type[pp]=="BH"){

      tpar<-(out$h[pp]-0.6)/0.4
      if(tpar==0){
        par[pp]=-1E10
      }else{
        par[pp]<-log(tpar/(1-tpar))
      }

      devs<-getBH(par[pp],SSB,rec,SSBpR,mode=2,plot=plot,R0=R0)
      resid[[pp]]<-data.frame(yrs=yr_nam,SSB=SSB,rec=rec,devs=devs)
      VC[[pp]]<-matrix(0,nrow=1)

    }else if(type[pp]=="HS"){

      ind_inflect<-yr_nam<1996&yr_nam>1989
      SSB_inflect<-mean(SSB[ind_inflect])
      SSB0<-R0*SSBpR
      inflect<-SSB_inflect/SSB0
      par[pp]<-log(inflect/(1-inflect))
      VC[[pp]]<-matrix(0,nrow=1)

      devs<-getHS_BFT(yr_nam,SSB,rec,SSBpR,mode=2,plot=plot,R0=R0)
      resid[[pp]]<-data.frame(yrs=yrs,SSB=SSB,rec=rec,devs=devs)

    }

    if(plot)legend('topleft',legend=pnam[pp],text.font=2,bty='n')

  }

  if(plot)  mtext("Spawning Biomass (kg)",1,line=0.8,outer=T);
  if(plot)  mtext("Recruits (n)",2,line=0.8,outer=T)
  lnR0<-log(out$R0)
  return(list(type=rep(type,out$np),par1=par,lnR0=lnR0,VC=VC,resid=resid))

}


SRplot<-function(out,years=NULL,type=c("BH","BH","HS","BH"),plot=T,SRminyr=c(1,1,1,1),SRmaxyr=c(1,1,1,1)){

  # what recruitments are estimated
  nt<-rep(1,2)
  yswitch<-rep(NA,2)

  for(pp in 1:out$np) {
    recs<-unique(out$R0[pp,])
    nt[pp]<-length(recs)
    if(nt[pp]>1)yswitch[pp]=match(recs[2],out$R0[pp,])
  }

  tott<-sum(nt)
  if(tott==2){
    row1<-row2<-c(1,1,2,2)
  }else{
    if(nt[1]==1){
      row1<-c(tott,1,1,tott)
      tott<-tott+1
      n1<-1
    }else{
      row1<-c(1,1,2,2)
      n1=2
    }

    if(nt[2]==1){
      row2<-c(tott+1,nt[1]+1,nt[1]+1,tott+1)
    }else{
      row2<-c(1,1,2,2)+nt[1]
    }
  }

  if(plot){
   par(mfrow=c(out$np,2),mai=c(0.4,0.5,0.3,0.05),omi=c(0.5,0.5,0.5,0.01))
   layout(cbind(row2,row1))
  }
  #for(i in 1:sum(nt))plot(i,i)

  blocksize<-sum(out$RDblock==1)

  resid<-new('list') #array(NA,c(out$np,3))
  pnam<-c("East","West")

  Rec<-out$Rec_wd#mu# got to add back in mortality from end of time step
  VC<-new('list')

  i=1
  R0s<-rep(NA,sum(nt))
  par<-rep(NA,sum(nt))

  for(pp in 1:out$np){

    Fec<-out$mat_age[pp,]*out$wt_age[out$ny,,pp]
    surv<-exp(-cumsum(c(0,out$M_age[pp,1:(out$na-1)])))


    #SSBpR<-surv*Fec,1,sum)+surv[,out$na]*exp(- out$M_age[pp,out$na])/(1-exp(- out$M_age[pp,out$na]))*Fec

    SSBpR=sum(surv*Fec)#*exp(-OMI@Ma[,OMI@na])/(1-exp(-OMI@Ma[,OMI@na]))*OMI@Fec[,OMI@na] #SSBpR based on M, mat and growth
    SSBpR=SSBpR+surv[out$na]*exp(-out$M_age[pp,out$na])/(1-exp(-out$M_age[pp,out$na]))*Fec[out$na]

    for(tt in 1:nt[pp]){

      ind<-(1:out$ny)[unique(out$R0[pp,])[tt] == out$R0[pp,]]
      if(tt==1)R0ind<-1
      if(tt==2)R0ind<-out$ny
      R0<-out$R0[pp,R0ind]
      R0s[i]<-R0
      yr_nam<-years[1]+ind-1
      rec<-out$Rec_wd[pp,ind]
      SSB=out$SSB_mu[pp,ind]
      if(type[i]=="BH"){
        par[i]<-out$h[pp,R0ind]
      }else if(type[i]=="HS"){
        if(pp==2)ind_inflect<-yr_nam<1996&yr_nam>1989 # hinge 1990-1995 in West
        if(pp==1)ind_inflect<-(yr_nam==1973)          # hinge at 1973 in the East
        SSB_inflect<-mean(SSB[ind_inflect])
        SSB0<-R0*SSBpR
        par[i]<-SSB_inflect/SSB0
      }
      if(type[i]=="BH"){
        devs<-getBH(par[i],SSB,rec,SSBpR,mode=2,plot=plot,R0=R0)
      } else if(type[i]=="HS"){
        devs<-getHS_BFT(par[i],ind_inflect,SSB,rec,SSBpR,mode=2,plot=plot,R0=R0)
      }
      if(plot)legend('top',paste(SRminyr[i]+years[1]-1,"-",SRmaxyr[i]+years[1]-1),bty='n',text.font=2)
      resid[[i]]<-data.frame(yrs=yr_nam,SSB=SSB,rec=rec,devs=devs)
      VC[[i]]<-matrix(0,nrow=1)
      i<-i+1

      if(plot&nt[pp]==1)mtext(c("East stock","West stock")[pp],3,line=0.8)

    }
  }

  R0s_now<-SRtype_now<-par_now<-rep(NA,2)
  R0s_now[1]<-R0s[nt[1]]
  R0s_now[2]<-R0s[nt[1]+nt[2]]
  SRtype_now[1]<-type[nt[1]]
  SRtype_now[2]<-type[nt[1]+nt[2]]
  par_now[1]<-par[nt[1]]
  par_now[2]<-par[nt[1]+nt[2]]

  if(plot){
    if(sum(nt)>2)mtext(c("West stock","East stock"),3,at=c(0.25,0.75),line=0.8,outer=T)
    mtext("Spawning Biomass (kg)",1,line=0.8,outer=T)
    mtext("Recruits (n)",2,line=0.8,outer=T)
  }

  lnR0<-log(R0)
  return(list(type=type,par1=par,lnR0=lnR0,VC=VC,resid=resid,R0s_now=R0s_now,SRtype_now=SRtype_now,par_now=par_now))

}


SRopt<-function(out,plot=F,quiet=F,years=NULL,type="BH",R0p=NA){

  blocksize<-sum(out$RDblock==1)

  yrs1<-match(1:max(out$RDblock),out$RDblock)+floor(blocksize/2)
  yrs1[yrs1>out$ny]<-out$ny
  yrs<-yrs1
  if(!is.null(years))yrs<-yrs[yrs1>=years[1]&yrs1<=years[2]]
  paryrs<-match(yrs,yrs1)

  opt<-new('list')
  resid<-new('list') #array(NA,c(out$np,3))
  pnam<-c("East","West")

  if(plot)par(mfrow=c(1,out$np),mai=c(0.4,0.5,0.1,0.05),omi=c(0.5,0.5,0.01,0.01))

  if(is.na(R0p[1]))R0p<-out$muR#*exp(out$lnHR1)

  lnR0<-log(R0p)

  for(pp in out$np:1){

    #Rectemp<-mean(exp(out$lnRD[pp,1:2])*out$muR[pp]) # have a guess at R0 for initializing nlm
    #R0temp<-mean(Rectemp/(out$SSB[pp,yrs,out$spawns[pp]]/SSB0[pp]))
    surv<-exp(-cumsum(c(0,out$M_age[pp,1:(out$na-1)])))
    SSBpR=sum(surv*out$mat_age[pp,]*out$wt_age[out$ny,,pp]) #SSBpR based on M, mat and growth
    SSBpR=SSBpR+surv[out$na]*exp(-out$M_age[pp,out$na])/(1-exp(-out$M_age[pp,out$na]))*out$mat_age[pp,out$na]*out$wt_age[out$ny,out$na,pp]
      #muR(pp)*surv(pp,na)*stemp(pp,ss,rr)*mfexp(-Ma(pp,na))/(1-mfexp(-Ma(pp,na)))*Fec(pp,aa)
    SSB=out$SSB[pp,yrs,out$spawns[pp]]
    #SSB=apply(out$SSB[pp,yrs,],1,mean)

    rec=out$muR[pp]*exp(out$lnRD[pp,paryrs])

    R0<-R0p[pp]#SSB0[pp]/SSBpR

    #fscale<-getSteepness(pars,SSB=SSB,rec=rec, SSBpR=SSBpR,mode=1,plot=F)
    #opt<-nlm(getSR,p=pars,typsize=c(0.5,log(R0temp)),fscale=fscale,hessian=T,print.level=2,

    if(type=="BH"){

      pars<-0.5 # guess / starting values
      opt[[pp]]<-optim(pars,getBH,method="L-BFGS-B",lower=-6,upper=6., hessian=T,
                         SSB=SSB,rec=rec,SSBpR=SSBpR,mode=1, plot=F,R0=R0)

      devs<-getBH(opt[[pp]]$par,SSB,rec,SSBpR,mode=2,plot=plot,R0=R0)
      resid[[pp]]<-data.frame(yrs=yrs,SSB=SSB,rec=rec,devs=devs)


    }else if(type=="HS"){

      pars<--0.5 # guess / starting values
      opt[[pp]]<-optim(pars,getHS,method="L-BFGS-B",lower=-6.,upper=6., hessian=T,
                         SSB=SSB,rec=rec,SSBpR=SSBpR,mode=1, plot=F,R0=R0)

      devs<-getHS(opt[[pp]]$par,SSB,rec,SSBpR,mode=2,plot=plot,R0=R0)
      resid[[pp]]<-data.frame(yrs=yrs,SSB=SSB,rec=rec,devs=devs)

    }

    if(plot)legend('topleft',legend=pnam[pp],bty='n')

  }

  if(plot)  mtext("Spawning Biomass (kg)",1,line=0.8,outer=T);
  if(plot)  mtext("Recruits (n)",2,line=0.8,outer=T)

  if(type=="BH"){ # this is a simplification assuming that both are either HS or BH

    logith<-sapply(opt,FUN=function(x)x$par[1])#0.2+1/(1+exp(-sapply(opt,FUN=function(x)x$par[1])))*0.8
    VC<-lapply(opt,FUN=function(x)solve(x$hessian))

    if(!quiet)return(list(type=rep(type,out$np),par1=logith,lnR0=lnR0,VC=VC,resid=resid))

  }else if(type=="HS"){

    logitinflect<-sapply(opt,FUN=function(x)x$par[1])
    VC<-lapply(opt,FUN=function(x)solve(x$hessian))

    if(!quiet)return(list(type=rep(type,out$np),par1=logitinflect,lnR0=lnR0,VC=VC,resid=resid))

  }

}

sigR_AC_conv<-function(sig2,AC2){

  AC2[AC2<0]<-0.01
  AC1<-1.0509*AC2^0.7898
  sig1<-(2*sig2)/(1+AC1)
  list(AC1=AC1,sig1=sig1)

}




