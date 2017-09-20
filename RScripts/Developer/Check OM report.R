
library(ABTMSE)
loadABT()
dirs="C:/Users/tcar_/Documents/abft-mse/M3"
addlab=TRUE


make_fit_reports(dirs="C:/Users/tcar_/Documents/abft-mse/M3",addlab=TRUE)

nOMs<-length(dirs)
Obs<-Bad_Obs #load(paste0(getwd(),"/Objects/Observation_models/Bad_Obs"))

i<-1
input_dir<-dirs[i]

out<-M3read(input_dir)
load(paste(input_dir,"/OMI",sep=""))
load(system.file("ts.Rdata", package="ABTMSE"))
dat<-subset(ts,catchScenario=="Reported")


wta<-t(out$wt_age[33,,])
surv<-t(exp(-apply(out$M_age,1,cumsum)))
Fec<-out$mat_age*wta
na<-dim(Fec)[2]

SSB0=out$muR*exp(out$lnHR1)*apply(surv*Fec,1,sum)#     // Unfished Spawning Stock Biomass
SSB0=SSB0+out$muR*exp(out$lnHR1)*Fec[,na]*surv[,na]*exp(-out$M_age[,na])/(1-exp(-out$M_age[,na]))# // indefinite integral of surv added to get plus group SSB0


# summary report
dir=paste0("C:/Users/tcar_/Documents/abft-mse/Objects/OMs")
OMdirs<-NA


Sres1<-Sres2<-NULL#array(NA,c(nOMs,8))
out<-M3read(OMdirs[1])
SSBstore<-array(0,c(nOMs,out$np,out$ny))
SSB0store<-array(0,c(nOMs,out$np))

OMlab<-unlist(lapply(strsplit(OMdirs,"/"),FUN=function(x)x[length(x)]))
CPUEres<-new('list')

for(i in 1:nOMs){

  out<-M3read(OMdirs[i])
  load(paste0(OMdirs[i],"/OM"))

  VBi<-out$VB/rep(apply(out$VB,4,mean),each=out$ny*out$ns*out$nr)

  for(k in 1:OMI@nCPUEq){

    tempdat<-subset(OMI@CPUEobs,OMI@CPUEobs[,4]==k)
    obs<-as.numeric(tempdat[,6])
    pred<-exp(out$lnqCPUE[k])*VBi[tempdat[,c(1,2,3,5)]]

    if(i==1){
      CPUEres[[k]]<-log(obs)-log(pred)
    }else{
      CPUEres[[k]]<-cbind(CPUEres[[k]],log(obs)-log(pred))
    }

  }

  opt<-SRopt(out,plot=F,quiet=F,years=c(-Inf,Inf),type="BH",R0p=out$muR*exp(out$lnHR1))

  type<-OM@Rectype[1,]
  SRtypes<-unlist(lapply(strsplit(type,"_"),FUN=function(x)x[1]))

  res<-MSY_FAST(FML=out$FL[out$ny,,,,], iALK=out$iALK[,out$ny,,], N=out$N[,out$ny,,,],
                wt_age=t(out$wt_age[out$ny,,]), M_age=out$M_age, mat_age=out$mat_age,
                R0s=exp(opt$lnR0), fixpars=opt$par1,SRtypes=SRtypes)

  res[,c(2,3,6,7,8)]<-round(res[,c(2,3,6,7,8)],3)
  res[,c(1,4,5)]<-round(res[,c(1,4,5)]/1000,0)
  SSBs<-round(out$SSB[,out$ny,out$ns]/1000,0)

  OFLs<-round(res$UMSY*(res$BMSY/res$SSBMSY)*SSBs,0)
  Deps<-round(out$SSB[,out$ny,out$spawns[1]]/out$SSB0,3)
  Sres1<-rbind(Sres1,c(OM=OMlab[i],res[1,],D=round(out$D[1],3),Dep=Deps[1],SSB=SSBs[1],OFL=OFLs[1]))
  Sres2<-rbind(Sres2,c(OM=OMlab[i],res[2,],D=round(out$D[2],3),Dep=Deps[2],SSB=SSBs[2],OFL=OFLs[2]))

  SSBstore[i,,]<-apply(out$SSB,1:2,mean)/1000
  SSB0store[i,]<-out$SSB0/1000

}

Sres1<-as.data.frame(Sres1)
Sres2<-as.data.frame(Sres2)

names(Sres1)[c(3,8,9)]<-names(Sres2)[c(3,8,9)]<-c("FMSYa","SSBrel","recMSY")
#write.csv(as.matrix(Sres1),file="C:/Users/tcar_/Documents/Sres1.csv")


retseq<-function(res){

  nres<-length(res)
  str<-rep(1,nres)
  i<-1
  going<-TRUE
  while(going){

    same=TRUE
    j<-0
    while(same){

      if(res[i]!=res[i+j+1]|(j+i+1)==(nres+1)){

        same=FALSE
        str[i:(i+j)]<-j+1

      }else{
        j<-j+1
      }
    }

    if(i+j+1>nres)going=FALSE
    i<-i+j+1

  }

  str
}


resplot2<-function(yrs,res,horiz=T){

  col<-rep('blue',length(res))
  col[res<0]<-'dark grey'
  ACcol<-c("white","white","#ff000010","#ff000030","#ff000080",rep('red',200))
  ACval<-retseq(res<0)
  bcol=ACcol[ACval]
  par(lwd = 2)
  barplot(res,names.arg=yrs,border=bcol,col="white",horiz=horiz)
  abline(v=c(-0.25,0.25),lty=2,col="grey",lwd=2)
  abline(v=c(-0.5,0.5),col="grey",lwd=2)
  barplot(res,names.arg=yrs,border=NA,col=col,add=T,horiz=horiz)
  ac<-acf(res,plot=F)$acf[2,1,1]
  leg<-c(paste("StDev =",round(sd(res),2)),paste("AC =", round(ac,2)))
  legend('topleft',legend=leg,box.col="#ffffff99",bg="#ffffff99")
  #legend('topleft',legend=leg, box.col="#ffffff99",bg="#ffffff99")

}


chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
groups<-chunk2(1:OMI@nCPUEq,5)

OMsel<-(1:nOMs)[Design$Design_Ref==1]

for(j in 1:length(groups)){

  par(mfrow=c(length(groups[[j]]),length(OMsel)),mai=c(0.3,0.2,0.1,0.01),omi=c(0.3,0.3,0.2,0.05))

  for(k in groups[[j]]){

    for(i in OMsel){
      resplot2(yrs=rep(NA,length(CPUEres[[k]][,i])),res=CPUEres[[k]][,i])
      if(i==1)mtext(OMI@CPUEnames[k],2,line=1.5)
      if(k == groups[[j]][1])mtext(paste("OM =",i),3,line=0.8)
    }

  }

  cat("\n\n\\pagebreak\n")

}

mtext("Residual error. log(Obs.)-log(Pred.)",1,outer=T,line=0.5)



