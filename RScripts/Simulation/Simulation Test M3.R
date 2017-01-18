
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# --- Object-Oriented Management Strategy Evaluation using parallel processing  ------------

# --- Tom Carruthers   UBC
# --- Laurie Kell      ICCAT        

# Version alpha (preliminary)
# For M3 v0.18
# 9th July 2016


# Prerequisites ===================================================================================

rm(list=ls(all=TRUE))                  # Remove all existing objects from environment
set.seed(2)                            # Ensure reproducible results by setting random seed
setwd("G:/ABT-MSE/")                   # Set the working directory
source("Source/MSE_source.r")          # Load the source code
sfInit(parallel=T,cpus=detectCores())              # Initiate the cluster


# Define Operating model ==========================================================================

load("Objects/SimSamABM")          # Load an operating model definition (OMd) object
OMd@nsim<-as.integer(64)               # For demonstration do a small number of simulations
plot(OMd)                              # Plot the spatial definition of areas


# Create an Operating Model =======================================================================

OM<-new('OM',OMd)                      # Initialize a new operating model (OM) object
plot(OM)                               # Plot the spatial distribution of mature and immature fish

save(OM,file="Objects/SimSam_small")

load("Objects/SimSam_small")

# Load Observation model ==========================================================================
load("Objects/Good_Obs_no_bias")               # Load the precise and unbiased observation model ('Good')

simsam5<-new('SimSam',OM,Obs,movtype=1,OMDir="C:/M3",
             verbose=1,complexF=0,M3temp="C:/M3temp/")

save(simsam5,file="Objects/simsam5")

sfLapply(1:OM@nsim,runM3,M3temp="C:/M3temp") # run the M3 script

#load(file="Objects/simsam2")

simsam5<-simsameval(simsam5,"C:/M3temp",quiet=T)
save(simsam5,file="Objects/simsam5")

simsam<-simsam5

biaslist<-new('list')
biaslist[[1]]<-simsam@Perf[,1:2]
biaslist[[2]]<-simsam@Perf[,3:4]
biaslist[[3]]<-simsam@Perf[,5:6]
biaslist[[4]]<-simsam@Perf[,7:8]
biaslist[[5]]<-simsam@Perf[,9:10]
biasnam<-c("Frac. SSB in spawn. area","Stock depletion","Current biomass","Current exploitation rate","Unfished SSB")
biasplot(biaslist,biasnam,labs=T)

biasplot<-function(biaslist,biasnam,labs=F){
  
  nplots<-length(biasnam)
  ncol<-floor(nplots^0.5)
  nrow<-ceiling(nplots/ncol)
  
  par(mfrow=c(nrow,ncol),mai=c(0.4,0.4,0.05,0.05),omi=c(0.4,0.4,0.05,0.05))
  
  for(i in 1:nplots){
    
    xlim<-c(max(c(-200,min(biaslist[[i]][,1]))),min(c(200,max(biaslist[[i]][,1]))))
    ylim<-c(max(c(-200,min(biaslist[[i]][,2]))),min(c(200,max(biaslist[[i]][,2]))))
    if(!labs)plot(biaslist[[i]][,1],biaslist[[i]][,2],pch=19,col='#99999993',xlim=xlim, ylim=ylim,xlab="",ylab="",main="")
    if(labs){
      
      plot(biaslist[[i]][,1],biaslist[[i]][,2],pch=19,col='#99999903',xlim=xlim, ylim=ylim,xlab="",ylab="",main="")
      text(biaslist[[i]][,1],biaslist[[i]][,2],1:nrow(biaslist[[i]]),col='#99999993')
      
    }
    abline(v=0,col='black',lty=2)
    abline(h=0,col='black',lty=2)
     xb<-quantile(biaslist[[i]][,1],c(0.1,0.9))
    yb<-quantile(biaslist[[i]][,2],c(0.1,0.9))
    mux<-mean(biaslist[[i]][,1])
    muy<-mean(biaslist[[i]][,2])
    abline(v=mux,col='blue',lwd=1.5)
    abline(h=muy,col='red',lwd=1.5)
    polygon(c(xb[1],xb[1],xb[2],xb[2]),c(-100000,1000000,100000,-100000),col="#0000ff20",border=NA)
    polygon(c(-100000,1000000,100000,-100000),c(yb[1],yb[1],yb[2],yb[2]),c(-100000,1000000,100000,-100000),col="#ff000020",border=NA)
    legend('bottomright',legend=round(mux,2),text.col='blue',bty='n')
    legend('topleft',legend=round(muy,2),text.col='red',bty='n')
    legend('topright',biasnam[i],cex=1.3,bg="#ffffff60",box.col="#ffffff60")
 
  }
  
  mtext("Stock 1 (e.g. Eastern)",1,col='blue',outer=T,line=0.5)
  mtext("Stock 2 (e.g. Western)",2,col='red',outer=T,line=0.5)
  
}


simsameval<-function(.Object,M3temp='C:/M3temp',quiet=T){
  out<-new('list')
  nsim<-length(runs)
  runs<-paste(M3temp,1:nsim,sep="/")

  for(i in 1:nsim){
    out[[i]]<-M3read(runs[i],quiet=quiet)
    print(i)
  }
  
  Bfrac<-.Object@simlist$Bfrac
  Bfracp<-t(sapply(1:nsim,getBfrac,out,spawnr=c(4,1)))
  Bfracbias<-(Bfracp-Bfrac)/Bfrac*100
  
  # Bias in current depletion (SSB)
  #D<-.Object@simlist$D
  Dp<-t(sapply(1:nsim,getdep,out))
  D<-t(sapply(1:nsim,getdepsim,out))
  Dbias<-(Dp-D)/D*100
  
  # Bias in current B (absolute)
  Bcur<-.Object@simlist$Bcur
  Bcurp<-t(sapply(1:nsim,getBnow,out))
  Bcurbias<-(Bcurp-Bcur)/Bcur*100
  
  U<-.Object@simlist$U
  Up<-t(sapply(1:nsim,getUnow,out))
  Ubias<-(Up-U)/U*100
  
  SSB0<-.Object@simlist$SSB0
  SSB0p<-t(sapply(1:nsim,getSSB0,out))
  SSB0bias<-(SSB0p-SSB0)/SSB0*100
  
  
 # names(.Object@simlist)
  #names(out[[1]])
  .Object@Perf<-as.data.frame(cbind(Bfracbias,Dbias,Bcurbias,Ubias,SSB0bias))
  .Object
}

#Bfracp<-t(sapply(1:nsim,getBfrac,out,spawnr=spawnr))
#Bfracbias<-(Bfracp-Bfrac)/Bfrac

# Bias in current depletion (SSB)
#Dp<-t(sapply(1:nsim,getdep,out))
#Dbias<-(Dp-D)/D

# Bias in current SSB (absolute)
#SSBp<-t(sapply(1:nsim,getSSBnow,out))
#SSBbias<-(SSBp-Bcur)/Bcur

#Perf<-data.frame(Dbias,SSBbias,Bfracbias)




getdep<-function(x,out)out[[x]]$SSB[,out[[x]]$ny,out[[x]]$ns]/out[[x]]$SSB0


out$SSB[,out$ny,out$ns]/out$SSB0



movtype=1
OMDir="G:/M3"                          # version v0.18
verbose=1
complexF=0
complexRD=0



save(out,file="outsimstore")



pin_from_par(file="C:/M3/M3")
out<-M3read("G:/M3")
plotM3fit(out,outdir="G:/ABT-MSE/Results/OM_fits/simsamgrav2/")


simsam1<-new('SimSam',OM,Obs,movtype=2,OMDir="C:/M3",verbose=1,complexF=0)

save(simsam1,file="Results/Simsams/bias1")
