# =========================================================
# ====== IRW ==============================================
# =========================================================

# July 2019

# === ITERATIVE REWEIGHTING BY INVERSE VARIANCE =======================================================
#
# varvec = c(0.1,0.8,0.7); nvec=c(1,2,10); adj_by_n=T; adj_n_pow=1
# ITRW_IV(varvec, nvec, adj_by_n, adj_n_pow)

ITRW_IV<-function(varvec, nvec,adj_by_n=T, adj_n_pow=1){

  wt <- 1/(varvec)
  if(adj_by_n)  wt/nvec^adj_n_pow
  wt

}

# setwd("C:/Users/tcar_/Dropbox/abft-mse/Objects/Sense/v5_1_GOM_CV/1")
# library(ABTMSE);

#load(file='OMI')
#out<-M3read(OMDir=getwd())
#CalcCVs(out,OMI)


# ==== ITERATIVE REWEIGHTING BY CV =====================================================================

IRcv<-function(){

  IRfac<-1/3
  OK=FALSE
  OMdir='C:/Users/tcarruth/Dropbox/abft-mse/M3'
  logfile<-paste0(OMdir,"/IRlog.txt")
  write("# Iterative reweighting log",logfile,1,append=F)
  i<-0

  while(OK==FALSE){

    i<-i+1
    runM3(OMdir)
    out<-M3read(OMDir=OMdir)
    CVfacs<-unlist(CalcCVs(out,OMI))
    CVind<-c(1,2,3,4,5,6,8)
    fac<-rep(1,7)
    cond<-CVfacs<0.5|CVfacs>2
    if(sum(cond)==0)OK<-TRUE
    fac[cond]<-exp(log(CVfacs[cond])*IRfac)
    OMI@LHw[CVind]<-OMI@LHw[CVind]*fac
    M3write(OMI,OMdir=paste0(getwd(),"/M3"))  # Store this base operating model in the M3 directory
    write(c(i,fac,OMI@LHw),logfile,24,append=T)

  }
}


# ==== END =======================================================================================



