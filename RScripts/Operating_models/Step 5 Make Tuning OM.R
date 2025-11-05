


library(ABTMSE)
loadABT()

setwd("C:/Users/tcar_/Dropbox/abft-mse")
setwd("C:/Users/tcarruth/Dropbox/abft-mse")
datadir<-paste0(getwd(),"/R_package/ABTMSE/data/")


OMind<-1:48
OMw  <- OM_wt[1:48]
OMw2 <- OM_wt[OMind]

set.seed(1)
seeds<-1:48

sls<-slotNames(OM_1)
nsl<-length(sls)
dims<-list()
keep<-rep(NA,nsl)

for(i in 1:nsl){

  dimo<-dim(slot(OM_1,sls[i]))
  dims[[i]]<-dimo
  keep[i]=F

  if(!is.null(dimo)){

    if(dims[[i]][1]==48){
      keep[i]<-T
    }
  }

}

revslts<-sls[keep]

trimsims<-function(x,sn){ # keep sims from the low weight OM fitting (second sim is just sim 1 for now)
  ndims<-length(dim(x))
  take<-1:sn
  if(ndims==2)return(x[take,])
  if(ndims==3)return(x[take,,])
  if(ndims==4)return(x[take,,,])
  if(ndims==5)return(x[take,,,,])
  if(ndims==6)return(x[take,,,,,])
  if(ndims==7)return(x[take,,,,,,])
}


nsim<-4
for(i in 1:48){

  OM<-get(paste0('OM_',i))
  OM@nsim<-as.integer(nsim)
  OM@seed<-seeds[i]

  for(ss in 1:length(revslts)){
    obj<-slot(OM,revslts[ss])
    slot(OM,revslts[ss]) <- trimsims(obj,nsim)
  }

  to_file<-paste0(datadir,"OM_",i,"t")
  objname<-paste0("OM_",i,"t")
  assign(objname,OM)
  do.call(save, list(objname,file=to_file))

}


nsim<-8
for(i in 1:48){
  
  OM<-get(paste0('OM_',i))
  OM@nsim<-as.integer(nsim)
  OM@seed<-seeds[i]
  
  for(ss in 1:length(revslts)){
    obj<-slot(OM,revslts[ss])
    slot(OM,revslts[ss]) <- trimsims(obj,nsim)
  }
  
  to_file<-paste0(datadir,"OM_",i,"e")
  objname<-paste0("OM_",i,"e")
  assign(objname,OM)
  do.call(save, list(objname,file=to_file))
  
}


nsim<-16
for(i in 1:48){
  
  OM<-get(paste0('OM_',i))
  OM@nsim<-as.integer(nsim)
  OM@seed<-seeds[i]
  
  for(ss in 1:length(revslts)){
    obj<-slot(OM,revslts[ss])
    slot(OM,revslts[ss]) <- trimsims(obj,nsim)
  }
  
  to_file<-paste0(datadir,"OM_",i,"s")
  objname<-paste0("OM_",i,"s")
  assign(objname,OM)
  do.call(save, list(objname,file=to_file))
  
}



