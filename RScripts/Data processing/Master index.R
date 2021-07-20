# Master index.r
# March 2021
# R Script for deriving Master indices for MSE testing



# === Assessment and PSAT based MI ===================
MI<-list()
load(system.file("ts2017.Rdata", package="ABTMSE"))
dat<-ts2017

load(file=paste(getwd(),"/Data/Processed/Conditioning/PSAT",sep=""))
mov<-array(0,c(Base@np,Base@ns,Base@nr,Base@nr))
ind<-as.matrix(PSAT[,c(1,3,5,6)])
mov[ind]<-PSAT[,8]
mov[mov==0]<-0.001
mov<-mov/array(apply(mov,1:3,sum),dim(mov))

Wd<-Ed<-array(1/Base@nr,c(Base@ns,Base@nr))

for(i in 1:20){

  for(s in 1:4){

    if(s==1){
      Ed[s,]<-Ed[4,]%*%mov[1,4,,]
      Wd[s,]<-Wd[4,]%*%mov[2,4,,]
    }else{
      Ed[s,]<-Ed[s-1,]%*%mov[1,s-1,,]
      Wd[s,]<-Wd[s-1,]%*%mov[2,s-1,,]
    }

  }

}

By<-Base@years[1]:Base@years[2]

Etemp<-dat[dat$area=="East" & dat$assessment=="SS",3:4]
ind<-match(By,Etemp[,1])
Et<-Etemp[ind,2]
Et[is.na(Et)]<-Et[match(TRUE,is.na(Et))-1]

Wtemp<-dat[dat$area=="West" & dat$assessment=="SS",3:4]
ind<-match(By,Wtemp[,1])
Wt<-Wtemp[ind,2]
Wt[is.na(Wt)]<-Wt[match(TRUE,is.na(Wt))-1]

tempMI<-array(NA,c(Base@ny,Base@ns,Base@nr))
tind<-TEG(dim(tempMI))
tempMI[tind]<-Wt[tind[,1]]*Wd[tind[,2:3]]+Et[tind[,1]]*Ed[tind[,2:3]]
#tempMI<-tempMI/(mean(tempMI)/mean(MI[[1]]))
tempMI<-tempMI/mean(tempMI)

SpatPr<-read.csv(paste0(getwd(),"/data/Processed/Priors/Spatial_Priors.csv"),header=T)

SPs<-unique(SpatPr$Ino)

for(ss in SPs){
  
  dat<-SpatPr[SpatPr$Ino==ss,]
  area<-dat$Strata[1]
  tfrac<-dat$Index
  rfrac<-apply(tempMI[,,area],2,mean)
  rfrac<-rfrac/sum(rfrac)
  
  for(qq in 1:Base@ns){
    
    tempMI[,qq,area]<-tempMI[,qq,area]*tfrac[qq]/rfrac[qq]
  }
  
}

MI<-tempMI

prettyaxis<-function(lims){
  inc<-(lims[2]-lims[1])/5
  pretty(seq(lims[1]-inc,lims[2]+inc,length.out=12))
}
ploto<-T
if(ploto)plotindex2(Base,MI) #

save(MI,file=paste(getwd(),"/Data/Processed/Conditioning/MI",sep=""))



#





