# Historical catches.R
# November 2016
# R Script for assigning historical annual catches to season, age and area prior to 1960
# (Using Task1 in the west and Trap data in the East)

# Control parameters -------------------------------

niy<-10 # initial years of length comps uses to redistribute catches

CLcomp<-array(0,c(Base@nHy,Base@ns,Base@nr,Base@nl))
CLmat<-array(0,c(Base@nHy,Base@ns,Base@nr,Base@nl))  # a blank catch at length matrix
CLAmat<-array(0,c(Base@nHy,Base@ns,Base@nr,Base@nl,Base@na)) # blank catch at length at age matrix
wt_len<-Base@lwa[1]*Base@mulen^Base@lwb[1] # calculate weights for length bins



# Build an ALK (for assigning catch at length to catch at age)

wt_age<-array(NA,dim(agearray))
ind<-TEG(dim(len_age))
wt_age[ind]<-Base@lwa[ind[,1]]*len_age[ind]^Base@lwb[ind[,1]]

LenCV<-0.07
ALK<-array(NA,c(Base@np,Base@ny,Base@na,Base@nl))
ind<-TEG(dim(ALK))
Lind<-ind[,c(1,3,2)]
ALK[ind]<-dnorm(Base@mulen[ind[,4]],len_age[Lind],len_age[Lind]*LenCV)
sums<-apply(ALK,c(1,2,4),sum)
sind<-ind[,c(1,2,4)]
ALK<-ALK/sums[sind]


# Cdis where possible

dat<-read.csv(paste(getwd(),"/Data/Raw/Catch/cdis5015_BFT.csv",sep="")) #
dat<-dat[,c(2,5,7,9,13,14,15)]
names(dat)[c(1,4,5,6,7)]<-c("Year","Quarter","Lat","Lon","Catch")
dat<-subset(dat,dat$Year>(Base@Hyears[1]-1)&dat$Year<(Base@Hyears[2]+1))
dat<-assign_area(dat,Base@area_defs)
dat<-AssignFleet(dat,Base@Fleets)
dat$Year<-dat$Year-Base@Hyears[1]+1

Cpa<-dat[,c(1,4,8,9,7)]

CbyY<-aggregate(Cpa$Catch*1000,by=list(Cpa$Year,Cpa$Area),sum)
names(CbyY)<-c("Year","Area","Ckg")
CbyYarray<-array(0,c(Base@nHy,Base@nr))
CbyYarray[as.matrix(CbyY[,1:2])]<-CbyY[,3]


# This is where you calcualte a backward catch by year season area and lenght category
# CLobs processing

dat<-read.csv(paste(getwd(),"/Data/Raw/Task2/t2szBFT-all_v1.csv",sep=""))
dat<-dat[dat$YearC<Base@years[1],]
dat$FleetCode[dat$FleetCode=="USA-Rec"]<-"USA"
dat$FleetCode[dat$FleetCode=="USA-Com"]<-"USA"

names(dat)[names(dat)=="YearC"]<-"Year"
names(dat)[names(dat)=="TimePeriodCatch"]<-"TimePeriodID"
names(dat)[names(dat)=="GearGrpCode"]<-"GearGrp"

dat<-ICCATtoGEO2(dat)
dat<-assign_area(dat,Base@area_defs)

dat<-assign_quarter(dat)
names(dat)[names(dat)=="Subyear"]<-"Quarter"

dat<-AssignFleet(dat,Base@Fleets)
dat$Year<-dat$Year-Base@Hyears[1]+1
dat<-dat[dat$Year>(1955-Base@Hyears[1]),]
#test<-aggregate(rep(1,nrow(dat)),by=list(dat$Fleet),sum)
#cbind(Base@Fleets$name[test[,1]],test)
#test<-aggregate(rep(1,nrow(dat)),by=list(dat$FleetCode,dat$GearGrp),sum)
#test[order(test$x,decreasing=T),]

Lencat<-rep(NA,nrow(dat))

for(i in 1:Base@nl){

  Lencat[dat$ClassFrq>(Base@lenbins[i]-0.01)&dat$ClassFrq<Base@lenbins[i+1]]<-i

}

cond<-!is.na(Lencat)
Lencat<-Lencat[cond]
dat<-dat[cond,]


CLobs<-aggregate(dat$Nr,by=list(dat$Year,dat$Quarter,dat$Area,Lencat),sum,na.rm=T)
CLcomp[as.matrix(CLobs[,1:4])]<-CLobs[,5]
CLtot<-apply(CLcomp,1,sum,na.rm=T)

# backward imputation
toimpute<-match(TRUE,CLtot>0)-1
impyrs<-toimpute:(toimpute+niy-1)

impcomp<-CLcomp[impyrs,,,]/sum(CLcomp[impyrs,,,],na.rm=T)
impcomp<-apply(impcomp,2:4,sum,na.rm=T)
#sum(impcomp)
midyrs<-(toimpute+1):Base@nHy
byq<-apply(CLcomp,2,sum,na.rm=T)/sum(CLcomp,na.rm=T)
byr<-apply(CLcomp,3,sum,na.rm=T)/sum(CLcomp,na.rm=T)
bylen<-apply(CLcomp,4,sum,na.rm=T)/sum(CLcomp,na.rm=T)


qlmat<-array(0,c(Base@ns,Base@nl))
ind<-TEG(dim(qlmat))
qlmat[ind]<-byq[ind[,1]]*bylen[ind[,2]]

for(y in (toimpute+1):Base@nHy){

  for(r in 1:Base@nr){

    CLmat[y,,r,]<-CbyYarray[y,r]*qlmat

  }

}

#apply(CLmat,1,sum,na.rm=T)
#apply(CbyYarray,1,sum)

qrlmat<-array(0,c(Base@ns,Base@nr,Base@nl))
ind<-TEG(dim(qrlmat))
qrlmat[ind]<-byq[ind[,1]]*byr[ind[,2]]*bylen[ind[,3]]


areasE<-match(c("NEATL","EATL","SEATL","MED"),Base@areanams) # 45deg longitude split
areasW<-match(c("GOM","CAR","WATL","GSL","SCATL","NCATL"),Base@areanams) # 45deg longitude split

sumE<-sum(qrlmat[,areasE,])
sumW<-sum(qrlmat[,areasW,])

qrlmat[,areasE,]<-qrlmat[,areasE,]/sumE
qrlmat[,areasW,]<-qrlmat[,areasW,]/sumW


# Eastern catch-at-length reconstruction ---------------------

# get historical Task I catches
HCatE<-read.csv(paste(getwd(),"/Data/Raw/Catch/Hist Trap East.csv",sep=""))
HCatE$BFTtons<-as.numeric(HCatE$BFTtons)
HCatE<-aggregate(HCatE$BFTtons*1000,by=list(HCatE$Year),sum)
names(HCatE)<-c("Year","Ckg")
HCatE<-HCatE[HCatE$Year>=Base@Hyears[1]&HCatE$Year<=Base@Hyears[2],]
HCatE$Year<-HCatE$Year-Base@Hyears[1]+1                     # Standardize historical year index
HCatE<-HCatE[HCatE$Year<(toimpute+1),]


for(yy in 1:toimpute){

  ind<-as.matrix(expand.grid(yy,1:Base@ns,areasE,1:Base@nl))
  CLmat[ind]<-HCatE$Ckg[ind[,1]]*qrlmat[ind[,2:4]]

}


# Western catch at length reconstruction ---------------------

# get historical task 1 catches
HCatW<-read.csv(paste(getwd(),"/Data/Raw/Task1/t1nc_20161114_bft.csv",sep=""))
HCatW<-HCatW[HCatW$Stock=="ATW"&HCatW$Year>=Base@Hyears[1]&HCatW$Year<=Base@Hyears[2],c(2,6,8)]
HCatW<-aggregate(HCatW$Qty_t*1000,by=list(HCatW$Year),sum)
names(HCatW)<-c("Year","Ckg")
HCatW$Year<-HCatW$Year-Base@Hyears[1]+1                     # Standardize historical year index
HCatW<-HCatW[HCatW$Year<(toimpute+1),]
i<-0
for(yy in HCatW$Year){
  i<-i+1
  ind<-as.matrix(expand.grid(yy,1:Base@ns,areasW,1:Base@nl))
  CLmat[ind]<-HCatW$Ckg[i]*qrlmat[ind[,2:4]]

}

CLmat[is.na(CLmat)]<-0


biALK<-t(ALK[1,1,,]) # Eastern iALK in first year (eastern priority)
ind<-TEG(c(Base@nHy,Base@ns,Base@nr,Base@nl,Base@na))
CLAmat[ind]<-CLmat[ind[,1:4]]*biALK[ind[,4:5]]
CAmat<-HCobs<-apply(CLAmat,c(1,2,5,3),sum)

wt_age<-Base@wt_age[1,,1]

ind<-TEG(c(Base@nHy,Base@ns,Base@na,Base@nr))

par(mfrow=c(1,1),mar=rep(1,4))
recentC<-aggregate(Base@Cobs[,5],by=list(Base@Cobs[,1]),sum)$x
histC<-apply(HCobs,1,sum)
plot(c(histC,recentC),type="l")
abline(v=Base@nHy,col="red")

HCobs[ind]<-CAmat[ind]/(wt_age[ind[,3]]) # Catch is in kg
HCobs[,,1,]<-0 # for now ignore any catches in first age class
#HCobs<-as.data.frame(cbind(ind,CAmatN[ind]))
#names(HCobs)<-c("Year","Quarter","Area","Age","N")
#HCobs<-HCobs[HCobs$N>10,]/adjfactor

save(HCobs,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs",sep=""))

diag<-F
if(diag){  # some diagnostics

  # check disaggregation
  cbind(HCatE$Ckg,apply(CLmat[1:toimpute,,7:10,],1,sum))
  cbind(HCatW$Ckg,apply(CLmat[HCatW$Year,,1:6,],1,sum,na.rm=T))

  # check age assignment
  cbind(apply(CLmat,1,sum),apply(CLAmat,1,sum),apply(CAmat,1,sum))

  agg<-apply(CAmat,c(1,4),sum)

  RecCat<-array(NA,dim=c(Base@ny,Base@nr))
  RecC<-aggregate(Base@Cobs[,5],by=list(Base@Cobs[,1], Base@Cobs[,3]),sum)
  RecCat[as.matrix(RecC[,1:2])]<-RecC[,3]

  cols<-rep(c('red',"dark grey","black","green","blue"),2)
  ltys<-rep(c(2,1),each=5)

  par(mfrow=c(2,1),mai=c(0.9,0.9,0.2,0.05))
  matplot(Base@Hyears[1]:Base@years[2],rbind(agg*1000,RecCat),type='l',col=cols,lty=ltys,xlab="Year",ylab="Annual Catches (tonnes)")
  abline(v=Base@years[1]-0.5,col='#99999930',lwd=5)
  legend('topleft',legend=Base@areanams,col=cols,text.col=cols,lwd=1.2,lty=ltys,bty='n')

  tlev<-0.98*max(c(agg,RecCat),na.rm=T)
  alev<-0.93*max(c(agg,RecCat),na.rm=T)

  text(1938,tlev,"Eastern catches from historical trap data",col=cols[10])
  arrows(1955,alev,1945,alev,col='blue',length=0.1)

  text(1978,tlev,"Eastern catches from Task I data",col=cols[10])
  arrows(1965,alev,1975,alev,col='blue',length=0.1)


  matplot(Base@Hyears[1]:Base@years[2],log(rbind(agg,RecCat)),type='l',col=cols,lty=ltys,xlab="Year",ylab="Annual Catches (log tonnes)")
  abline(v=Base@years[1]-0.5,col='#99999930',lwd=5)

  text(1938,tlev,"Eastern catches from historical trap data",col=cols[10])
  arrows(1955,alev,1945,alev,col='blue',length=0.1)

  text(1978,tlev,"Eastern catches from Task I data",col=cols[10])
  arrows(1965,alev,1975,alev,col='blue',length=0.1)


  #apply(CLmat[,,10,],1,sum)/apply(CAmatN[,,10,],1,sum)

  #apply(CLmat[,,1:6,],1,sum)/apply(CAmatN[,,1:6,],1,sum)

  #cbind(apply(CAmatN[,,1:6,],4,sum)/max(apply(CAmatN[,,1:6,],4,sum)),apply(CAmatN[,,10,],3,sum)/max(apply(CAmatN[,,10,],3,sum)),wt_age/1000)

  #cbind(apply(CLmat[,,1:6,],4,sum),apply(CLmat[,,10,],3,sum),wt_len)


}
