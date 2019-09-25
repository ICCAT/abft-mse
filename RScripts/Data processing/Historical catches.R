# Historical catches.R
# November 2016
# R Script for assigning historical annual catches to season, age and area prior to 1960
# (Using Task1 in the west and Trap data in the East)

# Control parameters -------------------------------

niy<-5 # initial years of length comps uses to redistribute catches

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



# Eastern catch-at-length reconstruction ---------------------

# get historical Task I catches
HCatE<-read.csv(paste(getwd(),"/Data/Raw/Catch/Hist Trap East.csv",sep=""))
HCatE$BFTtons<-as.numeric(HCatE$BFTtons)
HCatE<-aggregate(HCatE$BFTtons*1000,by=list(HCatE$Year),sum)
names(HCatE)<-c("Year","Ckg")
HCatE<-HCatE[HCatE$Year>=Base@Hyears[1]&HCatE$Year<=Base@Hyears[2],]
HCatE$Year<-HCatE$Year-Base@Hyears[1]+1                     # Standardize historical year index

areasE<-match(c("NEATL","EATL","SEATL","MED"),Base@areanams) # 45deg longitude split

CE<-CbyY[CbyY$Area%in%areasE,]
CEa<-aggregate(CE$Ckg,by=list(CE$Year),sum)
names(CEa)<-c("Year","Ckg")
HCatE$Ckg[match(CEa$Year,HCatE$Year)]<-CEa$Ckg

# get recent distribution of catch comnposition
fleetsE<-(1:Base@nf)[Base@Fleets$gearTC=="TP"]               # Trap fleet selectivity

CLE<-CLobs[CLobs$Year<=niy & CLobs$Area%in%areasE,]         # Catch at length observations for specified fleets, years and areas
CLE<-aggregate(CLE$N,by=list(CLE$Subyear, CLE$Area, CLE$Length_category),sum)        # Summed over years and fleets
names(CLE)<-c("Subyear","Area","Length_category","Cw")
CLE$Cw<-CLE$Cw*wt_len[CLE$Length_category]                                           # convert numbers to weight for division of weights into age bins
CLE$Cw<-CLE$Cw/sum(CLE$Cw)                                                           # Seasonal / spatial fractions calculated


# disaggregate historical catch by recent distribution of catch comps
#ind<-as.matrix(cbind(rep(HCatE[,1],each=nrow(CLE)),CLE[,1:3]))
#CLEind<-rep(1:nrow(CLE),nrow(HCatE))
#CLmat[ind]<-CLE$Cw[CLEind]*HCatE$Ct[ind[,1]]                                         # fraction multiplied by total catch

for(yy in 1:nrow(HCatE)){

  ind<-as.matrix(cbind(rep(yy,nrow(CLE)),CLE[,1:3]))
  CLmat[ind]<-HCatE$Ckg[ind[,1]]*CLE[,4]

}


# Western catch at length reconstruction ---------------------

# get historical task 1 catches
HCatW<-read.csv(paste(getwd(),"/Data/Raw/Task1/t1nc_20161114_bft.csv",sep=""))
HCatW<-HCatW[HCatW$Stock=="ATW"&HCatW$Year>=Base@Hyears[1]&HCatW$Year<=Base@Hyears[2],c(2,6,8)]
HCatW<-aggregate(HCatW$Qty_t*1000,by=list(HCatW$Year),sum)
names(HCatW)<-c("Year","Ckg")
HCatW$Year<-HCatW$Year-Base@Hyears[1]+1                     # Standardize historical year index

# get recent distribution of catch comps
areasW<-match(c("GOM","CAR","WATL","GSL","SCATL","NCATL"),Base@areanams) # 45deg longitude split

CW<-CbyY[CbyY$Area%in%areasW,]
CWa<-aggregate(CW$Ckg,by=list(CW$Year),sum)
names(CWa)<-c("Year","Ckg")
HCatW$Ckg[match(CWa$Year,HCatW$Year)]<-CWa$Ckg


fleetsW<-1:Base@nf                                                       # all fleets

CLW<-CLobs[CLobs$Fleet%in%fleetsW & CLobs$Year<=niy & CLobs$Area%in%areasW,]         # Catch at length observations for specified fleets, years and areas
CLW<-aggregate(CLW$N,by=list(CLW$Subyear, CLW$Area, CLW$Length_category),sum)        # Summed over years and fleets
names(CLW)<-c("Subyear","Area","Length_category","Cw")
CLW$Cw<-CLW$Cw*wt_len[CLW$Length_category] # convert numbers to weight for division of weights into age bins
CLW$Cw<-CLW$Cw/sum(CLW$Cw)                                                           # Seasonal / spatial fractions calculated


# disaggregate historical catch by recent distribution of catch comps
#ind<-as.matrix(cbind(rep(HCatW[,1],each=nrow(CLW)),CLW[,1:3]))
#CLWind<-rep(1:nrow(CLW),nrow(HCatW))
#HCatWind<-rep(1:nrow(HCatW),each=nrow(CLW))
#CLmat[ind]<-CLW$Cw[CLWind]*HCatW$Ct[HCatWind]                                         # fraction multiplied by total catch

for(yy in 1:nrow(HCatW)){

  ind<-as.matrix(cbind(rep(HCatW$Year[yy],nrow(CLW)),CLW[,1:3]))
  CLmat[ind]<-HCatW$Ckg[ind[,1]-min(HCatW$Year)+1]*CLW[,4]

}

# Convert to catch at age ------------------------------------

adjfactor<-1

biALK<-t(ALK[1,1,,]) # Eastern iALK in first year (eastern priority)
ind<-TEG(c(Base@nHy,Base@ns,Base@nr,Base@nl,Base@na))
CLAmat[ind]<-CLmat[ind[,1:4]]*biALK[ind[,4:5]]
CAmat<-HCobs<-apply(CLAmat,c(1,2,5,3),sum)

CAmat<-CAmat/adjfactor

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
HCobs_new<-HCobs
save(HCobs_new,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs_new",sep=""))


diag<-F

if(diag){  # some diagnostics

  # check disaggregation
  cbind(HCatE$Ckg,apply(CLmat[,,7:10,],1,sum))
  cbind(HCatW$Ckg,apply(CLmat[HCatW$Year,,1:6,],1,sum))

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
