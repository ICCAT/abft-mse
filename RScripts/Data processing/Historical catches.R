# Historical catches.R
# November 2016
# R Script for assigning historical annual catches to season, age and area prior to 1960
# (Using Task1 in the west and Trap data in the East)

# Control parameters -------------------------------

niy<-20 # initial years of length comps uses to redistribute catches

CLmat<-array(0,c(Base@nHy,Base@ns,Base@nr,Base@nl))  # a blank catch at length matrix
CLAmat<-array(0,c(Base@nHy,Base@ns,Base@nr,Base@nl,Base@na)) # blank catch at length at age matrix
wt_len<-Base@lwa[1]*Base@mulen^Base@lwb[1] # calculate weights for length bins


# Build an ALK (for assigning catch at length to catch at age)

agearray<-array(rep(1:Base@na,each=Base@np),c(Base@np,Base@na,Base@ny))
len_age<-wt_age<-array(NA,dim(agearray))
ind<-TEG(dim(agearray))
len_age[ind]<-(
  Base@L1[ind[,1]]^Base@p[ind[,1]]+(Base@L2[ind[,1]]^Base@p[ind[,1]]-Base@L1[ind[,1]]^Base@p[ind[,1]])*
    (
      (1-exp(-Base@K[ind[,1]]*agearray[ind]))/
        (1-exp(-Base@K[ind[,1]]*Base@na))
    )
)^(1/Base@p[ind[,1]])

nlen<-length(Base@lenbins)-1

ind<-TEG(dim(wt_age))
wt_age[ind]<-Base@lwa[ind[,1]]*len_age[ind]^Base@lwb[ind[,1]]

LenCV<-0.07
ALK<-array(NA,c(Base@np,Base@ny,Base@na,Base@nl))
ind<-TEG(dim(ALK))
Lind<-ind[,c(1,3,2)]
ALK[ind]<-dnorm(Base@mulen[ind[,4]],len_age[Lind],len_age[Lind]*LenCV)
sums<-apply(ALK,c(1,2,4),sum)
sind<-ind[,c(1,2,4)]
ALK<-ALK/sums[sind]





# Eastern catch-at-length reconstruction ---------------------

# get historical Task I catches
HCatE<-read.csv(paste(getwd(),"/Data/Raw/Catch/Hist Trap East.csv",sep=""))
HCatE$BFTtons<-as.numeric(HCatE$BFTtons)
HCatE<-aggregate(HCatE$BFTtons,by=list(HCatE$Year),sum)
names(HCatE)<-c("Year","Ct")
HCatE<-HCatE[HCatE$Year>=Base@Hyears[1]&HCatE$Year<=Base@Hyears[2],]
HCatE$Year<-HCatE$Year-Base@Hyears[1]+1                     # Standardize historical year index


# get recent distribution of catch comnposition 
areasE<-match(c("NEATL","EATL","SEATL","MED"),Base@areanams) # 45deg longitude split
fleetsE<-(1:Base@nf)[Base@Fleets$gearTC=="TP"]               # Trap fleet selectivity

CLE<-CLobs[CLobs$Fleet%in%fleetsE & CLobs$Year<=niy & CLobs$Area%in%areasE,]         # Catch at length observations for specified fleets, years and areas
CLE<-aggregate(CLE$N,by=list(CLE$Subyear, CLE$Area, CLE$Length_category),sum)        # Summed over years and fleets
names(CLE)<-c("Subyear","Area","Length_category","Cw")
CLE$Cw<-CLE$Cw*wt_len[CLE$Length_category]                                           # convert numbers to weight for division of weights into age bins
CLE$Cw<-CLE$Cw/sum(CLE$Cw)                                                           # Seasonal / spatial fractions calculated


# disaggregate historical catch by recent distribution of catch comps
ind<-as.matrix(cbind(rep(HCatE[,1],each=nrow(CLE)),CLE[,1:3]))
CLEind<-rep(1:nrow(CLE),nrow(HCatE))
CLmat[ind]<-CLE$Cw[CLEind]*HCatE$Ct[ind[,1]]                                         # fraction multiplied by total catch



# Western catch at length reconstruction ---------------------

# get historical task 1 catches
HCatW<-read.csv(paste(getwd(),"/Data/Raw/Task1/t1nc_20161114_bft.csv",sep=""))
HCatW<-HCatW[HCatW$Stock=="ATW"&HCatW$Year>=Base@Hyears[1]&HCatW$Year<=Base@Hyears[2],c(2,6,8)]
HCatW<-aggregate(HCatW$Qty_t,by=list(HCatW$Year),sum)
names(HCatW)<-c("Year","Ct")
HCatW$Year<-HCatW$Year-Base@Hyears[1]+1                     # Standardize historical year index

# get recent distribution of catch comps
areasW<-match(c("GOM","CAR","WATL","GSL","SCATL","NCATL"),Base@areanams) # 45deg longitude split
fleetsW<-1:Base@nf                                                       # all fleets

CLW<-CLobs[CLobs$Fleet%in%fleetsW & CLobs$Year<=niy & CLobs$Area%in%areasW,]         # Catch at length observations for specified fleets, years and areas
CLW<-aggregate(CLW$N,by=list(CLW$Subyear, CLW$Area, CLW$Length_category),sum)        # Summed over years and fleets
names(CLW)<-c("Subyear","Area","Length_category","Cw")
CLW$Cw<-CLW$Cw*wt_len[CLW$Length_category] # convert numbers to weight for division of weights into age bins
CLW$Cw<-CLW$Cw/sum(CLW$Cw)                                                           # Seasonal / spatial fractions calculated


# disaggregate historical catch by recent distribution of catch comps
ind<-as.matrix(cbind(rep(HCatW[,1],each=nrow(CLW)),CLW[,1:3]))
CLWind<-rep(1:nrow(CLW),nrow(HCatW))
HCatWind<-rep(1:nrow(HCatW),each=nrow(CLW))
CLmat[ind]<-CLW$Cw[CLWind]*HCatW$Ct[HCatWind]                                         # fraction multiplied by total catch


# Convert to catch at age ------------------------------------

adjfactor<-1

biALK<-t(ALK[1,1,,]) # Eastern iALK in first year (eastern priority)
ind<-TEG(c(Base@nHy,Base@ns,Base@nr,Base@nl,Base@na))
CLAmat[ind]<-CLmat[ind[,1:4]]*biALK[ind[,4:5]]
CAmat<-HCobs<-apply(CLAmat,c(1,2,5,3),sum)

CAmat<-CAmat/adjfactor

wt_age<-Base@wt_age[1,,1]

ind<-TEG(c(Base@nHy,Base@ns,Base@na,Base@nr))
HCobs[ind]<-CAmat[ind]/(wt_age[ind[,3]]/1000) # Catch is in tonnes
HCobs[,,1,]<-0 # for now ignore any catches in first age class

#HCobs<-as.data.frame(cbind(ind,CAmatN[ind]))
#names(HCobs)<-c("Year","Quarter","Area","Age","N")
#HCobs<-HCobs[HCobs$N>10,]/adjfactor

save(HCobs,file=paste(getwd(),"/Data/Processed/Conditioning/HCobs",sep=""))

diag<-F
if(diag){  # some diagnostics
  
  # check disaggregation 
  cbind(HCatE$Ct,apply(CLmat[,,10,],1,sum))
  cbind(HCatW$Ct,apply(CLmat[87:96,,1:6,],1,sum))
  
  # check age assignment
  cbind(apply(CLmat,1,sum),apply(CLAmat,1,sum),apply(CAmat,1,sum))
  
  agg<-apply(CAmat,c(1,4),sum)
  
  RecCat<-array(NA,dim=c(Base@ny,Base@nr))
  RecC<-aggregate(Base@Cobs[,5],by=list(Base@Cobs[,1], Base@Cobs[,3]),sum)
  RecCat[as.matrix(RecC[,1:2])]<-RecC[,3]/1000

  cols<-rep(c('red',"dark grey","black","green","blue"),2)
  ltys<-rep(c(2,1),each=5)
  
  par(mfrow=c(2,1),mai=c(0.9,0.9,0.2,0.05))
  matplot(Base@Hyears[1]:Base@years[2],rbind(agg,RecCat),type='l',col=cols,lty=ltys,xlab="Year",ylab="Annual Catches (tonnes)")
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
