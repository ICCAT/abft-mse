
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Create a base operating model that can be modified for each of trial specifications
# The basic dimensionality / structure of this OM is the same for all trial specifications
# ie the fleets, areas, times and data are the same

# Tom Carruthers UBC
# Laurie Kell ICCAT

# 8th August 2016


# === Outstanding issues ==============

# iALK is currently made up!
# Area size in q modification in M3
# need to put the new indices in the model
# need to rename the slots of the OMI object
# need to recalculate depletion SSB/SSB0 and B/B0 inside model due to new SRA features
# add absolute abundance prior and slots

# === Main script =====================

# --- Set working directory ------

setwd("G:/ABT-MSE/")


# --- Source MSE functions and objects ------
library(ABTMSE)
#source("Source/MSE_source.r")
#source("Source/Objects.r")


# --- Define dimensions ------

Base<-new('OMI')
Base@Date<-as.character(Sys.time())
Base@Author<-"T. Carruthers (t.carruthers@oceans.ubc.ca)"
Base@Notes<-"This object serves as a framework for populating the various Trial Specifications"
Base@PrimarySource<-"ICCAT Bluefin data prep. meeting July 2016"
Base@years<-years<-c(1970,2014)
Base@Hyears<-Hyears<-c(1951,1969)


# --- (a) Create some prespecified areas (polygons with lons and lats) ------

source("RScripts/Data processing/Area definitions.r") # Creates objects: AreaNames, AreaDefs


# --- Define areas ------

Base@areas<-areas<-c("GOM","WAtlw","EAtlw","Med")
Base@areanams<-areanams<-c("GOM","WATL","EATL","MED")
Base@area_defs<-new('list')
for(i in 1:length(Base@areas))Base@area_defs[[i]]<-AreaDefs[[match(areas[i],AreaNames)]]

# --- (b) Define fleets ------

Fleets<-new('list') # note you must put ALL flag types ahead of specific fleet types of same gear - sequential fleet assignment
Fleets$name<-Fleets$gearTC<-c("LL","BB","PS")
Fleets$flag<-  c("ALL","ALL",  "ALL")
Fleets$FyS<-   c( 1960, 1960,   1960)
Fleets$FyE<-   c( 2015, 2015,   2015)

Fleets$Loc<-new('list') # What areas?
Fleets$Loc[[1]]<-Fleets$Loc[[2]]<-Fleets$Loc[[3]]<-1:4 # LL

Fleets$Q<-new('list') # What quarters of the year?
Fleets$Q[[1]]<-Fleets$Q[[2]]<-Fleets$Q[[3]]<-1:4       # LLOTH

Base@Fleets<-Fleets


# --- Length bin set up ------

Base@lenbins<-(1:16)*25                         # 25 cm length bins
nlen<-length(Base@lenbins)-1
Base@mulen<-(Base@lenbins[1:nlen]+Base@lenbins[2:(nlen+1)])/2


# --- Assign known dimensions ------

Base@nHy<-as.integer(Hyears[2]-Hyears[1]+1)       # No. historical years
Base@ny<-as.integer(years[2]-years[1]+1)          # No. years
Base@ns<-as.integer(4)                            # No. subyears
Base@np<-as.integer(2)                            # No. stocks
Base@na<-as.integer(18)                           # No. ages
Base@nr<-as.integer(length(areas))                # No. areas
Base@nf<-as.integer(length(Base@Fleets$name)+1)   # No. fleets
Base@nl<-as.integer(nlen)


# --- Recapture indexing by subyear

Base@nRPT<-as.integer(2) # recaptures are only calculated in consecutive timesteps
temp<-rep(1:Base@ns,ceiling(Base@nRPT/Base@ns)+Base@ns)
Base@RPTind<-array(NA,c(Base@ns,Base@nRPT))
for(ss in 1:Base@ns)Base@RPTind[ss,]<-temp[ss:(ss+Base@nRPT-1)]


# --- Misc ------

Base@sdur<-rep(1/Base@ns,Base@ns) # the duration of each subyear - we make these equal
#Base@nZeq<-as.integer(40)
Base@nydist<-as.integer(10)
#Base@nyeq<-as.integer(15)
yblock<-5 # the duration of recruitment deviation blocks
Base@RDblock<-rep(1:100,each=yblock)[1:Base@ny]
Base@nRD<-max(Base@RDblock)


# --- (c) Create iALK ------

Base@lwa<-c(2.95*10^-5,1.96*10^-5)   # length-weight conversion w=al^b
Base@lwb<-c(2.899,3.009)             # length-weight conversion w=al^b
Base@L1<-c(32.43,32.43)    # Richards growth curve
Base@L2<-c(263.64,263.64)  # Richards growth curve
Base@K<-c(0.26,0.26)       # Richards growth curve
Base@p<-c(0.97,0.97)       # Richards growth curve

source("RScripts/Data processing/iALK.r") # returns len_age wt_age iALK

Base@iALK<-iALK
Base@wt_age<-wt_age
Base@len_age<-len_age

# Fecundity / maturity / recruitment / mortality

Base@mat<-array(t(array(c(rep(0,5),rep(1,Base@na-5),
                  rep(0,9),rep(1,Base@na-9)),c(Base@na,Base@np))),c(Base@np,Base@na,Base@ny))
Base@Fec<-Base@mat[,,1]*Base@wt_age[,,1]
Base@spawns<-c(2,2)
Base@canspawn<-matrix(c(0,0,0,1,  1,0,0,0),ncol=Base@np) # matrix of spawning areas 1=can spawn
Base@Ma<-t(array(c(0.49,rep(0.24,4),0.2,0.175,0.15,0.125,rep(0.10,Base@na-9),
        rep(0.14,Base@na)),c(Base@na,Base@np)))


# --- (d) Process CATDIS data ------

source("RScripts/Data processing/CATDIS.r") # returns Cobs in the right format

Base@Cobs<-as.matrix(Cobs)
Base@nCobs<-nrow(Cobs)


# --- (e) Calculate master indices ------


T2cpue<-read.csv("Data/Raw/Task2/T2_errcheck.csv")
T2cpue$FleetID<-substring(T2cpue$FleetID,1,3)

# Calculate habitat area sizes by region and quarter

T2cpue<-assign_quarter(T2cpue) # Add quarter
T2cpue<-assign_area(T2cpue,Base@area_defs)

#T2cpue<-subset(T2cpue,T2cpue$Area==10)
test0<-aggregate(T2cpue$BFT,by=list(T2cpue$FleetID,T2cpue$GearGrpCode,T2cpue$CatchUnit),sum)
cond<-test0$Group3=="nr"
test0$x[cond]<-test0$x[cond]*70 # a daft guess at mean weight just to prioritize fleets by catch weight
test0[order(test0$x,decreasing=T),]
test<-aggregate(T2cpue$BFT,by=list(T2cpue$FleetID,T2cpue$GearGrpCode,T2cpue$CatchUnit,T2cpue$Area),sum)
test[order(test$x,decreasing=T),]
test2<-test[test[,4]==1,]
test2[order(test2$x,decreasing=T),]

by1<-T2cpue[T2cpue$SquareTypeCode=="1x1",]
by1<-assign_area(by1,Base@area_defs)
by1agg<-aggregate(by1$BFT,by=list(by1$Area,by1$Subyear,by1$Lat,by1$Lon),sum)
names(by1agg)<-c("Area","Subyear","Lat","Lon","C")
by1agg<-by1agg[by1agg$C>1,]

#AS<-aggregate(rep(1,nrow(by1agg)),by=list(by1agg$Area),sum)
#AreaSize<-AS$x/mean(AS$x)

AS<-aggregate(rep(1,nrow(by1agg)),by=list(by1agg$Area,by1agg$Subyear),sum)
AreaSize<-array(NA,c(Base@nr,Base@ns))
AreaSize[as.matrix(AS[,1:2])]<-AS[,3]
AreaSize<-AreaSize/mean(AreaSize,na.rm=T)
for(i in 1:Base@nr)AreaSize[i,is.na(AreaSize[i,])]<-mean(AreaSize[i,],na.rm=T)


# === Define a list of Master indices for trial specifications ======================================

MI<-new('list')
MI[[1]]<-MI[[2]]<-MI[[3]]<-array(NA,c(Base@ny,Base@ns,Base@nr)) #


# === First scenario set ===================

FleetID<-c("012JP00","025US00","004CA00","004CA00","016MA00","021ES00","021ES00") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
Gear<-c(   "LL",     "LL",       "RR",     "LL",      "TP",     "TP",     "HL")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
type<-c(   'nr',     'nr',       'kg',     "kg",      "kg",     "kg",     "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit

Fnam<-c("JP LL", "USA LL", "CA RR",  "CA LL",  "MA TP",  "ES TP",  "ES HL")   # Fleet names for graphing


FleetID<-c("012",       "004",         "016",      "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
Gear<-c(   "LL",         "RR",           "TP",       "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
type<-c(   'nr',          'kg',           "kg",       "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit

Fnam<-c("JP LL", "USA LL", "CA RR",  "MA TP",  "ES BB")   # Fleet names for graphing



cpue<-new('list')

ploty=1
if(ploty==1){
  par(mfrow=c(length(FleetID),4),mai=c(0.25,0.2,0.05,0.05),omi=c(0.05,1,0.5,0.01))
}
if(ploty==2){
  par(mfrow=c(length(FleetID),Base@nr),mai=c(0.25,0.2,0.05,0.05),omi=c(0.05,1,0.5,0.01))
}

xlimy<-c(-100,25)
ylimy<-c(-40,65)

prettyaxis<-function(lims){
  inc<-(lims[2]-lims[1])/5
  pretty(seq(lims[1]-inc,lims[2]+inc,length.out=12))
}

for(i in 1:length(FleetID)){

  temp<-T2cpue[T2cpue$FleetID==FleetID[i]&T2cpue$GearGrpCode==Gear[i]&T2cpue$Eff1>0&T2cpue$BFT>0&T2cpue$CatchUnit==type[i],]
  temp<-assign_area(temp,Base@area_defs)
  temp<-assign_quarter(temp)
  temp<-assign_year(temp,years)
  cond<-temp$Eff1>quantile(temp$Eff1,0.1)&temp$BFT>100
  temp<-subset(temp,cond)
  print(paste(i,Fnam[i],mean(temp$Eff1)))

  if(ploty==1){
    eff<-aggregate(temp$Eff1,by=list(temp$Subyear,temp$Lat,temp$Lon),sum)
    cat<-aggregate(temp$BFT,by=list(temp$Subyear,temp$Lat,temp$Lon),sum)
    N<-aggregate(rep(1,nrow(temp)),by=list(temp$Subyear,temp$Lat,temp$Lon),sum)

    for(qq in 1:4){
      datyE<-subset(eff,eff$Group.1==qq)
      datyC<-subset(cat,eff$Group.1==qq)
      CPE<-datyC$x/datyE$x
      CPE[CPE>(mean(CPE)+3*sd(CPE))]<-0
      CPE<-CPE[datyC$x>10]
      CPE<-CPE^0.5
      CPE<-CPE/mean(CPE)
      plot(datyE$Group.3,datyE$Group.2,cex=CPE,xlim=xlimy,ylim=ylimy,pch=19,col="#0000ff60",axes=F)

      for(rr in 1:Base@nr){
        polygon(Base@area_defs[[rr]],col="#ffffff60",lwd=1.7)
        #text(mean(Base@area_defs[[rr]]$x),mean(Base@area_defs[[rr]]$y),rr)
      }
      if(qq==1)mtext(Fnam[i],2,las=2,line=3)
      if(i==1)mtext(paste("Quarter",qq),3,line=1)
      if(i==(length(FleetID))){ axis(1,prettyaxis(xlimy),prettyaxis(xlimy))
      }else{axis(1,prettyaxis(xlimy))}
      if(qq==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
      }else{axis(2,prettyaxis(ylimy))}
    }

  }
  if(ploty==2){

    eff<-aggregate(temp$Eff1,by=list(temp$Year,temp$Area),sum)
    cat<-aggregate(temp$BFT,by=list(temp$Year,temp$Area),sum)

    CPE<-(cat$x/eff$x)*AreaSize[cat[,2]]

    CPE<-CPE/mean(CPE)
    for(rr in 1:Base@nr){
      if(length(eff[eff[,2]==rr,1])>0){
        plot(eff[eff[,2]==rr,1],CPE[eff[,2]==rr],type='l')
      }else{
        plot(1, type="n", axes=F, xlab="", ylab="")
      }
      if(rr==1)mtext(Fnam[i],2,las=2,line=2)
      if(i==1)mtext(Base@areanams[rr],3,line=1)
    }

  }
  eff<-aggregate(temp$Eff1,by=list(temp$Year,temp$Subyear,temp$Area),sum)
  cat<-aggregate(temp$BFT,by=list(temp$Year,temp$Subyear,temp$Area),sum)
  N<-  aggregate(rep(1,nrow(temp)),by=list(temp$Year,temp$Subyear,temp$Area),sum)
  crtemp<-(cat$x/eff$x)
  var<-3*sd(log(crtemp))
  mu<-mean(log(crtemp))
  cond<-log(crtemp)<(mu+var)&log(crtemp)>(mu-var)
  crtemp<-crtemp[cond]
  crtemp<-crtemp/mean(crtemp)
  cpue[[i]]<-as.data.frame(cbind(eff[cond,],cat$x[cond],crtemp,N$x[cond],rep(i,sum(cond))))
  names(cpue[[i]])<-c("Year","Subyear","Area","E","C","CPUE","N","Fleet")

}



CPUE<-cpue[[1]]
for(i in 2:length(cpue))CPUE<-rbind(CPUE,cpue[[i]])


YAint<-aggregate(rep(1,nrow(CPUE)),by=list(as.numeric(CPUE$Year),as.numeric(CPUE$Area)),sum)
YA<-array(NA,c(Base@ny,Base@nr))
YA[as.matrix(YAint[,1:2])]<-YAint[,3]

SAint<-aggregate(rep(1,nrow(CPUE)),by=list(as.numeric(CPUE$Subyear),as.numeric(CPUE$Area)),sum)
SA<-array(NA,c(Base@ns,Base@nr))
SA[as.matrix(SAint[,1:2])]<-SAint[,3]


# Create a pseudo area for interaction with time PArea
Parea_conv<-1:4
Parea<-Parea_conv[CPUE$Area]
CPUE<-cbind(CPUE,Parea)

for(i in c(1,2,3,8,9))CPUE[,i]<-as.factor(as.character(CPUE[,i]))
wt<-as.numeric(as.character(CPUE$N))
#out<-glm(log(CPUE)~Year*Area*Subyear+Fleet,data=CPUE,weights=wt)
out<-glm(log(CPUE)~Year*Area+Area*Subyear+Fleet*Area,data=CPUE,weights=wt)


newdat<-expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr,1)
newdat<-cbind(newdat,Parea_conv[newdat[,3]])
names(newdat)<-c("Year","Subyear","Area","Fleet","Parea")
for(i in 1:ncol(newdat))newdat[,i]<-as.factor(as.character(newdat[,i]))
pred<-predict(out,newdat)


ind<-as.matrix(expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr))
MI[[1]][ind]<-exp(pred)*AreaSize[ind[,3:2]]
MI[[1]]<-MI[[1]]*seq(1.8,0.2,length.out=Base@ny)

pCPUE<-MI[[1]]

anam<-Base@areanams
par(mfcol=c(Base@nr,Base@ns),mai=c(0.2,0.2,0.1,0.05),omi=c(0.5,1,0.25,0.01))
ylimy<-c(0,quantile(pCPUE,0.995,na.rm=T))

mtext("Year",1,line=1.5,outer=T)
for(a in 1:Base@nr){for(q in 1:Base@ns){
  MI[[1]][,q,a]<-smooth.spline(MI[[1]][,q,a],all.knots=T,spar=0.6)$y
}}


for(q in 1:Base@ns){for(a in 1:Base@nr){
  plot(Base@years[1]:Base@years[2],MI[[1]][,q,a],ylim=ylimy,type='l',col='red',axes=F)
  if(a==1)mtext(paste("Quarter",q),3,line=1)
  if(q==1)mtext(anam[a],2,line=3,las=1)
  if(q==4){axis(1,prettyaxis(years),prettyaxis(years))
  }else{axis(1,prettyaxis(years))}
  if(a==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
  }else{axis(2,prettyaxis(ylimy))}

}}

Base@RAI=MI[[1]]


# --- (f) Calculate partial F's ------

source("Rscripts/Data processing/Partial Fs.r") # returns CPUEobs

Base@nE<-Base@nf # same number of cpue indices (if not using master index) as fleets
Base@nEobs<-nrow(Eobs)
Base@Eobs<-Eobs


# --- (g) Process assessment indices ----

CPUEdat<-read.csv(file=paste(getwd(),"/Data/Processed/CPUE indices/CPUE indices compiled.csv",sep=""))
CPUEdat<-subset(CPUEdat,CPUEdat$Year>(years[1]-1))
CPUEdat$Fleet<-1
newa<-c(1,2,2,2,3,3,3,3,3,4)
CPUEdat$Area<-newa[CPUEdat$Area]
CPUEobs<-CPUEdat[,1:6]
CPUEobs[,1]<-CPUEobs[,1]-Base@years[1]+1
CPUEobs<-CPUEobs[CPUEobs$Year<=Base@ny,] # set maximum year to Base@ny

# Get the CPUE series names
firstrow<-match(1:max(CPUEdat$qNo),CPUEdat$qNo)
CPUEnames<-as.character(CPUEdat$Name[firstrow])

# Standardize to mean 1
mubyF<-aggregate(CPUEobs$Index,by=list(CPUEobs$qNo),mean)
CPUEobs$Index<-CPUEobs$Index/mubyF[CPUEobs$qNo,2]




Base@nCPUEq<-max(CPUEobs$qNo)
Base@nCPUEobs<-nrow(CPUEobs)
Base@CPUEobs<-as.matrix(CPUEobs)
Base@CPUEnames<-CPUEnames

# --- (h) Calculate length sample data ------


dat<-read.csv(paste(getwd(),"/Data/Raw/Task2/t2szBFT-all_v1.csv",sep=""))
dat<-dat[dat$YearC>(Base@years[1]-1)&dat$YearC<(Base@years[2]+1),]
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

dat$Year<-dat$Year-Base@years[1]+1


Lencat<-rep(NA,nrow(dat))
for(i in 1:Base@nl){

  Lencat[dat$ClassFrq>(Base@lenbins[i]-0.01)&dat$ClassFrq<Base@lenbins[i+1]]<-i

}
cond<-!is.na(Lencat)
Lencat<-Lencat[cond]
dat<-dat[cond,]

CLobs<-aggregate(dat$Nr,by=list(dat$Year,dat$Quarter,dat$Area,dat$Fleet,Lencat),sum)
names(CLobs)<-c("Year","Subyear","Area","Fleet","Length_category","N")

Base@nCLobs<-nrow(CLobs)
Base@CLobs<-as.matrix(CLobs)


# --- (i) Calculate historical catches ------

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
  fleetsE<-(1:Base@nf)[Base@Fleets$gearTC=="LL"]               # Trap fleet selectivity

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

  Base@HCobs<-HCobs # a 4D array y x s x a x  r


# --- (j) Fishery independent indices ------


  MED<-read.csv(paste(getwd(),"/Data/Raw/SSB/Balearic larval 2014.csv",sep=""))
  MED<-MED[MED$Survey.Year>(Base@years[1]-1)&MED$Survey.Year<(Base@years[2]+1),]
  MED$Survey.Year<-MED$Survey.Year-Base@years[1]+1
  ng<-nrow(MED)
  IobsMED<-cbind(MED$Survey.Year,rep(Base@spawns[1],ng),rep(match("MED",Base@areanams),ng),rep(1,ng),rep(1,ng),rep(2,ng),MED$Index.1)


  GOM<-read.csv(paste(getwd(),"/Data/Raw/SSB/Ingram 2014 index.csv",sep=""))
  GOM<-GOM[GOM$Year>(Base@years[1]-1)&GOM$Year<(Base@years[2]+1),]
  GOM$Year<-GOM$Year-Base@years[1]+1
  ng<-nrow(GOM)
  IobsGOM<-cbind(GOM$Year,rep(Base@spawns[2],ng),rep(match("GOM",Base@areanams),ng),rep(2,ng),rep(2,ng),rep(2,ng),GOM$ZIDL)

  #Year (1), subyear (2), area (3), stock (4)  index number (matching nI, 5), index type (biomass=1, SSB=2, column 6), the observed relative abundance index (column 7)


  RAIind<-TEG(dim(Base@RAI))
  IobsRAI<-cbind(RAIind,rep(1,nrow(RAIind)),rep(3,nrow(RAIind)),rep(3,nrow(RAIind)),Base@RAI[RAIind])


  #Iobs<-rbind(IobsMED,IobsGOM,IobsRAI) # alternatively add the master index to the biomass index.
  Iobs<-rbind(IobsMED,IobsGOM)

Base@nI<-as.integer(max(Iobs[,5])) # number of series
Base@nIobs<-nrow(Iobs) # number of data
Base@Iobs<-Iobs   # y s r i type(biomass/ssb) index


# --- (k) PSAT tags ------

Base@nma<-as.integer(3)
Base@ma<-c(rep(1,4),rep(2,4),rep(3,Base@na-8))

dat<-read.csv(paste(getwd(),"/Data/Raw/PSAT/BFT_etags_09292016.csv",sep="")) #

#dat<-AssignAge(dat,Base) # assign approximate ages bases on cohort slicing from eastern growth (needs only to get at rough age groups)


#AllPSATo<-read.csv(paste(getwd(),"/Data/Raw/PSAT/ALL_BFT_ElectronicTags_01122015.csv",sep=""))

expandtrack<-function(dat){

  datstr<-dat
  org<-c("CAR", "E_ATL","E_MED",  "GOM","GSL","NC_ATL", "NE_ATL", "SC_ATL", "SE_ATL", "W_ATL",  "W_MED")
  nu<-c(   2,     3,      4,       1,    2,     3,        3,        3,         3,      2,        4)#
  #got to here
  dat$Stock_Area<-nu[match(dat$Stock_Area,org)]
  dat$Start_Date<-as.POSIXct(dat$Start_Date, format = "%m/%d/%Y", tz = "UTC")
  dat$End_Date<-as.POSIXct(dat$End_Date, format = "%m/%d/%Y", tz = "UTC")

  cond<-is.na(dat$Start_Date)
  dat$Start_Date[cond]<-as.POSIXct(datstr$Start_Date[cond], format = "%m/%d/%Y", tz = "UTC")
  dat$End_Date[cond]<-as.POSIXct(datstr$End_Date[cond], format = "%m/%d/%Y", tz = "UTC")

  getlb<-function(x,vec){
    vec<-c(0,vec)
    max((1:length(vec))[x>vec])
  }
  #       TagID Area Date

  age<-sapply(dat$Length_cm,getlb,vec=Base@len_age[1,,1])
  agew<-sapply(dat$Weight_kg,getlb,vec=Base@wt_age[1,,1])
  age[is.na(age)]<-agew[is.na(age)]

  ageclass<-Base@ma[age]
  dat<-cbind(dat,ageclass)

  dat<-subset(dat,!is.na(dat$ageclass))

  for(r in 1:nrow(dat)){

    nd<-as.integer((dat$End_Date[r]-dat$Start_Date[r])+1)
    Date<-dat$Start_Date[r]+days(0:(nd-1))
    Year<-as.numeric(format(Date,"%Y"))
    Quarter<-ceiling(as.numeric(format(Date,"%m"))/3)
    TagID<-rep(as.character(dat$Tag_ID[r]),nd)
    Area<-rep(dat$Stock_Area[r],nd)
    AgeC<-rep(dat$ageclass[r],nd)

    temp2<-data.frame(TagID,Area,Date,Year,Quarter,AgeC)

    if(r==1)temp<-temp2
    if(r>1)temp<-rbind(temp,temp2)

  }

  temp

}

All<-expandtrack(dat)

mostfreq<-function(x){
  #x<-ceiling(runif(1000)*10)
  freq<-aggregate(rep(1,length(x)),by=list(x),sum)
  if(nrow(freq)>0){
    return(freq[which.max(freq$x),1])
  }else{
    return(NA)
  }
}
# As per M3 format population, subyear, time duration (quarters) til capture, from area, to area, N
stk<-array(rep(1:Base@np,each=Base@nr),c(Base@nr,Base@np))
ar<-array(rep(1:Base@nr,Base@np),c(Base@nr,Base@np))
ExSpawn<-cbind(stk[Base@canspawn==1],ar[Base@canspawn==1])

TagIDs<-unique(All$TagID)
nTags<-length(TagIDs)
tracks<-rep(0,6)

for(i in 1:nTags){

  dat<-subset(All,All$TagID==TagIDs[i])
  Trk<-aggregate(dat$Area,by=list(dat$Year,dat$Quarter,dat$AgeC),mostfreq)
  names(Trk)<-c("Year","Quarter","AgeClass","Area")
  timetemp<-as.numeric(paste(Trk$Year,Trk$Quarter,sep=""))
  Trk<-Trk[order(timetemp),]
  nT<-nrow(Trk)
  if(nT>1){
    #print(i)
    pop<-mostfreq(ExSpawn[match(Trk$Area,ExSpawn[,2]),1])
    for(j in 2:nT){  # loop over tracks by quarter
      if(j==2){
        temptrack<-c(pop,Trk$AgeClass[j-1],Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j])
      }else{
        temptrack<-rbind(temptrack,c(pop,Trk$AgeClass[j-1],Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j]))
      }
    }
    tracks<-rbind(tracks,temptrack)
  }
}

Tracks<-data.frame(tracks[2:nrow(tracks),])
names(Tracks)<-c("p","a","s","t","fr","tr")
anyna<-function(x)sum(is.na(x))==0
Tracks<-Tracks[apply(Tracks,1,anyna),] # remove any line with unknown stock, subyear, or area


PSAT<-aggregate(rep(1,nrow(Tracks)),by=list(Tracks$p,Tracks$a,Tracks$s,Tracks$t,Tracks$fr,Tracks$tr),sum)
names(PSAT)<-c("p","a","s","t","fr","tr","N")



PSAT2<- as.integer(PSAT[PSAT$N==max(PSAT$N),][1,]) # something uncontroversial good chance of eastern residency (western med) for a most likely eastern stock
SOOwt<-rep(0.01/(Base@np-1),Base@np)
SOOwt[PSAT2[1]]<-0.99
PSAT2<-c(PSAT2[c(3,2,4,5,6)],SOOwt)
PSAT2<-matrix(PSAT2,nrow=1)
PSAT<-as.matrix(PSAT)


Base@nPSAT<-nrow(PSAT)
Base@PSAT<-PSAT
Base@nPSAT2<-nrow(PSAT2)
Base@PSAT2<-PSAT2


# --- Conventional tags ------

Base@nTag<-as.integer(1) # currently this is placeholder for conventional tags that are ignored by M3
Base@Tag<-array(c(2,1,7,1,2,2,7,1,2,1),c(1,10))


# --- (l) Stock of origin observations ------

ma<-array(rep(c(rep(1,3),rep(2,5),rep(3,Base@na-8)),each=Base@np),c(Base@np,Base@na))
nma<-max(ma)


  SOO<-read.csv(paste(getwd(),"/Data/Raw/SOO/All SOO 29_7_2016.csv",sep=""),header=T)

  anyna<-function(x)(sum(is.na(x))+sum(x==""))==0
  SOO<-SOO[apply(SOO,1,anyna),]

  SOO<-subset(SOO,SOO$Year>(Base@years[1]-1)&SOO$Year<(Base@years[2]+1))
  SOO$Year<-SOO$Year-Base@years[1]+1

  org<-c("CAR", "E_ATL","E_MED",  "GOM","GSL","NC_ATL", "NE_ATL", "SC_ATL", "SE_ATL", "W_ATL",   "MED")
  nu<-c(   2,     3,      4,       1,    2,     3,        3,        3,         3,      2,        4)#

  SOO$BFT_Area<-nu[match(SOO$BFT_Area,org)]

  SOO$age[SOO$age>Base@na]<-Base@na
  SOO$age<-ma[1,SOO$age] # age class

  SOO$Prob.East[SOO$BFT_Area==1]<-0
  SOO$Prob.East[SOO$BFT_Area==10]<-1

  SOO1<-aggregate(SOO$Prob.East,by=list(SOO$age,SOO$Year,SOO$Quarter,SOO$BFT_Area),sum)
  SOO2<-aggregate(1-SOO$Prob.East,by=list(SOO$age,SOO$Year,SOO$Quarter,SOO$BFT_Area),sum)

  SOO1<-cbind(rep(1,nrow(SOO1)),SOO1)
  SOO2<-cbind(rep(2,nrow(SOO2)),SOO2)

  #SOOtemp<-expand.grid(2,2,50,1:4,5,10) #  !!!!!!!! temporary fix until we sort out SOO for GOM

  names(SOO1)<-names(SOO2)<-c("p","aa","y","s","r","N")

  SOOobs<-rbind(SOO1,SOO2)
  SOOobs<-SOOobs[SOOobs$N>0,]
  SOOobs<-as.matrix(SOOobs)

Base@nSOOobs<-nrow(SOOobs)
Base@SOOobs<-SOOobs


# --- Selectivities ------

Base@nsel<-Base@nf

Base@seltype<-c(2,3,3,3) # all fleets have thompson (potentially) dome-shaped selectivity
Base@selind<-1:Base@nf # No selectivity mirroring - selectivities correspond to fleets
Base@ratiolim<-c(0.1,0.4) # limits on the logistic slope paramter relative to position of inflection point
Base@infleclim<-c(4,15) # limits on the location of the inflection point (age)


# --- (m) Movement estimation and definition ------

Base@movtype<-as.integer(1)


  np<-Base@np
  nareas<-Base@nr
  ns<-Base@ns
  nma<-Base@nma


  can<-matrix(1,nrow=Base@np,ncol=Base@nr)
  can[1,match("GOM",Base@areanams)]<-0
  can[2,match("MED",Base@areanams)]<-0


  Tracks$fr<-newa[Tracks$fr]
  Tracks$tr<-newa[Tracks$tr]

  Tagg2<-aggregate(rep(1,nrow(Tracks)),by=list(Tracks$p,Tracks$a,Tracks$s,Tracks$fr,Tracks$tr),sum)

  tmov<-array(NA,c(Base@np,Base@nma,Base@ns,Base@nr,Base@nr))
  tmov[as.matrix(Tagg2[,1:5])]<-Tagg2[,6]

  movind<-mov1<-c(1,1,1,1,1)
  mov<-priormov<-priormov2<-array(NA,c(np,nma,ns,nareas,nareas))
  ind<-TEG(dim(mov))
  mov[ind]<-can[ind[,c(1,5)]]
  mov[ind]<-mov[ind]*can[ind[,c(1,4)]]

  # for assigning a prior

  for(pp in 1:np){
    for(aa in 1:nma){
      for(ss in 1:ns){
        for(rr in 1:nareas){
          npr<-sum(mov[pp,aa,ss,rr,],na.rm=T)
          if(npr>0){
            fR<-match(1,mov[pp,aa,ss,rr,])
            mov1<-rbind(mov1,c(pp,aa,ss,rr,fR))
            if(npr>1){
              oR<-grep(1,mov[pp,aa,ss,rr,])
              oR<-oR[oR!=fR]
              for(i in 1:length(oR)){
                movind<-rbind(movind,c(pp,aa,ss,rr,oR[i]))
              }
            }
          }
        }
      }
    }
  }

  movind<-movind[2:nrow(movind),]
  mov1<-mov1[2:nrow(mov1),]

  if(Base@movtype==1){ # if a gravity formulation these indices are for the to area that should be estimated by season

    firstr<-apply(can,1,which.max)
    ind<-expand.grid(1:ns,1:nma,1:np)[,3:1]
    mov1<-cbind(ind,firstr[ind[,1]],rep(999,ns*np))
    can2<-can
    can2[cbind(1:np,firstr)]<-0
    can2<-t(can2)
    nrest<-apply(can2,1,sum)
    indr<-array(1:nareas,c(nareas,np))
    indp<-array(rep(1:np,each=nareas),c(nareas,np))
    rs<-indr[can2==1]
    ps<-indp[can2==1]
    all<-expand.grid(1:length(rs),1:nma,1:ns)
    all<-cbind(ps[all[,1]],all[,2:3],rs[all[,1]])
    movind<-cbind(all,rep(999,nrow(all)))

  }

  movind<-as.matrix(movind)
  mov1<-as.matrix(mov1)

Base@nMP<-nrow(movind)+Base@ns*Base@np*Base@nma
Base@nmovind<-nrow(movind)
Base@movind<-movind
Base@nmov1<-nrow(mov1)
Base@mov1<-mov1


# --- Relating to likelihood functions ------

Base@CobsCV<-rep(0.2,Base@nf)         # CV on seasonal catch observations by area
Base@CPUEobsCV<-rep(0.25,Base@nCPUEq) # CV on seasonal CPUE observations by area
Base@IobsCV<-rep(0.25,Base@nI)        # CV on fishery independent indices
Base@RDCV<-1/(Base@ny/Base@nRD)^0.5   # CV for penalty on recruitment deviations (if blocked this is Std. Err.)
Base@SSBprior=c(1,1)                  # dummy prior for SSB (some operating models use fractions of other model estimated current SSB)
Base@SSBCV=0.01                       # default is a very tight prior on SSB
Base@nLHw<-as.integer(12)             # number of likelihood components that may be weighted
#          (1 catch, 2 cpue, 3 FIindex, 4 Lcomp, 5 SOO, 6 PSAT, 7 PSAT2, 8 RecDev, 9 mov, 10 sel, 11 SRA, 12 SSB )",datfile,1,append=T)
Base@LHw<-c(1/200,   1/1000,    1,      1/2000,  5,     1,      1,       10,       2,     1,      200,    0      ) # SSB index for each population


# --- Initial Values ------

Base@muR_ini<-c(350*8,350)*5000
Base@sel_ini<-t(array(c(0,0,(1:4)/4,rep(1,Base@nl-6)),c(Base@nl,Base@nf)))
Base@selpar_ini<-t(array(c(-5,0,-1),c(3,Base@nf)))
Base@selpar_ini[match('LL',Base@Fleets$name),]<- c(1,-1,99)# LL  - logistic - mode and sd are 0.26
Base@lnF_ini<-rep(log(0.001),nrow(Base@Cobs))
Base@lnRD_ini<-t(array(seq(-0.1,0.1,length.out=Base@ny),c(Base@ny,Base@np)))
Base@mov_ini<-tomt(array(1/Base@nr,c(Base@np,Base@ns,Base@na,Base@nr,Base@nr)))
Base@qCPUE_ini<-rep(1,Base@nCPUEq)
Base@qI_ini<-rep(1,Base@nI)
Base@D_ini<-c(sum(Base@RAI[,2,Base@canspawn[,1]==1][(Base@ny-2):Base@ny])/sum(Base@RAI[,2,Base@canspawn[,1]==1][1:3]),sum(Base@RAI[,2,Base@canspawn[,2]==1][(Base@ny-2):Base@ny])/sum(Base@RAI[,2,Base@canspawn[,2]==1][1:3]))# just for comparison with simulations
Base@complexRD<-as.integer(0)
Base@complexF<-as.integer(0)
Base@nF<-as.integer(1)
Base@MPind<-read.csv("Data/Processed/MP Indices/MP indices compiled.csv")
Base@nMPind<-nrow(Base@MPind)
Base@debug<-as.integer(0)
Base@verbose<-as.integer(1)
Base@datacheck<-as.integer(99999)


# Naming the Base operating model according to the various OM factors at level 1 -------

Base@Name<-"For building test unit"#paste(c("Base OM:",Names_Base),collapse=" ")
Base@OMfactors<-list("For sim","testing the", "M3 model")


# Save the base OMI  ------

OMI<-Base
OMI<-MatM_Ref(OMI,1)
M3write(OMI,OMdir=paste0(getwd(),"/M3"))  # Store this base operating model in the M3 operating model (precalculation of initial values if desired)
M3write(OMI,OMdir="C:/M3")  # Store this base operating model in the M3 operating model (precalculation of initial values if desired)


save(OMI,file=paste(getwd(),"/Objects/OMs/Test_OM",sep=""))
save(OMI,file=paste(getwd(),"/M3/OMI",sep=""))


# ==== END =======================================================================================



