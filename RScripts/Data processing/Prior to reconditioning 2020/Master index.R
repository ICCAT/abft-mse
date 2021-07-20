# Master index.r
# August 2016
# R Script for deriving Master indices for MSE testing
ignore<-F
if(ignore){ # the index based MI
# --- Controls ------------------------------------------------------------
# Area size calculations according to total number of fish observed per strata
#              c("GOM","WATL","GSL","SATL","NATL","EATL","MED")
numbersbyarea<-c(5,    10,    1,    10,    1,     1,     1)  #rep(1,7)  # Threshold for including in 1x1 deg regional size calculations
nfish <-5   # minimum number of fish per strata to quality for a CPUE observation

addCPUE <-TRUE # Add the assessment cpue indices
addCPUEwt<-10    # multiplier to all CPUE indices
CPUEup<-1       # additional upweight for certain CPUE indices

addI<-TRUE     # Add the assessment FI indices
addIwt<-10      # multipler to all FI indices
Iup<-20          # additional upweight for certain FI indices

varmult<-2    # outlier removal within time series
smoothpar<-0.8# 'spar' from the smoothspline funciton
Perc_inc<-3   # % annual increase in catchability for nominal cpue series
# Create a pseudo area for interaction with time PArea
#         c("GOM","WATL","GSL","SATL","NATL","EATL","MED")
Parea_conv<-c(1,    2,    2,     3,    3,     3,     3)
fillNAs<-FALSE # fill missing strata with a small number?
indset<-1       # set of task 2 cpue data

if(indset==1){
  #FleetID<-c("012JP00","025US00","004CA00","004CA00","016MA00","021ES00"#,"021ES00") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
  FleetID<-c("012",    "025",      "004",    "004",     "016",    "021",    "021", "021",    "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
  Gear<-c(   "LL",     "LL",       "RR",     "LL",      "TP",     "TP",     "HL",  "LL",     "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
  type<-c(   'nr',     'nr',       'kg',     "kg",      "kg",     "kg",     "kg",  "nr",     "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit
  Fnam<-c("JP LL",     "USA LL", "CA RR",  "CA LL",  "MA TP",  "ES TP",  "ES HL","ES LL", "ES BB")   # Fleet names for graphing
}

if(indset==2){
  FleetID<-c("012",          "004",     "016",    "021",    "021",    "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
  Gear<-c(   "LL",           "RR",      "TP",     "TP",     "HL",       "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
  type<-c(   'nr',            'kg',      "kg",     "kg",     "kg",      "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit
  Fnam<-c("JP LL",         "CA RR",    "MA TP",  "ES TP",    "ES HL", "ES BB")   # Fleet names for graphing
}

if(indset==3){
  FleetID<-c("012",    "025",      "004",    "004",     "016",        "021", "021",    "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
  Gear<-c(   "LL",     "LL",       "RR",     "LL",      "TP",          "HL",  "LL",     "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
  type<-c(   'nr',     'nr',       'kg',     "kg",      "kg",          "kg",  "nr",     "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit
  Fnam<-c("JP LL",     "USA LL", "CA RR",  "CA LL",  "MA TP",    "ES HL","ES LL", "ES BB")   # Fleet names for graphing
}

if(indset==4){
  FleetID<-c("012",    "025",              "016",        "021",     "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
  Gear<-c(   "LL",     "LL",                 "TP",          "HL",       "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
  type<-c(   'nr',     'nr',                 "kg",          "kg",       "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit
  Fnam<-c("JP LL",     "USA LL",     "MA TP",    "ES HL", "ES BB")   # Fleet names for graphing
}

if(indset==5){
  FleetID<-c("012",     "025",     "004",     "016",    "021",    "021",    "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
  Gear<-c(   "LL",       "LL",    "RR",      "TP",     "TP",     "HL",       "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
  type<-c(   'nr',       "nr",    'kg',      "kg",     "kg",     "kg",      "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit
  Fnam<-c("JP LL",       "USA LL", "CA RR",   "MA TP",  "ES TP",  "ES HL", "ES BB")   # Fleet names for graphing
}

if(indset==6){
  FleetID<-c("012",    "025",      "004",            "021"   ) # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
  Gear<-c(   "LL",     "LL",       "RR",            "HL"    )          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
  type<-c(   'nr',     'nr',       'kg',           "kg"    )                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit
  Fnam<-c("JP LL",     "USA LL", "CA RR",          "ES HL" )   # Fleet names for graphing
}



# end of controls ------------------------------------------------------
#T2cpue<-read.csv("Data/Raw/Task2/T2_2017.csv")#read.csv("Data/Raw/Task2/T2_errcheck.csv")
#T2cpue2<-read.csv("Data/Raw/Task2/T2_2018.csv")#read.csv("Data/Raw/Task2/T2_errcheck.csv")
#T2cpue<-rbind(T2cpue,T2cpue2[T2cpue2$YearC==2016,])
T2cpue<-read.csv("Data/Raw/Task2/T2_Nov072018_BFT.csv")#read.csv("Data/Raw/Task2/T2_errcheck.csv")


isnona<-function(x)sum(is.na(x))==0
T2cpue<-subset(T2cpue,apply(T2cpue,1,isnona))

T2cpue$FleetID<-substring(T2cpue$FleetID,1,3)

# Calculate habitat area sizes by region and quarter

T2cpue<-assign_quarter(T2cpue) # Add quarter
T2cpue<-ICCATtoGEO(T2cpue)
T2cpue<-assign_area(T2cpue,Base@area_defs)

# Red face filters:
# 1 (Craig Brown) essentially no bluefin in the GOM in quarters 3 or 4
T2cpue<-T2cpue[!(T2cpue$FleetID=="025"&T2cpue$GearGrpCode=="LL"&T2cpue$Area==1&(T2cpue$Subyear==3|T2cpue$Subyear==4)),]
# 2 JP LL starting in the GOM after 1970
#T2cpue<-T2cpue[!(T2cpue$FleetID=="012"&T2cpue$GearGrpCode=="LL"&T2cpue$Area==1),]


#dat<-T2cpue[T2cpue$FleetID=="025",]
checkrange<-function(dat){

  agg_y_r<-aggregate(rep(1,nrow(dat)),by=list(dat$Year-min(dat$Year)+1,dat$Area),sum)
  str<-array(0,c(max(agg_y_r[,1]),max(agg_y_r[,2])))
  str[as.matrix(agg_y_r[,1:2])]<-agg_y_r[,3]
  str

}
#checkrange(T2cpue)

#T2cpue<-subset(T2cpue,T2cpue$Area==10)
test0<-aggregate(T2cpue$BFT,by=list(T2cpue$FleetID,T2cpue$GearGrpCode,T2cpue$CatchUnit),sum)
cond<-test0$Group.3=="nr"
test0$x[cond]<-test0$x[cond]*70 # a daft guess at mean weight just to prioritize fleets by catch weight
test0[order(test0$x,decreasing=T),]

testSpat<-aggregate(T2cpue$BFT,by=list(T2cpue$FleetID,T2cpue$GearGrpCode,T2cpue$Area),sum)
testSpat<-testSpat[testSpat[,3]==10,]
testSpat[order(testSpat$x,decreasing=T),]

test<-aggregate(T2cpue$BFT,by=list(T2cpue$FleetID,T2cpue$GearGrpCode,T2cpue$CatchUnit,T2cpue$Area),sum)
test[order(test$x,decreasing=T),]
test2<-test[test[,4]==1,]
test2[order(test2$x,decreasing=T),]

by1<-T2cpue[T2cpue$SquareTypeCode=="1x1",]
by1<-assign_area(by1,Base@area_defs)
by1agg<-aggregate(by1$BFT,by=list(by1$Area,by1$Subyear,by1$Lat,by1$Lon),sum)

by5<-T2cpue[T2cpue$SquareTypeCode=="5x5",]
by5<-assign_area(by5,Base@area_defs)
by5agg<-aggregate(by5$BFT,by=list(by5$Area,by5$Subyear,by5$Lat,by5$Lon),sum)

names(by5agg)<-names(by1agg)<-c("Area","Subyear","Lat","Lon","C")

AS3tmp<-aggregate(rep(1,nrow(by5agg)),by=list(by5agg$Area,by5agg$Subyear),sum)
AS3<-array(NA,c(length(Base@areas),Base@ns))
AS3[as.matrix(AS3tmp[,1:2])]<-AS3tmp$x
refmin<-array(apply(AS3,1,min,na.rm=T),dim(AS3))
AS3[is.na(AS3)]<-refmin[is.na(AS3)]

names(by1agg)<-names(by5agg)<-c("Area","Subyear","Lat","Lon","C")
by1agg<-by1agg[by1agg$C>=numbersbyarea[by1agg$Area],]
by5agg<-by5agg[by5agg$C>=numbersbyarea[by5agg$Area],]
AS<-aggregate(rep(1,nrow(by1agg)),by=list(by1agg$Area),sum)
AS<-AS$x
AS<-AS/mean(AS)

AS2tmp<-aggregate(rep(1,nrow(by1agg)),by=list(by1agg$Area,by1agg$Subyear),sum,na.rm=T)
AS2<-array(NA,c(length(Base@areas),Base@ns))
AS2[as.matrix(AS2tmp[,1:2])]<-AS2tmp$x
refmin<-array(apply(AS2,1,min,na.rm=T),dim(AS2))
AS2[is.na(AS2)]<-refmin[is.na(AS2)]

# === Define a list of Master indices for trial specifications ======================================

MI<-new('list')
MI[[1]]<-MI[[2]]<-MI[[3]]<-array(NA,c(Base@ny,Base@ns,Base@nr)) #
ploto<-T

# === First scenario set ===================

cpue<-new('list')

ploty=2
if(ploty==1)par(mfrow=c(length(FleetID),4),mai=c(0.25,0.2,0.05,0.05),omi=c(0.05,1,0.5,0.01))
if(ploty==2)par(mfrow=c(Base@nr,4),mai=c(0.35,0.3,0.01,0.01),omi=c(0.05,1,0.5,0.01))
xlimy<-c(-100,25)
ylimy<-c(-40,65)

prettyaxis<-function(lims){
  inc<-(lims[2]-lims[1])/5
  pretty(seq(lims[1]-inc,lims[2]+inc,length.out=12))
}

for(i in 1:length(FleetID)){

  temp<-T2cpue[T2cpue$FleetID==FleetID[i]&T2cpue$GearGrpCode==Gear[i]&T2cpue$Eff1>0&T2cpue$BFT>0&T2cpue$CatchUnit==type[i],]
  temp<-assign_year(temp,years)
  cond<-temp$BFT>nfish
  temp<-subset(temp,cond)
  print(paste(i,Fnam[i],mean(temp$Eff1)))

  eff<-aggregate(temp$Eff1,by=list(temp$Year,temp$Subyear,temp$Area),sum)
  cat<-aggregate(temp$BFT,by=list(temp$Year,temp$Subyear,temp$Area),sum)
  N<-  aggregate(rep(1,nrow(temp)),by=list(temp$Year,temp$Subyear,temp$Area),sum)
  CPE<-eff
  CPE$x<-(cat$x/eff$x)#*AS2[as.matrix(cat[,3:2])]
  keepT<-rep(FALSE,nrow(CPE))

  for(rr in 1:Base@nr){
    for(qq in 1:Base@ns){
      cond<-CPE[,3]==rr&CPE[,2]==qq
      cpuetemp<-CPE[cond,4]
      yrtemp<-CPE[cond,1]
      cols<-rep('black',length(cpuetemp))

      if(sum(cond)>0){
        if(length(cpuetemp)>3){ # standard deviations from mean (smoothed line)
          cpuesm<-exp(smooth.spline(log(cpuetemp),all.knots=T,spar=smoothpar)$y)
          #plot(yrtemp,cpuetemp,ylim=c(0,max(cpuetemp)),yaxs="i")
          #lines(yrtemp,cpuesm,col="red")
          res<-log(cpuetemp/cpuesm)
          keep<-res<varmult&res>(-varmult)
          oind<-rep(FALSE,nrow(CPE))
          oind[cond]<-keep
          cond2=cond & oind
          keepT[cond2]=TRUE
          cols[!keep]<-'red'

        }else{ # standard deviations from mean (short time series)
          var<-varmult*sd(log(cpuetemp))
          mu<-mean(log(cpuetemp))
          keep<-log(cpuetemp)<(mu+var)&log(cpuetemp)>(mu-var)
          cond2=cond & log(CPE[,4])<(mu+var)&log(CPE[,4])>(mu-var)
          keepT[cond2]=TRUE
          cols[!keep]<-'red'
        }
      }
      if(sum(cond)>0){
        plot(CPE[cond,1],CPE[cond,4],pch=19,col=cols,ylim=c(0,max(CPE[cond,4],na.rm=T)),yaxs="i")
        if(length(cpuetemp)>3)lines(yrtemp,cpuesm,col="blue")
      }else{
        plot(1, type="n", axes=F, xlab="", ylab="")
      }
      if(qq==1)mtext(Base@areanams[rr],2,las=2,line=2)
      if(rr==1&qq==1)legend('top',legend=Fnam[i],text.col='blue',text.font=2,cex=1.2,bty='n')

    }
  }

  crtemp<-CPE$x[keepT]
  crtemp<-crtemp/mean(crtemp)
  cpue[[i]]<-as.data.frame(cbind(eff[keepT,],cat$x[keepT],crtemp,N$x[keepT],rep(i,sum(keepT))))
  names(cpue[[i]])<-c("Year","Subyear","Area","E","C","CPUE","N","Fleet")

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
      }
      if(qq==1)mtext(Fnam[i],2,las=2,line=3)
      if(i==1)mtext(paste("Quarter",qq),3,line=1)
      if(i==(length(FleetID))){ axis(1,prettyaxis(xlimy),prettyaxis(xlimy))
      }else{axis(1,prettyaxis(xlimy))}
      if(qq==1){axis(2,prettyaxis(ylimy),prettyaxis(ylimy))
      }else{axis(2,prettyaxis(ylimy))}
    }
  }
}


CPUE<-cpue[[1]]
for(i in 2:length(cpue)){

  CPUE<-rbind(CPUE,cpue[[i]])
}

# add fishing efficiency changes to nominal CPUE
qinc<-(1-(Perc_inc/100))^(1:Base@ny)
CPUE$CPUE<-CPUE$CPUE*qinc[CPUE$Year]

if(ploty==2)par(mfrow=c(Base@nr,4),mai=c(0.35,0.3,0.01,0.01),omi=c(0.05,1,0.5,0.01))

if(addCPUE){

  # cpue indices
  CPUEind<-read.csv("Data/Processed/CPUE indices/CPUE_indices compiled_2018OM.csv") #read.csv("Data/Raw/Task2/T2_errcheck.csv")
  CPUEind<-CPUEind[CPUEind$Year>=Base@years[1]&CPUEind$Year<=Base@years[2],]#&CPUEind$Name!="US_RR_177"&CPUEind$Name!="US_RR_144",]
  fleets<-unique(CPUEind$qNo)

  mapping=aggregate(rep(1,nrow(CPUEind)),by=list(CPUEind$qNo,CPUEind$Name),sum)
  cpuenams<-        c("CAN GSL","CAN SWNS", "JPLL_GOM",    "JPN_LL_Eatl_Med", "JPN_LL_NEAtl1",   "JPN_LL_NEAtl2",   "JPN_LL_West1",    "JPN_LL_West2",  "MOR_POR_TRAP",    "MOR_SPN_TRAP",    "SPN_BB",      "SPN_FR_BB",       "US_GOM_PLL1",     "US_GOM_PLL2",   "US_RR_115_144",   "US_RR_145",   "US_RR_177",       "US_RR_195",       "US_RR_66_114")
  MIwts0<-MIwts<-addCPUEwt*c( 1,        1,          1,             CPUEup,            1,                CPUEup,            1,                 CPUEup,            1,                   1,              1,            1,                 1,                 1,                1,                 1,            1,                  1,                1)
  MIwts[mapping[,1]]<-MIwts0[match(cpuenams,mapping[,2])]
  CPUEind$qNo<-match(CPUEind$qNo,fleets)
  CPUEind$Year<-CPUEind$Year-Base@years[1]+1

  for(i in 1:length(fleets)){

    temp<-CPUEind[CPUEind$qNo==i,]
    temp<-temp[,c(1,2,3,6,6,6,6,4)]
    names(temp)<-c("Year","Subyear","Area","E","C","CPUE","N","Fleet")
    temp$N<-MIwts[i]
    temp$Fleet<-temp$Fleet+length(FleetID)

    fnam<-unique(CPUEind$Name)[i]
    cpuetemp<-temp$CPUE
    yrtemp<-temp$Year

    if(length(cpuetemp)>3){ # standard deviations from mean (smoothed line)
      cpuesm<-exp(smooth.spline(log(cpuetemp),all.knots=T,spar=smoothpar)$y)
      #plot(yrtemp,cpuetemp,ylim=c(0,max(cpuetemp)),yaxs="i")
      #lines(yrtemp,cpuesm,col="red")
      res<-log(cpuetemp/cpuesm)
      keep<-res<varmult&res>(-varmult)

    }else{ # standard deviations from mean (short time series)

      var<-varmult*sd(log(temp$CPUE))
      mu<-mean(log(temp$CPUE))
      keep<-log(temp$CPUE)<(mu+var)&log(temp$CPUE)>(mu-var)

    }


    #CPUE<-rbind(CPUE,temp)
    cols<-rep("black",length(cpuetemp))
    cols[!keep]<-"red"
    plot(yrtemp,cpuetemp,pch=19,col=cols,ylim=c(0,max(cpuetemp,na.rm=T)))
    if(length(cpuetemp)>3)lines(yrtemp,cpuesm,col="blue")
    legend('topleft',legend=paste(unique(temp$Fleet),fnam,MIwts[i],sep="-"),bty='n')

    temp[keep,6]<-temp[keep,6]/mean(temp[keep,6])
    CPUE<-rbind(CPUE,temp[keep,])

  }

}

if(addI){

   # fishery independent indices

  CPUEind<-CPUEind0<-read.csv(paste(getwd(),"/Data/Raw/SSB/FI_indices_compiled_OM2018.csv",sep=""))
  fnams<-unique(CPUEind$Name)
  mapping=aggregate(rep(1,nrow(CPUEind)),by=list(CPUEind$Ino,CPUEind$Name),sum)
  cond<-CPUEind$Year>=Base@years[1]&CPUEind$Year<=Base@years[2]#&CPUEind$Name!="CAN_ACO_SUV"&CPUEind$Name!="MED_LAR_SUV"
  CPUEind<-CPUEind[cond,c(1,2,3,5,5,7,8)]

  fleets<-unique(CPUEind$Ino)
  FInams<-        c("CAN_ACO_SUV",   "FR_AER_SUV1",   "FR_AER_SUV2",     "GBYP_AER_SUV",  "GOM_LAR_SUV",     "MED_LAR_SUV")
  MIwts0<-addIwt*  c(Iup,              1,               Iup,               1,               Iup,               Iup)
  MIwts[mapping[,1]]<-MIwts0[match(FInams,mapping[,2])]

  CPUEind$Ino<-match(CPUEind$Ino,fleets)
  CPUEind$Year<-CPUEind$Year-Base@years[1]+1
  prevmaxfleet<-max(as.numeric(CPUE$Fleet))

  for(i in 1:length(fleets)){

    temp<-CPUEind[CPUEind$Ino==i,]
    temp<-temp[,c(1,2,3,6,6,6,6,4)]
    names(temp)<-c("Year","Subyear","Area","E","C","CPUE","N","Fleet")
    temp$N<-MIwts[i]
    temp$Fleet<-temp$Fleet+prevmaxfleet
    fnam<-unique(fnams)[i]

    cpuetemp<-temp$CPUE
    yrtemp<-temp$Year

    if(length(cpuetemp)>3){ # standard deviations from mean (smoothed line)
      cpuesm<-exp(smooth.spline(log(cpuetemp),all.knots=T,spar=smoothpar)$y)
      #plot(yrtemp,cpuetemp,ylim=c(0,max(cpuetemp)),yaxs="i")
      #lines(yrtemp,cpuesm,col="red")
      res<-log(cpuetemp/cpuesm)
      keep<-res<varmult&res>(-varmult)

    }else{ # standard deviations from mean (short time series)

      var<-varmult*sd(log(temp$CPUE))
      mu<-mean(log(temp$CPUE))
      keep<-log(temp$CPUE)<(mu+var)&log(temp$CPUE)>(mu-var)

    }

    #CPUE<-rbind(CPUE,temp)
    cols<-rep("black",length(cpuetemp))
    cols[!keep]<-"red"
    plot(yrtemp,cpuetemp,pch=19,col=cols,ylim=c(0,max(cpuetemp,na.rm=T)))
    if(length(cpuetemp)>3)lines(yrtemp,cpuesm,col="blue")
    legend('topleft',legend=paste(unique(temp$Fleet),fnam,MIwts[i],sep="-"),bty='n')

    temp[keep,6]<-temp[keep,6]/mean(temp[keep,6])
    CPUE<-rbind(CPUE,temp[keep,])

  }
}

if(fillNAs){
  ammend<-aggregate(CPUE$N,by=list(CPUE$Year,CPUE$Subyear,CPUE$Area),sum)
  code<-paste(ammend[,1],ammend[,2],ammend[,3],sep="-")

  allstrata<-expand.grid(1:max(CPUE$Year),1:max(CPUE$Subyear),1:max(CPUE$Area))
  allcode<-paste(allstrata[,1],allstrata[,2],allstrata[,3],sep="-")
  tofill<-!(allcode%in%code)
  nfill<-sum(tofill)
  temp<-cbind(allstrata[tofill,],rep(1,nfill),rep(0.00001,nfill),rep(0.0001,nfill),rep(1,nfill),rep(1,nfill))
  names(temp)<-c("Year","Subyear","Area","E","C","CPUE","N","Fleet")
  CPUE<-rbind(CPUE,temp)
}

YAint<-aggregate(rep(1,nrow(CPUE)),by=list(as.numeric(CPUE$Year),as.numeric(CPUE$Area)),sum)
YA<-array(NA,c(Base@ny,Base@nr))
YA[as.matrix(YAint[,1:2])]<-YAint[,3]

SAint<-aggregate(rep(1,nrow(CPUE)),by=list(as.numeric(CPUE$Subyear),as.numeric(CPUE$Area)),sum)
SA<-array(NA,c(Base@ns,Base@nr))
SA[as.matrix(SAint[,1:2])]<-SAint[,3]

Parea<-Parea_conv[CPUE$Area]
CPUE<-cbind(CPUE,Parea)

for(i in c(1,2,3,8,9))CPUE[,i]<-as.factor(as.character(CPUE[,i]))
wt<-log(as.numeric(as.character(CPUE$N)))

par(mfrow=c(8,6),mar=c(2,2,2,2))
for(i in 1:max(as.numeric(as.character(CPUE$Fleet))))plot(CPUE$CPUE[CPUE$Fleet==i],main=paste(i,CPUE$Area[CPUE$Fleet==i][1],"-"),type="l")
out<-glm(log(CPUE)~Year*Parea+Subyear*Area+Fleet,data=CPUE,weights=wt)

newdat<-expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr,1)
newdat<-cbind(newdat,Parea_conv[newdat[,3]])
names(newdat)<-c("Year","Subyear","Area","Fleet","Parea")
for(i in 1:ncol(newdat))newdat[,i]<-as.factor(as.character(newdat[,i]))
pred<-predict(out,newdat)

ind<-as.matrix(expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr))
MI[[1]][ind]<-exp(pred)*AS2[ind[,3:2]]  # AS = 1x1 just by area  AS2 = 1x1 by area and seson   AS3 = 5x5 by area and season

if(ploto)plotindex2(Base,pCPUE=MI[[1]]) # Figure 4

for(a in 1:Base@nr){for(q in 1:Base@ns){
  MI[[1]][,q,a]<-exp(smooth.spline(log(MI[[1]][,q,a]),all.knots=T,spar=0.6)$y)
  #MI[[2]][,q,a]<-smooth.spline(MI[[1]][,q,a],all.knots=T,spar=0.6)$y/(1.02^(0:(Base@ny-1)))
}}

# Exceptions and adjustments
#Few fish in the GOM in Quarter 3
GOM<-match('GOM',Base@areanams)
MI[[1]][,3,GOM]<- MI[[1]][,3,GOM]/100

#Few fish in the GSL in Quarter 1
GSL<-match('GSL',Base@areanams)
MI[[1]][,1,GSL]<- MI[[1]][,1,GSL]/100

#No fish in the SATL in Quarter 4
#SATL<-match('SATL',Base@areanams)
#MI[[1]][,4,SATL]<- MI[[1]][,4,SATL]/100

#No Western fish in the MED

# Exceptions and adjustments
MI[[1]][,,1]<- MI[[1]][,,1]/4
MI[[1]][,,2]<- MI[[1]][,,2]/3

if(ploto)plotindex2(Base,MI[[1]]) # Figure 5


for(y in 1:Base@ny){for(q in 2:Base@ns){
  frac<-MI[[1]][y,q,]/sum(MI[[1]][y,q,])
  MI[[1]][y,q,]<-frac*sum(MI[[1]][y,q-1,])
}}
}

ignore<-function(){

      # === Second scenario set (no ES LL data) ========================

      FleetID<-c("012JP00","025US00","004CA00","004CA00","016MA00","021ES00","021ES00","021ES00","021ES02") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
      Gear<-c(   "LL",     "LL",       "RR",     "LL",      "TP",     "TP",     "HL",  "LL",     "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
      type<-c(   'nr',     'nr',       'kg',     "kg",      "kg",     "kg",     "kg",  "nr",     "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit

      Fnam<-c("JP LL", "CA RR",   "MA TP",  "ES HL")   # Fleet names for graphing

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
      #c("GOM","CAR","WATL","GSL","SCATL","NCATL","NEATL","EATL","SEATL","MED")
      Parea_conv<-c(1,1,2,1,3,3,4,5,5,5)
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

      #newdat2<-newdat
      #newdat2$Fleet<-as.factor("1")
      #pred<-predict(out,newdat2)


      ind<-as.matrix(expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr))
      MI[[2]][ind]<-exp(pred)*AreaSize[ind[,3:2]]

      if(ploto)plotindex2(Base,MI[[2]]) # Figure 4


      for(a in 1:Base@nr){for(q in 1:Base@ns){
        MI[[2]][,q,a]<-smooth.spline(MI[[2]][,q,a],all.knots=T,spar=0.6)$y
      }}

      if(ploto)plotindex(Base,MI[[2]]) # Figure 5



      # === Third scenario set ===================



      FleetID<-c("012JP00","025US00","004CA00","004CA00","016MA00","021ES00","021ES00") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
      Gear<-c(   "LL",     "LL",       "RR",     "LL",      "TP",     "TP",     "HL")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
      type<-c(   'nr',     'nr',       'kg',     "kg",      "kg",     "kg",     "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit

      Fnam<-c("JP LL",    "USA LL",   "CA RR",  "CA LL",  "MA TP",  "ES TP",  "ES HL", "ES LL", "ES BB ")   # Fleet names for graphing

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
      Parea_conv<-c(1,1,2,1,3,4,3,5,5,5)
      Parea<-Parea_conv[CPUE$Area]
      CPUE<-cbind(CPUE,Parea)

      for(i in c(1,2,3,8,9))CPUE[,i]<-as.factor(as.character(CPUE[,i]))
      wt<-as.numeric(as.character(CPUE$N))
      #out<-glm(log(CPUE)~Year*Area*Subyear+Fleet,data=CPUE,weights=wt)
      out<-glm(log(CPUE)~Year*Area+Area*Subyear+Fleet,data=CPUE,weights=wt)


      newdat<-expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr,1)
      newdat<-cbind(newdat,Parea_conv[newdat[,3]])
      names(newdat)<-c("Year","Subyear","Area","Fleet","Parea")
      for(i in 1:ncol(newdat))newdat[,i]<-as.factor(as.character(newdat[,i]))
      pred<-predict(out,newdat)

      #newdat2<-newdat
      #newdat2$Fleet<-as.factor("1")
      #pred<-predict(out,newdat2)

      ind<-as.matrix(expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr))
      MI[[3]][ind]<-exp(pred)*AreaSize[ind[,3:2]]

      if(ploto)plotindex(Base,MI[[3]]) # Figure 4


      for(a in 1:Base@nr){for(q in 1:Base@ns){
        MI[[3]][,q,a]<-smooth.spline(MI[[3]][,q,a],all.knots=T,spar=0.4)$y
        #MI[[2]][,q,a]<-smooth.spline(MI[[1]][,q,a],all.knots=T,spar=0.6)$y/(1.02^(0:(Base@ny-1)))
      }}

      if(ploto)plotindex(Base,MI[[1]]) # Figure 5

      #jpeg("G:/temp/Index plots for SCRS paper.jpg",res=300,width=7,height=10,units='in')
      #plotMindex(Base,MI)
      #dev.off()

}

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

MI[[2]]<-tempMI

prettyaxis<-function(lims){
  inc<-(lims[2]-lims[1])/5
  pretty(seq(lims[1]-inc,lims[2]+inc,length.out=12))
}
ploto<-T
if(ploto)plotindex2(Base,MI[[2]]) #



# === Completely flat MI =============================

#MI[[3]]<-array(mean(MI[[1]]),dim(MI[[1]]))

#if(ploto)plotindex2(Base,MI[[3]]) #


save(MI,file=paste(getwd(),"/Data/Processed/Conditioning/MI",sep=""))



#





