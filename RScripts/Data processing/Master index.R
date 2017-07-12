# Master index.r
# August 2016
# R Script for deriving Master indices for MSE testing

T2cpue<-read.csv("Data/Raw/Task2/T2_2017.csv")#read.csv("Data/Raw/Task2/T2_errcheck.csv")
T2cpue$FleetID<-substring(T2cpue$FleetID,1,3)

# Calculate habitat area sizes by region and quarter

T2cpue<-assign_quarter(T2cpue) # Add quarter
T2cpue<-ICCATtoGEO(T2cpue)
T2cpue<-assign_area(T2cpue,Base@area_defs)

#T2cpue<-subset(T2cpue,!(T2cpue$FleetID=="012"&T2cpue$Area==10)) # Ignore Japanese data for the Med


#dat<-T2cpue[T2cpue$FleetID=="025",]
checkrange<-function(dat){
  agg_y_r<-aggregate(rep(1,nrow(dat)),by=list(dat$Year-min(dat$Year)+1,dat$Area),sum)

  str<-array(0,c(max(agg_y_r[,1]),max(agg_y_r[,2])))
  str[as.matrix(agg_y_r[,1:2])]<-agg_y_r[,3]
  str
}


#T2cpue<-subset(T2cpue,T2cpue$Area==10)
test0<-aggregate(T2cpue$BFT,by=list(T2cpue$FleetID,T2cpue$GearGrpCode,T2cpue$CatchUnit),sum)
cond<-test0$Group3=="nr"
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

names(by5agg)<-c("Area","Subyear","Lat","Lon","C")
#              c("GOM","CAR","WATL","GSL","SCATL","NCATL","NEATL","EATL","SEATL","MED")
#by1agg<-by5agg[by5agg$C,]

AS3tmp<-aggregate(rep(1,nrow(by5agg)),by=list(by5agg$Area,by5agg$Subyear),sum)
AS3<-array(NA,c(length(Base@areas),Base@ns))
AS3[as.matrix(AS3tmp[,1:2])]<-AS3tmp$x
refmin<-array(apply(AS3,1,min,na.rm=T),dim(AS3))
AS3[is.na(AS3)]<-refmin[is.na(AS3)]
#AS2[10,1]<-AS2[10,2]# to account for observation process




names(by1agg)<-c("Area","Subyear","Lat","Lon","C")
#              c("GOM","CAR","WATL","GSL","SCATL","NCATL","NEATL","EATL","SEATL","MED")
numbersbyarea<-c(  0,    1,    10,     0,     1,      1,     0,      0,      10,   1)
by1agg<-by1agg[by1agg$C>numbersbyarea[by1agg$Area],]

#AS<-aggregate(rep(1,nrow(by1agg)),by=list(by1agg$Area),sum)
#AreaSize<-AS$x/mean(AS$x)

AS<-aggregate(rep(1,nrow(by1agg)),by=list(by1agg$Area),sum)
AS<-AS$x
AS<-AS/mean(AS)

AS2tmp<-aggregate(rep(1,nrow(by1agg)),by=list(by1agg$Area,by1agg$Subyear),sum)
AS2<-array(NA,c(length(Base@areas),Base@ns))
AS2[as.matrix(AS2tmp[,1:2])]<-AS2tmp$x
refmin<-array(apply(AS2,1,min,na.rm=T),dim(AS2))
AS2[is.na(AS2)]<-refmin[is.na(AS2)]
AS2[10,1]<-AS2[10,2]# to account for observation process
AS2[1,]<-AS2[1,]*2 # Med adjustment
AS2[1,2]<-AS2[1,2]*2
AS2[7,]<-AS2[7,]*3

#AreaSize<-array(NA,c(Base@nr,Base@ns))
#AreaSize[as.matrix(AS[,1:2])]<-AS[,3]
#AreaSize<-AreaSize/mean(AreaSize,na.rm=T)
#for(i in 1:Base@nr)AreaSize[i,is.na(AreaSize[i,])]<-mean(AreaSize[i,],na.rm=T)


# === Define a list of Master indices for trial specifications ======================================

MI<-new('list')
MI[[1]]<-MI[[2]]<-MI[[3]]<-array(NA,c(Base@ny,Base@ns,Base@nr)) #

ploto<-T
# === First scenario set ===================


#FleetID<-c("012JP00","025US00","004CA00","004CA00","016MA00","021ES00"#,"021ES00") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
FleetID<-c("012",    "025",      "004",    "004",     "016",    "021",    "021", "021",    "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
Gear<-c(   "LL",     "LL",       "RR",     "LL",      "TP",     "TP",     "HL",  "LL",     "BB")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
type<-c(   'nr',     'nr',       'kg',     "kg",      "kg",     "kg",     "kg",  "nr",     "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit
Fnam<-c("JP LL",     "USA LL", "CA RR",  "CA LL",  "MA TP",  "ES TP",  "ES HL","ES LL", "ES BB")   # Fleet names for graphing

FleetID<-c("012",      "025",   "025",    "004",  "004", "021",    "016",      "021",  "023", "024",  "021") # Fleet code#FleetID<-c("012JP00","004CA00","005TW00","025US00","025US01","004CA00","021ES00","016MA00","021ES00","021ES00") # Fleet code
Gear<-c(   "LL",       "LL",    "RR",     "RR",   "LL",  "TP",     "TP",       "BB",   "PS",  "PS",    "PS")          # Gear group code#Gear<-c(   "LL",    "TL",      "LL",     "LL",     "LL",     "RR",     "LL",     "TP",     "TP",     "HL")          # Gear group code
type<-c(   'nr',       "nr",    "nr",     'kg',   "kg",  "kg",     "kg",       "kg",   "kg",  "kg",    "kg")                                       # Unit#type<-c(   'nr',    "kg",      "kg",     'nr',     "nr",     'kg',     "kg",     "kg",     "kg",     "kg")                                       # Unit

Fnam<-c("JP LL",      "US LL",  "US RR",  "CA RR", "CA LL", "ES TP",  "MA TP",  "ES BB","OTH PS","ES PS")   # Fleet names for graphing

cpue<-new('list')

ploty=2
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
  #temp<-T2cpue[T2cpue$FleetID==FleetID[i]&T2cpue$GearGrpCode==Gear[i],]#&T2cpue$BFT>0&T2cpue$CatchUnit==type[i],]
  #temp<-T2cpue[T2cpue$FleetID=="025"&T2cpue$GearGrpCode=="LL",]
  #temp<-assign_area(temp,Base@area_defs)
  #temp<-assign_quarter(temp)
  temp<-assign_year(temp,years)
  cond<-temp$Eff1>quantile(temp$Eff1,0.1)&temp$BFT>2
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

    CPE<-(cat$x/eff$x)*AS[cat[,2]]

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
  var<-2*sd(log(crtemp))
  mu<-mean(log(crtemp))
  cond<-log(crtemp)<(mu+var)&log(crtemp)>(mu-var)
  crtemp<-crtemp[cond]
  crtemp<-crtemp/mean(crtemp)
  cpue[[i]]<-as.data.frame(cbind(eff[cond,],cat$x[cond],crtemp,N$x[cond],rep(i,sum(cond))))
  names(cpue[[i]])<-c("Year","Subyear","Area","E","C","CPUE","N","Fleet")

}



CPUE<-cpue[[1]]
for(i in 2:length(cpue))CPUE<-rbind(CPUE,cpue[[i]])


# Add the assessment cpue indices
addMI<-TRUE

if(addMI){

  MIwt<-50
  CPUEind<-read.csv("Data/Processed/CPUE indices/CPUE indices compiled 2017 assessment.csv")#read.csv("Data/Raw/Task2/T2_errcheck.csv")

  CPUEind<-CPUEind[CPUEind$Year>=Base@years[1]&CPUEind$Year<=Base@years[2],]

  fleets<-unique(CPUEind$qNo)

  CPUEind$qNo<-match(CPUEind$qNo,fleets)
  CPUEind$Year<-CPUEind$Year-Base@years[1]+1


  for(i in 1:length(fleets)){

    temp<-CPUEind[CPUEind$qNo==i,]
    temp<-temp[,c(1,2,3,6,6,6,6,4)]
    names(temp)<-c("Year","Subyear","Area","E","C","CPUE","N","Fleet")
    temp$N<-MIwt
    temp$Fleet<-temp$Fleet+length(FleetID)
    CPUE<-rbind(CPUE,temp)
  }

}



YAint<-aggregate(rep(1,nrow(CPUE)),by=list(as.numeric(CPUE$Year),as.numeric(CPUE$Area)),sum)
YA<-array(NA,c(Base@ny,Base@nr))
YA[as.matrix(YAint[,1:2])]<-YAint[,3]

SAint<-aggregate(rep(1,nrow(CPUE)),by=list(as.numeric(CPUE$Subyear),as.numeric(CPUE$Area)),sum)
SA<-array(NA,c(Base@ns,Base@nr))
SA[as.matrix(SAint[,1:2])]<-SAint[,3]


# Create a pseudo area for interaction with time PArea
#         c("GOM","CAR","WATL","GSL","SCATL","NCATL","NEATL","EATL","SEATL","MED")
Parea_conv<-c(1,    2,    3,     1,    3,      3,      4,      4,     4,      5)
#Parea_conv<-c(1,1,2,1,3,4,3,5,5,5)
Parea<-Parea_conv[CPUE$Area]
CPUE<-cbind(CPUE,Parea)

for(i in c(1,2,3,8,9))CPUE[,i]<-as.factor(as.character(CPUE[,i]))
wt<-as.numeric(as.character(CPUE$N))
#out<-glm(log(CPUE)~Year*Area*Subyear+Fleet,data=CPUE,weights=wt)
out<-glm(log(CPUE)~Year*Parea+Subyear*Parea+Fleet,data=CPUE,weights=wt)


newdat<-expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr,1)
newdat<-cbind(newdat,Parea_conv[newdat[,3]])
names(newdat)<-c("Year","Subyear","Area","Fleet","Parea")
for(i in 1:ncol(newdat))newdat[,i]<-as.factor(as.character(newdat[,i]))
pred<-predict(out,newdat)

#newdat2<-newdat
#newdat2$Fleet<-as.factor("1")
#pred<-predict(out,newdat2)

ind<-as.matrix(expand.grid(1:(Base@years[2]-Base@years[1]+1),1:Base@ns,1:Base@nr))
#MI[[1]][ind]<-exp(pred)*AS[ind[,3]]#AreaSize[ind[,3:2]]
MI[[1]][ind]<-exp(pred)*AS2[ind[,3:2]]  # AS = 1x1 just by area  AS2 = 1x1 by area and seson   AS3 = 5x5 by area and season


if(ploto)plotindex2(Base,pCPUE=MI[[1]]) # Figure 4


for(a in 1:Base@nr){for(q in 1:Base@ns){
  MI[[1]][,q,a]<-exp(smooth.spline(log(MI[[1]][,q,a]),all.knots=T,spar=0.6)$y)
  #MI[[2]][,q,a]<-smooth.spline(MI[[1]][,q,a],all.knots=T,spar=0.6)$y/(1.02^(0:(Base@ny-1)))
}}


#for(i in 1:Base@ns)MI[[1]][,i,]<-MI[[1]][,i,]/mean(MI[[1]][,i,])

if(ploto)plotindex2(Base,MI[[1]]) # Figure 5


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
        MI[[3]][,q,a]<-smooth.spline(MI[[3]][,q,a],all.knots=T,spar=0.6)$y
        #MI[[2]][,q,a]<-smooth.spline(MI[[1]][,q,a],all.knots=T,spar=0.6)$y/(1.02^(0:(Base@ny-1)))
      }}

      if(ploto)plotindex(Base,MI[[1]]) # Figure 5

      #jpeg("G:/temp/Index plots for SCRS paper.jpg",res=300,width=7,height=10,units='in')
      #plotMindex(Base,MI)
      #dev.off()

}


save(MI,file=paste(getwd(),"/Data/Processed/Conditioning/MI",sep=""))



#





