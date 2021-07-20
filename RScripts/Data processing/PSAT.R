# PSAT.r
# Apr 2021
# R Script for formatting PSAT data

# --- Matt Lauretta --- GBYP - DFO - NOAA - WWF - Unimar - FCP - CB - IEO - AZTI - UCA
dat<-read.csv(paste(getwd(),"/Data/Other_2021_1/BFT_etags_12FEB2019.csv",sep=""))


# Exceptions ---------
# Matt Lauretta erroneous fish
cond<-dat$Tag_ID=="519703100" & dat$Stock_Area=="GOM"
dat$Stock_Area[cond]<-"W_ATL"
cond<-dat$Tag_ID=="2010_55308" & dat$Stock_Area=="GOM"
dat$Stock_Area[cond]<-"CAR"


# Print meta data
metadata<-F
if(metadata){
  datID<-dat[match(unique(dat$Tag_ID),dat$Tag_ID),]
  ntagy<-aggregate(rep(1,nrow(datID)),list(datID$Investigator),sum)
  sdate<-as.numeric(sapply(1:nrow(datID),function(x)strsplit(as.character(datID$Start_Date[x]),"/")[[1]][3]))
  edate<-as.numeric(sapply(1:nrow(datID),function(x)strsplit(as.character(datID$End_Date[x]),"/")[[1]][3]))
  start<-aggregate(sdate,list(datID$Investigator),min)
  end<-aggregate(edate,list(datID$Investigator),max)
  whatareas<-rep(NA,nrow(ntagy))
  for(i in 1:nrow(ntagy))whatareas[i]<-paste(unique(dat$Stock_Area[dat$Investigator==ntagy[i,1]]),collapse=" ")
  cbind(ntagy,start$x,end$x,whatareas)

}

# This code assigns the correct areas and processes tag tracks into single seasonal transitions
# set up cluster for parallel computing (now 20x faster!)
sfInit(cpus=detectCores(),parallel=T)
days<-lubridate::days
sfExport("days")

datstr<-dat
dat<-dat[dat$Days_Area>0,]

org<-c("CAR", "E_ATL","E_MED",  "GOM","GSL","NC_ATL", "NE_ATL", "SC_ATL", "SE_ATL", "W_ATL",  "W_MED", "S_ATL", "N_ATL", "MED")
nu<-c(   2,     6,      7,       1,    3,     5,       5,        4,         4,      2,        7,         4,     5,        7)#
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

dat0<-subset(dat,!(is.na(dat$ageclass)|is.na(dat$Start_Date)|is.na(dat$End_Date)))

tagexpand<-function(r,dat0){ # This function just expands a record (within area) into a daily set of records
  nd<-as.integer((dat0$End_Date[r]-dat0$Start_Date[r])+1)
  Date<-dat0$Start_Date[r]+days(0:(nd-1))
  Year<-as.numeric(format(Date,"%Y"))
  Quarter<-ceiling(as.numeric(format(Date,"%m"))/3)
  TagID<-rep(as.character(dat0$Tag_ID[r]),nd)
  Area<-rep(dat0$Stock_Area[r],nd)
  AgeC<-rep(dat0$ageclass[r],nd)
  out<-data.frame(TagID=TagID,Area=Area,Date=Date,Year=Year,Quarter=Quarter,AgeC=AgeC,stringsAsFactors = F)
}

temp_L<-sfLapply(1:nrow(dat0),tagexpand,dat0=dat0)
All<-bind_rows(temp_L,.id = "column_label")

Areas<-c(match("MED",Base@areanams),match("GOM",Base@areanams))
Natal<-cbind(All$TagID,match(All$Area,Areas))
Natal<-Natal[!is.na(Natal[,2]),]
NatalIDs<-data.frame(ID=Natal[,1],Stock=as.numeric(as.character(Natal[,2])))
NatalIDs<-aggregate(NatalIDs[,2],by=list(NatalIDs[,1]),max)
names(NatalIDs)<-c("TagID","Stock")

mostfreq<-function(x){ # just calculates the most frequent value in a vector (the area with the most days)
  freq<-aggregate(rep(1,length(x)),by=list(x),sum)
  if(nrow(freq)>0){
    return(freq[which.max(freq$x),1])
  }else{
    return(NA)
  }
}

#test<-All[All$Area==1,]
#unique(test$TagID)

# As per M3 format population, subyear, time duration (quarters) til capture, from area, to area, N
stk<-array(rep(1:Base@np,each=Base@nr),c(Base@nr,Base@np))
ar<-array(rep(1:Base@nr,Base@np),c(Base@nr,Base@np))
ExSpawn<-cbind(c(1,2),c(7,1)) # Exclusive spawning areas (for Stock ID of tags) each row is a stock, second column is the exclusive natal area

TagIDs<-unique(All$TagID)
nTags<-length(TagIDs)
Tracks<-NULL
Tracks2<-NULL

defosEAST<-unique(dat$Tag_ID[dat$Stock_Area==6 & dat$Investigator=="AZTI"]) # Dr Haritz Arrizabalaga stipulated 'certain Eastern fish'

simp_tracks<-function(i,All,TagIDs,byyear=F,defosEAST,NatalIDs,StYear=1965){ # simplify tracks to quarterly transitions
  #for(i in 1:nTags){
  #i<-658
  outtrack=NULL
  dat1<-subset(All,All$TagID==TagIDs[i])
  Trk<-aggregate(dat1$Area,by=list(dat1$Year,dat1$Quarter,dat1$AgeC),mostfreq)
  names(Trk)<-c("Year","Quarter","AgeClass","Area")
  timetemp<-as.numeric(paste(Trk$Year,Trk$Quarter,sep=""))
  Trk<-Trk[order(timetemp),]
  nT<-nrow(Trk)
  if(nT>1){
    pop<-NatalIDs[match(TagIDs[i],NatalIDs[,1]),2]
    if(is.na(pop))if(TagIDs[i]%in%defosEAST)pop<-1 # if EATL and AZTI (Haritz' stipulation)
    for(j in 2:nT){  # loop over tracks by quarter
      if(j==2){
        if(!byyear)temptrack<-c(pop,Trk$AgeClass[j-1],Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j],i)
        if(byyear)temptrack<-c(pop,Trk$AgeClass[j-1],Trk$Year[j-1]-StYear+1,Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j],i) # by year
      }else{
        if(!byyear)temptrack<-rbind(temptrack,c(pop,Trk$AgeClass[j-1],Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j],i))
        if(byyear)temptrack<-rbind(temptrack,c(pop,Trk$AgeClass[j-1],Trk$Year[j-1]-StYear+1,Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j],i)) # by year
      }
    }

    outtrack<-as.data.frame(matrix(temptrack,nrow=nT-1))

    if(!byyear)names(outtrack)<-c("p","a","s","t","fr","tr","tagno")
    if(byyear)names(outtrack)<-c("p","a","y","s","t","fr","tr","tagno")

  }else{
    if(!byyear)outtrack<-data.frame(p=numeric(),a=numeric(),s=numeric(),t=numeric(),fr=numeric(),tr=numeric(),tagno=numeric())
    if(!byyear)outtrack<-data.frame(p=numeric(),a=numeric(),y=numeric(),s=numeric(),t=numeric(),fr=numeric(),tr=numeric(),tagno=numeric())
  }
  outtrack

}

sfExport(list=c("mostfreq"))
Track_L<-sfLapply(1:nTags,simp_tracks,All=All,TagIDs=TagIDs,byyear=T,defosEAST=defosEAST,NatalIDs=NatalIDs,StYear=Base@years[1])
Tracks2<-bind_rows(Track_L)
Tracks<-Tracks2[,c(1,2,4,5,6,7,8)]

if(Impute){ # This does the movement pattern matching of Carruthers 2017 to assign stock of origin to fish not of a natal area
  source("Rscripts/Data processing/impSOO.r")
}

# remove tags of uncertain stock of origin (do not enter a natal area)
anyna<-function(x)sum(is.na(x))==0
complete<-apply(Tracks,1,anyna)
print(paste(round(sum(complete)/nrow(Tracks)*100,1),"% of tags are of known stock of origin. Removing ",nrow(Tracks)-sum(complete)," tracks."))
Tracks<-Tracks[complete,1:6] # remove any line with unknown stock, subyear, or area

# Aggregate tracks into total numbers of tags for any unique transition
PSAT<-aggregate(rep(1,nrow(Tracks)),by=list(Tracks$p,Tracks$a,Tracks$s,Tracks$t,Tracks$fr,Tracks$tr),sum)
names(PSAT)<-c("p","a","s","t","fr","tr","N")
sumfrom<-aggregate(PSAT$N,by=list(PSAT$p,PSAT$a,PSAT$s,PSAT$fr),sum)
totmov<-array(NA,c(Base@np,Base@nma,Base@ns,Base@nr))
totmov[as.matrix(sumfrom[,1:4])]<-sumfrom[,5]
Nfr<-totmov[as.matrix(PSAT[,c(1,2,3,5)])]
FR<-PSAT$N/Nfr
PSAT<-cbind(PSAT,FR)
PSAT$s<-c(2,3,4,1)[PSAT$s] # transcode to season moving into area




# =============================================================================================================================


fortrialspec=F

if(fortrialspec){
  Base@nma<-as.integer(3)
  PSATagg<-aggregate(PSAT$N,by=list(PSAT$p,PSAT$s,PSAT$fr,PSAT$tr),sum)
  tmov<-array("",c(Base@np,Base@ns,Base@nr,Base@nr))
  tmov[as.matrix(PSATagg[,1:4])]<-PSATagg[,5]
  #print(paste0("Do the number of tags match up: ",sum(as.numeric(tmov),na.rm=T)==sum(PSAT$N)))

  restab<-array("",c(Base@ns*Base@nr+Base@ns-1,Base@nr*Base@np+1))
  for(pp in 1:Base@np){
    for(ss in 1:Base@ns){
      for(rr in 1:Base@nr){
        restab[(Base@nr+1)*(ss-1)+rr,(Base@nr+1)*(pp-1)+(1:Base@nr)]<-tmov[pp,ss,rr,]
      }
    }
  }
  #print(paste0("Do the number of tags match up: ",sum(as.numeric(restab),na.rm=T)==sum(PSAT$N)))

  restab<-cbind(c(Base@areanams,"",Base@areanams,"",Base@areanams,"",Base@areanams),restab)
  restab<-as.data.frame(restab)
  names(restab)<-c("",Base@areanams,"",Base@areanams)
  write.csv(restab,file="Data/Raw/PSAT/PSATtableFeb2019.csv")

  for(a in 1:Base@nma){

    cond<-PSAT$a==a
    PSATagg<-aggregate(PSAT$N[cond],by=list(PSAT$p[cond],PSAT$s[cond],PSAT$fr[cond],PSAT$tr[cond]),sum)
    tmov<-array("",c(Base@np,Base@ns,Base@nr,Base@nr))
    tmov[as.matrix(PSATagg[,1:4])]<-PSATagg[,5]
    #print(paste0("Do the number of tags match up: ",sum(as.numeric(tmov),na.rm=T)==sum(PSAT$N)))

    restab<-array("",c(Base@ns*Base@nr+Base@ns-1,Base@nr*Base@np+1))
    for(pp in 1:Base@np){
      for(ss in 1:Base@ns){
        restab[(Base@nr+1)*(ss-1)+(1:Base@nr),(Base@nr+1)*(pp-1)+(1:Base@nr)]<-tmov[pp,ss,,]
      }
    }
    #print(paste0("Do the number of tags match up: ",sum(as.numeric(restab),na.rm=T)==sum(PSAT$N)))

    restab<-cbind(c(Base@areanams,"",Base@areanams,"",Base@areanams,"",Base@areanams),restab)
    restab<-as.data.frame(restab)
    names(restab)<-c("",Base@areanams,"",Base@areanams)
    write.csv(restab,file=paste0("Data/Raw/PSAT/PSATtableFeb2019_",a,".csv"))

  }

}

Tracks2<-Tracks2[is.na(Tracks2$p),] # remove any line with unknown stock, subyear, or area
Tracks2<-Tracks2[,1:7] # remove tagno

# PSAT2 are stocks of unknown origin. Technically these can be estimated by the model according to the expected movement probability of fish of both stocks of origin
#  ... ie if the model predicts are equal proportions E-W in a strata it can match the expected movement proportions against these observed movement proportions
PSAT2<-aggregate(rep(1,nrow(Tracks2)),by=list(Tracks2$a,Tracks2$y,Tracks2$s,Tracks2$t,Tracks2$fr,Tracks2$tr),sum)
names(PSAT2)<-c("a","y","s","t","fr","tr","N")
PSAT2<-subset(PSAT2,PSAT2$y<(Base@years[2]-Base@years[1]+2))
sumfrom<-aggregate(PSAT2$N,by=list(PSAT2$a,PSAT2$y,PSAT2$s,PSAT2$fr),sum)
totmov<-array(NA,c(Base@nma,Base@ny,Base@ns,Base@nr))
totmov[as.matrix(sumfrom[,1:4])]<-sumfrom[,5]
FR<-PSAT2$N/totmov[as.matrix(PSAT2[,c(1,2,3,5)])]
PSAT2<-cbind(PSAT2,FR)
PSAT2$s<-c(2,3,4,1)[PSAT2$s] # transcode to season moving into area


save(PSAT,file=paste(getwd(),"/Data/Processed/Conditioning/PSAT",sep=""))
save(PSAT2,file=paste(getwd(),"/Data/Processed/Conditioning/PSAT2",sep=""))

# --
