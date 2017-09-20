# PSAT.r
# August 2016
# R Script for formatting PSAT data

# --- Matt Lauretta --- GBYP - DFO - NOAA - WWF - Unimar - FCP - CB - IEO - AZTI - UCA

dat<-read.csv(paste(getwd(),"/Data/Raw/PSAT/BFT_etags_09292016.csv",sep="")) #
#dat<-read.csv(paste(getwd(),"/Data/Raw/PSAT/BFT_etags_15APR2017.csv",sep=""))
#dat<-AssignAge(dat,Base) # assign approximate ages bases on cohort slicing from eastern growth (needs only to get at rough age groups)

dat<-dat[dat$Days_Area>0,]


#AllPSATo<-read.csv(paste(getwd(),"/Data/Raw/PSAT/ALL_BFT_ElectronicTags_01122015.csv",sep=""))

expandtrack<-function(dat){

  datstr<-dat
  org<-c("CAR", "E_ATL","E_MED",  "GOM","GSL","NC_ATL", "NE_ATL", "SC_ATL", "SE_ATL", "W_ATL",  "W_MED")
  nu<-c(   2,     8,      10,       1,    4,     6,        7,        5,         9,      3,        10)#
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
ExSpawn<-cbind(c(1,2,2),c(10,1,4))

TagIDs<-unique(All$TagID)
nTags<-length(TagIDs)
Tracks<-rep(0,7)

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
        temptrack<-c(pop,Trk$AgeClass[j-1],Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j],i)
      }else{
        temptrack<-rbind(temptrack,c(pop,Trk$AgeClass[j-1],Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j],i))
      }
    }
    Tracks<-rbind(Tracks,temptrack)
  }
}

Tracks<-data.frame(Tracks[2:nrow(Tracks),])
names(Tracks)<-c("p","a","s","t","fr","tr","tagno")

if(Impute){
  source("Rscripts/Data processing/impSOO.r")
}

anyna<-function(x)sum(is.na(x))==0
Tracks<-Tracks[apply(Tracks,1,anyna),] # remove any line with unknown stock, subyear, or area
Tracks<-Tracks[,1:6] # remove tagno



PSAT<-aggregate(rep(1,nrow(Tracks)),by=list(Tracks$p,Tracks$a,Tracks$s,Tracks$t,Tracks$fr,Tracks$tr),sum)
names(PSAT)<-c("p","a","s","t","fr","tr","N")

fortrialspec=F
if(fortrialspec){
  tmov<-array(NA,c(Base@np,Base@ns,Base@nr,Base@nr))
  tmov[as.matrix(PSAT[,c(1,3,5,6)])]<-PSAT[,7]
  restab<-array("",c(Base@ns*Base@nr+Base@ns-1,Base@nr*Base@np+1))
  for(pp in 1:Base@np){
    for(ss in 1:Base@ns){
      restab[(Base@nr+1)*(ss-1)+(1:Base@nr),(Base@nr+1)*(pp-1)+(1:Base@nr)]<-tmov[pp,ss,,]
     }
  }
  restab<-cbind(c(Base@areanams,"",Base@areanams,"",Base@areanams,"",Base@areanams),restab)
  restab<-as.data.frame(restab)
  names(restab)<-c("",Base@areanams,"",Base@areanams)
  write.csv(restab,file="C:/abft-mse/Data/Raw/PSAT/PSATtableSept2017.csv")

}


PSAT2<- as.integer(PSAT[PSAT$N==max(PSAT$N),]) # something uncontroversial good chance of eastern residency (western med) for a most likely eastern stock
SOOwt<-rep(0.01/(Base@np-1),Base@np)
SOOwt[PSAT2[1]]<-0.99
PSAT2<-c(PSAT2[2:6],SOOwt)
PSAT2<-matrix(PSAT2,nrow=1)
PSAT<-as.matrix(PSAT)

save(PSAT,file=paste(getwd(),"/Data/Processed/Conditioning/PSAT",sep=""))
save(PSAT2,file=paste(getwd(),"/Data/Processed/Conditioning/PSAT2",sep=""))



# --
