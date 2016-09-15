
# ===================================================================================================================
# ==== ABT MSE data error checking  =================================================================================
# ===================================================================================================================

library(lubridate)


# --- Set working directory ------

setwd("G:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")

# --- Area definitions -----------

areas<-c("DP_GOM","DP_WATL","DP_GSL","DP_CATL","DP_EATL","DP_NEATL","DP_WMED","DP_EMED")
nareas<-length(areas)
area_defs<-new('list')
for(i in 1:nareas)area_defs[[i]]<-AreaDefs[[match(areas[i],AreaNames)]]


# --- Set working directory ------

setwd("G:/ABT-MSE/Data")


# ==== Task I and Task II ICCAT data ===============================================================

# --- Read raw data --------------

T1<-read.csv("Task1/T1.csv")
T2<-read.csv("Task2/T2.txt")
CAS<-read.csv("CAS/CAS.csv")


# --- ICCAT formatting -----------

T1<-subset(T1,T1$Species=="BFT")

T2<-ICCATtoGEO(T2)

CAS<-CAS[!is.na(CAS$Lon)&!is.na(CAS$Lat)&!is.na(CAS$SquareTypeCode),]
CAS<-ICCATtoGEO(CAS)
CAS<-CAS[!is.na(CAS$Lon)&!is.na(CAS$Lat),]


# --- Write data ----

write.csv(T1,file="Task1/T1_errcheck.csv")
write.csv(T2,file="Task2/T2_errcheck.csv")
write.csv(CAS,file="CAS/CAS_errcheck.csv")



# ==== PSAT data ===================================================================================

# --- Matt Lauretta --- GBYP - DFO - NOAA - WWF - Unimar - FCP - CB - IEO - AZTI - UCA 

AllPSAT<-read.csv("PSAT/ALL_BFT_ElectronicTags_01122015.csv")

expandtrack<-function(dat){
  
  datstr<-dat
  org<-c("C_ATL","CAR","E.ATL","E_ATL","E_MED", "GOM", "GSL", "NC_ATL", "NE.ATL", "NE_ATL", "SC_ATL", "SE.ATL", "SE_ATL", "W.MED", "W_ATL", "W_MED")
  nu<-c(  4,      2,    5,      5,      8,       1,     3,     4,        6,        6,        4,        5,        5,        7,       2,       7)
  dat$Stock_Area<-nu[match(dat$Stock_Area,org)]
  dat$Start_Date<-as.POSIXct(dat$Start_Date, format = "%Y-%m-%d", tz = "UTC") 
  dat$End_Date<-as.POSIXct(dat$End_Date, format = "%Y-%m-%d", tz = "UTC") 
  cond<-is.na(dat$Start_Date)
  dat$Start_Date[cond]<-as.POSIXct(datstr$Start_Date[cond], format = "%m/%d/%Y", tz = "UTC") 
  dat$End_Date[cond]<-as.POSIXct(datstr$End_Date[cond], format = "%m/%d/%Y", tz = "UTC") 
  
  
  #       TagID Area Date
  
  for(r in 1:nrow(dat)){
    nd<-as.integer((dat$End_Date[r]-dat$Start_Date[r])+1)
    Date<-dat$Start_Date[r]+days(0:(nd-1))
    Year<-as.numeric(format(Date,"%Y"))
    Quarter<-ceiling(as.numeric(format(Date,"%m"))/3) 
    TagID<-rep(as.character(dat$Tag_ID[r]),nd)
    Area<-rep(dat$Stock_Area[r],nd)
    
    temp2<-data.frame(TagID,Area,Date,Year,Quarter)
    
    if(r==1)temp<-temp2
    if(r>1)temp<-rbind(temp,temp2)  
    
  }
 
  temp 
  
}

All<-expandtrack(AllPSAT)

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
ExSpawn<-cbind(c(1,1,2),c(7,8,1))


TagIDs<-unique(All$TagID)
nTags<-length(TagIDs)
tracks<-rep(0,5)

for(i in 1:nTags){
  
  dat<-subset(All,All$TagID==TagIDs[i])
  Trk<-aggregate(dat$Area,by=list(dat$Year,dat$Quarter),mostfreq)
  names(Trk)<-c("Year","Quarter","Area")
  timetemp<-as.numeric(paste(Trk$Year,Trk$Quarter,sep=""))
  Trk<-Trk[order(timetemp),]
  nT<-nrow(Trk)
  if(nT>1){
    print(i)
    pop<-mostfreq(ExSpawn[match(Trk$Area,ExSpawn[,2]),1])
    for(j in 2:nT){  # loop over tracks by quarter
      if(j==2){
        temptrack<-c(pop,Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j])
      }else{
        temptrack<-rbind(temptrack,c(pop,Trk$Quarter[j-1],2,Trk$Area[j-1],Trk$Area[j]))
      }  
    }
    tracks<-rbind(tracks,temptrack)
  }
}

tracksAll<-data.frame(tracks[2:nrow(tracks),])
names(tracksAll)<-c("p","s","t","fr","tr")


# --- MAST PSAT 2007 -----------------------------------------------------------------------------------

#         GOM-L GSL  WATL+CATL EATL+NEATL  WMED
MAST_A<-c(1,    2,   3,        4,          5)
M3_A<-c(  1,    3,   2,        5,          7)
#         GOM-S GSL  WATL      EATL        WMED
# So its a bit of a fudge - the GOM is larger for MAST and encompasses some WATL
#                         - MAST combines W and C Atlantic
#                         - MAST comobines E and NE Atlantic

# The input files are a bit weird in that the start quarter is 1 off (one too large) for the release track 
refsq<-c(4,1:3) # the previous quarter
t1<-t2<-t3<-new('list')
qe=F
repfile<-"PSAT/MAST PSAT.txt"
n1<-scan(repfile,skip=1,nlines=1,quiet=qe)
n2<-scan(repfile,skip=2,nlines=1,quiet=qe)
n3<-scan(repfile,skip=3,nlines=1,quiet=qe)
sq1<-refsq[scan(repfile,skip=5,nlines=1,quiet=qe)]
sq2<-refsq[scan(repfile,skip=7,nlines=1,quiet=qe)]
sq3<-refsq[scan(repfile,skip=9,nlines=1,quiet=qe)]
eq1<-scan(repfile,skip=11,nlines=1,quiet=qe)
eq2<-scan(repfile,skip=13,nlines=1,quiet=qe)
eq3<-scan(repfile,skip=15,nlines=1,quiet=qe)
age1<-scan(repfile,skip=17,nlines=1,quiet=qe)
age2<-scan(repfile,skip=19,nlines=1,quiet=qe)
age3<-scan(repfile,skip=21,nlines=1,quiet=qe)
for(i in 1:n1)t1[[i]]<-scan(repfile,skip=22+i,nlines=1,quiet=qe)
for(i in 1:n2)t2[[i]]<-scan(repfile,skip=23+i+n1,nlines=1,quiet=qe)
for(i in 1:n3)t3[[i]]<-scan(repfile,skip=24+i+n1+n2,nlines=1,quiet=qe)

expand.tracks<-function(track,sq,pop,type='start'){  # break down all the track data into individual quarterly movements
  nt<-length(track) # number of areas tracked
  qs<-rep(1:4,100)[sq:(sq+nt-1)]
  if(type!="start"){
    sq<-(4-sq+1)
    qs<-(rep(4:1,100)[sq:(sq+nt-1)])[nt:1] # sq = eq
  }
  for(j in 2:nt){  # loop over tracks by quarter
    if(j==2){
      temptrack<-c(pop,qs[j-1],2,track[j-1],track[j])
    }else{
      temptrack<-rbind(temptrack,c(pop,qs[j-1],2,track[j-1],track[j]))
    }  
  }
  names(temptrack)<-c("p","s","t","fr","nr")
  temptrack
}

tracks<-rep(0,5)
for(i in 1:length(t1))tracks<-rbind(tracks,expand.tracks(t1[[i]],sq1[i],2))
for(i in 1:length(t2))tracks<-rbind(tracks,expand.tracks(t2[[i]],sq2[i],1))
for(i in 1:length(t3))tracks<-rbind(tracks,expand.tracks(t3[[i]],sq3[i],NA))

tracksMAST<-data.frame(tracks[2:nrow(tracks),])
names(tracksMAST)<-c("p","s","t","fr","tr")
tracksMAST$fr<-M3_A[tracksMAST$fr]
tracksMAST$tr<-M3_A[tracksMAST$tr]


# MAST ARCHIVAL 2007 west ----------------------------------------------------------------------------

aw<-new('list')
repfile<-"PSAT/MAST arch W.txt"
n<-scan(repfile,skip=1,nlines=1,quiet=qe)
eq<-scan(repfile,skip=3,nlines=1,quiet=qe)
age<-scan(repfile,skip=5,nlines=1,quiet=qe)
for(i in 1:n)aw[[i]]<-scan(repfile,skip=6+i,nlines=1,quiet=qe)
tracks<-rep(0,5)
for(i in 1:length(aw))tracks<-rbind(tracks,expand.tracks(aw[[i]],eq[i],2,type="end"))

tracksMAW<-data.frame(tracks[2:nrow(tracks),])
names(tracksMAW)<-c("p","s","t","fr","tr")
tracksMAW<-tracksMAW[tracksMAW$fr*tracksMAW$tr!=0,]
tracksMAW$fr<-M3_A[tracksMAW$fr]
tracksMAW$tr<-M3_A[tracksMAW$tr]


# MAST ARCHIVAL 2007 east ----------------------------------------------------------------------------

ae<-new('list')
repfile<-"PSAT/MAST arch E.txt"
n<-scan(repfile,skip=1,nlines=1,quiet=qe)
eq<-scan(repfile,skip=3,nlines=1,quiet=qe)
age<-scan(repfile,skip=5,nlines=1,quiet=qe)
for(i in 1:n)ae[[i]]<-scan(repfile,skip=6+i,nlines=1,quiet=qe)
tracks<-rep(0,5)
for(i in 1:length(ae))tracks<-rbind(tracks,expand.tracks(ae[[i]],eq[i],1,type="end"))

tracksMAE<-data.frame(tracks[2:nrow(tracks),])
names(tracksMAE)<-c("p","s","t","fr","tr")
tracksMAE<-tracksMAE[tracksMAE$fr*tracksMAE$tr!=0,]
tracksMAE$fr<-M3_A[tracksMAE$fr]
tracksMAE$tr<-M3_A[tracksMAE$tr]


Ttracks2<-rbind(tracksAll,tracksMAST,tracksMAW,tracksMAE)
Ttracks<-tracksAll
Tagg<-aggregate(rep(1,nrow(Ttracks)),by=list(Ttracks$s,Ttracks$fr,Ttracks$tr),sum)
tmov<-array(NA,c(4,nareas,nareas))
tmov[as.matrix(Tagg[,1:3])]<-Tagg[,4]

write.csv(Ttracks,file="PSAT/Ttracks2.csv")

