# SOO.r
# August 2016
# R Script for formatting Stock of Origin data

SOO<-read.csv(paste(getwd(),"/Data/Raw/SOO/All SOO 29_7_2016.csv",sep=""),header=T)

anyna<-function(x)(sum(is.na(x))+sum(x==""))==0
SOO<-SOO[apply(SOO,1,anyna),]

SOO<-subset(SOO,SOO$Year>(Base@years[1]-1)&SOO$Year<(Base@years[2]+1))
SOO$Year<-SOO$Year-Base@years[1]+1

org<-c("CAR", "E_ATL","E_MED",  "GOM","GSL","NC_ATL", "NE_ATL", "SC_ATL", "SE_ATL", "W_ATL",   "MED") 
nu<-c(   2,     8,      10,       1,    4,     6,        7,        5,         9,      3,        10)# 

SOO$BFT_Area<-nu[match(SOO$BFT_Area,org)]

SOO$age[SOO$age>Base@na]<-Base@na
SOO$age<-ma[1,SOO$age] # age class

SOO$Prob.East[SOO$BFT_Area==1]<-0
SOO$Prob.East[SOO$BFT_Area==10]<-1

SOO1<-aggregate(SOO$Prob.East,by=list(SOO$age,SOO$Year,SOO$Quarter,SOO$BFT_Area),sum)
SOO2<-aggregate(1-SOO$Prob.East,by=list(SOO$age,SOO$Year,SOO$Quarter,SOO$BFT_Area),sum)

SOO1<-cbind(rep(1,nrow(SOO1)),SOO1)
SOO2<-cbind(rep(2,nrow(SOO2)),SOO2)

SOOtemp<-expand.grid(2,2,50,1:4,5,10) #  !!!!!!!! temporary fix until we sort out SOO for GOM

names(SOO1)<-names(SOO2)<-names(SOOtemp)<-c("p","aa","y","s","r","N")

SOOobs<-rbind(SOO1,SOO2,SOOtemp)
SOOobs<-SOOobs[SOOobs$N>0,]
SOOobs<-as.matrix(SOOobs)

save(SOOobs,file=paste(getwd(),"/Data/Processed/Conditioning/SOOobs",sep=""))

