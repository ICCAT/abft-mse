# PSAT.r
# August 2016
# R Script for testing SOO assignment by derived movement matrix


# --- Matt Lauretta --- GBYP - DFO - NOAA - WWF - Unimar - FCP - CB - IEO - AZTI - UCA

#subset(tracks,!is.na(tracks$p))
conv<-function(relsize=c(1,1),Priormov,ny=100,ns=4){
  recmov<-array(NA,dim(Priormov))
  nr<-dim(Priormov)[4]
  for(p in 1:np){
    vec<-rep(relsize[p]/nr,nr)
    for(y in 1:ny){
      for(s in 1:ns){
        recmov[p,s,,]<-vec*Priormov[p,s,,]
        vec<-apply(recmov[p,s,,],2,sum)
      }
    }
  }
  recmov
}


np<-Base@np
ns<-Base@ns
nr<-Base@nr


pw<-1/nr
PriorSOO<-array(0,c(np,ns,nr,nr))
PriorSOO[1,,,]<-rep(c(0,rep(pw,nr-1)),each=ns*nr)
PriorSOO[2,,,]<-rep(c(rep(pw,nr-1),0),each=ns*nr)

trackskp<-Tracks[!is.na(Tracks$p),]
TSOO<-aggregate(rep(1,nrow(trackskp)),by=list(trackskp$p,trackskp$s,trackskp$fr,trackskp$tr),sum)
movSOO<-Priormov<-LikeSOO<-array(0,c(np,ns,nr,nr))
movSOO[as.matrix(TSOO[,1:4])]<-TSOO[,5]
movind<-TEG(dim(movSOO))
movSOO<-movSOO/array(apply(movSOO,1:3,sum),dim(movSOO))
movSOO[is.na(movSOO)]<-0
Priormov<-(movSOO+PriorSOO)/array(apply(movSOO+PriorSOO,1:3,sum),dim(movSOO))
recmov<-conv(c(1,1),Priormov,ny=20)
LHD1<-(recmov[1,,,])/(recmov[1,,,]+recmov[2,,,])
LHD1[,,7]<-1
LHD1[,,1]<-0

LHD2<-recmov[2,,,]/(recmov[1,,,]+recmov[2,,,])
LHD2[,,7]<-0
LHD2[,,1]<-1

Imptagnos<-unique(subset(Tracks,is.na(Tracks$p))[,7])

for(tt in Imptagnos){

  temp<-subset(Tracks,Tracks$tagno==tt)
  ind<-as.matrix(temp[,c(3,5,6)])

  prob1<-prod(LHD1[ind])
  prob2<-prod(LHD2[ind])

  rat<-prob1/prob2
  if(rat==Inf)rat==1

  stk<-NA

  if(rat>2)stk<-1
  if(rat<0.5)stk<-2

  Tracks[Tracks$tagno==tt,1]<-stk
  print(paste(tt,rat,stk,sep=" - "))

}


