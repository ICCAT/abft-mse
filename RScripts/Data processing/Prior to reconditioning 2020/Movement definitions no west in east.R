
# Movement definitions.r
# August 2016

np<-Base@np
nareas<-Base@nr
ns<-Base@ns
nma<-Base@nma


can<-matrix(1,nrow=Base@np,ncol=Base@nr)
can[1,match("GOM",Base@areanams)]<-0
can[2,match("MED",Base@areanams)]<-0
can[2,match("EATL",Base@areanams)]<-0
can[2,match("SATL",Base@areanams)]<-0
can[2,match("NATL",Base@areanams)]<-0


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

save(movind,file=paste(getwd(),"/Data/Processed/Conditioning/movind",sep=""))
save(mov1,file=paste(getwd(),"/Data/Processed/Conditioning/mov1",sep=""))

