# iALK.r
# August 2016
# R Script for inventing an inverse age-length key (conditional probability of length given age) in the absence of a currently available iALK

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
#Len_age[ind]<-Linf[ind[,1]]*(1-exp(-K[ind[,1]]*(agearray[ind]-t0[ind[,1]])))
wt_age[ind]<-Base@lwa[ind[,1]]*len_age[ind]^Base@lwb[ind[,1]]

LenCV<-0.07
iALK<-array(NA,c(Base@np,Base@ny,Base@na,Base@nl))
ind<-TEG(dim(iALK))
Lind<-ind[,c(1,3,2)]
iALK[ind]<-dnorm(Base@mulen[ind[,4]],len_age[Lind],len_age[Lind]*LenCV)
sums<-apply(iALK,1:3,sum)
sind<-ind[,1:3]
iALK<-iALK/sums[sind]

save(iALK,file=paste(getwd(),"/Data/Processed/Conditioning/iALK",sep=""))



#contour(iALK[1,1,,],nlevels=20)
#contour(iALK[2,1,,],add=T,col='red',nlevels=20)

#p<--0.39
#p<-0.95
#K<-0.26
#plot((32.43^p+(263.64^p-32.43^p)*(1-exp(-K*(1:35)))/(1-exp(-K*35)))^(1/p))

