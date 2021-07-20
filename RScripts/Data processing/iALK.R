# iALK.r
# April 2021
# R Script for inventing an inverse age-length key (conditional probability of length given age) in the absence of a currently available iALK

agearray<-array(rep(1:Base@na,each=Base@np),c(Base@np,Base@na,Base@ny))
len_age<-len_age2<-wt_age<-array(NA,dim(agearray))
ind<-TEG(dim(agearray))
len_age[ind]<-(
                 Base@L1[ind[,1]]^Base@p[ind[,1]]+(Base@L2[ind[,1]]^Base@p[ind[,1]]-Base@L1[ind[,1]]^Base@p[ind[,1]])*
                 (
                   (1-exp(-Base@K[ind[,1]]*agearray[ind]))/
                   (1-exp(-Base@K[ind[,1]]*Base@A2[ind[,1]]))
                 )
               )^(1/Base@p[ind[,1]])


len_age2[ind]<-Base@Linf[ind[,1]]*(1-exp(-Base@K[ind[,1]]*(agearray[ind]-Base@t0[ind[,1]])))

cond<-is.na(len_age[,1,1])
len_age[cond,,]<-len_age2[cond,,]

nlen<-length(Base@lenbins)-1

ind<-TEG(dim(wt_age))
wt_age[ind]<-Base@lwa[ind[,1]]*len_age[ind]^Base@lwb[ind[,1]]


iALK<-array(NA,c(Base@np,Base@ny,Base@na,Base@nl))
ind<-TEG(dim(iALK))
Lind<-ind[,c(1,3,2)]
iALK[ind]<-dlnorm(Base@mulen[ind[,4]],log(len_age[Lind]),(Base@Lvar_a[ind[,1]]*len_age[Lind]+Base@Lvar_b[ind[,1]])/len_age[Lind]) # SD is determined by linear model of Allioud et al. 2017
sums<-apply(iALK,1:3,sum)
sind<-ind[,1:3]
iALK<-iALK/sums[sind]

# Visual check of iALK (LAK)
contour(x=1:Base@na,y=Base@mulen,iALK[1,1,,],nlevels=10)
contour(x=1:Base@na,y=Base@mulen,iALK[2,1,,],add=T,col='red',nlevels=10)
legend('topleft',legend=c("West","East"),bty='n',text.col=c('red','black'))

save(iALK,file=paste(getwd(),"/Data/Processed/Conditioning/iALK",sep=""))
