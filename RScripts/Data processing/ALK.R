# ALK.r
# August 2018
# R Script for inventing an age-length key (conditional probability of age given length) in the absence of a currently available iALK
#Base@na<-as.integer(18)

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
#Len_age[ind]<-Linf[ind[,1]]*(1-exp(-K[ind[,1]]*(agearray[ind]-t0[ind[,1]])))
wt_age[ind]<-Base@lwa[ind[,1]]*len_age[ind]^Base@lwb[ind[,1]]


ALK<-array(NA,c(Base@np,Base@ny,Base@na,Base@nl))
ind<-TEG(dim(ALK))
Lind<-ind[,c(1,3,2)]
ALK[ind]<-dnorm(Base@mulen[ind[,4]],len_age[Lind],Base@Lvar_a[ind[,1]]*len_age[Lind]+Base@Lvar_b[ind[,1]])
sums<-apply(ALK,c(1,2,4),sum)
sind<-ind[,c(1,2,4)]
ALK<-ALK/sums[sind]

save(ALK,file=paste(getwd(),"/Data/Processed/Conditioning/ALK",sep=""))

contour(x=1:Base@na,y=Base@mulen,ALK[1,1,,],nlevels=50)
contour(x=1:Base@na,y=Base@mulen,ALK[2,1,,],add=T,col='red',nlevels=50)
legend('topleft',legend=c("West","East"),bty='n',text.col=c('red','black'))


#p<--0.39
#p<-0.95
#K<-0.26
#plot((32.43^p+(263.64^p-32.43^p)*(1-exp(-K*(1:35)))/(1-exp(-K*35)))^(1/p))

#L1=33.0
#L2=270.6
#p=???0.12
#A1=0
#A2=34
#K=0.22

#age=seq(0,40,1)

#mean size at age
#L_age = (L1^p+(L2^p-L1^p)*(1-exp(-K*(age-A1)))/(1-exp(-K*(A2-A1))))^(1/p);L_age
