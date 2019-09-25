# SOO.r
# July 2018
# R Script for formatting Stock of Origin data

LinInterp<-function(x,y,xlev,ascending=FALSE,zeroint=FALSE){

  if(zeroint){
    x<-c(0,x)
    y<-c(0,y)
  }

  if(ascending){
    cond<-(1:length(x))<which.max(x)
  }else{
    cond<-rep(TRUE,length(x))
  }

  close<-which.min((x[cond]-xlev)^2)

  ind<-c(close,close+(x[close]<xlev)*2-1)
  ind <- ind[ind <= length(x)]
  if (length(ind)==1) ind <- c(ind, ind-1)
  if (min(ind)==0) ind <- 1:2
  ind<-ind[order(ind)]

  pos<-(xlev-x[ind[1]])/(x[ind[2]]-x[ind[1]])
  max(1,y[ind[1]]+pos*(y[ind[2]]-y[ind[1]]))

}




load(paste0(getwd(),"/Data/Raw/SOO/SooN.Rdata"))
load(paste0(getwd(),"/Data/Raw/SOO/pE.Rdata"))
SOOg<-pE
SOO<-read.csv(paste0(getwd(),"/Data/ICCAT_2019/Joint East West Mixing Data 15042019.csv"),header=T)[,c(5,6,10,11,13,15,16)]
SOO_old<-read.csv(paste0(getwd(),"/Data/Raw/SOO/Joint East West mixing data updated 06072018 with geneticID.csv"),header=T,sep=";")[,c(5,6,10,11,13,15,16)]
names(SOO)<-c("Year","Length","age","Prob.East","BFT_Area","Quarter","Method")

SOO<-subset(SOO,SOO$Year>(Base@years[1]-1)&SOO$Year<(Base@years[2]+1))
SOO$Year<-SOO$Year-Base@years[1]+1

print(paste("Total SOO assigments:",nrow(SOO)))
print(paste("SOO assignments missing age:",sum(is.na(SOO$age))))
print(paste("SOO assignments missing age and length:",sum(is.na(SOO$age)&is.na(SOO$Length))))

meths<-unique(SOO$Method)
org<-c("CAR", "E_ATL","E_MED",  "GOM","GSL","NC_ATL", "NE_ATL", "SC_ATL", "SE_ATL", "W_ATL",  "W_MED")
nu<-c(   2,     6,      7,       1,    3,     5,       5,        4,         4,      2,        7)#
nu_nam<-c("GOM","WATL","GSL","SATL","NATL","EATL","MED")

SOO$BFT_Area<-nu[match(SOO$BFT_Area,org)]

# Fill missing ages with cohort sliced ages using covariate length data
cond_fill<-is.na(SOO$age)&!is.na(SOO$Length)
ind<-(1:nrow(SOO))[cond_fill]
Lage<-apply(Base@len_age[,,1],2,mean)
Lengths<-as.numeric(as.character(SOO$Length[cond_fill]))
for(i in 1:length(Lengths))  SOO$age[ind[i]]<-floor(LinInterp(x=Lage,y=1:length(Lage),xlev=Lengths[i]))

print(paste("SOO assignments missing age after imputation:",sum(is.na(SOO$age))))

# Remove entries missing at least one required field
SOO<-SOO[,c(1,3,4,5,6,7)] # lose the length data
anyna<-function(x)sum(is.na(x))==0
keep<-apply(SOO,1,anyna)
print(paste("SOO assignments with all required covariates:",sum(keep)))
SOO<-SOO[keep,]

# Convert to age class
SOO$age[SOO$age>Base@na]<-Base@na
SOO$age<-ma[1,SOO$age] # age class


# define signature distributions and optimization function
res<-0.05
breaks<-seq(0,1,by=res)

# Otolith Chemistry
pE<-hist(SOO$Prob.East[SOO$BFT_Area==7 & SOO$Method=="Otolith Chemistry"],breaks=breaks)$density
pW<-hist(SOO$Prob.East[SOO$BFT_Area==1 & SOO$Method=="Otolith Chemistry"],breaks=breaks)$density
pE<-pE/sum(pE)
pW<-pW/sum(pW)

# Genetics
pEg<-hist(SOOg$probMED[SOOg$origin=="MED"]/100,breaks=breaks)$density
pWg<-hist(SOOg$probMED[SOOg$origin=="GOM"]/100,breaks=breaks)$density
pEg<-pEg/sum(pEg)
pWg<-pWg/sum(pWg)

getSOO<-function(x,strata,SOO,pE,pW,breaks,plot=F,type='Otolith Chemistry'){

  cond<-SOO$Method==type & SOO$Year==strata[x,1] &
    SOO$age==strata[x,2] & SOO$BFT_Area==strata[x,3] & SOO$Quarter == strata[x,4]
  pM<-hist(SOO$Prob.East[cond],breaks=breaks,plot=F)$density

  pM<-pM/sum(pM)
  if(plot)barplot(pM,border="white",axes=F)


  temp<-optim(c(0,-1),SOOopt,hessian=T,pM=pM,pE=pE,pW=pW)
  probElogit<-temp$par[1]
  probE=exp(probElogit)/(1+exp(probElogit))
  SElogit<-solve(temp$hessian)[1,1]^0.5
  SE<-exp(SElogit)/(1+exp(SElogit))

  LB=probElogit-(1.96*SElogit)
  UB=probElogit+(1.96*SElogit)

  LB<-exp(LB)/(1+exp(LB))
  UB<-exp(UB)/(1+exp(UB))


  sig=exp(temp$par[2])

  #pu = (sqrt(probE) + sig)^2
  #pl = (sqrt(probE) - sig)^2
  #SE<-(pu-pl)/2

  if(plot){
    #legend('topleft',legend=c(paste0("y=",strata[x,1]),paste0("age=",strata[x,2])),cex=0.8,bty='n')
    #legend('top',legend=c(paste0("ar=",strata[x,3]),paste0("q=",strata[x,4])),cex=0.8,bty='n')

    pred<-(pE*probE)+(pW*(1-probE))
    pred<-(pred/sum(pred))

    breakmu<-((breaks[1:(length(breaks)-1)]+breaks[2:length(breaks)])/2)*24

    lines(breakmu,pred,col="#ff00ff80",lwd=3)
    mtext(paste0("R=",round(probE*100,0),"% (",nu_nam[strata[x,3]],", Q",strata[x,4],")"),3,line=-0.04,cex=0.6)

    legend('top',legend=paste0("[",round(LB*100,0),"%,",round(UB*100,0),"%]"),cex=0.8,bty='n')
  }
  c(probE,SE,probElogit,SElogit)

}

SOOopt<-function(par,pM,pE,pW){

  rat=exp(par[1])/(1+exp(par[1]))
  sig=exp(par[2])


  pMexp<-rat*pE+(1-rat)*pW
  pMexp<-pMexp/sum(pMexp)
  #sig<-sd((sqrt(pM)-sqrt(pMexp)))
  obj<-sum(log(sig) + (sqrt(pM)-sqrt(pMexp))^2/(2*sig^2))
  obj<-obj-dnorm(par[1],0,sd=5,log=T)
  return(obj)

}


SOO_OC<-SOO[SOO$Method=='Otolith Chemistry' & !(SOO$BFT_Area%in%c(1,7)),]
strata1<-aggregate(rep(1,nrow(SOO_OC)),by=list(SOO_OC$Year,SOO_OC$age,SOO_OC$BFT_Area,SOO_OC$Quarter),FUN=sum)
strata<-strata1[strata1$x>5,]

if(summary){
  jpeg("data/Raw/SOO/Summaries/derivedSOO_SE_2.jpg",width=7.5,height=11,res=300,units='in')
  par(mfrow=c(18,6),mai=c(0.01,0.01,0.2,0.01))

  out<-sapply(1:nrow(strata),getSOO,strata=strata,SOO=SOO,pE=pE,pW=pW,breaks=breaks,plot=F)

  ord<-order(out[1,])
  out<-sapply(ord[2:length(ord)],getSOO,strata=strata,SOO=SOO,pE=pE,pW=pW,breaks=breaks,plot=T)

  dev.off()
}

suppressWarnings({
  out<-sapply(1:nrow(strata),getSOO,strata=strata,SOO=SOO,pE=pE,pW=pW,breaks=breaks,plot=F)
})



SOO_G<-SOO[SOO$Method=='Genetics' & !(SOO$BFT_Area%in%c(1,7)),]# SOO_G<-SOO[SOO$Method=='Genetics',]
strata1<-aggregate(rep(1,nrow(SOO_G)),by=list(SOO_G$Year,SOO_G$age,SOO_G$BFT_Area,SOO_G$Quarter),FUN=sum)
strata_g<-strata1[strata1$x>5,]

if(summary){
  jpeg("data/Raw/SOO/Summaries/derivedSOO_SE_2_Genetics.jpg",width=7.5,height=6,res=300,units='in')
  par(mfrow=c(6,5),mai=c(0.01,0.01,0.2,0.01))

  out_g<-sapply(1:nrow(strata_g),getSOO,strata=strata_g,SOO=SOO,pE=pEg,pW=pWg,breaks=breaks,plot=F,type="Genetics")

  ord<-order(out_g[1,])
  out_g<-sapply(ord[2:length(ord)],getSOO,strata=strata_g,SOO=SOO,pE=pEg,pW=pWg,breaks=breaks,plot=T,type="Genetics")

  dev.off()
}

suppressWarnings({
  out_g<-sapply(1:nrow(strata_g),getSOO,strata=strata_g,SOO=SOO,pE=pE,pW=pW,breaks=breaks,plot=F,type="Genetics")
})


cond<-(out[1,]^0.5-out[2,])<0
redface=sum(cond)

SOOobs_o<-cbind(strata[,c(2,1,4,3,5)],t(out[3:4,]),rep(1,nrow(strata)))
SOOobs_g<-cbind(strata_g[,c(2,1,4,3,5)],t(out_g[3:4,]),rep(2,nrow(strata_g)))
names(SOOobs_o)<-names(SOOobs_g)<-c("a","y","s","r","N","probE","SE","Type")

if(add_Genetics){
  SOOobs<-rbind(SOOobs_o,SOOobs_g)
}else{
  SOOobs<-SOOobs_o
}

#p a y s r N
plot(SOOobs[,5],SOOobs[,7])
plot(SOOobs[,4],SOOobs[,6])

# Add the missing genetic signature for the GOM and MED for tabulation purposes
#if(add_Genetics2){
 # gom_g_sig<-SOOg$probMED[SOOg$origin=="GOM"]/100
  #ng<-length(gom_g_sig)
  #gsig<-as.data.frame(cbind(rep(1,ng),rep(1,ng),gom_g_sig,rep(1,ng),rep(1,ng),rep("Genetics")))
  #for(x in 1:5)gsig[,x]<-as.numeric(as.character(gsig[,x]))
  #names(gsig)<-names(SOO)
  #SOO<-rbind(SOO,gsig)
#}

if(summary){

# ----- SOO summary by Area  --------------

  SOO_AM<-aggregate(rep(1,nrow(SOO)),by=list(SOO$Method,SOO$BFT_Area),sum)
  SOO_AM_mat<-matrix(0,nrow=2,ncol=7)
  SOO_AM_mat[cbind(
             as.numeric(match(SOO_AM$Group.1,meths)),
             as.numeric(SOO_AM$Group.2))] <- SOO_AM$x
  SOO_AM_mat<-data.frame(SOO_AM_mat,row.names = meths)
  names(SOO_AM_mat)<-nu_nam
  write.csv(SOO_AM_mat,"data/Raw/SOO/Summaries/SOO_AM.csv")

  jpeg("data/Raw/SOO/Summaries/SOObyarea_2.jpg",width=6,height=7,res=400,units='in')
    cols<-c("red","lightblue","green","grey","black","pink","blue")
    par(mfrow=c(2,1),mai=c(0.5,0.5,0.4,0.1),omi=c(0.5,0.5,0,0))
    ymaxs<-c(6,14)
    for(mm in 1:length(meths)){

      plot(density(SOO$Prob.East[SOO$BFT_Area==7 & SOO$Method==meths[mm]],from=0,to=1,adj=0.5),col=cols[7],main="",ylim=c(0,ymaxs[mm]))
      for(i in 1:6) if(SOO_AM_mat[mm,i]>2)lines(density(SOO$Prob.East[SOO$BFT_Area==i & SOO$Method==meths[mm]],from=0,to=1),col=cols[i])
      if(mm==2)legend('topleft',legend=Base@areanams,text.col=cols,bty='n')
      mtext(meths[mm],3,line=0.5,font=2)
    }
    mtext("Assigment score",1,outer=T)
    mtext("Rel. Freq.",2,outer=T)

  dev.off()


  # quarter and area
  SOOom<-SOO[SOO$Method=='Otolith Chemistry',]
  SOO_QR<-aggregate(rep(1,nrow(SOOom)),by=list(SOOom$Quarter,SOOom$BFT_Area),sum)
  SOO_QR_mat<-matrix(0,nrow=4,ncol=7)
  SOO_QR_mat[cbind(SOO_QR$Group.1,SOO_QR$Group.2)] <- SOO_QR$x
  SOO_QR_mat<-data.frame(SOO_QR_mat,row.names =paste0("Q",1:4))
  names(SOO_QR_mat)<-nu_nam
  write.csv(SOO_QR_mat,"data/Raw/SOO/Summaries/SOO_QR_otolith.csv")

  # quarter and area
  SOOom<-SOO[SOO$Method=='Genetics',]
  SOO_QR<-aggregate(rep(1,nrow(SOOom)),by=list(SOOom$Quarter,SOOom$BFT_Area),sum)
  SOO_QR_mat<-matrix(0,nrow=4,ncol=7)
  SOO_QR_mat[cbind(SOO_QR$Group.1,SOO_QR$Group.2)] <- SOO_QR$x
  SOO_QR_mat<-data.frame(SOO_QR_mat,row.names =paste0("Q",1:4))
  names(SOO_QR_mat)<-nu_nam
  write.csv(SOO_QR_mat,"data/Raw/SOO/Summaries/SOO_QR_genetics.csv")


  # ---- SOO coverage ------------------

  SOO_dec<-SOO
  SOO_dec$Year<-SOO_dec$Year+1973
  SOO_dec$Year<-floor(SOO_dec$Year/10)*10
  SOO_dec$BFT_Area<-nu_nam[SOO_dec$BFT_Area]
  SOO_AMDQ<-aggregate(rep(1,nrow(SOO_dec)),by=list(SOO_dec$Method,SOO_dec$BFT_Area,SOO_dec$Year,SOO_dec$Quarter),sum)

  names(SOO_AMDQ)<-c("Method","Area","Decade","Quarter","n")
  write.csv(SOO_AMDQ,"data/Raw/SOO/Summaries/SOO_AMDQ.csv")

  sum(SOO_AMDQ$n)
  sum(SOO_AMDQ$n[SOO_AMDQ$n>5])
  sum(SOO_AMDQ$n[SOO_AMDQ$n>10])

  SOO_AMYQ<-aggregate(rep(1,nrow(SOO_dec)),by=list(SOO_dec$Method,SOO_dec$BFT_Area,SOO_dec$Year,SOO_dec$Quarter,SOO_dec$age),sum)
  names(SOO_AMYQ)<-c("Method","Area","Decade","Quarter","Age","n")
  sum(SOO_AMYQ$n[SOO_AMYQ$n>5])
  sum(SOO_AMYQ$n[SOO_AMYQ$n>10])

  SOO_AMDQA<-aggregate(rep(1,nrow(SOO)),by=list(SOO$Method,SOO$BFT_Area,SOO$Year,SOO$Quarter,SOO$age),sum)
  names(SOO_AMYQ)<-c("Method","Area","Year","Quarter","Age","n")
  sum(SOO_AMYQ$n[SOO_AMYQ$n>5])
  sum(SOO_AMYQ$n[SOO_AMYQ$n>10])

  # SOO quantities
  SOO_quant<-aggregate(SOO$Prob.East,by=list(SOO$BFT_Area,SOO$Method),quantile,p=c(0.05,0.5,0.95))
  SOO_quant$Group.1<-nu_nam[SOO_quant$Group.1]
  write.csv(SOO_quant,"data/Raw/SOO/Summaries/SOO_quant.csv")


  # SOO raw, with cut-off and new
  SOO_quant<-aggregate(SOO$Prob.East,by=list(SOO$BFT_Area),quantile,p=c(0.05,0.5,0.95))
  SOOraw_quant=SOO_quant #[SOO_quant$Group.2=="Otolith Chemistry",]

  SOOcut<-SOO[SOO$Prob.East<0.3|SOO$Prob.East>0.7,]
  SOOcut_quant<-aggregate(SOOcut$Prob.East,by=list(SOOcut$BFT_Area),quantile,p=c(0.05,0.5,0.95))
  #SOOcut_quant=SOOcut_quant[SOOcut_quant$Group.2=="Otolith Chemistry",]

  nuProb<-exp(SOOobs[,6])/(1+exp(SOOobs[,6]))
  SOOnew_quant<-aggregate(nuProb,by=list(SOOobs[,4]),quantile,p=c(0.05,0.5,0.95))


  # comparison table
  comptab<-matrix(NA,nrow=9,ncol=8)

  ind<-1:7
  comptab[1:3,ind]<-t(SOOraw_quant[,2])
  comptab[2,8]<-1

  ind<-SOOcut_quant[,1]
  comptab[4:6,ind]<-t(SOOcut_quant[,2])
  comptab[5,8]<-nrow(SOO[(SOO$Prob.East<0.3|SOO$Prob.East>0.7),])/nrow(SOO)

  ind<-SOOnew_quant[,1]
  comptab[7:9,ind]<-t(SOOnew_quant[,2])
  comptab[8,8]<-sum(SOO_AMYQ$n[SOO_AMYQ$n>5])/nrow(SOO)
  comptab[7:9,7]<-1
  comptab[7:9,1]<-0
  write.csv(comptab,"data/Raw/SOO/Summaries/comptab.csv")

  jpeg("data/Raw/SOO/Summaries/compplot.jpg",width=9,height=2.6,res=400,units='in')

    plotwhisk<-function(i,comptab){
      mids<-c(0.7,1.9)
      tick<-0.2
      j<-1
      lines(c(mids[j]-tick,mids[j]+tick),rep(comptab[4,i],2))
      lines(c(mids[j]-tick,mids[j]+tick),rep(comptab[6,i],2))
      lines(rep(mids[j],2),comptab[c(4,6),i])

      j<-2
      lines(c(mids[j]-tick,mids[j]+tick),rep(comptab[7,i],2))
      lines(c(mids[j]-tick,mids[j]+tick),rep(comptab[9,i],2))
      lines(rep(mids[j],2),comptab[c(7,9),i])
    }

    cols<-c("grey","#ff00ff80")
    par(mfrow=c(1,8),mai=c(0.1,0.25,0.1,0.1),omi=c(0.01,0.3,0.2,0.01))
    for(i in 1:7){


      barplot(comptab[c(5,8),i],col=cols,border=NA,ylim=c(0,1))
      mtext(nu_nam[i],3,line=0.2)
      if(is.na(comptab[7,i]))text(1.75,0.5,"NA",col='#ff00ff80',font=2,cex=1.2)
      plotwhisk(i,comptab)

    }
    mtext("Prob.East",2,line=0.5,outer=T)
    plot.new()
    legend('center',legend=c("30-70cut","Mixture"),text.col=cols,text.font=2)

  dev.off()
  # description of approach


  pE<-hist(SOO$Prob.East[SOO$BFT_Area==7],breaks=breaks)$density
  pW<-hist(SOO$Prob.East[SOO$BFT_Area==1],breaks=breaks)$density
  pE<-pE/sum(pE)
  pW<-pW/sum(pW)

  cond<-SOO$Method=="Otolith Chemistry"  & SOO$BFT_Area==3 & SOO$Quarter == 3

  pM<-hist(SOO$Prob.East[cond],breaks=breaks,axes=F,col='blue',border="white",main="")$density
  pM<-pM/sum(pM)

  jpeg("data/Raw/SOO/Summaries/method_description.jpg",width=6,height=7,res=400,units='in')

   par(mfrow=c(2,1),mai=c(0.1,0.1,0.4,0.3),omi=c(0.3,0.3,0.01,0.01))

   Ecol<-"#ff000060"
   Wcol<-"#0000ff60"
   barplot(pE,names.arg=breaks,border=FALSE,col=Ecol)
   barplot(pW,names.arg=breaks,border=FALSE,col=Wcol,add=T)
   mtext("Assignment data for natal areas",3,line=0.4,font=2)
   legend('top',legend=c("GOM (Western)","Med (Eastern)"),fill=c(Wcol,Ecol),border='white',bty='n',horiz=T)



   barplot(pM,name.arg=breaks,border=FALSE,col="#99999970",axes=F)
   mtext("Assigment data in a mixed strata i",3,line=0.2,font=2)
   x<-match(134,strata$x)
   out2<-sapply(x,getSOO,strata=strata,SOO=SOO,pE=pE,pW=pW,breaks=breaks,plot=F)
   rat<-out2[1,1]
   comb<-pE*rat+pW*(1-rat)
   comb<-comb/sum(comb)
   mubreak<-(breaks[1:(length(breaks)-1)]+breaks[2:length(breaks)])*12
   lines(mubreak,pE*rat,col=Ecol,lwd=2)
   lines(mubreak,pW*(1-rat),col=Wcol,lwd=2)
   lines(mubreak,comb,col="#ff00ff80",lwd=3)

   legend('top',legend=c("Western distrbn. ((1-R)=66%)","Mixture distrbn.","Eastern distrbn. (R = 34%)"),col=c(Wcol,"#ff00ff80",Ecol),lwd=c(2,3,2),bty='n')
   mtext("Assignment score",1,line=0.1,outer=T)
   mtext("Rel. Freq.",2,line=0.1,outer=T)


  dev.off()

  ylim=mean(pM)





}

if(testSOO){
  yrs=c(5,10,20,30,40,50)
  SOOmat<-array(5,c(Base@nma,6,Base@ns,Base@nr-2))
  #SOOmat<-array(1:1000,c(Base@nma,Base@ny,Base@ns,Base@nr))
  SOOmat[,,,1:2]<-(-5)
  ind<-expand.grid(1:Base@nma,yrs,1:Base@ns,2:6)
  SOOobs<-cbind(ind,rep(9999,nrow(ind)),as.vector(SOOmat),rep(0.5,nrow(ind)))
  names(SOOobs)<-c("a","y","s","r","N","probE","SE")


}



#SOO$Prob.East[SOO$BFT_Area==1]<-0
#SOO$Prob.East[SOO$BFT_Area==7]<-1

#SOO1<-aggregate(SOO$Prob.East,by=list(SOO$age,SOO$Year,SOO$Quarter,SOO$BFT_Area),sum)
#SOO2<-aggregate(1-SOO$Prob.East,by=list(SOO$age,SOO$Year,SOO$Quarter,SOO$BFT_Area),sum)

#SOO1<-cbind(rep(1,nrow(SOO1)),SOO1)
#SOO2<-cbind(rep(2,nrow(SOO2)),SOO2)

#SOOtempGOM<-expand.grid(2,1:3,1,1:4,1,1000) #  !!!!!!!! temporary fix until we sort out SOO for GOM
#SOOtempMed<-expand.grid(1,1:3,1,1:4,10,1000) #  !!!!!!!! temporary fix until we sort out SOO for GOM
#SOOtempSEATL<-expand.grid(1,1:3,1,1:4,9,1000) #  !!!!!!!! temporary fix until we sort out SOO for GOM
#SOOtempCAR<-expand.grid(2,1:3,1,1:4,2,1000) #  !!!!!!!! temporary fix until we sort out SOO for GOM

#names(SOO1)<-names(SOO2)<-names(SOOtempGOM)<-names(SOOtempMed)<-names(SOOtempSEATL)<-names(SOOtempCAR)<-c("p","aa","y","s","r","N")
#names(SOO1)<-names(SOO2)<-c("p","aa","y","s","r","N")

#SOOobs<-rbind(SOO1,SOO2)#,SOOtempGOM,SOOtempMed,SOOtempSEATL,SOOtempGSL)
#SOOobs<-SOOobs[SOOobs$N>0,]
wt<-1/(SOOobs[,7]^2)
SOOobs<-cbind(SOOobs,wt)
SOOobs<-as.matrix(SOOobs)

save(SOOobs,file=paste(getwd(),"/Data/Processed/Conditioning/SOOobs",sep=""))

