# Plot SOO

setwd("C:/Users/tcar_/Dropbox/abft-mse")
setwd("C:/Users/tcarruth/Dropbox/abft-mse")


load(file=paste0(getwd(),"/objects/OMs/1/OMI"))
SOO<-OMI@SOOobs

jpeg(paste0(getwd(),"/Data/Raw/SOO/SOObyyear.jpg"),width=6,height=6,res=400,units='in')

par(mai=c(0.4,0.4,0.1,0.1),omi=c(0.4,0.4,0.01,0.01))
layout(mat=matrix(c(2,1,6,4,5,3),nrow=3))

yrs<-1964+min(SOO[,2]):max(SOO[,2])       
logit<-function(x)exp(x)/(1+exp(x)) 
cols<-c('#ff000060','#0000ff60')

for(a in 2:(OMI@nr-1)){
    
  cond<-SOO[,4]==a 
  plot(1,1,xlim=range(yrs),ylim=c(0,1),col='white')
  legend('topleft',legend=OMI@areanams[a],bty='n',text.font=2)
  if(sum(cond)>1){
    
    sub<-SOO[cond,]
 
    for(i in 1:nrow(sub)){
      
      t=sub[i,8]
      points(sub[i,2]+1964,logit(sub[i,6]),col=cols[t],pch=19)
      
    }
  }
 
}
plot(1,1,col='white',axes=F)
legend('center',legend=c("Otolith Microchemistry","Genetics"),text.col=c('red','blue'),bty='n',cex=1.2)
mtext("Probability Eastern Origin",2,outer=T,line=0.5)
mtext("Year",1,outer=T,line=0.5)

dev.off()


