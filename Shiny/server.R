

F_FMSY<-readRDS("./data/F_FMSY.rds")
B_BMSY<-readRDS("./data/B_BMSY.rds")
AAVY<-readRDS("./data/AAVY.rds")
Y<-readRDS("./data/Y.rds")
Dep<-readRDS("./data/Dep.rds")
MPs<-readRDS("./data/MPs.rds")
OMi<-readRDS("./data/OMi.rds")
nMP<-length(MPs)
ny<-dim(B_BMSY)[3]

shinyServer(function(input, output, session){
 
  output$Tplot<-renderPlot({
    
    # Tplot1
    SR1<-input$SR1
    if(SR1=="ALL")SR1<-c("BH7","BH98")
    Mov1<-input$Mov1
    if(Mov1=="ALL")Mov1<-c("Grav","Frac")
    Up1<-input$Up1
    if(Up1=="ALL")Up1<-c("Up3","Up4")
    Rec1<-input$Rec1
    if(Rec1=="ALL")Rec1<-c("StableR","LowR")
    CBias1<-input$CBias1
    if(CBias1=="ALL")CBias1<-c("NoCbias","SPCbias")
    MMat1<-input$MMat1
    if(MMat1=="ALL")MMat1<-c("Mconst-AMhigh-Mage-AMlow","Mconst-AMhigh-Mconst-AMhigh", "Mage-AMlow-Mage-AMlow")
    Dep1<-input$Dep1
    if(Dep1=="ALL")Dep1<-c("Dep_30_40","Dep_15_20")
    
    ind1<-(1:nrow(OMi))[OMi[,1]%in%SR1 & OMi[,2]%in%Mov1 & OMi[,3]%in%Up1 & OMi[,4]%in%Rec1 & OMi[,5]%in%CBias1 & OMi[,6]%in%MMat1 & OMi[,7]%in%Dep1]
   
    
    # Tplot2  
    SR2<-input$SR2
    if(SR2=="ALL")SR2<-c("BH7","BH98")
    Mov2<-input$Mov2
    if(Mov2=="ALL")Mov2<-c("Grav","Frac")
    Up2<-input$Up2
    if(Up2=="ALL")Up2<-c("Up3","Up4")
    Rec2<-input$Rec2
    if(Rec2=="ALL")Rec2<-c("StableR","LowR")
    CBias2<-input$CBias2
    if(CBias2=="ALL")CBias2<-c("NoCbias","SPCbias")
    MMat2<-input$MMat2
    if(MMat2=="ALL")MMat2<-c("Mconst-AMhigh-Mage-AMlow","Mconst-AMhigh-Mconst-AMhigh", "Mage-AMlow-Mage-AMlow")
    Dep2<-input$Dep2
    if(Dep2=="ALL")Dep2<-c("Dep_30_40","Dep_15_20")
    
    ind2<-(1:nrow(OMi))[OMi[,1]%in%SR2 & OMi[,2]%in%Mov2 & OMi[,3]%in%Up2 & OMi[,4]%in%Rec2 & OMi[,5]%in%CBias2 & OMi[,6]%in%MMat2 & OMi[,7]%in%Dep2]
  
    P1_1 <- switch(input$P1, 
                   "Y" = apply(Y[,ind1]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind1],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind1,]>1),dim(F_FMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind1,]>-10000),dim(F_FMSY[,ind1,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind1,]<1),dim(B_BMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind1,]>-10000),dim(B_BMSY[,ind1,])),1,sum,na.rm=T),
                   "Dep" =  apply(Dep[,ind1],1,mean,na.rm=T))
  
    P1_2 <- switch(input$P2, 
                   "Y" = apply(Y[,ind1]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind1],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind1,]>1),dim(F_FMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind1,]>-10000),dim(F_FMSY[,ind1,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind1,]<1),dim(B_BMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind1,]>-10000),dim(B_BMSY[,ind1,])),1,sum,na.rm=T),
                   "Dep"=  apply(Dep[,ind1],1,mean,na.rm=T))
    
    P1_3 <- switch(input$P3, 
                   "Y" = apply(Y[,ind1]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind1],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind1,]>1),dim(F_FMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind1,]>-10000),dim(F_FMSY[,ind1,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind1,]<1),dim(B_BMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind1,]>-10000),dim(B_BMSY[,ind1,])),1,sum,na.rm=T),
                   "Dep"=  apply(Dep[,ind1],1,mean,na.rm=T))
    
    P1_4 <- switch(input$P4, 
                   "Y" = apply(Y[,ind1]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind1],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind1,]>1),dim(F_FMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind1,]>-10000),dim(F_FMSY[,ind1,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind1,]<1),dim(B_BMSY[,ind1,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind1,]>-10000),dim(B_BMSY[,ind1,])),1,sum,na.rm=T),
                   "Dep"=  apply(Dep[,ind1],1,mean,na.rm=T))
  
    P2_1 <- switch(input$P1, 
                   "Y" = apply(Y[,ind2]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind2],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind2,]>1),dim(F_FMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind2,]>-10000),dim(F_FMSY[,ind2,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind2,]<1),dim(B_BMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind2,]>-10000),dim(B_BMSY[,ind2,])),1,sum,na.rm=T),
                   "Dep"=  apply(Dep[,ind2],1,mean,na.rm=T))
    
    P2_2 <- switch(input$P2, 
                   "Y" = apply(Y[,ind2]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind2],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind2,]>1),dim(F_FMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind2,]>-10000),dim(F_FMSY[,ind2,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind2,]<1),dim(B_BMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind2,]>-10000),dim(B_BMSY[,ind2,])),1,sum,na.rm=T),
                   "Dep"=  apply(Dep[,ind2],1,mean,na.rm=T))
    
    P2_3 <- switch(input$P3, 
                   "Y" = apply(Y[,ind2]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind2],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind2,]>1),dim(F_FMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind2,]>-10000),dim(F_FMSY[,ind2,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind2,]<1),dim(B_BMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind2,]>-10000),dim(B_BMSY[,ind2,])),1,sum,na.rm=T),
                   "Dep"=  apply(Dep[,ind2],1,mean,na.rm=T))
    
    P2_4 <- switch(input$P4, 
                   "Y" = apply(Y[,ind2]/1000,1,mean,na.rm=T),
                   "AAVY" = apply(AAVY[,ind2],1,mean,na.rm=T),
                   "F_FMSY" = apply(array(as.integer(F_FMSY[,ind2,]>1),dim(F_FMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(F_FMSY[,ind2,]>-10000),dim(F_FMSY[,ind2,])),1,sum,na.rm=T),
                   "B_BMSY" = apply(array(as.integer(B_BMSY[,ind2,]<1),dim(B_BMSY[,ind2,])),1,sum,na.rm=T)/apply(array(as.numeric(B_BMSY[,ind2,]>-10000),dim(B_BMSY[,ind2,])),1,sum,na.rm=T),
                   "Dep"=  apply(Dep[,ind2],1,mean,na.rm=T))
    
    xlab1<-switch(input$P1, 
                  "Y" = "Mean yield (,000 t)",
                  "AAVY" = "Av. Ann. Var. Yd.",
                  "F_FMSY" =  "Prob. overfishing",
                  "B_BMSY" = "Prob. overfished",
                  "Dep"=  "Depletion")
    
    ylab1<-switch(input$P2, 
                  "Y" = "Mean yield (,000 t)",
                  "AAVY" = "Av. Ann. Var. Yd.",
                  "F_FMSY" =  "Prob. overfishing",
                  "B_BMSY" = "Prob. overfished",
                  "Dep"=  "Depletion")
    
    xlab2<-switch(input$P3, 
                  "Y" = "Mean yield (,000 t)",
                  "AAVY" = "Av. Ann. Var. Yd.",
                  "F_FMSY" =  "Prob. overfishing",
                  "B_BMSY" = "Prob. overfished",
                  "Dep"=  "Depletion")
    
    ylab2<-switch(input$P4, 
                  "Y" = "Mean yield (,000 t)",
                  "AAVY" = "Av. Ann. Var. Yd.",
                  "F_FMSY" =  "Prob. overfishing",
                  "B_BMSY" = "Prob. overfished",
                  "Dep"=  "Depletion")
 
    #layout(t(matrix(c(3,1,1,1,3,3,1,1,1,3,3,1,1,1,3,3,2,2,2,3,3,2,2,2,3,3,2,2,2,3),c(5,4))))
    
    xlim1<-range(c(P1_1,P2_1),na.rm=T)
    ylim1<-range(c(P1_2,P2_2),na.rm=T)
    
    par(mfrow=c(1,2),mar=c(4,4,0.05,0.05),omi=c(0.01,0.05,1,0.01))  
    
    plot(1,1,xlab=xlab1,ylab=ylab1,xlim=xlim1,ylim=ylim1,col='white')
    text(P1_1,P1_2,MPs,col="#0000ff70",font=2)
    text(P2_1,P2_2,MPs,col="#ff000070",font=2)
    
    mtext("Plot 1",3,line=0.5)
    
    xlim2<-range(c(P1_3,P2_3),na.rm=T)
    ylim2<-range(c(P1_4,P2_4),na.rm=T)
    
    plot(1,1,xlab=xlab2,ylab=ylab2,xlim=xlim2,ylim=ylim2,col='white')
    
    text(P1_3,P1_4,MPs,col="#0000ff70",font=2)
    text(P2_3,P2_4,MPs,col="#ff000070",font=2)
    
    mtext("Plot 2",3,line=0.5)
    
   
   
  
  },res=80)

  output$Proplot<-renderPlot({
    
    # Tplot1
    SR1<-input$SR1
    if(SR1=="ALL")SR1<-c("BH7","BH98")
    Mov1<-input$Mov1
    if(Mov1=="ALL")Mov1<-c("Grav","Frac")
    Up1<-input$Up1
    if(Up1=="ALL")Up1<-c("Up3","Up4")
    Rec1<-input$Rec1
    if(Rec1=="ALL")Rec1<-c("StableR","LowR")
    CBias1<-input$CBias1
    if(CBias1=="ALL")CBias1<-c("NoCbias","SPCbias")
    MMat1<-input$MMat1
    if(MMat1=="ALL")MMat1<-c("Mconst-AMhigh-Mage-AMlow","Mconst-AMhigh-Mconst-AMhigh", "Mage-AMlow-Mage-AMlow")
    Dep1<-input$Dep1
    if(Dep1=="ALL")Dep1<-c("Dep_30_40","Dep_15_20")
    
    ind1<-(1:nrow(OMi))[OMi[,1]%in%SR1 & OMi[,2]%in%Mov1 & OMi[,3]%in%Up1 & OMi[,4]%in%Rec1 & OMi[,5]%in%CBias1 & OMi[,6]%in%MMat1 & OMi[,7]%in%Dep1]
    
    
    # Tplot2  
    SR2<-input$SR2
    if(SR2=="ALL")SR2<-c("BH7","BH98")
    Mov2<-input$Mov2
    if(Mov2=="ALL")Mov2<-c("Grav","Frac")
    Up2<-input$Up2
    if(Up2=="ALL")Up2<-c("Up3","Up4")
    Rec2<-input$Rec2
    if(Rec2=="ALL")Rec2<-c("StableR","LowR")
    CBias2<-input$CBias2
    if(CBias2=="ALL")CBias2<-c("NoCbias","SPCbias")
    MMat2<-input$MMat2
    if(MMat2=="ALL")MMat2<-c("Mconst-AMhigh-Mage-AMlow","Mconst-AMhigh-Mconst-AMhigh", "Mage-AMlow-Mage-AMlow")
    Dep2<-input$Dep2
    if(Dep2=="ALL")Dep2<-c("Dep_30_40","Dep_15_20")
    
    ind2<-(1:nrow(OMi))[OMi[,1]%in%SR2 & OMi[,2]%in%Mov2 & OMi[,3]%in%Up2 & OMi[,4]%in%Rec2 & OMi[,5]%in%CBias2 & OMi[,6]%in%MMat2 & OMi[,7]%in%Dep2]
     
    
    # The Biomass / BMSY projection plots
    par(mfcol=c(4,2),mar=c(3,3,0.05,0.05),omi=c(0.05,0.05,0.6,0.05)) 
    
    makeTransparent<-function(someColor, alpha=100){
      newColor<-col2rgb(someColor)
      apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                  blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
    }
    
    # make the projection plots
    
    MP1=match(input$MP1,MPs)
    MP2=match(input$MP2,MPs)
    MP3=match(input$MP3,MPs)
    MP4=match(input$MP4,MPs)
    
    #FMSYr<-quantile(MSEobj@F_FMSY,c(0.001,0.90),na.rm=T)
    BMSYr<-c(0,3)#quantile(B_BMSY[c(MPt,MPb),unique(c(ind1,ind2)),],c(0.001,0.9),na.rm=T)
    
    colsse<-rainbow(100,start=0,end=0.36)[1:100]
    colB<-rep(colsse[100],ceiling(BMSYr[2]*100))
    colB[1:100]<-colsse
    colB<-makeTransparent(colB,60)
    #colsse<-rainbow(200,start=0,end=0.36)[200:1]
    #colF<-rep(colsse[200],ceiling(FMSYr[2]*100))
    #colF[1:200]<-colsse
    #colF<-makeTransparent(colF,60)
    
    lwdy<-2
    
   
    pind1<-sample(ind1,50,replace=TRUE)
    pind2<-sample(ind2,50,replace=TRUE)
    if(length(ind1)==length(ind2))if(sum(ind1==ind2)==length(ind1))pind2<-pind1
    proyears<-dim(F_FMSY)[3]
    
    plot(B_BMSY[MP1,pind1[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP1,pind1[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP1,pind1[i],],col=colB[ceiling(B_BMSY[MP1,pind1[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP1,bty='n')
    
    mtext("OM set A",3,line=1,col="blue",cex=0.8)    
    
    plot(B_BMSY[MP2,pind1[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP2,pind1[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP2,pind1[i],],col=colB[ceiling(B_BMSY[MP2,pind1[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP2,bty='n')
    
    plot(B_BMSY[MP3,pind1[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP3,pind1[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP3,pind1[i],],col=colB[ceiling(B_BMSY[MP3,pind1[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP3,bty='n')
    
    plot(B_BMSY[MP4,pind1[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP4,pind1[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP4,pind1[i],],col=colB[ceiling(B_BMSY[MP4,pind1[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP4,bty='n')
    
    
    plot(B_BMSY[MP1,pind2[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP1,pind2[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP1,pind2[i],],col=colB[ceiling(B_BMSY[MP1,pind2[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP1,bty='n')
    
    mtext("OM set B",3,line=1,col="red",cex=0.8)
    
    plot(B_BMSY[MP2,pind2[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP2,pind2[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP2,pind2[i],],col=colB[ceiling(B_BMSY[MP2,pind2[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP2,bty='n')
    
    plot(B_BMSY[MP3,pind2[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP3,pind2[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP3,pind2[i],],col=colB[ceiling(B_BMSY[MP3,pind2[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP3,bty='n')
    
    plot(B_BMSY[MP4,pind2[1],],ylim=BMSYr,col=colB[ceiling(B_BMSY[MP4,pind2[1],proyears]*100)],type='l',lwd=lwdy,ylab="",xlab="")
    abline(h=1,col=makeTransparent("grey",80),lwd=3)
    for(i in 2:50)lines(B_BMSY[MP4,pind2[i],],col=colB[ceiling(B_BMSY[MP4,pind2[i],proyears]*100)],lwd=lwdy)
    legend('topright',legend=input$MP4,bty='n')
    
    
    
    mtext("B/BMSY",2,outer=T,line=-0.6,cex=0.8)
    
    mtext("Projection year",1,outer=T,line=-0.6,cex=0.8)
    #if(is.na(nam))mtext(deparse(substitute(MSEobj)),3,outer=T,line=0.3,font=2)
    #if(!is.na(nam))mtext(MSEobj@Name,3,outer=T,line=0.3,font=2)
    
    
    
  },res=100)
  
  output$Wormplot<-renderPlot({
    
    # Tplot1
    SR1<-input$SR1
    if(SR1=="ALL")SR1<-c("BH7","BH98")
    Mov1<-input$Mov1
    if(Mov1=="ALL")Mov1<-c("Grav","Frac")
    Up1<-input$Up1
    if(Up1=="ALL")Up1<-c("Up3","Up4")
    Rec1<-input$Rec1
    if(Rec1=="ALL")Rec1<-c("StableR","LowR")
    CBias1<-input$CBias1
    if(CBias1=="ALL")CBias1<-c("NoCbias","SPCbias")
    MMat1<-input$MMat1
    if(MMat1=="ALL")MMat1<-c("Mconst-AMhigh-Mage-AMlow","Mconst-AMhigh-Mconst-AMhigh", "Mage-AMlow-Mage-AMlow")
    Dep1<-input$Dep1
    if(Dep1=="ALL")Dep1<-c("Dep_30_40","Dep_15_20")
    
    ind1<-(1:nrow(OMi))[OMi[,1]%in%SR1 & OMi[,2]%in%Mov1 & OMi[,3]%in%Up1 & OMi[,4]%in%Rec1 & OMi[,5]%in%CBias1 & OMi[,6]%in%MMat1 & OMi[,7]%in%Dep1]
    
    
    # Tplot2  
    SR2<-input$SR2
    if(SR2=="ALL")SR2<-c("BH7","BH98")
    Mov2<-input$Mov2
    if(Mov2=="ALL")Mov2<-c("Grav","Frac")
    Up2<-input$Up2
    if(Up2=="ALL")Up2<-c("Up3","Up4")
    Rec2<-input$Rec2
    if(Rec2=="ALL")Rec2<-c("StableR","LowR")
    CBias2<-input$CBias2
    if(CBias2=="ALL")CBias2<-c("NoCbias","SPCbias")
    MMat2<-input$MMat2
    if(MMat2=="ALL")MMat2<-c("Mconst-AMhigh-Mage-AMlow","Mconst-AMhigh-Mconst-AMhigh", "Mage-AMlow-Mage-AMlow")
    Dep2<-input$Dep2
    if(Dep2=="ALL")Dep2<-c("Dep_30_40","Dep_15_20")
    
    ind2<-(1:nrow(OMi))[OMi[,1]%in%SR2 & OMi[,2]%in%Mov2 & OMi[,3]%in%Up2 & OMi[,4]%in%Rec2 & OMi[,5]%in%CBias2 & OMi[,6]%in%MMat2 & OMi[,7]%in%Dep2]
     
    Bref<-input$Bref
    LB<-input$LB
    UB<-input$UB
    
    par(mfcol=c(nMP,2),mar=c(0.1,0.1,0.1,0.1),omi=c(0.6,0,0.6,0))
    
    # OM set A ----------------------
    
    Bprob<-apply(B_BMSY[,ind1,]>Bref,c(1,3),sum)/dim(B_BMSY[,ind1,])[2]
    
    BLB<-Bprob>LB
    BUB<-Bprob>UB
    
    col<-array('red',dim(Bprob))
    col[BLB&!BUB]="yellow"
    col[BUB]="green"
    
    for(i in 1:nMP){
      plot(c(1,ny+1),c(-1,1),col='white',axes=F)
      # abline(h=0)
      if(i==1)mtext("OM set A",3,line=1,col="blue",cex=0.8)    
      
      for(ys in 1:ny){
        x<-c(ys-1,ys,ys,ys-1)
        y<-c(rep(Bprob[i,ys],2),rep(-Bprob[i,ys],2))
        pol<-data.frame(x,y)
        polygon(pol,col=col[i,ys],border=NA)
      }
      
      legend('topleft',legend=MPs[i],bty='n')
    }
    
    axis(1,pretty(1:ny),pretty(1:ny))
    
    # OM set B ----------------------
    
    Bprob<-apply(B_BMSY[,ind2,]>Bref,c(1,3),sum)/dim(B_BMSY[,ind2,])[2]
    
    nMP<-dim(B_BMSY)[1]
    ny<-dim(B_BMSY)[3]
    
    BLB<-Bprob>LB
    BUB<-Bprob>UB
    
    col<-array('red',dim(Bprob))
    col[BLB&!BUB]="yellow"
    col[BUB]="green"
    
    for(i in 1:nMP){
      plot(c(1,ny+1),c(-1,1),col='white',axes=F)
      # abline(h=0)
      if(i==1)mtext("OM set B",3,line=1,col="red",cex=0.8)    
      
      for(ys in 1:ny){
        x<-c(ys-1,ys,ys,ys-1)
        y<-c(rep(Bprob[i,ys],2),rep(-Bprob[i,ys],2))
        pol<-data.frame(x,y)
        polygon(pol,col=col[i,ys],border=NA)
      }
      
      legend('topleft',legend=MPs[i],bty='n')
    }
    
      
    axis(1,pretty(1:ny),pretty(1:ny))
    mtext("Projection year",1,outer=T,line=2.2)
    
   
  },res=100)
  
  
  # Button set A --------------------------------
 
  # Reference set ---------------
  
  observeEvent(input$Ref1,{ 
    updateSelectInput(session, "SR1", choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Mov1", choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav","ALL" = "ALL"),selected = "Grav")
    updateSelectInput(session, "Up1",  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4","ALL" = "ALL"),selected = "Up3")
    updateSelectInput(session, "Rec1", choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR","ALL" = "ALL"), selected = "StableR")
    updateSelectInput(session, "CBias1", choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias","ALL" = "ALL"), selected = "NoCbias")
    updateSelectInput(session, "MMat1", choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                       "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                       "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                       "ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "Dep1", choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                      "ALL" = "ALL"), selected = "ALL")
  })
  
  # Robustness set ---------------
  
  observeEvent(input$Rob1,{ 
    updateSelectInput(session, "SR1", choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Mov1", choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav","ALL" = "ALL"),selected = "Frac")
    updateSelectInput(session, "Up1",  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4","ALL" = "ALL"),selected = "Up4")
    updateSelectInput(session, "Rec1", choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR","ALL" = "ALL"), selected = "LowR")
    updateSelectInput(session, "CBias1", choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias","ALL" = "ALL"), selected = "SPCbias")
    updateSelectInput(session, "MMat1", choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                       "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                       "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                       "ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "Dep1", choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                      "ALL" = "ALL"), selected = "ALL")
  })
  
  # Reset to defaults ---------------
  
  observeEvent(input$Res1,{ 
    updateSelectInput(session, "SR1", choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Mov1", choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Up1",  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Rec1", choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR","ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "CBias1", choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias","ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "MMat1", choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                       "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                       "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                       "ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "Dep1", choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                      "ALL" = "ALL"), selected = "ALL")
  })
  
  
  # Button set B --------------------------------
  
  # Reference set ---------------
  
  observeEvent(input$Ref2,{ 
    updateSelectInput(session, "SR2", choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Mov2", choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav","ALL" = "ALL"),selected = "Grav")
    updateSelectInput(session, "Up2",  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4","ALL" = "ALL"),selected = "Up3")
    updateSelectInput(session, "Rec2", choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR","ALL" = "ALL"), selected = "StableR")
    updateSelectInput(session, "CBias2", choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias","ALL" = "ALL"), selected = "NoCbias")
    updateSelectInput(session, "MMat2", choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                       "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                       "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                       "ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "Dep2", choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                      "ALL" = "ALL"), selected = "ALL")
  })
  
  # Robustness set ---------------
  
  observeEvent(input$Rob2,{ 
    updateSelectInput(session, "SR2", choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Mov2", choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav","ALL" = "ALL"),selected = "Frac")
    updateSelectInput(session, "Up2",  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4","ALL" = "ALL"),selected = "Up4")
    updateSelectInput(session, "Rec2", choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR","ALL" = "ALL"), selected = "LowR")
    updateSelectInput(session, "CBias2", choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias","ALL" = "ALL"), selected = "SPCbias")
    updateSelectInput(session, "MMat2", choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                       "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                       "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                       "ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "Dep1", choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                      "ALL" = "ALL"), selected = "ALL")
  })
  
  # Reset to defaults ---------------
  
  observeEvent(input$Res2,{
    updateSelectInput(session, "SR2", choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Mov2", choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Up2",  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4","ALL" = "ALL"),selected = "ALL")
    updateSelectInput(session, "Rec2", choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR","ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "CBias2", choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias","ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "MMat2", choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                   "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                   "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                   "ALL" = "ALL"), selected = "ALL")
    updateSelectInput(session, "Dep2", choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                      "ALL" = "ALL"), selected = "ALL")
  })
  
  
})