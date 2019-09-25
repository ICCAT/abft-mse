

library(shiny)
library(DT)
library(ABTMSE)
# setwd("C:/Users/tcar_/Dropbox/abft-mse/R_package/ABTMSE/inst/shiny_apps/ABTMSE")


source("./global.R")

shinyServer(function(input, output, session){

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  Tplot<-function()plot(1:10)
  OMnames<<-c(1:6,10:15,19:24,28:33)
  nOMs<-length(OMnames)
  OMcode<<-apply(OMgrid,1,function(x)paste0(x,collapse=" "))

  getind1<-function() (1:nOMs)[(OMgrid[,1]%in%input$Rec1 & OMgrid[,2]%in%input$SSB1 & OMgrid[,3]%in%input$SM1)]

  output$test1<-renderText({
    OMnames[getind1()]
  })

  output$test12<-renderText({
    paste(OMcode[getind1()],collapse=", ")
  })

  getind2<-function() (1:nOMs)[(OMgrid[,1]%in%input$Rec2 & OMgrid[,2]%in%input$SSB2 & OMgrid[,3]%in%input$SM2)]

  output$test2<-renderText({
    OMnames[getind2()]
  })

  output$test22<-renderText({
    paste(OMcode[getind2()],collapse=", ")
  })

  maketable<-function(pp,tabno){

    if(tabno==1)ind<<-getind1()
    if(tabno==2)ind<<-getind2()

    pind<-(1:length(pnames))[pnames%in%c(input$PM1,input$PM2,input$PM3,input$PM4)]

    if(length(ind)>1){
      if(tabno==1){
        tab<-apply(MET[,ind,pp,,pind],3:4,quantile,p=0.5)
      }else{
        tab<-apply(MET[,ind,pp,,pind],3:4,quantile,p=0.5)
      }
    }else{
      if(tabno==1){
        tab<-apply(MET[,ind,pp,,pind],2:3,quantile,p=0.5)
      }else{
        tab<-apply(MET[,ind,pp,,pind],2:3,quantile,p=0.5)
      }
    }
    tab<-as.data.frame(tab,row.names=MPnames)
    names(tab)<-pnames[pind]
    #mult<-c(rep(1E-6,4),rep(100,6),rep(1,4))[pind]
    #tab[,1:4]<-tab[,1:4]*rep(mult,each=nrow(tab))
    #tab[,5:10]<-tab[,5:10]*100
    round(tab,1)
  }

  output$table1E <- DT::renderDataTable(maketable(pp=1,tabno=1),options = list(dom = 't'))#,rownames=T,spacing="xs",digits=0,align="c")
  output$table1W <- DT::renderDataTable(maketable(pp=2,tabno=1),options = list(dom = 't'))#,rownames=T,spacing="xs",digits=0)
  output$table2E <- DT::renderDataTable(maketable(pp=1,tabno=2),options = list(dom = 't'))#,rownames=T,spacing="xs",digits=0)
  output$table2W <- DT::renderDataTable(maketable(pp=2,tabno=2),options = list(dom = 't'))#,rownames=T,spacing="xs",digits=0)

  observeEvent(input$OM1,{
    OMind<-match(input$OM1,OMnames)
    updateCheckboxGroupInput(session,"Rec1",selected=OMgrid[OMind,1])
    updateCheckboxGroupInput(session,"SSB1",selected=OMgrid[OMind,2])
    updateCheckboxGroupInput(session,"SM1",selected=OMgrid[OMind,3])

  })

  observeEvent(input$OM2,{
    OMind<-match(input$OM2,OMnames)
    updateCheckboxGroupInput(session,"Rec2",selected=OMgrid[OMind,1])
    updateCheckboxGroupInput(session,"SSB2",selected=OMgrid[OMind,2])
    updateCheckboxGroupInput(session,"SM2",selected=OMgrid[OMind,3])

  })

  # turn on all reference OMs
  observeEvent(input$oRef1,{
    updateCheckboxGroupInput(session,"Rec1",selected=unique(OMgrid[,1]))
    updateCheckboxGroupInput(session,"SSB1",selected=unique(OMgrid[,2]))
    updateCheckboxGroupInput(session,"SM1",selected=unique(OMgrid[,3]))
  })

  observeEvent(input$oRef2,{
    updateCheckboxGroupInput(session,"Rec2",selected=unique(OMgrid[,1]))
    updateCheckboxGroupInput(session,"SSB2",selected=unique(OMgrid[,2]))
    updateCheckboxGroupInput(session,"SM2",selected=unique(OMgrid[,3]))
  })

  # turn on all robustness OMs
  observeEvent(input$oRob1,{
    updateCheckboxGroupInput(session,"Rob1",selected=paste0("R",1:4))
  })

  observeEvent(input$oRob2,{
    updateCheckboxGroupInput(session,"Rob2",selected=paste0("R",1:4))
  })

  # Reset to OM1
  observeEvent(input$oRes1,{
    updateCheckboxGroupInput(session,"Rec1",selected=OMgrid[1,1])
    updateCheckboxGroupInput(session,"SSB1",selected=OMgrid[1,2])
    updateCheckboxGroupInput(session,"SM1",selected=OMgrid[1,3])
    updateCheckboxGroupInput(session,"Rob1",selected=1:4)
  })

  observeEvent(input$oRes2,{
    updateCheckboxGroupInput(session,"Rec2",selected=OMgrid[1,1])
    updateCheckboxGroupInput(session,"SSB2",selected=OMgrid[1,2])
    updateCheckboxGroupInput(session,"SM2",selected=OMgrid[1,3])
    updateCheckboxGroupInput(session,"Rob2",selected=1:4)
  })

  # Zeh plots
  custombar<-function(dat,MPnams,tickwd1=0.085,tickwd2=0.045,lwd1=2,lwd2=1,xlab=T,ylim=NA,ynam=""){

    par(mar=c(12,5,0.5,0.5))
    nMPer<-nrow(dat)
    incr<-(max(dat)-min(dat))*0.05
    if(is.na(ylim[1]))ylim=range(dat)
    plot(dat[,5],ylim=ylim,xlim=c(0.25,nMPer+0.25),col='white',axes=F,ylab="",xlab="")

    if(xlab){
      axis(1,-1:(nMPer+1),c("","",MPnams,""),las=2,font=2,cex.axis=1)
    }else{
      axis(1,-1:(nMPer+1),rep("",nMPer+3))
    }

    incr<-(max(dat)-min(dat))*0.2
    yp<-pretty(seq(min(dat)-incr,max(dat)+incr,length.out=12))
    axis(2,yp,yp,las=2)
    big<-1E20
    polygon(c(-big,big,big,-big),c(-big,-big,big,big),col='lightsteelblue1')
    points(dat[,3],pch=19,cex=1.1)
    abline(h=yp,col='white')

    for(i in 1:nMPer){

      lines(c(i-tickwd1/2,i+tickwd1/2),c(dat[i,2],dat[i,2]),lwd=lwd1) # lower interquartile
      lines(c(i-tickwd1/2,i+tickwd1/2),c(dat[i,4],dat[i,4]),lwd=lwd1) # upper interquartile

      lines(c(i-tickwd2/2,i+tickwd2/2),c(dat[i,1],dat[i,1]),lwd=lwd1) # lower 80%
      lines(c(i-tickwd2/2,i+tickwd2/2),c(dat[i,5],dat[i,5]),lwd=lwd2) # upper 80%

      lines(c(i,i),c(dat[i,1],dat[i,5]),lwd=lwd2) # 80%
      lines(c(i,i),c(dat[i,2],dat[i,4]),lwd=lwd1) # 80%

    }

    mtext(ynam,2,line=4)

  }

  ZehP<-function(MET,Pnam,pp,Ino){

    PMind<<-match(input$Zeh_PM,pnames)
    if(Ino==1)ind<<-getind1()
    if(Ino==2)ind<<-getind2()
    res<-MET[,ind,pp,,PMind]
    ylim=range(c(MET[,getind1(),pp,,PMind],MET[,getind2(),pp,,PMind]))
    ndim<-length(dim(res))

    if(ndim==2)store<-apply(res,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
    if(ndim==3)store<-apply(res,3,quantile,p=c(0.05,0.25,0.5,0.75,0.95))

    custombar(dat=t(store),MPnams=MPnames,ylim=ylim,ynam=input$Zeh_PM)

   # mtext("Candidate Management Procedure",1,line=6.8,font=2,outer=T)

  }


  ZehOM<-function(MET,Pnam,pp){

    PMind<<-match(input$Zeh_PM1,pnames)
    MPind<<-match(input$Zeh_MP1,MPnames)
    ind<<-getind1()

    res<-MET[,ind,pp,MPind,PMind]
    ndim<-length(dim(res))

    if(ndim==2){
      store<-apply(res,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
    }else{
      store<-matrix(quantile(res,c(0.05,0.25,0.5,0.75,0.95)),ncol=1)
    }
    custombar(dat=t(store),MPnams=OMcode[ind],ynam=input$Zeh_PM1)

    mtext("Operating model",1,line=4,font=1)

  }

  Zeh_sz=500
  output$Zeh1E<-renderPlot(ZehP(MET,MPnames,1,1),height=Zeh_sz*0.8,width=Zeh_sz)
  output$Zeh2E<-renderPlot(ZehP(MET,MPnames,1,2),height=Zeh_sz*0.8,width=Zeh_sz)
  output$Zeh1W<-renderPlot(ZehP(MET,MPnames,2,1),height=Zeh_sz*0.8,width=Zeh_sz)
  output$Zeh2W<-renderPlot(ZehP(MET,MPnames,2,2),height=Zeh_sz*0.8,width=Zeh_sz)

  # Button set 1
  output$ZehE<-renderPlot(ZehOM(MET,input$Zeh_PM1,1),height=Zeh_sz*0.8,width=Zeh_sz*1.5)
  output$ZehW<-renderPlot(ZehOM(MET,input$Zeh_PM1,2),height=Zeh_sz*0.8,width=Zeh_sz*1.5)


  # Trade-off plot
  Tplot<-function(x=1:10,y=1:10,xlab="xlab",ylab="ylab",MPnames=as.character(1:10),bars=F,MPlabs=F){
    par(mai=c(1.5,1.5,0.01,0.01))
    #layout(matrix(c(1,2),nrow=1),widths=c(1,0.8))
    MPcols<<-rep(c("black","red","green","blue","orange","grey","purple","brown"),10)
    if(!MPlabs)plot(x[2,],y[2,],col=MPcols,xlab="",ylab="",cex=1.3,pch=19)
    if(MPlabs){
      plot(x[2,],y[2,],col="white",xlab="",ylab="")
      text(x[2,],y[2,],MPnames,col=MPcols,font=2)
    }
    mtext(input$T_PMx,1,line=1.6)
    mtext(input$T_PMy,2,line=1.6)

    if(bars){
      for(MP in 1:ncol(x)){
        lines(c(x[1,MP],x[3,MP]),rep(y[2,MP],2),col=MPcols[MP])
        lines(rep(x[2,MP],2),c(y[1,MP],y[3,MP]),col=MPcols[MP])
      }
    }
  }

  Tleg<-function(MPcols){
    MPcols<-rep(c("black","red","green","blue","orange","grey","purple","brown"),10)
    par(mai=rep(0.01,4))
    plot(1,1,col='white',xlab="",ylab="",axes=F)
    if(!input$labs)legend('center',legend=MPnames,text.col= MPcols,bty='n')
  }

  Twrap<-function(pp,tabno){

    PMx<-match(input$T_PMx,pnames)
    PMy<-match(input$T_PMy,pnames)
    if(tabno==1)ind<-getind1()
    if(tabno==2)ind<-getind2()
    datx<-MET[,ind,pp,,PMx]
    daty<-MET[,ind,pp,,PMy]

    if(length(dim(datx))==3){
      x<-apply(datx,3,quantile,p=c(0.05,0.5,0.95))
      y<-apply(daty,3,quantile,p=c(0.05,0.5,0.95))
    }else{
      x<-apply(datx,2,quantile,p=c(0.05,0.5,0.95))
      y<-apply(daty,2,quantile,p=c(0.05,0.5,0.95))
    }

    Tplot(x,y,xlab=input$T_PMx,ylab=input$T_PMy,MPnames=MPnames,bars=input$bars,MPlabs=input$labs)
    #Tplot(x,y,xlab="",ylab="",MPnames=1:10,bars=F,MPlabs=F)
  }

  output$Tplot11<-renderPlot(Twrap(pp=1,tabno=1), height=Zeh_sz*0.8,width=Zeh_sz*0.75)
  output$Tplot12<-renderPlot(Twrap(pp=1,tabno=2), height=Zeh_sz*0.8,width=Zeh_sz*0.75)
  output$Tplot21<-renderPlot(Twrap(pp=2,tabno=1), height=Zeh_sz*0.8,width=Zeh_sz*0.75)
  output$Tplot22<-renderPlot(Twrap(pp=2,tabno=2), height=Zeh_sz*0.8,width=Zeh_sz*0.75)
  output$Tleg<-renderPlot(Tleg(),width=Zeh_sz*0.4)
  output$Tleg2<-renderPlot(Tleg(),width=Zeh_sz*0.4)

})
