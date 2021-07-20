
library(shiny)
library(DT)
library(fmsb)
library(shinyalert)
library(ggplot2)
library(viridis)
library(gridExtra)

options(shiny.maxRequestSize=1000*1024^2)

source("./global.R")

shinyServer(function(input, output, session){

  source("./Source/Figures.R",local=TRUE)
  source("./Source/Tables.R",local=TRUE)
  source("./Source/Misc.R",local=TRUE)

  changed<-reactiveVal(0)
  Design<-CompRes$Design
  OMgrid<-Design$Design_Ref

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

 # Tplot<-function()plot(1:10)
  nOMs<-length(OMnames)+length(ROMnames)
  OMcode<<-apply(OMgrid,1,function(x)paste0(x,collapse=" "))

  output$test1<-renderText({
    c(OMnames,ROMnames)[getind1()]
  })

  output$test12<-renderText({
    paste(c(OMcode,ROMcode)[getind1()],collapse=", ")
  })

  output$test2<-renderText({
    c(OMnames,ROMnames)[getind2()]
  })

  output$test22<-renderText({
    paste(c(OMcode,ROMcode)[getind2()],collapse=", ")
  })

  output$CMPhelp <- DT::renderDataTable(DT::datatable(data.frame(Code=MPnames,Description=CompRes$CMPdesc)))
  output$PMhelp <- DT::renderDataTable(DT::datatable(data.frame(Code=pnames,Description=CompRes$pdesc),options=list(pageLength=25)))

  output$table1E <- DT::renderDataTable(DT::datatable(maketable(pp=1,tabno=1),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel')))) #,rownames=T,spacing="xs",digits=0,align="c")
  output$table1W <- DT::renderDataTable(DT::datatable(maketable(pp=2,tabno=1),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel'))))#,rownames=T,spacing="xs",digits=0)
  output$table2E <- DT::renderDataTable(DT::datatable(maketable(pp=1,tabno=2),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel'))))#,rownames=T,spacing="xs",digits=0)
  output$table2W <- DT::renderDataTable(DT::datatable(maketable(pp=2,tabno=2),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel'))))#,rownames=T,spacing="xs",digits=0)


  output$table2_1E <- DT::renderDataTable(DT::datatable(maketable2(pp=1,tabno=1),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel')))) #,rownames=T,spacing="xs",digits=0,align="c")
  output$table2_1W <- DT::renderDataTable(DT::datatable(maketable2(pp=2,tabno=1),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel'))))#,rownames=T,spacing="xs",digits=0)
  output$table2_2E <- DT::renderDataTable(DT::datatable(maketable2(pp=1,tabno=2),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel'))))#,rownames=T,spacing="xs",digits=0)
  output$table2_2W <- DT::renderDataTable(DT::datatable(maketable2(pp=2,tabno=2),extensions='Buttons',options = list(dom = 'Btpl',pageLength=25,buttons = c('csv', 'excel'))))#,rownames=T,spacing="xs",digits=0)




  observeEvent(input$OM1,{
    OMind<-match(input$OM1,OMnames)
    updateCheckboxGroupInput(session,"Rec1",selected=OMgrid[OMind,1])
    updateCheckboxGroupInput(session,"SM1",selected=OMgrid[OMind,2])
    updateCheckboxGroupInput(session,"Scale1",selected=OMgrid[OMind,3])
    updateCheckboxGroupInput(session,"Comp1",selected=OMgrid[OMind,4])

  })

  observeEvent(input$OM2,{
    OMind<-match(input$OM2,OMnames)
    updateCheckboxGroupInput(session,"Rec2",selected=OMgrid[OMind,1])
    updateCheckboxGroupInput(session,"SM2",selected=OMgrid[OMind,2])
    updateCheckboxGroupInput(session,"Scale2",selected=OMgrid[OMind,3])
    updateCheckboxGroupInput(session,"Comp2",selected=OMgrid[OMind,4])

  })

  # turn on all reference OMs
  observeEvent(input$oRef1,{
    if(input$oRef1  == 0 | input$oRef1%%2 == 0){
      updateCheckboxGroupInput(session,"Rec1",selected="")
      updateCheckboxGroupInput(session,"SM1",selected="")
      updateCheckboxGroupInput(session,"Scale1",selected="")
      updateCheckboxGroupInput(session,"Comp1",selected="")
    }else{
      updateCheckboxGroupInput(session,"Rec1",selected=OMgrid[,1])
      updateCheckboxGroupInput(session,"SM1",selected=OMgrid[,2])
      updateCheckboxGroupInput(session,"Scale1",selected=OMgrid[,3])
      updateCheckboxGroupInput(session,"Comp1",selected=OMgrid[,4])
    }
  })

  observeEvent(input$oRef2,{
    if(input$oRef2  == 0 | input$oRef2%%2 == 0){
      updateCheckboxGroupInput(session,"Rec2",selected="")
      updateCheckboxGroupInput(session,"SM2",selected="")
      updateCheckboxGroupInput(session,"Scale2",selected="")
      updateCheckboxGroupInput(session,"Comp2",selected="")
    }else{
      updateCheckboxGroupInput(session,"Rec2",selected=OMgrid[,1])
      updateCheckboxGroupInput(session,"SM2",selected=OMgrid[,2])
      updateCheckboxGroupInput(session,"Scale2",selected=OMgrid[,3])
      updateCheckboxGroupInput(session,"Comp2",selected=OMgrid[,4])
    }
  })

  # turn on all robustness OMs
  observeEvent(input$oRob1,{
    if(input$oRob1  == 0 | input$oRob1%%2 == 0){
      updateCheckboxGroupInput(session,"Rob1",selected="")
    }else{
      updateCheckboxGroupInput(session,"Rob1",selected=ROMnames)
    }
  })

  observeEvent(input$oRob2,{
    if(input$oRob2  == 0 | input$oRob2%%2 == 0){
      updateCheckboxGroupInput(session,"Rob2",selected="")
    }else{
      updateCheckboxGroupInput(session,"Rob2",selected=ROMnames)
    }
  })


  observeEvent(input$zeh_violin,{
    changed(10)
  })


  observeEvent(input$zehPM_violin,{
    changed(11)
  })

  observeEvent(input$StochDet,{
    #temp<-changed(1)
    if(input$StochDet=="Stochastic"){
      CompRes<<-readRDS("./data/CompRes.rda") # Can be loaded by user
    }else{
      CompRes<<-readRDS("./data/CompResd.rda") # Can be loaded by user
    }
    MET<<-CompRes$MET
    pnames<<-CompRes$pnames
    MPnames<<-CompRes$MPnames
    OMnames<<-CompRes$OMnames[!grepl("R",CompRes$OMnames)]
    ROMnames<<-CompRes$OMnames[grepl("R",CompRes$OMnames)]
    ROMcode<<-CompRes$ROMcode
    changed(changed()+1)

    updateCheckboxGroupInput(session,"CMPs",label=NULL,choices=MPnames, selected=MPnames,inline=T)

    updateSelectInput(session,"Zeh_MP1",selected=MPnames[2],choices=MPnames)
    updateSelectInput(session,"Zeh_MP2",selected="None",choices=c("None",MPnames))

    updateSelectInput(session,"ZehPM_MP1",selected=MPnames[1],choices=MPnames)
    updateSelectInput(session,"ZehPM_MP2",selected=MPnames[2],choices=MPnames)
    updateSelectInput(session,"ZehPM_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

    updateSelectInput(session,"S_MP1",selected=MPnames[1],choices=MPnames)
    updateSelectInput(session,"S_MP2",selected=MPnames[2],choices=MPnames)
    updateSelectInput(session,"S_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

    updateSelectInput(session,"Sm_MP1",selected=MPnames[1],choices=MPnames)
    updateSelectInput(session,"Sm_MP2",selected=MPnames[2],choices=MPnames)

    updateSelectInput(session,"R_MP1",selected=MPnames[1],choices=MPnames)
    updateSelectInput(session,"R_MP2",selected=MPnames[2],choices=MPnames)
    updateSelectInput(session,"R_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

    updateSelectInput(session,"REW_MP1",selected=MPnames[1],choices=MPnames)
    updateSelectInput(session,"REW_MP2",selected=MPnames[2],choices=MPnames)
    updateSelectInput(session,"REW_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)



  })

  observeEvent(input$resamp,{
    changed(changed()+1)
  })

  # Reset to OM1
  observeEvent(input$oRes1,{
    updateCheckboxGroupInput(session,"Rec1",selected=OMgrid[1,1])
    updateCheckboxGroupInput(session,"SM1",selected=OMgrid[1,2])
    updateCheckboxGroupInput(session,"Scale1",selected=OMgrid[1,3])
    updateCheckboxGroupInput(session,"Comp1",selected=OMgrid[1,4])
    updateCheckboxGroupInput(session,"Rob1",selected="")
  })

  observeEvent(input$oRes2,{
    updateCheckboxGroupInput(session,"Rec2",selected=OMgrid[1,1])
    updateCheckboxGroupInput(session,"SM2",selected=OMgrid[1,2])
    updateCheckboxGroupInput(session,"Scale2",selected=OMgrid[1,3])
    updateCheckboxGroupInput(session,"Comp2",selected=OMgrid[1,4])
    updateCheckboxGroupInput(session,"Rob2",selected="")
  })

  Zeh_sz=500
  output$Zeh1E<-renderPlot(ZehP(MET,MPnames,1,1),height=Zeh_sz*0.8)
  output$Zeh2E<-renderPlot(ZehP(MET,MPnames,1,2),height=Zeh_sz*0.8)
  output$Zeh1W<-renderPlot(ZehP(MET,MPnames,2,1),height=Zeh_sz*0.8)
  output$Zeh2W<-renderPlot(ZehP(MET,MPnames,2,2),height=Zeh_sz*0.8)

  output$ZehPM1E<-renderPlot(ZehPM(MET,MPnames,1,1),height=Zeh_sz*0.8)
  output$ZehPM2E<-renderPlot(ZehPM(MET,MPnames,1,2),height=Zeh_sz*0.8)
  output$ZehPM1W<-renderPlot(ZehPM(MET,MPnames,2,1),height=Zeh_sz*0.8)
  output$ZehPM2W<-renderPlot(ZehPM(MET,MPnames,2,2),height=Zeh_sz*0.8)


  # Button set 1
  output$ZehE<-renderPlot(ZehOM(MET,input$Zeh_PM1,1),height=Zeh_sz*0.8,res=64)
  output$ZehW<-renderPlot(ZehOM(MET,input$Zeh_PM1,2),height=Zeh_sz*0.8,res=64)

  output$Tplot11<-renderPlot(Twrap(pp=1,tabno=1), height=Zeh_sz*0.7)
  output$Tplot12<-renderPlot(Twrap(pp=1,tabno=2), height=Zeh_sz*0.7)
  output$Tplot21<-renderPlot(Twrap(pp=2,tabno=1), height=Zeh_sz*0.7)
  output$Tplot22<-renderPlot(Twrap(pp=2,tabno=2), height=Zeh_sz*0.7)
  output$Tleg<-renderPlot(Tleg())
  output$Tleg2<-renderPlot(Tleg())


  # Projection plots

  output$YBP1E<-renderPlot(YBP(1,1,leg=T),height=Zeh_sz*0.7)
  output$YBP2E<-renderPlot(YBP(1,2),height=Zeh_sz*0.7)
  output$YBP1W<-renderPlot(YBP(2,1),height=Zeh_sz*0.7)
  output$YBP2W<-renderPlot(YBP(2,2),height=Zeh_sz*0.7)

  # Stochastic Projection plots

  output$S1E<-renderPlot(StochP(1,1,leg=T),height=Zeh_sz*0.7)
  output$S2E<-renderPlot(StochP(1,2),height=Zeh_sz*0.7)
  output$S1W<-renderPlot(StochP(2,1),height=Zeh_sz*0.7)
  output$S2W<-renderPlot(StochP(2,2),height=Zeh_sz*0.7)

  # Individual Projection plots

  output$Sm1E<-renderPlot(StochPm(1,1,leg=T),height=Zeh_sz*0.7)
  output$Sm2E<-renderPlot(StochPm(1,2),height=Zeh_sz*0.7)
  output$Sm1W<-renderPlot(StochPm(2,1),height=Zeh_sz*0.7)
  output$Sm2W<-renderPlot(StochPm(2,2),height=Zeh_sz*0.7)

  # Individual radar plots

  output$R1E<-renderPlot(radarP(1,1,leg=T),height=Zeh_sz*0.7)
  output$R2E<-renderPlot(radarP(1,2),height=Zeh_sz*0.7)
  output$R1W<-renderPlot(radarP(2,1),height=Zeh_sz*0.7)
  output$R2W<-renderPlot(radarP(2,2),height=Zeh_sz*0.7)

  # E/W radar

  output$REW1<-renderPlot(radarPEW(1,leg=T),height=Zeh_sz*0.7)
  output$REW2<-renderPlot(radarPEW(2),height=Zeh_sz*0.7)

  observeEvent(input$Load,{

    filey<-input$Load

    tryCatch({
      CompRes<<-readRDS(file=filey$datapath)
      MET<<-CompRes$MET
      pnames<<-CompRes$pnames
      MPnames<<-CompRes$MPnames
      OMnames<<-CompRes$OMnames[!grepl("R",CompRes$OMnames)]
      ROMnames<<-CompRes$OMnames[grepl("R",CompRes$OMnames)]
      ROMcode<<-CompRes$ROMcode

      updateCheckboxGroupInput(session,"CMPs",label=NULL,choices=MPnames, selected=MPnames,inline=T)

      updateSelectInput(session,"Zeh_MP1",selected=MPnames[2],choices=MPnames)
      updateSelectInput(session,"Zeh_MP2",selected="None",choices=c("None",MPnames))

      updateSelectInput(session,"ZehPM_MP1",selected=MPnames[1],choices=MPnames)
      updateSelectInput(session,"ZehPM_MP2",selected=MPnames[2],choices=MPnames)
      updateSelectInput(session,"ZehPM_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

      updateSelectInput(session,"S_MP1",selected=MPnames[1],choices=MPnames)
      updateSelectInput(session,"S_MP2",selected=MPnames[2],choices=MPnames)
      updateSelectInput(session,"S_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

      updateSelectInput(session,"Sm_MP1",selected=MPnames[1],choices=MPnames)
      updateSelectInput(session,"Sm_MP2",selected=MPnames[2],choices=MPnames)

      updateSelectInput(session,"R_MP1",selected=MPnames[1],choices=MPnames)
      updateSelectInput(session,"R_MP2",selected=MPnames[2],choices=MPnames)
      updateSelectInput(session,"R_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

      updateSelectInput(session,"REW_MP1",selected=MPnames[1],choices=MPnames)
      updateSelectInput(session,"REW_MP2",selected=MPnames[2],choices=MPnames)
      updateSelectInput(session,"REW_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)


      changed(changed()+1)

    },

    error = function(e){
      #AM(paste0(e,"\n"))
      shinyalert("File read error", "This does not appear to be a list saved using saveRDS()", type = "error")
      return(0)
    })

    if(!all(c("MET","dynSSB0",   "dynSSBMSY", "R0", "SSB0","Rec","CW","CWa","B_BMSY","F_FMSY","name",
                              "CMPdesc","pdesc","ROMcode","pnames","MPnames","OMnames","Design")%in%names(CompRes))){
      shinyalert("Incorrect class of object", "This does not appear to be a list created by the function Results_compiler() of the ABTMSE package (v6.6.16 or later) and saved using saveRDS()", type = "error")
      return(0)
    }

  })

  observeEvent(input$CMPs,{
    MPind<-getMPind()
    MPnames_s<-MPnames[MPind]


    updateSelectInput(session,"Zeh_MP1",selected=MPnames_s[2],choices=MPnames_s)
    updateSelectInput(session,"Zeh_MP2",selected="None",choices=c("None",MPnames_s))

    updateSelectInput(session,"ZehPM_MP1",selected=MPnames_s[1],choices=MPnames_s)
    updateSelectInput(session,"ZehPM_MP2",selected=MPnames_s[2],choices=MPnames_s)
    updateSelectInput(session,"ZehPM_MP3",selected=MPnames_s[min(3,length(MPnames_s))],choices=MPnames_s)

    updateSelectInput(session,"S_MP1",selected=MPnames_s[1],choices=MPnames_s)
    updateSelectInput(session,"S_MP2",selected=MPnames_s[2],choices=MPnames_s)
    updateSelectInput(session,"S_MP3",selected=MPnames_s[min(3,length(MPnames_s))],choices=MPnames_s)

    updateSelectInput(session,"Sm_MP1",selected=MPnames_s[1],choices=MPnames_s)
    updateSelectInput(session,"Sm_MP2",selected=MPnames_s[2],choices=MPnames_s)

    updateSelectInput(session,"R_MP1",selected=MPnames_s[1],choices=MPnames_s)
    updateSelectInput(session,"R_MP2",selected=MPnames_s[2],choices=MPnames_s)
    updateSelectInput(session,"R_MP3",selected=MPnames_s[min(3,length(MPnames_s))],choices=MPnames_s)

    updateSelectInput(session,"REW_MP1",selected=MPnames_s[1],choices=MPnames_s)
    updateSelectInput(session,"REW_MP2",selected=MPnames_s[2],choices=MPnames_s)
    updateSelectInput(session,"REW_MP3",selected=MPnames_s[min(3,length(MPnames_s))],choices=MPnames_s)


  })


})
