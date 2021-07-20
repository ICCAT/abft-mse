
getind1<-function(){
  (1:nOMs)[c((OMgrid[,1]%in%input$Rec1 & OMgrid[,2]%in%input$SM1 & OMgrid[,3]%in%input$Scale1 & OMgrid[,4]%in%input$Comp1),ROMnames%in%input$Rob1)]
}

getind2<-function(){
  (1:nOMs)[c((OMgrid[,1]%in%input$Rec2 & OMgrid[,2]%in%input$SM2 & OMgrid[,3]%in%input$Scale2 & OMgrid[,4]%in%input$Comp2),ROMnames%in%input$Rob2)]
}

getMPind<-function(){
  MPind<-MPnames%in%input$CMPs
  MPind[1]<-TRUE # must have zero C
  if(sum(MPind)<2)MPind[2]<-TRUE # at least 1 other CMPs
  MPind
}

