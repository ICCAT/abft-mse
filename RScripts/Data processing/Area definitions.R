
# ================================================================================
# === ABT MSE area definitions ===================================================
# ================================================================================

# Set working directory

#setwd("G:/ABT-MSE/")
ploty=F

# -- Names ------------------------------------------------------
AreaNames<-c("GOM","WAtl","WAtlw","GSL","EAtl","EAtlw",
             "Med","WMed","CMed","EMed","BS","Braz","WAfr","WCMed",
             "DP_GOM","DP_WATL","DP_GSL","DP_CATL","DP_EATL","DP_NEATL","DP_WMED","DP_EMED",
             "ML_CAR","ML_WATL","ML_NCATL","ML_SCATL","ML_EATL","ML_SEATL",        # additional areas from 2015 SGM M.Lauretta
             "GOM_11","GSL_11", "CAR_11", "WATL_11", "SCATL_11", "NCATL_11", 
             "NEATL_11", "EATL_11", "SEATL_11", "WMED_11", "EMED_11","MED_11",
             "GOM_7","WATL_7","GSL_7","SATL_7","NATL_7","EATL_7","MED_7") 

# -- Lons / Lats ------------------------------------------------

AreaDefs<-new('list')

# GOM
AreaDefs[[1]]<-data.frame(x=c(-100,-100,-83,-55,-55,-65,-65,-75,-75,-82,-82),
                          y=c( 31,  20,  10, 10, 20, 20, 25, 25, 30, 30, 31))

# WAtl
AreaDefs[[2]]<-data.frame(x=c(-82,-82,-75,-75,-65,-65,-55,-55,-45,-45,-58,-58,-52,-62,-64, -75),
                          y=c(31,  30, 30, 25, 25, 20, 20, 10, 10, 60, 60, 50, 46, 45, 45.5,40))

# WAtlw  Western Atlantic-wide WAtl + GSL
AreaDefs[[3]]<-data.frame(x=c(-82,-82,-75,-75,-65,-65,-55,-55,-45,-45,-58, -58,-75),
                          y=c( 31, 30, 30, 25, 25, 20, 20, 10, 10, 60, 60,  50, 50))

# GSL
AreaDefs[[4]]<-data.frame(x=c(-70,-62,-52,-58,-68),
                          y=c( 48, 45, 46, 50, 50))

# EAtl
AreaDefs[[5]]<-data.frame(x=c(-45,-6, 0, 8, 8,-45),
                          y=c(36, 36,43,52,70,70)) 

# EAtlw
AreaDefs[[6]]<-data.frame(x=c(-45,-10,-6, 0, 8, 8,-45),
                          y=c(10,  10, 36,43,52,70,70)) 

# Med   (WMed + CMed+ EMed)
AreaDefs[[7]]<-data.frame(x=c(-6,0, 37,37,27,  22,5),
                          y=c(36,29,29,38,40.2,47,47))

# WMed
AreaDefs[[8]]<-data.frame(x=c(-6,0, 8, 11,15,17, 15,7),
                          y=c(36,29,29,37,38,39.5,41,49))

# CMed
AreaDefs[[9]]<-data.frame(x=c(8, 11,15,17,  15,9, 22,28,28),
                          y=c(29,37,38,39.5,41,47,47,39,29))

# EMed
AreaDefs[[10]]<-data.frame(x=c(28,38,38,28),
                           y=c(29,29,39,39))

# BS Black Sea
AreaDefs[[11]]<-data.frame(x=c(27,43,43,27),
                           y=c(40.2,40.2,50,50))

# Braz Brazil
AreaDefs[[12]]<-data.frame(x=c(-70,-70,-25,-25,-40),
                           y=c( 10,-30,-30, 0,  10))

# WAfr West Africa
AreaDefs[[13]]<-data.frame(x=c(-25,-40,-45,-45,-6, 30,-25),
                           y=c( 0,  10, 10, 36, 36,-20,-20))

# WCMed West and Central Med
AreaDefs[[14]]<-data.frame(x=c(-6,0, 28,28,27,  22,5),
                           y=c(36,29,29,38,40.2,47,47))

# DP_GOM
AreaDefs[[15]]<-data.frame(x=c(-100,-95,-88,-80,-80,-85,-100),
                           y=c(20,   17, 20, 20, 25, 35, 35))

# DP_WATL
AreaDefs[[16]]<-data.frame(x=c(-95,-70,-70,-25,-25,-30,-30,-45,-45,-100,-100,-80,-70,-60,-55,-55,-70,-85,-80,-80,-88),
                           y=c( 17, 0, -50,-50, 5,  5,  10, 10, 80,  80,  60,  50, 55, 55, 50, 45, 45, 35, 25, 20, 20))

# DP_GSL
AreaDefs[[17]]<-data.frame(x=c(-70,-55,-55,-60,-70),
                           y=c(45,  45, 50, 55, 55))

# DP_CATL
AreaDefs[[18]]<-data.frame(x=c(-45,-30,-30,-45),
                           y=c( 10, 10, 80, 80))

# DP_EATL
AreaDefs[[19]]<-data.frame(x=c(-30,-25,-25, 20,20,-5,-5, 5, 15,15,-15,-15,-30),
                           y=c( 5, 5,  -50,-50,30, 30,40,50,50,60, 60, 50, 50))

# DP_NEATL
AreaDefs[[20]]<-data.frame(x=c(-30,-15,-15,15,15,45,45,-30),
                           y=c( 50, 50, 60,60,50,50,80, 80))

# DP_WMED
AreaDefs[[21]]<-data.frame(x=c(-5,23,23,5,-5),
                           y=c(30,30,50,50,40))

# DP_EMED
AreaDefs[[22]]<-data.frame(x=c(23,45,45,23),
                           y=c(30,30,50,50))

# ML_CAR
AreaDefs[[23]]<-data.frame(x=c(-82.5,-75,-75,-65,-65,-55,-55,-70,-95,-88,-80,-80,-82.5),
                           y=c(30,30,25,25,20,20,0,0,16.5,20,20,25,30))

# ML_WATL
AreaDefs[[24]]<-data.frame(x=c(-70,-55,-55,-65,-65,-75,-75,-82.5,-85,-70,-55,-55,-60,-70,-80,-100,-100,-45,-45,-30,-30,-25,-25,-70),
                           y=c(0,0,20,20,25,25,30,30,35,45,45,50,55,55,50,60,80,80,10,10,5,5,-50,-50))

# ML_NCATL
AreaDefs[[25]]<-data.frame(x=c(-30,-45,-45,-30), 
                           y=c(40,40,80,80))

# ML_SCATL
AreaDefs[[26]]<-data.frame(x=c(-30,-45,-45,-30), 
                           y=c(10,10,40,40))

# ML_EATL
AreaDefs[[27]]<-data.frame(x=c(-30,-30,-15,-15,15,15,5,-5), 
                           y=c(40,50,50,60,60,50,50,40))

# ML_SEATL
AreaDefs[[28]]<-data.frame(x=c(-30,-30,-5,-5,20,20,-25,-25,-30), 
                           y=c(10,40,40,30,30,-50,-50,5,5))

# 12 areas of the CMG and Matt Lauretta

# GOM_11
AreaDefs[[29]]<-data.frame(x=c(-100,-95,-88,-80,-80,-85,-100),
                           y=c(20,   17, 20, 20, 25, 35, 35))

# GSL_11
AreaDefs[[30]]<-data.frame(x=c(-70,-55,-55,-60,-70),
                           y=c(45,  45, 50, 55, 55))

# CAR_11
AreaDefs[[31]]<-data.frame(x=c(-82.5,-75,-75,-65,-65,-55,-55,-70,-95,-88,-80,-80,-82.5),
                           y=c(30,    30, 25, 25, 20, 20, 0,  0, 16.5,20, 20, 25, 30))

# WATL_11
AreaDefs[[32]]<-data.frame(x=c(-70,-55,-55,-65,-65,-75,-75,-82.5,-85,-70,-55,-55,-60,-70,-80,-100,-100,-45,-45,-30,-30,-25,-25,-70),
                           y=c(0,0,20,20,25,25,30,30,35,45,45,50,55,55,50,60,80,80,10,10,5,5,-50,-50))

# SCATL_11
AreaDefs[[33]]<-data.frame(x=c(-30,-45,-45,-30), 
                           y=c(10,10,40,40))

# NCATL_11
AreaDefs[[34]]<-data.frame(x=c(-30,-45,-45,-30), 
                           y=c(40,40,80,80))

# NEATL_11
AreaDefs[[35]]<-data.frame(x=c(-30,-15,-15,15,15,45,45,-30),
                           y=c( 50, 50, 60,60,50,50,80, 80))

# EATL_11
AreaDefs[[36]]<-data.frame(x=c(-30,-30,-15,-15,15,15,5,-5), 
                           y=c(40,50,50,60,60,50,50,40))

# SEATL_11
AreaDefs[[37]]<-data.frame(x=c(-30,-30,-5,-5,20,20,-25,-25,-30), 
                           y=c(10,40,40,30,30,-50,-50,5,5))

# WMED_11
AreaDefs[[38]]<-data.frame(x=c(-5,23,23,5,-5),
                           y=c(30,30,50,50,40))

# EMED_11
AreaDefs[[39]]<-data.frame(x=c(23,45,45,23),
                           y=c(30,30,50,50))

# MED_11
AreaDefs[[40]]<-data.frame(x=c(-5,45,45,5,-5),
                           y=c(30,30,50,50,40))


# 7 area 2018 area definitions

# GOM_7
AreaDefs[[41]]<-data.frame(x=c(-100,-95,-88,-80,-80,-85,-100),
                           y=c(20,   17, 20, 20, 25, 35, 35))

# WATL_7
AreaDefs[[42]]<-data.frame(x=c(-70,-95,-88,-80,-80,-85,-70,-55,-55,-60,-70,-80,-100,-100,-45,-45,-30,-30,-25,-25,-70),
                           y=c(0,16.5,20, 20, 25,35,45,45,50,55,55,50,60,80,80,10,10,5,5,-50,-50))

# GSL_7
AreaDefs[[43]]<-data.frame(x=c(-70,-55,-55,-60,-70),
                           y=c(45,  45, 50, 55, 55))

# SATL_7
AreaDefs[[44]]<-data.frame(x=c(-30,-45,-45,-5,-5,20,20, -25,-25,-30), 
                           y=c(10, 10,  40,40,30,30,-50,-50,5,  5))

# NATL_7
AreaDefs[[45]]<-data.frame(x=c(-30,-15,-15,45,45,-45,-45,-30),
                           y=c( 50, 50, 60,60,80, 80, 40, 40))

# EATL_7
AreaDefs[[46]]<-data.frame(x=c(-30,-30,-15,-15,15,15,5,-5), 
                           y=c(40,50,50,60,60,50,50,40))

# MED_7
AreaDefs[[47]]<-data.frame(x=c(-5,45,45,5,-5),
                           y=c(30,30,50,50,40))





save(AreaNames,file=paste(getwd(),"/Data/Processed/Area definitions/AreaNames",sep=""))
save(AreaDefs,file=paste(getwd(),"/Data/Processed/Area definitions/AreaDefs",sep=""))

if(ploty){
  ind<-41:47
  library(maps)
  library(mapdata)
  cols<-rep(c("#ff000040","#00ff0040","#0000ff40","#00000040","#ff00ff40"),10)
  
  map(xlim=c(-100,50),ylim=c(-50,80))
  abline(v=(-20:20)*10,col='grey')
  abline(h=(-20:20)*10,col='grey')
  abline(v=0,col="red")
  abline(h=0,col="red")
  
  for(i in 1:length(AreaNames[ind])){
    polygon(AreaDefs[[ind[i]]],col=cols[i])
    text(mean(AreaDefs[[ind[i]]]$x),mean(AreaDefs[[ind[i]]]$y),AreaNames[ind[i]],col='white',font=2,cex=0.8)         
  }
  
}




