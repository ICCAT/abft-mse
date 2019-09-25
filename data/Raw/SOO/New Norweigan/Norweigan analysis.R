

# Norweigan data
setwd("C:/Users/tcar_/Dropbox/abft-mse/data/Raw/SOO/New Norweigan")

library(SDMTools)
library(maps)
library(mapdata)


AreaDefs<-new('list')

AreaDefs[[1]]<-data.frame(x=c(-100,-95,-88,-80,-80,-85,-100),
                          y=c(20,   17, 20, 20, 25, 35, 35))

# WATL_7
AreaDefs[[2]]<-data.frame(x=c(-70,-95,-88,-80,-80,-85,-70,-55,-55,-60,-70,-80,-100,-100,-45,-45,-30,-30,-25,-25,-70),
                          y=c(0,16.5,20, 20, 25,35,45,45,50,55,55,50,60,80,80,10,10,5,5,-50,-50))

# GSL_7
AreaDefs[[3]]<-data.frame(x=c(-70,-55,-55,-60,-70),
                          y=c(45,  45, 50, 55, 55))

# SATL_7
AreaDefs[[4]]<-data.frame(x=c(-30,-45,-45,-5,-5,20,20, -25,-25,-30),
                          y=c(10, 10,  40,40,30,30,-50,-50,5,  5))

# NATL_7
AreaDefs[[5]]<-data.frame(x=c(-30,-15,-15,45,45,-45,-45,-30),
                          y=c( 50, 50, 60,60,80, 80, 40, 40))

# EATL_7
AreaDefs[[6]]<-data.frame(x=c(-30,-30,-15,-15,15,15,5,-5),
                          y=c(40,50,50,60,60,50,50,40))

# MED_7
AreaDefs[[7]]<-data.frame(x=c(-5,45,45,5,-5),
                          y=c(30,30,50,50,40))


assign_area<-function(dat,polys){

  Area<-rep(NA,nrow(dat))

  for(aa in 1:length(polys)){

    out<-pnt.in.poly(cbind(dat$Longitude,dat$Latitude),polys[[aa]])

    Area[out$pip==1]<-aa

  }

  cond<-!is.na(Area)
  Area<-Area[cond]

  cbind(dat[cond,],Area)

}

load("dat.Rdata")

dat_with_area<-assign_area(dat,AreaDefs)









