
# ===================================================================================================================
# ==== ABT MSE data formatting code =================================================================================
# ===================================================================================================================

AssignAge<-function(dat,Base)
{
  
  Age<-unlist(sapply(X=(1:nrow(dat)),FUN=tbin,vec=as.numeric(as.character(dat$Length_cm)),bins=Base@len_age[1,,1]))
  agew<-unlist(sapply(X=(1:nrow(dat)),FUN=tbin,vec=as.numeric(as.character(dat$Weight_kg)),bins=Base@wt_age[1,,1]))
  Age[is.na(Age)]<-agew[is.na(Age)]
  cbind(dat,Age)
  
}


tbin<-function(X,vec,bins){
  
  test<-(1:length(bins))[vec[X]>c(0,bins[1:(length(bins)-1)])&vec[X]<bins]
  if(length(test)==1){
    return(test)
  }else{
    return(NA)
  }
  
}  

AssignFleet<-function(dat,Fleets){
  
  nf<-length(Fleets$name)
  Fleet<-rep(nf+1,nrow(dat))
  
  for(i in 1:nf){
    
    condg<-Fleets$gearTC[i]==dat$GearGrp 
    
    if(Fleets$flag[i]=="ALL"){
      condf<-rep(TRUE,nrow(dat))
    }else{
      condf<- Fleets$flag[i]==dat$FleetCode 
    }
    
    condy<-dat$Year>(Fleets$FyS[i]-1) & dat$Year<(Fleets$FyE[i]+1)
    condr<-dat$Area%in%Fleets$Loc[[i]]
    condq<-dat$Quarter%in%Fleets$Q[[i]]
    mcond<-condg&condf&condy&condr&condq
    Fleet[mcond]<-i
    
  }
  temp<-cbind(dat,Fleet)
  temp
}


ICCATtoGEO<-function(dato){   # Convert ICCAT format (corner closest to GMT/equator) to South West corner
  
  ICCATdat<-subset(dato,dato$SquareTypeCode!="none"&dato$SquareTypeCode!="ICCAT")
  
  Latadj<-as.numeric(array(unlist(strsplit(as.character(ICCATdat$SquareTypeCode),"x")),dim=c(2,nrow(ICCATdat)))[1,])
  Lonadj<-as.numeric(array(unlist(strsplit(as.character(ICCATdat$SquareTypeCode),"x")),dim=c(2,nrow(ICCATdat)))[2,])
  
  ICCATdat$Lon[ICCATdat$QuadID==1]<-ICCATdat$Lon[ICCATdat$QuadID==1]+(Lonadj[ICCATdat$QuadID==1]/2)
  ICCATdat$Lat[ICCATdat$QuadID==1]<-ICCATdat$Lat[ICCATdat$QuadID==1]+(Latadj[ICCATdat$QuadID==1]/2)
  ICCATdat$Lon[ICCATdat$QuadID==2]<-ICCATdat$Lon[ICCATdat$QuadID==2]+(Lonadj[ICCATdat$QuadID==2]/2)
  ICCATdat$Lat[ICCATdat$QuadID==2]<--ICCATdat$Lat[ICCATdat$QuadID==2]-(Latadj[ICCATdat$QuadID==2]/2)
  ICCATdat$Lon[ICCATdat$QuadID==3]<--ICCATdat$Lon[ICCATdat$QuadID==3]-(Lonadj[ICCATdat$QuadID==3]/2)
  ICCATdat$Lat[ICCATdat$QuadID==3]<--ICCATdat$Lat[ICCATdat$QuadID==3]-(Latadj[ICCATdat$QuadID==3]/2)
  ICCATdat$Lon[ICCATdat$QuadID==4]<--ICCATdat$Lon[ICCATdat$QuadID==4]-(Lonadj[ICCATdat$QuadID==4]/2)
  ICCATdat$Lat[ICCATdat$QuadID==4]<-ICCATdat$Lat[ICCATdat$QuadID==4]+(Latadj[ICCATdat$QuadID==4]/2)
  
  ICCATdat
  
}

ICCATtoGEO2<-function(ICCATdat){   # Convert ICCAT format (corner closest to GMT/equator) to South West corner
  
  ICCATdat<-subset(ICCATdat,!ICCATdat$GeoStrata%in%c("ICCAT","LatLon"))
  
  Latadj<-as.numeric(array(unlist(strsplit(as.character(ICCATdat$GeoStrata),"x")),dim=c(2,nrow(ICCATdat)))[1,])
  Lonadj<-as.numeric(array(unlist(strsplit(as.character(ICCATdat$GeoStrata),"x")),dim=c(2,nrow(ICCATdat)))[2,])
  
  ICCATdat$Lon[ICCATdat$QuadID==1]<-ICCATdat$Lon[ICCATdat$QuadID==1]+(Lonadj[ICCATdat$QuadID==1]/2)
  ICCATdat$Lat[ICCATdat$QuadID==1]<-ICCATdat$Lat[ICCATdat$QuadID==1]+(Latadj[ICCATdat$QuadID==1]/2)
  ICCATdat$Lon[ICCATdat$QuadID==2]<-ICCATdat$Lon[ICCATdat$QuadID==2]+(Lonadj[ICCATdat$QuadID==2]/2)
  ICCATdat$Lat[ICCATdat$QuadID==2]<--ICCATdat$Lat[ICCATdat$QuadID==2]-(Latadj[ICCATdat$QuadID==2]/2)
  ICCATdat$Lon[ICCATdat$QuadID==3]<--ICCATdat$Lon[ICCATdat$QuadID==3]-(Lonadj[ICCATdat$QuadID==3]/2)
  ICCATdat$Lat[ICCATdat$QuadID==3]<--ICCATdat$Lat[ICCATdat$QuadID==3]-(Latadj[ICCATdat$QuadID==3]/2)
  ICCATdat$Lon[ICCATdat$QuadID==4]<--ICCATdat$Lon[ICCATdat$QuadID==4]-(Lonadj[ICCATdat$QuadID==4]/2)
  ICCATdat$Lat[ICCATdat$QuadID==4]<-ICCATdat$Lat[ICCATdat$QuadID==4]+(Latadj[ICCATdat$QuadID==4]/2)
  
  ICCATdat
  
}





assign_area<-function(dat,polys){
  
  Area<-rep(NA,nrow(dat))
  
  for(aa in 1:length(polys)){
    
    out<-pnt.in.poly(cbind(dat$Lon,dat$Lat),polys[[aa]])
    
    Area[out$pip==1]<-aa
    
  }  
  
  cond<-!is.na(Area)
  Area<-Area[cond]
 
  cbind(dat[cond,],Area)

}  

assign_quarter<-function(dat){  
 
 quarter<-c(rep(1:4,each=3),1:4)
 Subyear<-quarter[dat$TimePeriodID]
 if(!"TimePeriodID"%in%names(dat)&"TimeCatch"%in%names(dat)) Subyear<-quarter[dat$TimeCatch]
 cond<-!is.na(Subyear)
 Subyear<-Subyear[cond]
 cbind(dat[cond,],Subyear)
 
}

assign_fleet<-function(dat,fleets){
  
  Fleet<-match(dat$GearGrpCode,fleets)
  if(!"GearGrpCode"%in%names(dat)&"GearGrp"%in%names(dat))Fleet<-match(dat$GearGrp,fleets)
  Fleet[is.na(Fleet)]<-length(fleets)+1
  cbind(dat,Fleet)
  
}  

assign_year<-function(dat,years){
  
  
  dat<-dat[dat$YearC>(years[1]-1)&dat$YearC<(years[2]+1),]
  Year<-as.numeric(dat$YearC)-years[1]+1
  cbind(dat,Year)
  
}  

exp_agg_CAS<-function(dat,len_bins){
  
  lenfields<-grep('X',names(dat))
  lenfields<-lenfields[2:length(lenfields)]
  for(i in lenfields)names(dat)[i]<-substr(names(dat)[i],2,nchar(names(dat)[i]))
  cms<-as.numeric(names(dat)[lenfields])
  bins<-rep(NA,length(cms))
  for(i in 1:length(cms)){
    val<-(1:(length(len_bins)-1))[cms[i]>((len_bins[1:(length(len_bins)-1)])-0.1)&cms[i]<(len_bins[2:length(len_bins)]-0.1)]
    if(length(val)>0){bins[i]<-val
    }else{bins[i]<-NA}
  } 
  lendat<-dat[,lenfields]
  ind<-as.matrix(expand.grid(1:nrow(dat),1:length(lenfields)))
  Ri<-ind[,1]
  Ci<-ind[,2]
  Len_class<-bins[Ci]
  N<-lendat[ind]
  newdat<-as.data.frame(cbind(dat$Year[Ri],dat$Subyear[Ri],dat$Area[Ri],dat$Fleet[Ri],Len_class))
  names(newdat)<-c("Year","Subyear","Area","Fleet","Len_class")
  newdat2<-aggregate(N,by=list(newdat$Year,newdat$Subyear,newdat$Area,newdat$Fleet,newdat$Len_class),sum)
  names(newdat2)<-c("Year","Subyear","Area","Fleet","Len_class","N")
  subset(newdat2,newdat2$N>0)
  
} 

calcarea<-function(dat){
  
  AreaSize<-rep(NA,nrow(dat))
  PID<-rep(1,4)
  SID<-rep(1,4)
  POS<-1:4
  
  for(i in 1:nrow(dat)){
    X<-c(dat$Lon[i]-0.5,dat$Lon[i]+0.5,dat$Lon[i]+0.5,dat$Lon[i]-0.5)
    Y<-c(dat$Lat[1]-0.5,dat$Lat[1]-0.5,dat$Lat[1]+0.5,dat$Lat[1]+0.5)
    AreaSize[i]<-unlist(calcArea(as.PolySet(data.frame(PID,SID,POS,X,Y),projection="LL"))[3])
  }
  
  cbind(dat,AreaSize)
  
}


