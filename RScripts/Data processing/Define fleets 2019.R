# Define fleets.r
# August 2016
# R Script for defining fleets for this operating model

CPUEdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_4/CPUE_indices compiled_2019OM_JPLLsplit.csv",sep=""))
aggregate(rep(1,nrow(CPUEdat)),by=list(CPUEdat$Fleet,CPUEdat$Name),sum)
CLdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_4/CLobs_25cm_JPLLsplit.csv",sep=""))
FleetDefs<-read.csv(file=paste(getwd(),"/data/ICCAT_2019_4/FleetDefs.csv",sep=""),header=F)


#                1        2        3        4        5          6            7        8       9      10       11       12       13      14     15        16        17
Fleets <-      new('list') # note you must put ALL flag types ahead of specific fleet types of same gear - sequential fleet assignment
Fleets$name <- FleetDefs[,2]
Fleets$LB <-  aggregate(CLdat$Length_category,by=list(CLdat$Fleet),min)$x-25/2
Fleets$LB[Fleets$LB<0]<-0
Fleets$UB <-   aggregate(CLdat$Length_category,by=list(CLdat$Fleet),max)$x+25/2
Fleets$UB[Fleets$UB>500]<-500


todo<-T
if(!todo){
  Fleets$name<-  c("LLOTH","LLJPN","BBold_E","BBold_SE","BBnew","PSMedRec","PSMedLOld","PSMedSOld","TPOld","TPnew","RRCan","RRUSA")
  Fleets$gearTC<-c("LL", "LL",   "BB",     "BB",      "BB",   "PS" ,     "PS",       "PS",       "TP",   "TP",   "RR",   "RR")
  Fleets$flag<-  c("ALL","JPN",  "ALL",    "ALL",     "ALL",  "ALL",     "ALL",      "ALL",      "ALL",  "ALL",  "CAN",  "USA")
  Fleets$FyS<-   c( 1994, 1960,   1960,     1960,     2009,    2009,      1960,       1960,      1960,   2009,   1988,   1988)
  Fleets$FyE<-   c( 2016, 2016,   2008,     2008,     2016,    2016,      2008,       2008,      2008,   2016,   2016,   2016)

  #                 1      2       3        4       5       6          7         8        9        10       11        12        13         14
  #Fleets<-new('list') # note you must put ALL flag types ahead of specific fleet types of same gear - sequential fleet assignment
  #Fleets$name<-c("LLOTH","LLOTH","LLOTH","LLJPN","LLJPN", "BBnew", "PSMEDnew", "PSNOR", "PSHRV", "PSWnew", "TPnew", "RRCan", "RRUSAFS", "RRUSAFB")
  #Fleets$gearTC<-c("LL", "LL",   "LL",   "LL",   "LL",    "BB" ,   "PS",       "PS",    "PS",    "PS",     "TP",    "RR",    "RR",      "RR")
  #Fleets$flag<-  c("ALL","ALL",  "ALL",  "JPN",  "JPN",   "ESP",   "ALL",      "NOR",   "HRV",   "USA",    "SPN",   "CAN",   "USA",     "USA")
  # other flags (data already assigned by iccat)           FRA     not HRV                        CAN      MOR PORTUGAL
  #Fleets$FyS<-   rep(1960,16)
  #Fleets$FyE<-   rep(2016,16)


  Fleets$Loc<-new('list') # What areas?
  Fleets$Loc[[1]]<-1:7    # LLOTH
  Fleets$Loc[[2]]<-1:7    # LLJPN
  Fleets$Loc[[3]]<-6      # BBold_E
  Fleets$Loc[[4]]<-4      # BBold_SE
  Fleets$Loc[[5]]<-1:7    # BBnew
  Fleets$Loc[[6]]<-7      # PSMedRec
  Fleets$Loc[[7]]<-7      # PSMedLOld
  Fleets$Loc[[8]]<-7      # PSMedSOld
  Fleets$Loc[[9]]<-1:7    # TPOld
  Fleets$Loc[[10]]<-1:7   # TPnew
  Fleets$Loc[[11]]<-1:7   # RRCan
  Fleets$Loc[[12]]<-1:7   # RRUSA

  Fleets$Q<-new('list') # What quarters of the year?
  Fleets$Q[[1]]<-1:4       # LLOTH
  Fleets$Q[[2]]<-1:4       # LLJPN
  Fleets$Q[[3]]<-1:4       # BBold_E
  Fleets$Q[[4]]<-1:4       # BBold_SE
  Fleets$Q[[5]]<-1:4       # BBnew
  Fleets$Q[[6]]<-1:4       # PSMedRec
  Fleets$Q[[7]]<-2         # PSMedLOld
  Fleets$Q[[8]]<-c(1,3,4)  # PSMedSOld
  Fleets$Q[[9]]<-1:4      # TPOld
  Fleets$Q[[10]]<-1:4      # TPnew
  Fleets$Q[[11]]<-1:4      # RRCan
  Fleets$Q[[12]]<-1:4      # RRUSA
}

save(Fleets,file=paste(getwd(),"/Data/Processed/Conditioning/Fleets",sep=""))



