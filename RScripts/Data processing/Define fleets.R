# Define fleets.r
# August 2016
# R Script for defining fleets for this operating model

Fleets<-new('list') # note you must put ALL flag types ahead of specific fleet types of same gear - sequential fleet assignment
Fleets$name<-c("LLOTH","LLJPN","BBold","BBnew","PSMedRec","PSMedLOld","PSMedSOld","PSWestOld","PSWestnew","TPOld","TPnew","RRCan","RRUSA")
Fleets$gearTC<-c("LL", "LL",   "BB",   "BB",   "PS" ,     "PS",       "PS",       "PS",       "PS",       "TP",   "TP",   "RR",   "RR")
Fleets$flag<-  c("ALL","JPN",  "ALL",  "ALL",  "ALL",     "ALL",      "ALL",      "ALL",      "ALL",      "ALL",  "ALL",  "CAN",  "USA")
Fleets$FyS<-   c( 1960, 1960,   1960,   2009,   2009,      1960,       1960,       1960,       1987,       1960,   2009,   1988,   1988)
Fleets$FyE<-   c( 2015, 2015,   2008,   2015,   2015,      2008,       2008,       1986,       2015,       2008,   2015,   2015,   2015)

Fleets$Loc<-new('list') # What areas?
Fleets$Loc[[1]]<-1:10 # LLOTH
Fleets$Loc[[2]]<-1:10 # LLJPN
Fleets$Loc[[3]]<-1:10 # BBold
Fleets$Loc[[4]]<-1:10 # BBnew
Fleets$Loc[[5]]<-10   # PSMedRec
Fleets$Loc[[6]]<-10   # PSMedLOld
Fleets$Loc[[7]]<-10   # PSMedSOld
Fleets$Loc[[8]]<-1:9  # PSWestOld
Fleets$Loc[[9]]<-1:9  # PSWestnew
Fleets$Loc[[10]]<-1:10# TPOld
Fleets$Loc[[11]]<-1:10# TPnew
Fleets$Loc[[12]]<-1:10# RRCan
Fleets$Loc[[13]]<-1:10# RRUSA

Fleets$Q<-new('list') # What quarters of the year?
Fleets$Q[[1]]<-1:4       # LLOTH
Fleets$Q[[2]]<-1:4       # LLJPN
Fleets$Q[[3]]<-1:4       # BBold
Fleets$Q[[4]]<-1:4       # BBnew
Fleets$Q[[5]]<-1:4       # PSMedRec
Fleets$Q[[6]]<-2         # PSMedLOld
Fleets$Q[[7]]<-c(1,3,4)  # PSMedSOld
Fleets$Q[[8]]<-1:4       # PSWestOld
Fleets$Q[[9]]<-1:4       # PSWestnew
Fleets$Q[[10]]<-1:4      # TPOld
Fleets$Q[[11]]<-1:4      # TPnew   
Fleets$Q[[12]]<-1:4      # RRCan
Fleets$Q[[13]]<-1:4      # RRUSA   

save(Fleets,file=paste(getwd(),"/Data/Processed/Conditioning/Fleets",sep=""))



