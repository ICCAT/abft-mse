# Define fleets.r
# August 2016
# R Script for defining fleets for this operating model

Fleets<-new('list') # note you must put ALL flag types ahead of specific fleet types of same gear - sequential fleet assignment
Fleets$name<-c("LLOTH","LLJPN","BBold_E","BBold_SE","BBnew","PSMedRec","PSMedLOld","PSMedSOld","TPOld","TPnew","RRCan","RRUSA")
Fleets$gearTC<-c("LL", "LL",   "BB",     "BB",      "BB",   "PS" ,     "PS",       "PS",       "TP",   "TP",   "RR",   "RR")
Fleets$flag<-  c("ALL","JPN",  "ALL",    "ALL",     "ALL",  "ALL",     "ALL",      "ALL",      "ALL",  "ALL",  "CAN",  "USA")
Fleets$FyS<-   c( 1994, 1960,   1960,     1960,     2009,    2009,      1960,       1960,      1960,   2009,   1988,   1988)
Fleets$FyE<-   c( 2015, 2015,   2008,     2008,     2015,    2015,      2008,       2008,      2008,   2015,   2015,   2015)

Fleets$Loc<-new('list') # What areas?
Fleets$Loc[[1]]<-1:10 # LLOTH
Fleets$Loc[[2]]<-1:10 # LLJPN
Fleets$Loc[[3]]<-8    # BBold_E
Fleets$Loc[[4]]<-9    # BBold_SE
Fleets$Loc[[5]]<-1:10 # BBnew
Fleets$Loc[[6]]<-10   # PSMedRec
Fleets$Loc[[7]]<-10   # PSMedLOld
Fleets$Loc[[8]]<-10   # PSMedSOld
Fleets$Loc[[9]]<-1:10# TPOld
Fleets$Loc[[10]]<-1:10# TPnew
Fleets$Loc[[11]]<-1:10# RRCan
Fleets$Loc[[12]]<-1:10# RRUSA

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

save(Fleets,file=paste(getwd(),"/Data/Processed/Conditioning/Fleets",sep=""))



