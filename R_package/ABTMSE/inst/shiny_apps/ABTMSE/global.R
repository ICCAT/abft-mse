# CompRes<-readRDS("C:/Users/tcar_/Dropbox/abft-mse/R_package/ABTMSE/inst/shiny_apps/ABTMSE/data/CompResd.rda")
CompRes<<-readRDS("./data/CompResd.rda") # Can be loaded by user
MET<<-CompRes$MET
pnames<<-CompRes$pnames
MPnames<<-CompRes$MPnames
OMnames<<-CompRes$OMnames[!grepl("R",CompRes$OMnames)]
ROMnames<<-CompRes$OMnames[grepl("R",CompRes$OMnames)]
ROMcode<<-CompRes$ROMcode
MPcols<<-rep(c("black","red","green","blue","orange",
               "grey","purple","brown","pink","darkblue",
               "darkgreen","darkred","deeppink3","khaki",
               "turquoise3","tan3"),10)

Syear<<-1965

#MET<<-readRDS("./data/MET.rds")
#pnames<<-readRDS("./data/pnames.rds")
#MPnames<<-readRDS("./data/MPnames.rds")
#OMnames<<-readRDS("./data/OMnames.rds")
#OMgrid<<-readRDS("./data/OMgrid.rds")
