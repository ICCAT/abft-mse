# Define fleets.r
# April 2021
# R Script for defining fleets for this operating model

# Developer code for checking changes in CPUE and fleet defintions before / after reconditioning
# CPUEdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2020_1/CPUE_indices compiled_OM2020Mar_with_lt_newcalibrateddata.csv",sep="")) # Load CPUE indices
# CPUEdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2021_2/CPUE_indices compiled_OM2021Apr.csv",sep="")) # Load CPUE indices
# CPUEsummary<-cbind(aggregate(rep(1,nrow(CPUEdat)),by=list(CPUEdat$Fleet,CPUEdat$Name),sum),aggregate(CPUEdat$Year,by=list(CPUEdat$Fleet,CPUEdat$Name),min)[,3], aggregate(CPUEdat$Year,by=list(CPUEdat$Fleet,CPUEdat$Name),max)[,3])                                # Take a look at the fleets
# names(CPUEsummary)<-c("Fleet_Num","Name","nyears","First_yr","Last_yr")
# write.csv(CPUEsummary,"C:/Users/tcar_/Dropbox/BFT MSE/Communications/Data for reconditioning/CPUEsummary.csv")
# CLdat<-read.csv(file=paste(getwd(),"/data/ICCAT_2021_2/CLobs_25cm_Apr2021.csv",sep=""))                # Load the Catch-at-Length data

FleetDefs<-read.csv(file=paste(getwd(),"/data/ICCAT_2021_2/FleetDefs.csv",sep=""),header=F,stringsAsFactors = F)            # Load the fleet definitions (unchanged from before reconditioning)
Fleets <-      new('list') # note you must put ALL flag types ahead of specific fleet types of same gear - sequential fleet assignment
Fleets$name <- FleetDefs[,2]
Fleets$LB <-  as.numeric(FleetDefs[,3])
Fleets$UB <-  as.numeric(FleetDefs[,4])

# ! CHECK
# cbind(Fleets$name,Fleets$LB,Fleets$UB)  # !TSD check against Table 3.1

save(Fleets,file=paste(getwd(),"/Data/Processed/Conditioning/Fleets",sep=""))



