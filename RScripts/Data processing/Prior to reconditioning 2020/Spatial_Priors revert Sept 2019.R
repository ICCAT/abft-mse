# Spatial priors

SpatPr<-read.csv(paste0(getwd(),"/data/Processed/Priors/Spatial_Priors.csv"),header=T)

for_wt<-1/SpatPr$CV^2
sum_for_wt<-aggregate(for_wt,list(SpatPr$Ino),sum)$x
w<-for_wt/sum_for_wt[SpatPr$Ino]

SpatPr<-cbind(SpatPr,w)
