# Length observations.r
# April 2021
# R Script for formatting the ICCAT reported length observations by fleet

CL25<-read.csv(paste(getwd(),"/data/ICCAT_2021_3/CLobs_25cm_2021Apr19.csv",sep=""))
hist(CL25$Length_category,breaks=c(0,Base@lenbins))
CL25<-CL25[CL25$Length_category>0,] # ignore compositions for fish length 25cm or less
CL25$Length_category<-ceiling(CL25$Length_category/25) # minus 1 because the length bins start at 25-50 (mulen 37.5)
CLobs<-aggregate(CL25$N,by=list(CL25$Year,CL25$Subyear, CL25$Area, CL25$Fleet, CL25$Length_category),sum)
names(CLobs)<-c("Year","Subyear","Area","Fleet","Length_category","N")
#CLobs<-CLobs[CLobs$Year>=Base@years[1]&CLobs$Year<=Base@years[2],] # Truncate to model years
CLobs<-CLobs[CLobs$Year>=Base@years[1]&CLobs$Year<=2016,] # !! WG decided not to fit to length comps after 2016
CLobs$Year<-CLobs$Year-Base@years[1]+1

test<-aggregate(CLobs$N,by=list(CLobs$Year,CLobs$Subyear, CLobs$Area, CLobs$Fleet),sum) # get totals per strata for calculation of fractions
names(test)<-c("Year","Subyear","Area","Fleet","N")

for(i in 1:nrow(test)){ # divide by totals to get fractions
  cond<-CLobs$Year==test$Year[i] & CLobs$Subyear==test$Subyear[i] & CLobs$Area == test$Area[i] & CLobs$Fleet == test$Fleet[i]
  CLobs$N[cond]<-CLobs$N[cond]/test$N[i]
}

# Remove compositions that are not accompanied by catches (supposed to be 'complete' catches and model doesn't predict compositions for zero catch observations)
Catcode<-paste(Cobs[,1],Cobs[,2],Cobs[,3],Cobs[,4],sep="-")
CLcode<-paste(CLobs[,1],CLobs[,2],CLobs[,3],CLobs[,4],sep="-")
keep<-CLcode%in%Catcode
CLobs<-CLobs[keep,]

save(CLobs,file=paste(getwd(),"/Data/Processed/Conditioning/CLobs",sep=""))

diag<-F
if(diag){

  Fnams<-Fleets$name
  Flabs<-Fnams[CLobs[,4]]
  CLobs_lab<-cbind(CLobs,Flabs)
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_wrap(~Year , ncol=10) # comps by year
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_wrap(~Flabs,ncol=5) # comps by fleet
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_grid(~Subyear) # comps by quarter

}





























