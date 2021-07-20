# Length observations.r
# April 2019
# R Script for formatting the ICCAT reported length observations by fleet

# what you're aiming for!
#load(file=paste(getwd(),"/Data/Processed/Conditioning/CLobs",sep="")); CLobs_old<-CLobs
#test0<-aggregate(CLobs_old$N,by=list(CLobs_old$Year,CLobs_old$Subyear, CLobs_old$Area, CLobs_old$Fleet),sum)

CL5<-CL25<-read.csv(paste(getwd(),"/data/ICCAT_2019_4/CLobs_5cm_JPLLsplit.csv",sep=""))
CL25<-CL25[CL25$Length_category>25,]
CL25$Length_category<-ceiling(CL25$Length_category/25)-1 # minus 1 because the length bins start at 25-50 (mulen 37.5)
CLobs<-aggregate(CL25$N,by=list(CL25$Year,CL25$Subyear, CL25$Area, CL25$Fleet, CL25$Length_category),sum)
names(CLobs)<-c("Year","Subyear","Area","Fleet","Length_category","N")
CLobs<-CLobs[CLobs$Year>=Base@years[1]&CLobs$Year<=Base@years[2],]
CLobs$Year<-CLobs$Year-Base@years[1]+1

test<-aggregate(CLobs$N,by=list(CLobs$Year,CLobs$Subyear, CLobs$Area, CLobs$Fleet),sum)
names(test)<-c("Year","Subyear","Area","Fleet","N")

for(i in 1:nrow(test)){
  cond<-CLobs$Year==test$Year[i] & CLobs$Subyear==test$Subyear[i] & CLobs$Area == test$Area[i] & CLobs$Fleet == test$Fleet[i]
  CLobs$N[cond]<-CLobs$N[cond]/test$N[i]
}

Catcode<-paste(Cobs[,1],Cobs[,2],Cobs[,3],Cobs[,4],sep="-")
CLcode<-paste(CLobs[,1],CLobs[,2],CLobs[,3],CLobs[,4],sep="-")
keep<-CLcode%in%Catcode
CLobs<-CLobs[keep,]


#save(CLobs,file=paste(getwd(),"/Data/Processed/Conditioning/CLobs 2019",sep=""))

diag<-F
if(diag){

  Fnams<-Fleets$name
  Flabs<-Fnams[CLobs[,4]]
  #Flabs<-Flabs[CLobs$Fleet]
  CLobs_lab<-cbind(CLobs,Flabs)
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_wrap(~Year , ncol=10)
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_grid(~Flabs)
  ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_grid(~Subyear)
  #ggplot(CLobs_lab, aes(x=Length_category)) + geom_histogram() + facet_grid(~Area,~Subyear)


}





























