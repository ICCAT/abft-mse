# Movement exclusions
# Feb 2019
# T carruthers

#

#               stock   age class season   from area   to area
MEM<-array(NA,c(Base@np,Base@nma,  Base@ns, Base@nr,    Base@nr))
ER<-(-20)
#No eastern fish in the GOM in Quarter 3
GOM<-match('GOM',Base@areanams)
if(GOM_EXC){
  MEM[2,,3,,GOM]<-ER
}


#No fish in the GSL in Quarter 1
#GSL<-match('GSL',Base@areanams)
#MEM[,,1,,GSL]<-ER

#No fish in the SATL in Quarter 4
#SATL<-match('SATL',Base@areanams)
#MEM[,,4,,SATL]<-ER

#No Western fish in the MED
MED<-match('MED',Base@areanams)
MEM[2,,,,MED]<-ER

#No Eastern fish in the GOM
MEM[1,,,,GOM]<-ER



if(use_tag_exclusions){

  convnames<-function(x){

    vec<-strsplit(x,split="[|]")[[1]]
    Alex<-c("GOM","CAR","W_ATL","GSL","SC_ATL","SE_ATL","NC_ATL","NE_ATL","E_ATL","W_MED","E_MED")
    nuu<-c("GOM","WATL","WATL","GSL","SATL",  "SATL",   "NATL", "NATL",  "EATL",  "MED","MED")
    noo<-  c(1,   2,     2,      3,    4,        4,       5,      5,       6,       7,     7)
    noo[match(vec,Alex)]

  }

  nMEM<-array(TRUE,c(Base@np,Base@nma,  Base@ns, Base@nr,    Base@nr))

  tagex<-read.csv(paste0(getwd(),"/data/Processed/Priors/Valid Tag transitions_V2.csv"),header=T,stringsAsFactors=F)

  ind<-expand.grid(1:Base@np,1:Base@nma,1:Base@ns,1:Base@nr)
  ind<-as.matrix(cbind(ind,ind[,4]))
  nMEM[ind]<-FALSE # Positive diagnonal

  for(i in 1:nrow(tagex)){

    q<-as.numeric(strsplit(tagex$Quarter[i],"_")[[1]][2])
    af<-convnames(tagex$FromA[i])
    at<-convnames(tagex$ToA[i])
    nMEM[1:Base@np,1:Base@nma,q,af,at]<-FALSE

  }

  MEM[nMEM]<-ER
 
  GOM<-match('GOM',Base@areanams)
  SATL<-match('SATL',Base@areanams)
  
  #No Fish moving from GOM to SATL
  MEM[,,3,GOM,SATL]<-ER

}


MovExc<-cbind(TEG(dim(MEM)),as.vector(MEM))
totmov<-nrow(MovExc)
MovExc<-MovExc[!is.na(MovExc[,6]),]
MovExc<-as.data.frame(MovExc)

names(MovExc)<-c("p","a","s","r","r","Wt")
print(paste0(round(nrow(MovExc)/totmov*100,2),"% of movement transitions excluded - ",nrow(MovExc)," of ",totmov," transitions"))

printout<-F
if(printout){

  tabout<-array(0,c(8*7,3*7))

  for(pp in 1:2){
    for(s in 1:4){
      tabout[(s-1)*8+(1:7),(pp-1)*8+(1:7)]<-is.na(MEM[pp,1,s,,])
    }
  }

  write.csv(tabout,"C:/Users/tcar_/Dropbox/BFT MSE/Communications/Exclusions/Exclusions.csv")

}

