


avail<-function(classy){

  #return(unique(c(ls('package:ABT-MSE')[unlist(lapply(ls('package:ABT-MSE'),
  #                                                    getclass,classy=classy))], ls(envir=.GlobalEnv)[unlist(
  #                                                      lapply(ls(envir=.GlobalEnv),getclass,classy=classy))])))
  return(c(ls(envir=.GlobalEnv)[unlist(lapply(ls(envir=.GlobalEnv),getclass,classy=classy))]))

}

loadABT<-function(){

  chk <- "package:ABTMSE" %in% search()
  datadir<- paste(searchpaths()[match("package:ABTMSE",search())],"/data/",sep="")
  list.files(datadir)


}
