

# Automatic reports for MSE objects

#' Make standardized MSE MP performance report
#'
#' @param MSE An object of class MSE return from a function call MSE<-new('MSE',...)
#' @param dir A character string that is the directory where the report will be saved (default is current working directory)
#' @param Author A character string; the author of the report
#' @param introtext A character string; optional; Any relevant text the author would like to include about the MSE
#' @param filenam A character string: optional: a specific name of the report file so for Daves_MP_perf.html, filenam="Daves_MP_perf"
#' @return an html report in the directory dir
#' @export
MSE_report<-function(MSE,dir=NA,Author='Anon',introtext=NA,filenam="MSE_report"){

  if(is.na(dir))dir<-getwd()

  params<-list('MSE'=MSE,'Author'=Author,'introtext'=introtext)
  render(input=system.file("MSEreport.Rmd", package="ABTMSE"), output_file=paste0(dir,"/",filenam,".html",sep=""),params=params)

}
