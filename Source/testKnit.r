
library(DLMtool) 
library(rmarkdown)
for (i in 1:length(DLMdat))  assign(DLMdat[[i]]@Name,DLMdat[[i]])

sfInit(parallel=TRUE,cpus=detectCores())
sfExportAll()

OM <- new("OM",  Stock=Snapper,  Fleet=IncE_NDom,  Observation=Imprecise_Biased) 

MPs <- avail("DLM_output")[1:5]
TestRun <- runMSE(OM=OM, MPs=MPs, nsim=5, proyears=50, 
	interval=5, pstar=0.5, maxF=1, timelimit=10, reps=1, CheckMPs=FALSE)

report <- function(MSEobj, output_file=NULL, output_dir=NULL) {
   if(is.null(output_dir)) output_dir <- getwd()
   if(is.null(output_file)) output_file <- "Output.pdf"
   render(input="knitrtest.Rmd", output_file=output_file, output_dir=output_dir) 
   print(paste0("Report written to: ", output_dir, "/", output_file))
}

report(MSEobj=TestRun)



