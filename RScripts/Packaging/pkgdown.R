
#install.packages("devtools")
#devtools::install_github("hadley/pkgdown")

library('devtools')
library('pkgdown')
library('dplyr')

#pkgdown::build_site()
#build_home(pkg,path)

pkgdownloc<- "C:/Users/Tom Carruthers/GitHub/GitHub/pkgdown/R"
files<-list.files(pkgdownloc)
tobesourced<-paste(pkgdownloc,files,sep="/")
for(i in 1:length(tobesourced))source(tobesourced[i])


pkg<-"C:/ABT-MSE/R_package/ABTMSE"
path="C:/ABT-MSE/R_package/ABTMSE/docs"


build_site_tom<-
function(pkg = ".",
                   path = "docs",
                   examples = TRUE,
                   run_dont_run = FALSE,
                   mathjax = TRUE,
                   preview = interactive(),
                   seed = 1014
) {

  pkg <- as_pkgdown(pkg)
  path <- rel_path(path, pkg$path)

  init_site(pkg, path)

  build_home(pkg, path = path)

  build_reference(pkg,
                  lazy = FALSE,
                  examples = examples,
                  run_dont_run = run_dont_run,
                  mathjax = mathjax,
                  seed = seed,
                  path = file.path(path, "reference"),
                  depth = 1L
  )

  #build_articles(pkg, path = file.path(path, "articles"), depth = 1L)

  #build_news(pkg, path = file.path(path, "news"), depth = 1L)

  if (preview) {
    preview_site(path)
  }
  invisible(TRUE)
}



setwd("C:/ABT-MSE/R_package/ABTMSE/")
library(ABTMSE)
build_site_tom()
# YOU WERE HERE


