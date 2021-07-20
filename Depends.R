
# Installation script for ABTMSE dependencies

packages<-c("roxygen2","snowfall","maps","mapdata",
  "wordcloud","abind","PBSmapping","MASS",
  "shiny","parallel","lubridate","rmarkdown","knitr",
  "mvtnorm","ggplot2","kableExtra","DT","geoR",
  "shinyBS","shinyWidgets","fmsb","viridis",
  "reldist")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

