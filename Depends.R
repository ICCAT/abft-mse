
# Installation script for ABTMSE dependencies

packages<-c("roxygen2","snowfall","maps","mapdata",
  "wordcloud","abind","SDMTools","PBSmapping","MASS",
  "shiny","parallel","lubridate","rmarkdown","knitr",
  "mvtnorm","ggplot2")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

