
#=== Packaging order of operations =================

# --- Optional -------------------

# - Rebuild OMIs
# - Re-run M3 operating models
# - Create OMs

# --- Data / examples -------------

setwd("C:/ABT-MSE/")
source(paste0(getwd(),"/R Scripts/Packaging/Copy data to package.R"))

# --- Documentation ---------------

library(roxygen2)
roxygen2::roxygenize()


# --- Package ---------------------

# !!! build source


# --- Website ---------------------

source(paste0(getwd(),"/R Scripts/Packaging/pkgdown.R"))

# !!! copy/docs to root
