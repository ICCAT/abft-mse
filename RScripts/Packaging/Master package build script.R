
#=== Packaging order of operations =================

# --- Optional -------------------

# - Rebuild OMIs
# - Re-run M3 operating models
# - Create OMs

# --- Data ------------------------

setwd("C:/ABT-MSE/")
source(paste0(getwd(),"/R Scripts/Packaging/Copy data to package.R"))

# --- Documentation ---------------

library(roxygen2)
roxygen2::roxygenize()

# --- 