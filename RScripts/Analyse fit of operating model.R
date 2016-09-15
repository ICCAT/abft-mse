
# =================================================================================================
# ==== ABT MSE ==== Atlantic Bluefin Tuna Management Strategy Evaluation ==========================
# =================================================================================================

# Analysing fit of the operating model to data 
# Tom Carruthers UBC
# Laurie Kell ICCAT
# Joe Powers LSU

# 12th November 2015

# Prerequisites ============================================

setwd("G:/ABT-MSE/")
source("Source/MSE_source.r")
source("Source/Objects.r")

# Write the M3 input file ==================================

source("Rscripts/Condition operating model.r")

# Run the M3 model  ========================================

system("C:/M3/M3.exe")

# Analyse fit ============================================== 

out<-M3read("C:/M3")

plotM3fit(out,"G:/ABT-MSE/Results/OM_fits/CMG_Monterey/")
getM3res(out,"G:/ABT-MSE/Results/OM_fits/CMG_Monterey/")

out2<-M3read("C:/M3")

plotM3fit(out2,"G:/ABT-MSE/Results/OM_fits/simsam/")
getM3res(out2,"G:/ABT-MSE/Results/OM_fits/simsam/")




