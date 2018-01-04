## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::opts_chunk$set(dpi=75)

## ----loadsecret,echo=F,warning=F,error=F,message=F-----------------------
library(ABTMSE)
loadABT(quiet=T)


## ----plotareas,echo=F,message=F,warning=F,error=F,fig.width=3, fig.height=2.5----
par(mar=rep(0.01,4))
areaplot(MSE_example)

## ----install_package,eval=F----------------------------------------------
#  install.packages("C:/ABT-MSE/R_package/ABTMSE_2.1.0.tar.gz")

## ----load_library, eval=F------------------------------------------------
#  library(ABTMSE)
#  ??ABTMSE

## ----loadlibrary---------------------------------------------------------
library(ABTMSE)

## ----seed----------------------------------------------------------------
set.seed(1) 

## ----loaddata------------------------------------------------------------
loadABT()

## ----avail---------------------------------------------------------------
avail('OMI')
avail('OM')
avail('Obs')

## ----design_all_levse----------------------------------------------------
Design$all_levs

## ----design_all_lnams----------------------------------------------------
Design$all_lnams

## ----design_design_ref---------------------------------------------------
Design$Design_Ref

## ----design_list_OMs-----------------------------------------------------
avail('OM')

## ----qs_avail_obs--------------------------------------------------------
avail('Obs')
Bad_Obs

## ---- code you might want one day ---------------------------------------
# finding DD-compatible indices
# aggregate(Bad_Obs@MPind$Year,by=list(Bad_Obs@MPind$Name),FUN=min)
