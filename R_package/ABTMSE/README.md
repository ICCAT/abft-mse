# ABTMSE

The Atlantic-Wide Research Programme on Bluefin Tuna (GBYP) aims to develop a new scientific management framework by improving data collection, knowledge of key biological and ecological processes, assessment models and management. 

A critical component of the GBYP is the construction of a robust advice framework consistent with the precautionary approach (GBYP 2014). A Management Strategy Evaluation (MSE, Cochrane 1998, Butterworth 1999, Kell et al. 2014, Punt et al. 2014) approach has been proposed to address this goal (Anon. 2014b). 

MSE establishes operating models that represent credible hypotheses for population and fishery dynamics which are used to quantify the efficacy of various management procedures. These management procedures may encompass a wide range of complexity from conventional stock assessments linked to harvest control rules (Hilborn 2003) through to simple empirical management procedures that calculate catch limits directly from resource monitoring data indices (Geromont and Butterworth 2014a;b, Kell et al. 2015).

ABTMSE is an R package designed for the easy and rapid testing of management procedures for Atlantic bluefin tuna.

## Installation

ABTMSE is not currently available from CRAN, but you can install the development version from github with:

```R
# install.packages("devtools")
devtools::install_github("ICCAT/abft-mse/R_package/ABTMSE")
```

## Usage

Create MSE objects using the 'new' function:

```R
library(ABTMSE)
loadABT()
myMSE<-new('MSE',OM_example, Bad_Obs,MPs=list(c("UMSY","UMSY")))
plot(myMSE)
```

