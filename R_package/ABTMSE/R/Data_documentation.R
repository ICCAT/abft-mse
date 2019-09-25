
# =====================================================================
# === Data documentation ==============================================
# =====================================================================

# Test unit objects ---------------

data(Allocation, envir=environment())

#' Default 2020 Allocation (a matrix area x fleet)
#'
#' @name Allocation
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL


#' TEST unit object
#'
#' @name TEST
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' Operating model input object for test unit
#'
#' @name OMI_test
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' Operating model object for test unit
#'
#' @name OM_test
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

# Observation models ---------------

#' Generic bad quality observation error model
#'
#' @name Bad_Obs
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' Generic good quality observation error model
#'
#' @name Good_Obs
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' #' Generic perfect information observation error model (for everthing but indices)
#'
#' @name Perfect_Obs
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' #' An example observation error model (for everthing but indices)
#'
#' @name Obs_example
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' A simulated data set
#'
#' \describe{
#' \item{Cobs}{A matrix of [sim x nyear]}
#' \item{Iobs}{A 3D array of observed relative abundance indices dimensions [sim x index x year]}
#' \item{K}{A vector nsim long of von Bertalanffy growth parameter K estimates}
#' \item{Linf}{A vector nsim long of growth parameter Linf estimates (maximum length)}
#' \item{t0}{A vector nsim long of growth parameter t0 estimates}
#' \item{M}{A matrix of natural mortality rate at age [sim x age]}
#' \item{Bt}{A vector nsim long of current biomass estimates}
#' \item{MSY}{A vector nsim long of MSY estimates}
#' \item{BMSY}{A vector nsim long of BMSY estimates}
#' \item{UMSY}{A vector nsim long of UMSY estimates}
#' \item{a}{A vector nsim long of Length-weight conversion parameter a W=aL^b}
#' \item{b}{A vector nsim long of Length-weight conversion parameter b W=aL^b}
#' \item{nages}{An integer value specifying the total number of age classes}
#' \item{ageM}{A vector nsim long of estimates of age at 50 per cent maturity}
#' \item{Mat}{A matrix of maturity at age [sim x age]}
#' \item{Bt_PI}{A vector nsim long of current biomass estimates known perfectly (PI: Perfect Information)}
#' \item{UMSY_PI}{A vector nsim long of UMSY nown perfectly (PI: Perfect Information)}
#' \item{CAA}{A 3D array of catch at age observations [sim x age x year]}
#' \item{CAL}{A vector of lengths representing the start-end points of the length bins corresponding to CAL data]}
#' \item{CAL_bins}{The intervals for the length binning of the CAL data}
#' \item{TAC}{A matrix nsim rows by n TAC updates that stores all previous TACs that have been set}
#' \item{TAC}{A vector nsim long of the current (2018) TAC recommendation}
#' \item{MPrec}{A vector nsim long of the previous recommendations of the management procedure}
#' }
#' @name dset
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' An example simulated data set for the Eastern stock
#'
#' \describe{
#' \item{Cobs}{A matrix of [sim x nyear]}
#' \item{Iobs}{A 3D array of observed relative abundance indices dimensions [sim x index x year]}
#' \item{K}{A vector nsim long of von Bertalanffy growth parameter K estimates}
#' \item{Linf}{A vector nsim long of growth parameter Linf estimates (maximum length)}
#' \item{t0}{A vector nsim long of growth parameter t0 estimates}
#' \item{M}{A matrix of natural mortality rate at age [sim x age]}
#' \item{Bt}{A vector nsim long of current biomass estimates}
#' \item{MSY}{A vector nsim long of MSY estimates}
#' \item{BMSY}{A vector nsim long of BMSY estimates}
#' \item{UMSY}{A vector nsim long of UMSY estimates}
#' \item{a}{A vector nsim long of Length-weight conversion parameter a W=aL^b}
#' \item{b}{A vector nsim long of Length-weight conversion parameter b W=aL^b}
#' \item{nages}{An integer value specifying the total number of age classes}
#' \item{ageM}{A vector nsim long of estimates of age at 50 per cent maturity}
#' \item{Mat}{A matrix of maturity at age [sim x age]}
#' \item{Bt_PI}{A vector nsim long of current biomass estimates known perfectly (PI: Perfect Information)}
#' \item{UMSY_PI}{A vector nsim long of UMSY nown perfectly (PI: Perfect Information)}
#' \item{CAA}{A 3D array of catch at age observations [sim x age x year]}
#' \item{CAL}{A vector of lengths representing the start-end points of the length bins corresponding to CAL data]}
#' \item{CAL_bins}{The intervals for the length binning of the CAL data}
#' \item{TAC}{A matrix nsim rows by n TAC updates that stores all previous TACs that have been set}
#' \item{TAC}{A vector nsim long of the current (2018) TAC recommendation}
#' \item{MPrec}{A vector nsim long of the previous recommendations of the management procedure}
#' }
#' @name dset_example_East
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

#' An example simulated data set for the Western stock
#'
#' \describe{
#' \item{Cobs}{A matrix of [sim x nyear]}
#' \item{Iobs}{A 3D array of observed relative abundance indices dimensions [sim x index x year]}
#' \item{K}{A vector nsim long of von Bertalanffy growth parameter K estimates}
#' \item{Linf}{A vector nsim long of growth parameter Linf estimates (maximum length)}
#' \item{t0}{A vector nsim long of growth parameter t0 estimates}
#' \item{M}{A matrix of natural mortality rate at age [sim x age]}
#' \item{Bt}{A vector nsim long of current biomass estimates}
#' \item{MSY}{A vector nsim long of MSY estimates}
#' \item{BMSY}{A vector nsim long of BMSY estimates}
#' \item{UMSY}{A vector nsim long of UMSY estimates}
#' \item{a}{A vector nsim long of Length-weight conversion parameter a W=aL^b}
#' \item{b}{A vector nsim long of Length-weight conversion parameter b W=aL^b}
#' \item{nages}{An integer value specifying the total number of age classes}
#' \item{ageM}{A vector nsim long of estimates of age at 50 per cent maturity}
#' \item{Mat}{A matrix of maturity at age [sim x age]}
#' \item{Bt_PI}{A vector nsim long of current biomass estimates known perfectly (PI: Perfect Information)}
#' \item{UMSY_PI}{A vector nsim long of UMSY nown perfectly (PI: Perfect Information)}
#' \item{CAA}{A 3D array of catch at age observations [sim x age x year]}
#' \item{CAL}{A vector of lengths representing the start-end points of the length bins corresponding to CAL data]}
#' \item{CAL_bins}{The intervals for the length binning of the CAL data}
#' \item{TAC}{A matrix nsim rows by n TAC updates that stores all previous TACs that have been set}
#' \item{TAC}{A vector nsim long of the current (2018) TAC recommendation}
#' \item{MPrec}{A vector nsim long of the previous recommendations of the management procedure}
#' }
#' @name dset_example_West
#' @docType data
#' @author Tom Carruthers \email{t.carruthers@oceans.ubc.ca}
#' @keywords data
NULL

