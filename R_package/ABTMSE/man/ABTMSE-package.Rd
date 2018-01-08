\name{ABTMSE-package}
\alias{ABTMSE-package}
\alias{ABTMSE}
\docType{package}
\title{Atlantic Bluefin Tuna Management Strategy Evaluation}
\description{
Testing management systems for Atlantic Bluefin Tuna
}
\details{
\tabular{ll}{
Package: \tab ABTMSE\cr
Type: \tab Package\cr
Version: \tab 2.3\cr
Date: \tab 2018-01-08\cr
License: \tab GPL-2\cr
Depends: \tab methods\cr
}
}
\author{
Tom Carruthers
<t.carruthers@fisheries.ubc.ca>
}
\references{
http://www.iccat.int/Documents/CVSP/CV072_2016/n_7/CV07201782.pdf
http://www.iccat.int/Documents/CVSP/CV072_2016/n_7/CV07201796.pdf
}
\keyword{data-limited}
\keyword{fisheries}
\keyword{stock assessment}
\keyword{data-poor}
\keyword{management strategy evaluation}
\examples{
library(ABTMSE)
loadABT()
sfInit(parallel=TRUE,cpus=detectCores())   # initiate the cluster
#myMSE<-new('MSE',OM_example,Bad_Obs,MPs=list(c("MeanC","MeanC")))
plot(myMSE)
getperf(myMSE)
Tplot(myMSE)
sfStop()
}
