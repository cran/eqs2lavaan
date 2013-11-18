\name{eqs2lavaan}
\alias{eqs2lavaan}
\title{
Convert EQS Output to lavaan Syntax
}
\description{
By using an unedited .out file from EQS, the needed information can be extracted for use in R.  The resulting syntax will be converted to lavaan.  Because of the various scripting techniques in EQS, not all output will translate perfect to EQS (a warning will be supplied instead of results).  To get the most out of your code conversion, be sure to understand how to use lavaan for SEM research.
}
\usage{
eqs2lavaan(eqs, data = NULL)
}
\arguments{
  \item{eqs}{
Specific path location of an EQS .out file.  If this file is in your R directory, only the title will be necessary, otherwise a path location will also be needed.
}
  \item{data}{
Optional use of data already imported in R.  Specifying a path location of a .csv file will also allow the data to be entered into the syntax as opposed to the shortened covariance matrix and descriptive statistics from the .out file.  Actual data will provide closer results to those in EQS because rounding will not occur.
}
}
\details{
Conversion errors are always possible and .out files for failed conversions would be greatly appreciated for future updates.  Send any misread .out files to the author at \url{craigmk@my.uri.edu}.  The translated lavaan code is checked for similarity to the EQS chi-square goodness of fit and thus erroneous results will not be returned.  Only standard models will likely translate correctly as simulations, groups comparisons, and growth models are not included as of the initial release.
}
\value{
An object of class \code{\link{lavaan}},
}
\references{
Bentler, P. M. (2008). EQS Program Manual. Encino, CA: Multivariate Software Inc.

Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/.
}
\author{
Craig M. Krebsbach \email{craigmk@my.uri.edu}
}
\seealso{
\link{lavaan}
\link{plotCov}
\link{eqsDesc}
}
\examples{
# EQS required to get a necessary .out file
# Run for62.eqs from the EQS examples and save .out to R directory location

\dontrun{e2l <- eqs2lavaan("for62.out")}
\dontrun{summary(e2l)}
}
\keyword{EQS}
\keyword{lavaan}
\keyword{SEM}
\keyword{cov}
\keyword{out}
\keyword{model}
\keyword{CFA}
\keyword{analysis}
\keyword{heat}