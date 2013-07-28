\name{eqsDesc}
\alias{eqsDesc}
\title{
Extract Descriptive Statistics from an EQS Output File
}
\description{
Mean, standard deviation, kurtosis, and skewness are displayed in an EQS .out file and this function converts these into R for your use.  The returned matrix will display the vitals for all relevant variables and can be selected by column.
}
\usage{
eqsDesc(eqs)
}
\arguments{
  \item{eqs}{
    A valid unedited .out file from EQS output.
  }
}
\value{
A matrix with four columns (mean, sd, skewness, and kurtosis).
}
\references{
Bentler, P. M. (2008). EQS Program Manual. Encino, CA: Multivariate Software Inc.

Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/.
}
\author{
Craig M. Krebsbach \email{craigmk@my.uri.edu}
}
\note{
This function will still be usable for more complex SEM models that end up not being translatable by \code{\link{eqs2lavaan}}.
}
\seealso{
\code{\link{eqs2lavaan}}
\code{\link{eqsCov}}
\code{\link{eqsCorr}}
}
\examples{
# EQS required to get a necessary .out file
# Run for62.eqs from the EQS examples and save .out to R directory location

\dontrun{eqsCorr("for62.out")}
}
\keyword{EQS}
\keyword{lavaan}
\keyword{desc}
\keyword{mean}
\keyword{sd}
\keyword{kurt}
\keyword{skew}
\keyword{model}
\keyword{CFA}
\keyword{dev}