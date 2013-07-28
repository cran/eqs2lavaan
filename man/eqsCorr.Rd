\name{eqsCorr}
\alias{eqsCorr}
\title{
Translate Covariance Matrix in EQS Output to a Correlation Matrix in R
}
\description{
The EQS .out file contains all the necessary information to return a correlation matrix.  This function allows you to extract the necessary info and save the correlation matrix for your own use in R.
}
\usage{
eqsCorr(eqs)
}
\arguments{
  \item{eqs}{
    A valid unedited .out file from EQS output.
  }
}
\value{
A correlation matrix.
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
\code{\link{plotCov}}
\code{\link{eqsCorr}}
\code{\link{eqsDesc}}
\code{\link{eqs2lavaan}}
}
\examples{
# EQS required to get a necessary .out file
# Run for62.eqs from the EQS examples and save .out to R directory location

\dontrun{eqsCorr("for62.out")}
}
\keyword{EQS}
\keyword{lavaan}
\keyword{cor}
\keyword{SEM}
\keyword{model}
\keyword{matrix}
\keyword{CFA}
\keyword{cor2cov}
\keyword{cov2cor}