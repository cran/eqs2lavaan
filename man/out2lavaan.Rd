\name{out2lavaan}
\alias{out2lavaan}
\title{
Function Called by the eqs2lavaan Function
}
\description{
Returns a list of extracted information from the EQS .out file.  
}
\usage{
out2lavaan(eqs)
}
\arguments{
  \item{eqs}{
Specific path location of an EQS .out file.  If this file is in your R directory, only the title will be necessary, otherwise a path location will also be needed.
}
}
\details{
This is a function that is referenced mainly by \code{\link{eqs2lavaan}}, however it can be used in a standalone format as well.
}
\value{
A list of the covariance matrix and descriptive statistics extracted from the EQS .out file supplied.
}
\references{
Bentler, P. M. (2008). EQS Program Manual. Encino, CA: Multivariate Software Inc.

Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/.
}
\seealso{
\code{\link{eqs2lavaan}}
}
\examples{
# EQS required to get a necessary .out file
# Run for62.eqs from the EQS examples and save .out to R directory location

\dontrun{out2lavaan("for62.out")}
}
\keyword{EQS}
\keyword{lavaan}
\keyword{out}