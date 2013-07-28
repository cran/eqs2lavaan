\name{plotCov}
\alias{plotCov}
\title{
Plot a Covariance/Correlation Heat Map
}
\description{
Any covariance matrix can be entered and an image with covariance and correlation matrices will be plotted.
}
\usage{
plotCov(cov)
}
\arguments{
  \item{cov}{
    A covariance matrix.
  }
}
\value{
An image containing heat map based items for the covariance and resulting correlation matrix.
}
\author{
Craig M. Krebsbach \email{craigmk@my.uri.edu}
}
\note{
The diagonal is all 1's from the covariance matrix, the correlation matrix diagonal of all 1's is left out.  No EQS code is necessary to use this function, it will work with any covariance matrix.}
\seealso{
\code{\link{eqsCov}}
\code{\link{eqsCorr}}
\code{\link{eqsDesc}}
\code{\link{cor2cov}}
}
\examples{
library(datasets)
\donttest{plotCov(ability.cov$cov)}
\donttest{plotCov(cov(attitude))}

library(lavaan)
HolzingerSwineford1939
# Select variables x1-x9 only
HS39 <- cov(HolzingerSwineford1939[,7:15])
\donttest{plotCov(HS39)}
}
\keyword{cov}
\keyword{cor}
\keyword{corr}
\keyword{EQS}
\keyword{lavaan}
\keyword{eqsCov}
\keyword{eqsCorr}
\keyword{matrix}
\keyword{image}
\keyword{heat}
\keyword{heat map}
\keyword{plot}