\name{LRroeIS2years}
\alias{LRroeIS2years}
\docType{data}
\title{
  Simulated Increase in Home-Range Size for the Large-Range Roe Deer
}
\description{

  This dataset contains the home-range size of 100 simulated large-range
  roe deer, estimated unig the 95\% minimum convex polygon.  The data
  simulate a temporal increase in home-range size, growing from an
  average of 400 ha in year 1 to 700 ha in year 5 (see Vignette for more details)

}
\usage{data("LRroeIS2years")}
\format{
  This dataset is a list with two components (year 1 and year 5), each
  component containing 100 simulated home-range size (in ha).
}
\source{
  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\references{
  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\examples{
data(LRroeIS2years)
boxplot(LRroeIS2years, xlab="Year", ylab="Home-range size (ha)")
}
\keyword{datasets}
