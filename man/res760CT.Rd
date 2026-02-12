\name{res760CT}
\alias{res760CT}
\docType{data}
\title{
  Population Size and Trend Estimates for our Simulations of a Camera
  Trap Study on a Roe Deer population Based on 760 traps (no Habitat Selection).
}
\description{
  This dataset contains the point estimates and bootstrap confidence
  intervals on population size and trends for our 100 simulations of a
  5-year camera trap monitoring based on 760 traps of a roe deer population (no habitat
  selection simulated). See the vignette for more details and for the code used to create this
  object.
}
\usage{data("res760CT")}
\format{

  This dataset is a list with 2 components named \code{REM} and
  \code{IS}.  Each component is a list of 100 data.frames, containing
  the population size and trend estimates for the random encounter model
  and instantaneous sampling, accounting for the detectability and for
  the presence of inactivity period.\cr

  Each data.frame contains the point estimate (\code{PtEst}), minimum
  and maximum limits of the 90 percent confidence interval estimated by
  bootstrap (\code{EstCIl,EstCIu}) on the population sizes of each year
  (\code{N1} to \code{N5}), as well as trend \code{lambda} (exponential
  of the slope of the regression of log(N) on the year t, minus 1), and
  rate of change (N5-N1)/N1 (\code{changeRate}). For population size
  estimate, the coefficient of variation \code{EstCV} estimated by
  bootstrap is also provided (500 bootstrap iterations).

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

## No examples.

}
\keyword{datasets}
