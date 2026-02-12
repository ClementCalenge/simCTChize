\name{roe760coef}
\alias{roe760coef}
\docType{data}
\title{
  Population Size and Trend Estimates for our Simulations of a Camera
  Trap Study on a Roe Deer population Based on 760 traps (no Habitat
  Selection) and a Study period of 3.95 Days.
}
\description{
  This dataset contains the point estimates and bootstrap confidence
  intervals on population size and trends for our 100 simulations of a
  5-year camera trap monitoring based on 760 traps of a roe deer population (no habitat
  selection simulated), with a study period of 3.95 days. Warning, these
  estimations are biased as they suppose a study period of 30 days. See the
  vignette for the code used to create this object, and the post
  processing for bias correction.
}
\usage{data("roe760coef")}
\format{

  This dataset is a list with 6 components.  The first three components
  are estimates obtained with the random encounter model: (i)
  \code{REM_raw} is a list of 1000 data.frames containing estimates (see
  below) obtained by the random encounter model without accounting for
  imperfect detectability or different activity periods; (ii)
  \code{REM_detect} is a similar list with estimates obtained by the
  random encounter model accounting for imperfect detectability (the
  probability of encounter detection is estimated from the data) but
  without accounting for the presence of inactivity periods; (iii)
  \code{REM_detect_active} is a similar list but accounting for both
  imperfect detectability and the presence of inactivity periods. The
  last three components, \code{IS_raw}, \code{IS_detect},
  \code{IS_detect_active} are similar lists, but using the instantaneous
  sampling rather than the importance sampling.\cr

  Each data.frame contains the point estimate (\code{PtEst}), minimum
  and maximum limits of the 90 percent confidence interval estimated by
  bootstrap (\code{EstCIl,EstCIu}) on the population sizes of each year
  (\code{N1} to \code{N5}), as well as trend \code{lambda} (exponential
  of the slope of the regression of log(N) on the year t, minus 1), and
  rate of change (N5-N1)/N1 (\code{changeRate}). For population size
  estimate, the coefficient of variation \code{EstCV} estimated by
  bootstrap is also provided.

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

## See the vignette

}
\keyword{datasets}
