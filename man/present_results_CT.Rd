\name{present_results_CT}
\alias{present_results_CT}
\title{
  Summarize the Results of a Simulation of a Five-Year Study of a Deer Population.
}
\description{
  This function summarize the results of a simulation of a five-year
  study of a roe deer or red deer population.
}
\usage{
present_results_CT(roeNoHS, dfPopSize)
}
\arguments{
  \item{roeNoHS}{
    A list of five elements named \code{"REM_raw"}, \code{"REM_detect"},
    \code{"REM_detect_active"},  \code{"IS_raw"}, \code{"IS_detect"},
    \code{"IS_detect_active"}.  Each element is a list of N components,
    each component being a matrix with 7 rows (the population size of
    the five years + the trend and the relative change in population
    size) and four columns (the point estimate \code{PtEst}, the
    estimate of the standard error \code{EstSE}, the limits of the 90
    percent confidence interval \code{EstCIl} and \code{EstCIu}).
  }
  \item{dfPopSize}{
    A data.frame with the same structure as the dataset \code{dfPopSize}
    given in the package (5 rows = 5 years of monitoring; 3 variables named:
    (i)\code{Ntot}, the total number of animals during a given year,
    (ii)\code{Old}, the number of animals surviving from the previous
    year, (iii) \code{New}, the number of new roe deer born during the
    current year).
  }
}
\details{
  This function presents some summary statistics of the simulations.
}
\value{
  
  The result is a data.frame, which presents for each method and each
  parameter (population size for each year, trend and relative change):
  the true value of the parameter, the mean value (over all simulations)
  of the parameter, the standard error (standard deviation of the
  distribution of estimated parameters), the mean estimated standard
  error (the mean of the standard error estimated by bootstrap), the
  standard error on the estimated standard error (the standard deviation
  of the distribution of estimated standard error), the coverage
  probability (Pcov: the probability that the 90 percent confidence
  interval includes the true value), the probability to identify a
  decrease (PDec, for lambda and relative change, the proportion of
  simulations for which the mean parameter was lower than 0), and the
  probability to identify a significant decrease (PsigD, for lambda and
  relative change, the proportion of simulations for which the upper
  bound of the CI was lower than 0).

}
\references{
  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{roeNoHS}} for an example dataset that can be used with
  this function.
}
\examples{

present_results_CT(roeNoHS, dfPopSize)


}
\keyword{datagen}
