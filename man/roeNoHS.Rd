\name{roeNoHS}
\alias{roeNoHS}
\docType{data}
\title{
  Population Size and Trend Estimates for our Simulations of a Camera
  Trap Study on a Roe Deer population Based on 100 traps (no Habitat Selection)
}
\description{
  This dataset contains the point estimates and bootstrap confidence
  intervals on population size and trends for our 1000 simulations of a
  5-year camera trap monitoring based on 100 traps of a roe deer population (no habitat
  selection simulated). See examples for the code used to create this
  object.
}
\usage{data("roeNoHS")}
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

## The actual dataset containing the results of the 1000 simulations
## was too large to fit in this package. We illustrate the code used
## to generate the dataset roeNoHS using only the 4 simulations stored
## in sim4iter.

## We first wrote this function:

compute_all <- function(simrd5nohs)
{
    ## REM
    REM_raw <- processCTStudy(simrd5nohs, method="REM",
                              detectability=FALSE, active=FALSE)
    REM_detect <- processCTStudy(simrd5nohs, method="REM",
                                 detectability=TRUE, active=FALSE)
    REM_detect_active <- processCTStudy(simrd5nohs, method="REM",
                                        detectability=TRUE, active=TRUE)


    ## IS
    IS_raw <- processCTStudy(simrd5nohs, method="IS",
                             detectability=FALSE, active=FALSE)
    IS_detect <- processCTStudy(simrd5nohs, method="IS",
                                detectability=TRUE, active=FALSE)
    IS_detect_active <- processCTStudy(simrd5nohs, method="IS",
                                       detectability=TRUE, active=TRUE)

    
    roeNoHS <- list(REM_raw=REM_raw, REM_detect=REM_detect,
                    REM_detect_active=REM_detect_active,
                    IS_raw=IS_raw, IS_detect=IS_detect,
                    IS_detect_active=IS_detect_active)

    return(roeNoHS)
}

\dontrun{
## Warning: very slow code !!
roeNoHSsubsample <- compute_all(sim4iter)

roeNoHSsubsample
}

}
\keyword{datasets}
