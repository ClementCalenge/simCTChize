\name{roeHSBiasedSamOffPaths}
\alias{roeHSBiasedSamOffPaths}
\docType{data}
\title{
  Population Size and Trend Estimates for our Simulations of a Camera
  Trap Study on a Roe Deer population Based on 100 traps (all placed outside
  the habitat type selected by the roe deer)
}
\description{
  This dataset contains the point estimates and bootstrap confidence
  intervals on population size and trends for our 1000 simulations of a
  5-year camera trap monitoring based on 100 traps of a roe deer
  population all placed outside 
  the habitat type selected by the roe deer. See examples for the code used to create this
  object.
}
\usage{data("roeHSBiasedSamOffPaths")}
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
  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\references{
  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\examples{

## We first simulated a habitat selection by the roe deer and a
## stratified sampling of the traps, using this code:
## DONT RUN IT, it is extremely slow !!!

\dontrun{
    
simrd5withhsoffpaths <- simulateCTStudy5years(nCT = 100, duration = 30,
                                              niter = 500, dfPopSize, 
                                              listPatches, contourChize, contourN=northChize,
                                              contourS=southChize,
                                              dff=relationAnimalSigmaMaxd, habitatMap,
                                              listptg=mvtMatrices$listMatBetween,
                                              ptp=mvtMatrices$MatWithin, stratSampling=TRUE,
                                              propPaths=0, pathsMap=pathsMap,
                                              pathsBuffer=pathsBuffer, offPathsMap=offPathsMap,
                                              offPathsBuffer=offPathsBuffer,
                                              habitatSelection=TRUE,
                                              nofi = 0, backup = TRUE)
}


## We then used the same code as the one used for roeNoHS (see the
## examples on the help page of this dataset)

}
\keyword{datasets}
