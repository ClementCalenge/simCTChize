\name{roeStratSam}
\alias{roeStratSam}
\docType{data}
\title{
  Population Size and Trend Estimates for our Simulations of a Camera
  Trap Study on a Roe Deer population Based on a stratified sample of 100 traps
}
\description{
  This dataset contains the point estimates and bootstrap confidence
  intervals on population size and trends for our 1000 simulations of a
  5-year camera trap monitoring based on 100 traps of a roe deer
  population.  The sample of camera traps was stratified: a proportion p
  of the traps was placed in the neighborhood of paths (the habitat type
  selected by the roe deer).  Three proportions were used in this
  dataset: p = 0.25, p = 0.5 and p = 0.75. See examples for the code
  used to create this object.
}
\usage{data("roeStratSam")}
\format{

  This dataset is a list with 3 components, each component corresponding
  to a proportion p of the traps placed in habitats selected by the roe
  deer (0.25, 0.5 and 0.75). Each component is itself a list with 6
  components.  The first three components 
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

## We first simulated a habitat selection by the roe deer and a
## stratified sampling of the traps, using this code:
## DONT RUN IT, it is extremely slow !!!
## (took ten days to run on a computer)

\dontrun{

listSims <- list()
roeStratSam <- list()
testedp <- c(0.25, 0.5, 0.75)
    
for (i in 1:3) {

    listSims[[i]] <- simulateCTStudy5years(nCT = 100, duration = 30,
                                           niter = 500, dfPopSize, 
                                           listPatches, contourChize, contourN=northChize,
                                           contourS=southChize,
                                           dff=relationAnimalSigmaMaxd, habitatMap,
                                           listptg=mvtMatrices$listMatBetween,
                                           ptp=mvtMatrices$MatWithin, stratSampling=TRUE,
                                           propPaths=testedp[i], pathsMap=pathsMap,
                                           pathsBuffer=pathsBuffer, offPathsMap=offPathsMap,
                                           offPathsBuffer=offPathsBuffer,
                                           habitatSelection=TRUE,
                                           nofi = 0, backup = TRUE)


    REM_raw <- processCTStudy(listSims[[i]], method="REM",
                              detectability=FALSE, active=FALSE,
                              stratified=TRUE, pathsBuffer=pathsBuffer)

    REM_detect <- processCTStudy(listSims[[i]],  method="REM",
                                 detectability=TRUE, active=FALSE,
                                 stratified=TRUE, pathsBuffer=pathsBuffer)

    REM_detect_active <- processCTStudy(listSims[[i]], method="REM",
                                        detectability=TRUE, active=TRUE,
                                        stratified=TRUE, pathsBuffer=pathsBuffer)

    IS_raw <- processCTStudy(listSims[[i]], method="IS",
                             detectability=FALSE, active=FALSE,
                             stratified=TRUE, pathsBuffer=pathsBuffer)

    IS_detect <- processCTStudy(listSims[[i]], method="IS",
                                detectability=TRUE, active=FALSE,
                                stratified=TRUE, pathsBuffer=pathsBuffer)

    IS_detect_active <- processCTStudy(simrd5nohs, method="IS",
                                       detectability=TRUE, active=TRUE,
                                       stratified=TRUE, pathsBuffer=pathsBuffer)

    roeStratSam[[i]] <- list(REM_raw=REM_raw, REM_detect=REM_detect,
                             REM_detect_active=REM_detect_active,
                             IS_raw=IS_raw, IS_detect=IS_detect,
                             IS_detect_active=IS_detect_active)
    
}


}


}
\keyword{datasets}
