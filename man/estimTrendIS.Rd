\name{estimTrendIS}
\alias{estimTrendIS}
\alias{estimTrendRem}
\alias{bootstrapTrendIS}
\alias{bootstrapTrendRem}
\title{
  Estimation of Population Trend from Data Collected by a Camera Trap Study
}
\description{
  These functions allow the estimation of the trend of population size in a study
  based on the data collected from a camera trap study.
}
\usage{

estimTrendIS(listencounters, r, theta, detct,
             surfaceChize=25979850,
             hoursPerDay = 24, nCT = 100, duration = 30,
             stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)

estimTrendRem(listencounters, vectorSpeed, r, theta,
              surfaceChize=25979850,
              hoursPerDay = 24, Pdetect = 1,
              nCT = 100, duration = 30,
              stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)

bootstrapTrendRem(listencounters, vectorSpeed, r, theta,
                  surfaceChize=25979850,
                  hoursPerDay = 24, Pdetect = 1,
                  nCT = 100, duration = 30,
                  stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187,
                  Nboot=1000,
                  verbose=TRUE)

bootstrapTrendIS(listencounters,r, theta, detct,
                 surfaceChize=25979850,
                 hoursPerDay = 24, nCT = 100, duration = 30,
                 stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187,
                 Nboot=1000,
                 verbose=TRUE)

}
\arguments{
  \item{listencounters}{
    
    A list of data.frames (one per year). Each data.frame containing the
    results of a camera trap monitoring during 30 days of a given year.
    This data.frame contain one row per encounter between an animal and
    a camera trap during the study. It must contain the following
    variables: (i) \code{detection} is a logical value indicating if the
    encounter has been detected by the camera trapreturned; (ii)
    \code{whichani} is an integer variable indicating the animal ID for
    the encounter, (iii) \code{whichCT} is an integer variable
    indicating the camera trap ID for the encounter, (iv)
    \code{beginning}: an integer value indicating the date/time of the
    beginning of the encounter (converted as integer from the class
    \code{POSIXct}), (v) \code{beginning}: an integer value indicating
    the date/time of the end of the encounter (converted as integer from
    the class \code{POSIXct}), (vi) \code{duration}: the duration of the
    encounter in seconds, (vii) \code{Pdetect}: the theoretical
    probability of detection (as estimated by a survival model), (viii)
    \code{when}: a list indicating, for each encounter, the date/time of
    all associations during the encounter, expressed as integer values
    (converted from the class \code{POSIXct}.

  }
  \item{r}{
    The radius of detection zone in metres.
  }
  \item{theta}{
    The angle of the detection zone (in radians).
  }
  \item{detct}{
    A vector of length \code{nCT} indicating for each camera
    trap the mean detection probability of an association.
  }
  \item{Pdetect}{
    The mean probability of detection of an encounter by a camera trap.
  }
  \item{hoursPerDay}{
    Number of hours per day during which the traps were active.
  }
  \item{nCT}{
    Number of camera traps.
  }
  \item{duration}{
    The duration (in days) of the study. Default to 30 days.
  }
  \item{vectorSpeed}{
    A vector with the same length as \code{listencounters} containing,
    for each year, the mean movement speed of animals (in meters/sec).
  }
  \item{surfaceChize}{
    Numeric value indicating the surface area of the Chize study area
    (in square metres).
  }
  \item{stratified}{
    Logical value indicating whether the sample of camera traps was
    stratified or not (i.e. a given fraction of the traps was placed
    near paths).
  }
  \item{whichCTpaths}{
    A logical vector of length \code{nCT} indicating for each trap
    whether it was located (\code{TRUE}) or not (\code{FALSE}) in paths
    habitat.
  }
  \item{surfPaths}{
    Numeric value indicating the surface area of the "paths" habitat
    (in square metres).
  }
  \item{Nboot}{
    Number of bootstrap samples
  }
  \item{verbose}{
    logical value. Whether progress information should be printed.
  }
}
\details{

  \code{estimTrendIS} estimates the trend in population size over the
  area using Instantaneous Sampling (IS).  This trend corresponds to the
  exponential of the coefficient b in the equation: log(N) = a + bt (see
  vignette). \code{estimTrendRem} estimates this trend using Random
  Encounter Model (REM). The bootstrap functions return trend estimates
  for \code{Nboot} bootstrap samples (for each sample, the camera traps
  are sampled with replacement).
  
}
\value{
  For \code{estimTrendIS} and \code{estimTrendRem}, a vector containing
  the population size estimate for each year as well as the trend
  estimate (exponential of the coefficient b in the equation described
  in the details).  For \code{bootstrapTrendIS} and
  \code{bootstrapTrendRem}, a matrix with \code{Nboot} rows, where each
  row correspond to the vector returned by \code{estimTrendIS} and
  \code{estimTrendRem}.
}
\references{
  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{estimIS}} and \code{\link{bootstrapREM}}.
}
\examples{
## Consider the example dataset sim4iter generated by simulateCTStudy5years
## Focus on the first iteration (= first study), and build the object
## listencounter required by the functions

listencounters <- lapply(sim4iter$encounters[[1]]$results,
                         function(x) do.call(rbind,x))


## Similarly, calculate the vector of mean speed for each year
vectorSpeed <- sapply(sim4iter$speed[[1]], mean)


## We also store the mean detectability for each trap in a vector (see
## the examples in the help page of estimIS for a description of the
## calculations used to calculate this vector)
prop <- c(0.2416, 0.242, 0.2428, 0.2423, 0.2418, 0.0062, 0.2419, 0.242, 
          0.2426, 0.0053, 0.2417, 0.2432, 0.2486, 0.2422, 0.2417, 0.242, 
          0.242, 0.2448, 0.2424, 0.0068, 0.0063, 0.2412, 0.2417, 0.2423, 
          0.242, 0.2415, 0.2411, 0.0055, 0.2417, 0.242, 0.2417, 0.2421, 
          0.006, 0.0048, 0.2407, 0.2416, 0.0055, 0.2416, 0.2317, 0.2416, 
          0.0054, 0.2408, 0.2435, 0.2405, 0.0058, 0.242, 0.2418, 0.0041, 
          0.2355, 0.0055, 0.2425, 0.0041, 0.2422, 0.2416, 0.2419, 0.2421, 
          0.0069, 0.2418, 0.2405, 0.0057, 0.2417, 0.2412, 0.2418, 0.2408, 
          0.2421, 0.2415, 0.2414, 0.2409, 0.0053, 0.2392, 0.2414, 0.2136, 
          0.2373, 0.2423, 0.242, 0.2416, 0.2416, 0.241, 0.0045, 0.2414, 
          0.0074, 0.2427, 0.2421, 0.2415, 0.2424, 0.2427, 0.0057, 0.2419, 
          0.0048, 0.2416, 0.2419, 0.2406, 0.1178, 0.0058, 0.2424, 0.2458, 
          0.243, 0.2416, 0.234, 0.2469)



##############################################
##
## Instantaneous sampling

## Estimate the trend of the population using IS
estimTrendIS(listencounters, r=20, theta=0.175, detct=prop)

## 200 bootstrap estimates of this trend
bois <- bootstrapTrendIS(listencounters, r=20, theta=0.175,
                         detct=prop, Nboot=200, verbose=FALSE)
head(bois)

## Confidence interval
apply(bois, 2, quantile, c(0.05, 0.95))




##############################################
##
## Random Encounter Model

## Estimate the trend of the population using REM
estimTrendRem(listencounters, vectorSpeed=vectorSpeed,
              r=20, theta=0.175)

## 200 bootstrap estimates of this trend
bore <- bootstrapTrendRem(listencounters, vectorSpeed=vectorSpeed,
                          r=20, theta=0.175,
                          Nboot=200, verbose=FALSE)
head(bore)

## Confidence interval
apply(bore, 2, quantile, c(0.05, 0.95))


}
\keyword{models }
