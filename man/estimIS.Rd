\name{estimIS}
\alias{estimIS}
\alias{estimRem}
\alias{estimatePopSize}
\title{
  Estimation of Population Size from Data Collected by a Camera Trap Study
}
\description{
  These functions allow the estimation of the population size on a study
  based on the data collected from a camera trap study.
}
\usage{
estimIS(x, r, theta, detct, hoursPerDay = 24, nCT = 100, duration = 30,
        surfaceChize=25979850,
        stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)

estimRem(x, dayrange, r, theta, hoursPerDay=24, Pdetect=1, nCT=100,
         duration=30, surfaceChize=25979850,
        stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)

estimatePopSize(x, dayrangeAll, dayrangeActive, detct, r=20, theta,
                nCT, surfaceChize=25979850, xActive=NULL, duration=30,
                stratified=FALSE, whichCTpaths=NULL,
                surfPaths=9857187)

}
\arguments{
  \item{x}{
    
    A data.frame containing the results of a camera trap monitoring
    during 30 days of a given year.  This data.frame contain one row per
    encounter between an animal and a camera trap during the study. It
    must contain the following variables: (i) \code{detection} is a
    logical value indicating if the encounter has been detected by the
    camera trapreturned; (ii) \code{whichani} is an integer variable
    indicating the animal ID for the encounter, (iii) \code{whichCT} is
    an integer variable indicating the camera trap ID for the encounter,
    (iv) \code{beginning}: an integer value indicating the date/time of
    the beginning of the encounter (converted as integer from the class
    \code{POSIXct}), (v) \code{beginning}: an integer value indicating
    the date/time of the end of the encounter (converted as integer from
    the class \code{POSIXct}), (vi) \code{duration}: the duration of the
    encounter in seconds, (vii) \code{Pdetect}: the theoretical
    probability of detection (as estimated by a survival model), (viii)
    \code{when}: a list indicating, for each encounter, the date/time of
    all associations during the encounter, expressed as integer values
    (converted from the class \code{POSIXct}.

  }
  \item{xActive}{
    
    A data.frame with the same structure as \code{x}, but focusing only
    on data collected during the active period.  If \code{NULL}, the
    function builds this object by removing all encounters occurring
    between 8h and 17h.
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
    The mean probability of detection of an encounter by a camera
    trap. When \code{stratified} is \code{TRUE}, the probability of
    detection is calculated from the data for each strata when
    \code{Pdetect < 1} 
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
  \item{dayrange}{
    The mean movement speed of animals (in meters/sec).
  }
  \item{dayrangeAll}{
    The mean movement speed of animals during the whole study period (in meters/sec).
  }
  \item{dayrangeActive}{
    The mean movement speed of animals during the activity period (in meters/sec).
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
  
}
\details{

  \code{estimIS} estimates the animal number over the area using
  Instantaneous Sampling (IS). \code{estimRem} estimates this animal
  number using Random Encounter Model (REM). Finally,
  \code{estimatePopSize} combines these two functions to estimate
  population size in the study area with: (i) REM on a modified
  dataset where all encounters are detected (perfect detectability),
  (ii) REM on the raw dataset (with some of the encounters not
  detected), without any attempt to correct for imperfect
  detectability or inactivity periods, (iii) REM on the dataset
  collected during active periods of the roe deer, and also accounting
  for imperfect detectability (calculating the probability of
  detection from the data), (iv) IS on the raw dataset (without
  accounting for the activity periods or imperfect detectability), (iv)
  IS on the dataset accounting for detectability, but not for the
  activity periods, (v) IS on the dataset focusing on activity periods,
  and accounting for imperfect detectability.
  
}
\value{
  For \code{estimIS} and \code{estimRem}, the estimated population
  size.\cr

  For \code{estimatePopSize}, a vector with the six estimated population
  sizes.
}
\references{
  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{bootstrapREM}} for a means to estimate standard errors on
  population size estimates.
}
\examples{
## Consider the example dataset sim4iter generated by simulateCTStudy5years
## Focus on the first iteration (= first study), and the first year
firstyear <- sim4iter$encounters[[1]]$results[[1]]

## bind all the pairs CT/animals in one data.frame
firstyeardf <- do.call(rbind, firstyear)

## Calculate mean dayrange for first study and first year
dr1 <-  mean(sim4iter$speed[[1]][[1]])

## Calculate mean dayrange for active periods for
## first study and first year
dra1 <-  mean(sim4iter$activeSpeed[[1]][[1]])

## Estimate for the IS, accounting for detectability.

## Calculation of the probability of detection (can be slow
    ## though it remains reasonable; nevertheless we give the
    ## resulting vector below)
\dontrun{
    ## get the required information to calculate the mean detection proba
    ## of associations on each trap:
    ## habitat
    suu <- sim4iter$encounters[[1]]$indicetraps$hab
    
    ## Seed used for the "content" of the detection zone 
    set.seed(sim4iter$encounters[[1]]$creationCT[2])

    ## Calculate raster maps of the 100 CT
    list100CT <- lapply(1:100, function(j) {
        cat("Traps: ", j,"\r")
        organiseCT(suu[j])
    })

    ## Mean detectability:
    prop <- sapply(1:length(list100CT), function(trap)
        mean(1-exp(-list100CT[[trap]]$vu)))

}

## The result is the following
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


## Estimate for the IS    
estimIS(firstyeardf, 20, theta=0.175, detct=prop)


## Estimate for the REM
estimRem(firstyeardf, dr1, 20, 0.175,
         Pdetect=mean(firstyeardf$detection), duration=30)

    
estimatePopSize(firstyeardf, dr1, dra1, prop, r=20, theta=0.175,
                nCT=100)


}
\keyword{models }
