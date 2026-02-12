\name{bootstrapREM}
\alias{bootstrapREM}
\alias{bootstrapIS}
\alias{bootstrapREMcompar}
\alias{bootstrapIScompar}
\title{
  Estimation of Standard Errors of Population Size Estimates with the Bootstrap
}
\description{
  These functions can be used to estimate the standard error of
  population size estimates for random encounter models and
  instantaneous sampling with the bootstrap.
}
\usage{
bootstrapREM(x, Nboot, dayrange, r, theta, hoursPerDay = 24,
             surfaceChize=25979850, nCT = 100, detection = TRUE,
             stratified=FALSE, whichCTpaths=NULL,
             surfPaths=9857187)

bootstrapREMcompar(x, x2, Nboot, dayrange, dayrange2, r, theta,
                   hoursPerDay = 24, surfaceChize=25979850, nCT = 100, detection = TRUE,
                   stratified=FALSE, whichCTpaths=NULL,
                   surfPaths=9857187)

bootstrapIS(x, Nboot, r, theta, detct, hoursPerDay = 24,
            surfaceChize=25979850, nCT = 100,
            stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)

bootstrapIScompar(x, x2, Nboot, r, theta, detct, hoursPerDay = 24,
                  surfaceChize=25979850, nCT = 100,
                  stratified=FALSE, whichCTpaths=NULL,
                  surfPaths=9857187)
}
\arguments{
  \item{x}{
    
    A data.frame containing the results of a camera trap monitoring
    during 30 days of a given year.  This data.frame contains one row per
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
\item{x2}{

  A data.frame containing the same kind of data as \code{x}, but for the
  second year of study.

}
\item{Nboot}{
  Number of bootstrap samples
}
\item{dayrange}{
  Mean movement speed (in meters per second) of animals for the first year.
}
\item{dayrange2}{
  Mean movement speed (in meters per second) of animals for the second year.
}
\item{r}{
  Mean radius (in metres) of the detection zone of the camera traps. 
}
\item{theta}{
  Mean angle (in radians) of the detection zone of the camera traps. 
}
\item{hoursPerDay}{
  Number of hours per day during which the traps were active.
}
\item{surfaceChize}{
  Numeric value indicating the surface area of the Chize study area
}
\item{nCT}{
  Number of camera traps.
}
\item{detection}{
  Logical value. If \code{TRUE}, the function supposes a perfect
  detectability of the encounters (i.e. supposes that the detection
  probability of encounter is equal to 1). Otherwise the mean detection
  probability of encounters is estimated from the data.
}
\item{detct}{
  A vector with the same length as \code{nCT} indicating for each camera
  trap the mean detection probability of an association.
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
  These functions resample camera traps to estimate the precision of
  population size estimates with instantaneous sampling and random
  encounter models.  The function ending with \code{compar} allow the
  estimation of the imprecision of the rate of change (N2-N1)/N1 between
  two years.
}
\value{
  The functions \code{bootstrapIS} and \code{bootstrapREM} return a
  vector of \code{Nboot} population size estimates corresponding to the
  \code{Nboot} bootstrap samples.\cr

  The functions \code{bootstrapIScompar} and \code{bootstrapREMcompar}
  return a vector of \code{Nboot} estimates of the rate of change
  between the two years corresponding to the \code{Nboot} bootstrap
  samples.
  
}
\references{

  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.

}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{estimIS}} for more details on the population size
  estimation using instantaneous sampling and random encounter models. 
}
\examples{

## Consider the example dataset sim4iter generated by simulateCTStudy5years
## Focus on the first iteration (= first study), and the first and fifth years
year1l <- sim4iter$encounters[[1]]$results[[1]]
year5l <- sim4iter$encounters[[1]]$results[[5]]

## bind all the pairs CT/animals in one data.frame
year1df <- do.call(rbind, year1l)
year5df <- do.call(rbind, year5l)

## Calculate mean dayrange for first study and first and fifth years
dr1 <-  mean(sim4iter$speed[[1]][[1]])
dr5 <-  mean(sim4iter$speed[[1]][[5]])


## Mean association detectability for each trap (see the help page
## of estimIS for more details on the estimation of this vector)
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




##################################
##
## Instantaneous sampling (activity not taken into account): 

## Point estimate
(esti1 <- estimIS(year1df, 20, theta=0.175, detct=prop))
(esti5 <- estimIS(year5df, 20, theta=0.175, detct=prop))

## bootstrap samples to estimate the corresponding coefficients
## of variation
## year 1
biy1 <- bootstrapIS(year1df, Nboot=1000, r=20, theta=0.175,
                    detct=prop, nCT = 100)
(cv1 <- sd(biy1)/mean(biy1))

## year 5
biy5 <- bootstrapIS(year5df, Nboot=1000, r=20, theta=0.175,
                    detct=prop, nCT = 100)
(cv5 <- sd(biy5)/mean(biy5))

## Estimated rate of change:
(esti5-esti1)/esti1

## Bootstrap confidence interval on this rate of change
brci <- bootstrapIScompar(year1df, year5df, Nboot=1000, r=20,
                          theta=0.175, detct=prop,
                          nCT = 100)

quantile(brci, c(0.05, 0.95))




##################################
##
## Random Encounter models (activity not taken into account): 

## Point estimate
(estr1 <- estimRem(year1df, dayrange=dr1, r=20, theta=0.175,
                   Pdetect=mean(year1df$detection)))
(estr5 <- estimRem(year5df, dayrange=dr1, r=20, theta=0.175,
                   Pdetect=mean(year5df$detection)))

## bootstrap samples to estimate the corresponding coefficients
## of variation
## year 1
bry1 <- bootstrapREM(year1df, Nboot=1000, dayrange=dr1, r=20, theta=0.175,
                    nCT = 100, detection=TRUE)
(cvr1 <- sd(bry1)/mean(bry1))

## year 5
bry5 <- bootstrapREM(year5df, Nboot=1000, dayrange=dr5, r=20, theta=0.175,
                     nCT = 100, detection=TRUE)
(cvr5 <- sd(bry5)/mean(bry5))

## Estimated rate of change:
(esti5-esti1)/esti1

## Bootstrap confidence interval on this rate of change:
brcr <- bootstrapREMcompar(year1df, year5df, dayrange=dr1,
                           dayrange2 = dr5, Nboot=1000, r=20,
                           theta=0.175, detection=TRUE,
                           nCT = 100)

quantile(brcr, c(0.05, 0.95))



}
\keyword{robust }
\keyword{distribution }
