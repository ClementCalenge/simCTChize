\name{HRspeed}
\alias{HRspeed}
\docType{data}
\title{
  Changes With Time in Home-Range Size and Mean Speed of Roe Deer
}
\description{
  When we carried out our simulations, the parameters of roe
  deer movements changed with time.  This data.frame illustrates on a
  set of 200 roe deer how the resulting home-range size (estimated by
  the minimum convex polygon) and the mean movement speed changed from
  year 1 to year 5.
}
\usage{data("HRspeed")}
\format{
  A data frame with 400 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector indicating the year.}
    \item{\code{HRsize}}{a numeric vector indicating the home-range size
      (expressed in hectares)}
    \item{\code{speed}}{a numeric vector indicating the mean speed
      (expressed in metres/second)}
  }
}
\details{
  See examples for the code used to build this data.frame.
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
## This dataset has been obtained using the following code:

## list of Patches used for year 1 and year 5
patchesYear1 <- listPatches[sapply(listPatches,nrow)<=10]
patchesYear5 <- listPatches

## remember that the list of movement matrices contain one element per
## value of sigma2 comprised between 1000 and 10000. Thus:
mvtMatricesYear1 <- mvtMatrices
mvtMatricesYear1$listMatBetween <- mvtMatricesYear1$listMatBetween[1:3]
mvtMatricesYear5 <- mvtMatrices


## Load adehabitatHR for home-range size estimation
if (require(adehabitatHR)) {

## Initialization
simHRspeed <- list()

## Simulation
set.seed(777) ## for reproducibility

## For each year
for (year in c(1,5)) {

    homeRangeSizey <- numeric(0) 
    speedy <- numeric(0)

    ## And for each animal
    for (i in 1:200) {
        cat("year", year,"animal", i,"\r")

        lipt <- list(ptg=mvtMatricesYear1$listMatBetween[[sample(1:3,1)]],
                     ptp=mvtMatrices$MatWithin)

        z <- roeDeerMovement(30, lixy=patchesYear1, verbose=FALSE, lipt=lipt)

        ## Calculation of average speed
        speedy[i] <- mean(sqrt(rowSums((z[-1,]-z[-nrow(z),])^2)))

        ## Home-range size estimation if the animal had been monitored by
        ## GPS with one relocation every 20 min
        m <- as.integer(seq(1,nrow(z),by=60*20))
        zb <- z[m,]
        homeRangeSizey[i] <- mcp(SpatialPoints(zb))[[2]]
    }
    simHRspeed[[year]] <- data.frame(year=year, HRsize=homeRangeSizey,
                                     speed=speedy)
}

HRspeed <- do.call(rbind, simHRspeed)

}
}
\keyword{datasets}
