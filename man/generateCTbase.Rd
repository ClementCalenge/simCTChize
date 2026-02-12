\name{generateCTbase}
\alias{generateCTbase}
\alias{obstaclesQ}
\alias{organiseCT}
\title{
  Simulate the Raster Map of Detection Zone of a Camera Trap
}
\description{

  The function \code{obstaclesQ} returns a raster map of the detection
  zone of a camera trap with obstacles hiding a part of the zone.  The
  function \code{generateCTbase} takes this maps and accounts for a
  decreasing probability of detection of an animal with distance from a
  camera trap.  The function \code{organiseCT} combines the two previous
  functions for the simulations of a camera trap study in the Chize
  forest (it simulates three types of zones: traps placed in coppice,
  in regeneration, or in open area, and parameterizes the functions
  suitably).

}
\usage{
obstaclesQ(depth, stemsha=5000, diamMin=0.2, diamMax=0.3, res=0.1)

generateCTbase(eg, orientationE, angle, depth, D1 = 5, D2 = 15)

organiseCT(habitatType, fieldangle=0.175, depth=20)
}
\arguments{
  \item{eg}{
    A data.frame returned by the function \code{obstaclesQ}, with three
    columns: the x and y coordinates of the pixels in the detection
    zone, and a logical variable named \code{vu} indicating if the pixel is
    visible from the camera trap.
  }
  \item{orientationE}{
    The angle relative to the East direction of the side of the
    detection zone.
  }
  \item{angle}{
    The angular width of the detection zone (in radians)
  }
  \item{depth}{
    The radius (in metres) of the detection zone.
  }
  \item{D1}{
    The smaller distance under which the probability to detect an
    animal is 0.99 or greater.
  }
  \item{D2}{
    The larger distance beyond which less than 1 percent of animals are
    detected.
  }
  \item{stemsha}{
    The density of stems per hectare to simulate in the detection zone.
  }
  \item{diamMin}{
    The minimum diameter of stems to simulate.
  }
  \item{diamMax}{
    The maximum diameter of stems to simulate.
  }
  \item{res}{
    The resolution of the raster map of the detection probability within
    the detection zone(in meters).
  }
  \item{habitatType}{
    The habitat type where the camera trap is located. Either
  \code{"Coppice"}, \code{"REG"} (for regeneration), or \code{"OPE"}
  (open habitat).
  }
  \item{fieldangle}{
    The angular width of the detection zone (in radians)
  }
}
\value{
  The function \code{obstaclesQ} returns a data.frame with three
  columns: the x and y coordinates of the pixels in the detection
  zone, and a logical variable named \code{vu} indicating if the pixel is
  visible from the camera trap.\cr

  The function \code{generateCTbase} returns the same sort of
  data.frame, but the variable \code{vu} is now a continuous variable
  containing the probability of detection of an animal in 1 second.\cr

  The function \code{organiseCT} returns the same object as
  \code{generateCTbase}, but with attributes containing specifications
  of the trap (angleEast, fieldAngle, and radius).
}
\references{
  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\examples{

## generate the raster map of a camera trap
## in coppice
set.seed(777)  ## for reproducibility
ct <- organiseCT("Coppice")
head(ct)

## Show the detection zone
u <- log(ct[,3]+0.001)
v <- (u-min(u))/(max(u)-min(u))
plot(ct[,1],ct[,2], col=grey(v), pch=16, cex=0.5, asp=1,
     xlab="X", ylab="Y", main="Detection zone of a camera trap")

## Notice the presence of a tree blocking the view on the side
## of the detection zone camera trap.


## Play a bit with the other functions

## Surroundings of a camera trap in a forest with dense forest
obs <- obstaclesQ(20, stemsha=500, diamMin=0.5, diamMax=1, res=0.1)
head(obs)
plot(obs[,1], obs[,2], asp=1,
     col=obs[,3]+2, pch=16, cex=0.5)
points(0,0) ## place of the camera trap

## also generate very large bushes but with small density:
obs2 <- obstaclesQ(20, stemsha=50, diamMin=3, diamMax=8, res=0.1)
head(obs2)
plot(obs2[,1], obs2[,2], asp=1,
     col=obs2[,3]+2, pch=16, cex=0.5)
points(0,0) ## place of the camera trap

## Combine the two sort of structures, and
## we suppose that the probability of detection when a deer
## is in or behind the bushes is 0.5 when the detection is perfect
## everywhere (except for the trees and bush)
obs[obs2==0&obs==1] <- 0.5
plot(obs[,1], obs[,2], asp=1,
     col=grey(obs[,3]*0.8), pch=16, cex=0.5)
points(0,0) ## place of the camera trap

## Now place a camera trap with ultra-wide angle directed toward the East
## With a probability of detection equal to 0.99 or greater under 5m
## and 0.01 beyond 15 m.
ctt <- generateCTbase(obs, orientationE=0, angle=pi/4, depth=20,
                      D1 = 5, D2 = 15)
u <- log(ctt[,3]+0.001)
v <- (u-min(u))/(max(u)-min(u))
plot(ctt[,1],ctt[,2], col=grey(v), pch=16, cex=0.5, asp=1,
     xlab="X", ylab="Y", main="Detection zone of a camera trap")


}
\keyword{ datagen }
