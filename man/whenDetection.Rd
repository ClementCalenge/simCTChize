\name{whenDetection}
\alias{whenDetection}
\title{
  When Does a Camera Trap Detects an Animal Passing in its Detection Zone
}
\description{
  This function takes a set of relocations from an animal and the a camera
  trap,  and returns the detected encounters and associations.
}
\usage{
whenDetection(movement, CT)
}
\arguments{
  \item{movement}{
    A data.frame with three columns (coordinates and dates of
    relocations) returned by the function \code{roeDeerMovement}.
  }
  \item{CT}{
    A raster map of the detectability by a camera trap, returned by the
    function \code{organiseCT}
  }
}
\value{
  
  A data.frame containing the results of a camera trap monitoring during
  30 days of a given year.  This data.frame contains one row per
  encounter between an animal and a camera trap during the study. It
  contains the following variables: (i) \code{detection} is a logical
  value indicating if the encounter has been detected by the camera
  trap returned; (ii) \code{whichani} is an integer variable indicating
  the animal ID for the encounter, (iii) \code{whichCT} is an integer
  variable indicating the camera trap ID for the encounter, (iv)
  \code{beginning}: an integer value indicating the date/time of the
  beginning of the encounter (converted as integer from the class
  \code{POSIXct}), (v) \code{beginning}: an integer value indicating the
  date/time of the end of the encounter (converted as integer from the
  class \code{POSIXct}), (vi) \code{duration}: the duration of the
  encounter in seconds, (vii) \code{Pdetect}: the theoretical
  probability of detection (as estimated by a survival model), (viii)
  \code{when}: a list indicating, for each encounter, the date/time of
  all associations during the encounter, expressed as integer values
  (converted from the class \code{POSIXct}.

}
\references{

  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
  
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{roeDeerMovement}} to generate roe deer movement,
  \code{\link{organiseCT}} to generate camera traps.
}
\examples{

################################
##
## We first simulate roe deer movement

## We use the same approach as in the examples of the function
## roeDeerMovement See this help page for further explanation.
setpat <- list(listPatches[[1]])
lipt <- list(ptg=mvtMatrices$listMatBetween[[1]],
             ptp=mvtMatrices$MatWithin)
set.seed(777) ## for reproducibility
rd <- roeDeerMovement(30, setpat, lipt = lipt)

## Transform into a data.frame, and add the date
rd <- as.data.frame(rd)
rd$date <- 1:nrow(rd)

################################
##
## Then simulate a camera trap in Coppice


## We use the same approach as in the examples of the function
## organiseCT. See this help page for further explanation.
set.seed(777)  ## for reproducibility
ct <- organiseCT("Coppice")


################################
## Match the two

## (we suppose that the camera trap
## is at the centroid of the home range)
## attribute cooori must be set
attr(ct, "cooori") <- c(0,0)                    

u <- log(ct[,3]+0.001)
v <- (u-min(u))/(max(u)-min(u))
plot(ct[,1],ct[,2], col=grey(v), pch=16, cex=0.7, asp=1,
     xlab="X", ylab="Y", main="Detection zone of a camera trap")
lines(rd[,1:2], col="red")

## We use the function whenDetection to identify when
## the roe deer passes through the detection zone
wd <- whenDetection(as.data.frame(rd), ct)

## Look at the encounters:
wd[,1:5]

## All of ther were detected
## Look at the times when the animals were detected, say
## for the fifth encounter
wd$When[5]


}
\keyword{ datagen }
