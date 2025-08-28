\name{listPatches}
\alias{listPatches}
\docType{data}
\title{
  List of 25 Sets of Patch Centroids for Roe Deer Movement in Chize
}
\description{
  This dataset is a list containing the 25 sets of patch centroids used
  to simulate roe deer movement in Chize (see details)
}
\usage{data("listPatches")}
\format{
  A list of 25 matrices containing the xy coordinates of the patch
  centroids.
}
\details{
  This list was obtained from the GPS monitoring of 25 roe deer in
  Chize during one month (in March), between 2003 and 2008. The
  utilization distribution of these 25 animals was estimated using the
  kernel method with a small bandwidth (h = 50 m), and local maxima of the
  estimated UD were derived as patch centroids. Each matrix is centered
  so that the mean xy coordinates of all patches of a given animal is
  equal to (0,0). The original position is available as the attribute
  \code{"scaled:center"} of the matrix.   
}
\source{
  Office Francais de la Biodiversite, Birieux, France.
}
\references{
  Une ref.
}
\examples{

## We use this list of set of patches:
str(listPatches)

## Let's say that we will simulate animals with the first
## set of patches:
setpat <- list(listPatches[[1]])

## We use the dataset mvtMatrices for the set of matrices
str(mvtMatrices)

## We will simulate an animal with sigma=1000 for between-patches
## movements (first matrix of listMatBetween).
lipt <- list(ptg=mvtMatrices$listMatBetween[[1]],
             ptp=mvtMatrices$MatWithin)

## Simulation of movement for 30 days:
set.seed(777) ## for reproducibility
rd <- roeDeerMovement(30, setpat, lipt = lipt)

## movement (centroid = c(0,0))
plot(rd[,1:2], ty="l")

## Add the patch centroid for information
points(setpat[[1]], pch=21, cex=2, bg="yellow")


}
\keyword{datasets}
