\name{pathsMap}
\alias{pathsMap}
\docType{data}
\title{
  Map of the Paths Location in the Chize Study Area
}
\description{
  This dataset is an object of class \code{sf} storing the map of
  the paths location encountered on the study area (actually, a buffer
  of 50 cm around path segments).
}
\usage{data("pathsMap")}
\format{
  An object of class \code{sf} (from the package of the same name).
}
\source{
  Office Francais de la Biodiversite, Birieux, France.
}
\examples{
if (require(sf)) {
    plot(pathsMap)
}
}
\keyword{datasets}
