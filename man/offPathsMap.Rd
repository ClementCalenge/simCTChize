\name{offPathsMap}
\alias{offPathsMap}
\docType{data}
\title{
  Map of the Areas Far From Paths in the Chize Study Area
}
\description{
  
  This dataset is an object of class \code{sfc} storing the map of the
  areas located at a distance greater than 20 m from the paths location
  encountered on the study area.

}
\usage{data("offPathsMap")}
\format{
  An object of class \code{sfc} (from the package of the same name).
}
\source{
  Office Francais de la Biodiversite, Birieux, France.
}
\examples{
if (require(sf)) {
    plot(offPathsMap, col="grey")
}
}
\keyword{datasets}
