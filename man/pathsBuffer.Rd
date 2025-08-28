\name{pathsBuffer}
\alias{pathsBuffer}
\docType{data}
\title{
  Twenty metres buffer from the Paths in the Chize Study Area
}
\description{
  This dataset is an object of class \code{sf} storing the map of
  a 20 m buffer from the paths in the study area.
}
\usage{data("pathsBuffer")}
\format{
  An object of class \code{sf} (from the package of the same name).
}
\source{
  Office Francais de la Biodiversite, Birieux, France.
}
\examples{
if (require(sf)) {
    plot(pathsBuffer)
}
}
\keyword{datasets}
