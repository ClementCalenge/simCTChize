\name{habitatMap}
\alias{habitatMap}
\docType{data}
\title{
  Habitat Map of the Chize Study Area
}
\description{
  This dataset is an object of class \code{sf} storing the map of
  the three habitat types encountered on the study area (coppice,
  regeneration and open areas).
}
\usage{data("habitatMap")}
\format{
  An object of class \code{sf} (from the package of the same name).
}
\source{
  Office Francais de la Biodiversite, Birieux, France.
}
\examples{
if (require(sf)) {
    plot(habitatMap)
}
}
\keyword{datasets}
