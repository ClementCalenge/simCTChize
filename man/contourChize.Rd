\name{contourChize}
\alias{contourChize}
\alias{northChize}
\alias{southChize}
\docType{data}
\title{
  Spatial Coordinates of the Contour of the Chize Study Area
}
\description{
  These datasets contain the coordinates of the limits of the Chize
  study area, as well as the limits of the Northern and Southern part of
  the study area
}
\usage{

data("contourChize")

data("northChize")

data("southChize")

}
\format{
  These are data.frames with two columns (the x and y coordinates) of
  the contour.
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

plot(contourChize, asp=1, type="l", lwd=4)
polygon(northChize, asp=1, col="blue")
polygon(southChize, asp=1, col="green")

}
\keyword{datasets}
