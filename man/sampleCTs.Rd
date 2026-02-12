\name{sampleCTs}
\alias{sampleCTs}
\title{
  Resample a Camera Trap Study with a Smaller Number of Camera Traps
}
\description{
  This function takes the results of a previous simulation of a camera
  traps study and returns only the data concerning the first \code{nCT}
  traps (useful to test the effect of the number of traps on the
  precision of the estimation)
}
\usage{
sampleCTs(listSim, nCT)
}
\arguments{
  \item{listSim}{
    A list of class \code{simCT} returned by the function
    \code{simulateCTStudy5years}. 
  }
  \item{nCT}{
    The number of camera traps to return in the results
  }
}
\value{
  An object of class \code{simCT} concerning only the first
  \code{nCT} camera traps.
}
\references{
  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{simulateCTStudy5years}}
}
\examples{

## Consider sim4iter
sim4iter

## sample the first 25 traps
sampleCTs(sim4iter, 25)

}
\keyword{ datagen }
