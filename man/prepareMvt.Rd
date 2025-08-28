\name{prepareMvt}
\alias{prepareMvt}
\title{
  Function to Prepare the Matrices Required to Simulate Roe Deer
  Movement Within or Between Patches 
}
\description{
  This function use the functions from the package \code{ctmm} to
  calculate the matrices (Green, Sigma, Lambda) required to simulate a
  bivariate Ornstein-Uhlenbeck with foraging (OUF) process.
}
\usage{
prepareMvt(tau = c(50, 30), sigma = 10)
}
\arguments{
  \item{tau}{
    Vector of length 2 containing the two autocorrelation parameters
    describing the movement of the animals (the first one describes the
    temporal autocorrelation of position and velocity respectively. 
}
\item{sigma}{
  standard deviation of the process.  
}
}
\details{
  
  This function relies heavily on the internal structure of the package
  ctmm (calls objects stored in the local environment
  \code{ctmm:::Kalman.env}).  Note that there is a risk that the
  internal structure of \code{ctmm} changes in the future, which could
  cause problem with this function. To prevent such problems, the
  package also provides the sets of matrices used in our simulations as
  datasets of the package (see the dataset \code{mvtMatrices}).
  
}
\value{
  A list of the three matrices required to simulate the OUF process
}
\references{

  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.

}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link[ctmm]{ctmm}}, \code{\link{mvtMatrices}}
}
\examples{
## Illustration of how we generated the dataset mvtMatrices

listMatBetween <- lapply(seq(1000,10000, by=1000),
                  function(x) prepareMvt(c(4000,4000), x))
MatWithin <- prepareMvt(c(300,300), 200)
mvtMatrices <- list(listMatBetween=listMatBetween,
                    MatWithin=MatWithin)

}
\keyword{datagen}
