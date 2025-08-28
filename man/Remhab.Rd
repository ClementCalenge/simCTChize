\name{Remhab}
\alias{Remhab}
\alias{IShab}
\title{
  Estimate the Population Size in a Habitat Type
}
\description{
  The functions \code{Remhab} and \code{IShab} can be used to estimate 
  population size in a given habitat type for each year, using a chosen
  method (REM or IS).
}
\usage{

Remhab(re, paths=TRUE, detection=TRUE,
       active=TRUE, verbose=TRUE, fieldangle=0.175, depth=20)

IShab(re, paths=TRUE, detection=TRUE,
      active=TRUE, verbose=TRUE,
      prop=NULL, fieldangle=0.175, depth=20)

}
\arguments{
  \item{re}{
    An object returned by the function \code{simulateCTStudy5years}.
  }
  \item{paths}{
    Logical value.  Whether population size should be estimated in the
    paths neighborhood (\code{TRUE}) or not (\code{FALSE}).
  }
  \item{detection}{
    Logical value.  Whether the detectability should be accounted for in
    the estimation (for the REM, the detection probability is estimated
    from the data).
  }
  \item{active}{
    Logical value.  Whether the activity of the animals should be
    accounted for in the estimation.
  }
  \item{verbose}{
    Logical value. Whether information should be returned to the user.
  }
  \item{prop}{
    A numeric vector containing the mean detection probability  for each trap.
  }
  \item{fieldangle}{
    The angular width of the detection zone (in radians)
  }
  \item{depth}{
    The radius (in metres) of the detection zone.
  }
  
}
\value{
  
  A list with one element per simulation, each element being a vector
  with five estimates corresponding to the five years.
  
}
\references{

  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.

}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{estimIS}} for more details on the population size
  estimation using instantaneous sampling and random encounter models. 
}
\examples{

## No examples


}
\keyword{robust }
\keyword{distribution }
