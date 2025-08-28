\name{relationAnimalSigmaMaxd}
\alias{relationAnimalSigmaMaxd}
\docType{data}
\title{
  Relationship Between Parameters of an OUF Process and the Maximum
  Predicted Distance of the Animal to its Home Range Centroid
}
\description{
  This dataset contains the predictions of a model assessing the
  relationship between the parameter \code{sigma} of an
  Orstein-Uhlenbeck with foraging (OUF) process, the set of patches in a
  home range, and the
  maximum predicted distance of the animal to its home-range centroid (see
  details). 
}
\usage{data("relationAnimalSigmaMaxd")}
\format{
  A data frame with 250 observations on the following 3 variables.
  \describe{
    \item{\code{sigma}}{a numeric vector}
    \item{\code{ani}}{a numeric vector}
    \item{\code{dmaxp}}{a numeric vector}
  }
}
\details{
  
  This dataset is used by the function \code{simulateCTStudy5years} to
  simulate roe deer movements that are located within the study area
  (and avoid movement outside the study area, as such movement would
  lead to a temporary decrease of the number of animals present in the
  study area). We simulated the movements of 10 roe deer for one month
  with the function \code{roeDeerMovement}, using a specific set of
  patches (the different set of patches used here are stored in the
  dataset \code{listPatches}) and a given value of \code{sigma} (the
  actual value of sigma used by the Ornstein-Uhlenbeck is 1000 times the
  value indicated in the column \code{sigma} of this dataset). We then
  calculated the maximum distance between a simulated relocation and the
  centroid of the home range. We then used a linear model to predict the
  logarithm of this maximum distance as a fonction of the set of patch
  and of \code{sigma}. For each combination of value of \code{sigma} and
  set of patches, the dataset \code{relationAnimalSigmaMaxd} stores the
  predicted maximum distance + 3 times the standard error of this
  prediction in the variable \code{dmaxp}.
  
}
\source{
  Obtained by simulations.
}
\references{
  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\examples{
data(relationAnimalSigmaMaxd)

head(relationAnimalSigmaMaxd)
}
\keyword{datasets}
