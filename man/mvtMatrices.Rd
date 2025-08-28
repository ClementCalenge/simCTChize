\name{mvtMatrices}
\alias{mvtMatrices}
\docType{data}
\title{
  Matrices Used to Simulate the OUF processes building up the Roe Deer Movements
}
\description{
  The set of matrices required to simulate Ornstein-Uhlenbeck with
  foraging processes that are the building blocks of simulations of roe
  deer movement.
}
\usage{data("mvtMatrices")}
\format{
  
  This list has two elements named \code{listMatBetween} and
  \code{MatWithin}, containing respectively the matrices (Green, Sigma,
  Lambda) required to simulate a bivariate Ornstein-Uhlenbeck with
  foraging (OUF) process.\cr

  The element \code{listMatBetween} is a list with 10 sets of three
  matrices, corresponding to 10 different values of sigma. Each element
  of this matrix corresponds to a possibility (with a more or less
  important value of \code{sigma)} for the between-patch movement of the
  roe-deer.  The element \code{matWithin} corresponds to the triplet of
  matrices used to describe the within-patch movement.
}
\details{
  
  This dataset was prepared using the function \code{prepareMvt} from
  the package (see the examples for an example). Because this function
  relies on internal functions of the package \code{ctmm} (which can be
  problematic if the internal structure of \code{ctmm} changes), we
  preferred to also provide the result as a dataset of the package. This
  leads to a package \code{simCTChize} less dependent of external
  package. 

}
\source{
  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\references{
  Calenge et al. 2025. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\examples{
## The dataset is created using the following command lines:
listMatBetween <- lapply(seq(1000,10000, by=1000),
                  function(x) prepareMvt(c(4000,4000), x))
MatWithin <- prepareMvt(c(300,300), 200)
mvtMatrices <- list(listMatBetween=listMatBetween,
                    MatWithin=MatWithin)

}
\keyword{datasets}
