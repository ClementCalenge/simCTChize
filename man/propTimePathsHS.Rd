\name{propTimePathsHS}
\alias{propTimePathsHS}
\docType{data}
\title{
  Proportion of the Time Spent by 1000 Animals in Paths Neighborhood in
  our Simulations of a Population During Five Years
}
\description{
  
  This dataset contains the proportion of the time spent in paths
  neighborhood by 1000 animals simulated during the five years of our
  study (with a simulated selection of this neighborhood by the animals
  and a simulated increase in home-range size).
  
}
\usage{data("propTimePathsHS")}
\format{

  This dataset is a data.frame with 1000 rows (the 1000 animals) and
  five columns (the five years).

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

## We simulated this dataset using the following code
## warning: this code is slow (approx 20 min)
\dontrun{

## Number of simulated animals
ntotani <- 1000


## Required for the simulation: the centroid of animals is located at
## at least 600 m from the boundary of the study area (to avoid
## movement outside the study area).
cartec <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize))))
sf::st_crs(cartec) <- sf::st_crs(pathsBuffer)
cartec <- sf::st_buffer(cartec, -600)

## We then intersected the map of the paths neighborhood with this
## "eroded map"
corePaths <- sf::st_intersection(pathsBuffer, cartec)
po1 <- sf::st_sfc(sf::st_polygon(list(as.matrix(northChize))))
sf::st_crs(po1) <- sf::st_crs(corePaths)

## Because 193/300 animals were located in the northern part, we
## intersected this paths map with the limits of the north an south
## of Chize
northPaths <- sf::st_intersection(corePaths, po1)
po1 <- sf::st_sfc(sf::st_polygon(list(as.matrix(southChize))))
sf::st_crs(po1) <- sf::st_crs(corePaths)
southPaths <- sf::st_intersection(corePaths, po1)

## Parameters of movement to simulate the increase in home range size
## (through the use of sigma) and the habitat selection (through
## HSlocation)
paramani <- lapply(1:ntotani, function(i) {
    cat(i,"\r")
    north <- (runif(1) < 193/300)
    minsigan <- round(4 * (1:5)/5)
    maxsigan <- round(3 + 7 * (0:4)/4)
    sigmaan <- sapply(1:5, function(t) sample(minsigan[t]:maxsigan[t], 
                                              1))
    zone <- southPaths
    if (north) 
        zone <- northPaths
    lxyz <- HSlocation(zone, pathsBuffer)
    return(list(lxyz = lxyz, sigmaan = sigmaan))
})

## Simulation of the movements of the 1000 animals +
## calculation of the proportion of relocations in paths neighborhood.
propTimePathsHS <- matrix(0, nrow=1000, ncol=5)
for (iAni in 1:ntotani) {
    cat(iAni,"\r")
    for (year in 1:5) {

        ## Simulate the movement
        paAni <- paramani[[iAni]]
        lipt <- list(ptg=mvtMatrices$listMatBetween[[paAni$sigmaan[year]]],
                     ptp=mvtMatrices$MatWithin)
        lixyt <- list(paAni$lxyz$patchs, paAni$lxyz$patchs)
        z <- roeDeerMovement(30, lixyt, lipt = lipt, 
                             verbose = FALSE)
        z <- t(t(z)+as.vector(paAni$lxyz$centroide))

        ## To speed up the calculation, consider one reloc every 1000
        ## seconds
        z <- z[seq(1,nrow(z), by=1000),]

        ## Join with the map of paths Buffer, and calculate the proportion of
        ## relocations that are in this habitat type
        z <- as.data.frame(z)
        jo <- st_join(st_as_sf(z,coords=c("x","y"), crs=st_crs(pathsBuffer)),pathsBuffer)
        propTimePathsHS[iAni, year] <- mean(!is.na(jo$id))
    }
}

}


}
\keyword{datasets}
