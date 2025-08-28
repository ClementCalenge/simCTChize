\name{HSlocation}
\alias{HSlocation}
\title{
  Simulate a Set of Random Patches of a Roe Deer Home Range Located in a
  Given Habitat Type.
}
\description{
  
  This function is used by the function \code{simulateCTStudy5years} to
  simulate habitat selection by the roe deer.
  
}
\usage{
HSlocation(area, selectedArea)
}
\arguments{
  \item{area}{
    An object of class \code{sf} (from the package of the same name)
    containing the limits of the area where the centroid is randomly sampled.
  }
  \item{selectedArea}{
    An object of class \code{sf} (from the package of the same name)
    containing the limits of the areas where the patches coordinates are
    sampled within 100-300 m from the centroid.    
  }
}
\details{
  
  This function first random sample a centroid in the area
  \code{area}. Then a random number of patches (uniform between 3 and
  10) is selected in the selected areas \code{selectedArea}, within a
  random distance from this centroid (this distance is drawn randomly
  from a uniform distribution bounded by 100 m and 300 m).
}
\value{
  
  A list with two components: (i) \code{centroide} is a vector of length
  2 containing the coordinates of the centroid, (ii) \code{patchs} is a
  matrix of length two containing the xy coordinates of the sampled
  patches. 

}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{simulateCTStudy5years}}
}
\examples{

 if (require(sf)) {

   ## Chize map
   chizeMap <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize))))
   plot(chizeMap, col="red")

   ## Erode this map by 600 m
   erodedMap <- sf::st_buffer(chizeMap, -600)
   plot(erodedMap, col="red")
  
   ## Intersects with areas located < 20 m from paths
   sf::st_crs(pathsBuffer) <- sf::st_crs(erodedMap)
   corePaths <- sf::st_intersection(pathsBuffer, erodedMap)
   plot(sf::st_geometry(chizeMap), col="grey")
   plot(corePaths, col="red", add=TRUE)

   ## A random centroid will be randomly drawn from the maps in
   ## corePaths and patches will be drawn from pathsBuffer
   set.seed(777) ## for reproducibility
   (locs <- HSlocation(corePaths, pathsBuffer))
   
   ## add these points on the map:
   plot(sf::st_geometry(chizeMap), col="grey")
   plot(pathsBuffer, col="red", add=TRUE)
   points(sweep(locs$patchs, 2, locs$centroide, "+"), pch=21,
          bg="yellow")
   ## All the points are on the paths
 }

}
\keyword{ datagen }
