\name{strsampleCT}
\alias{strsampleCT}
\title{
  Stratified Sampling for Camera Trap Studies
}
\description{
  This function randomly samples camera trap locations so that a given
  proportion is located on paths.
}
\usage{
strsampleCT(nCT = 100, propPaths = 0.75, pathsMap, pathsBuffer, offPathsMap, offPathsBuffer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nCT}{
    The number of camera trap locations to sample
  }
  \item{propPaths}{
    The proportion of traps on the paths.
  }
  \item{pathsMap}{
    An object of class \code{sf/sfc} (from the package of the same name)
    storing the location of the paths in the study area (polygon
    features).  See the dataset \code{pathsMap} for such a map in 
    Chize (it is actually a 50 cm buffer around the paths linear
    features).
  }
  \item{pathsBuffer}{
    An object of class \code{sf/sfc} (from the package of the same name)
    storing the map of all areas located at less than 20 m from a path
    in the study area (polygon features).  This distance of 20 m is
    necessary as this is the maximum radius of the detection zone of a
    camera trap.  This map is used to identify all areas detectable from
    a trap located on the path See the dataset \code{pathsBuffer}
    for such a map in Chize. 
  }
  \item{offPathsMap}{
    An object of class \code{sf/sfc} (from the package of the same name)
    storing the map of all areas located at more than 20 m from a path
    in the study area (polygon features).  See the dataset
    \code{offPathsMap} for such a map in Chize.     
  }
  \item{offPathsBuffer}{
    An object of class \code{sf/sfc} (from the package of the same name)
    storing the map of all areas located at more than 40 m from a path
    in the study area (polygons).  This map is used to make that the
    detection zone of all sampled CTs in the off-paths area does not
    intersect the area close to the paths (which is selected by the
    deer). See the dataset \code{offPathsBuffer} for such a map in Chize. 
  }
}
\value{
  A matrix with two columns containing the xy coordinates of the camera
  traps. 
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{simulateCTStudy5years}}
}
\examples{

 if (require(sf)) {
  
   ## randomly sample 100 camera traps all on paths
   set.seed(777) ## for reproducibility
   ctloc <- strsampleCT(nCT = 100, propPaths = 1,
                        pathsMap, pathsBuffer, offPathsMap,
                        offPathsBuffer)
   
   ## add these points on the map:
   chizeMap <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize))))
   plot(sf::st_geometry(chizeMap), col="grey")
   plot(pathsBuffer, col="red", add=TRUE)
   points(ctloc, pch=21,
          bg="yellow")
   ## All the points are on the paths
 }


}
\keyword{ datagen }
