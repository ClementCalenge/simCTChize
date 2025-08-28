strsampleCT <- function(nCT=100, propPaths=0.75,
                        pathsMap, pathsBuffer,
                        offPathsMap, offPathsBuffer)
{

    if (!((inherits(pathsMap, "sf"))|(inherits(pathsMap,"sfc"))))
        stop("pathsMap should inherit the class \"sf\" or \"sfc\"")
    if (!((inherits(pathsBuffer, "sf"))|(inherits(pathsBuffer,"sfc"))))
        stop("pathsBuffer should inherit the class \"sf\" or \"sfc\"")
    if (!((inherits(offPathsMap, "sf"))|(inherits(offPathsMap,"sfc"))))
        stop("offPathsMap should inherit the class \"sf\" or \"sfc\"")
    if (!((inherits(offPathsBuffer, "sf"))|(inherits(offPathsBuffer,"sfc"))))
        stop("offPathsBuffer should inherit the class \"sf\" or \"sfc\"")

    ## number of CTs in and out of the path area
    ndans <- round(nCT*propPaths)
    nhors <- round(nCT-ndans)
    locdans <- matrix(numeric(0), ncol=2)
    lochors <- matrix(numeric(0), ncol=2)

    ## If any CT in the path area
    if (ndans>0) {

        ## sample ndans points in the path area
        ptsdans <- sf::st_sample(pathsBuffer,ndans)
        ## get the coordinates and build a buffer of 25 m around the
        ## point. Converts to sf
        locdans <- sf::st_coordinates(ptsdans)
        dbuf <- data.frame(idb=1:nrow(locdans))
        dbuf$geom <- sf::st_buffer(ptsdans, 25)
        bu <- sf::st_as_sf(dbuf)
        sf::st_agr(bu) <- "constant"
        sf::st_agr(pathsMap) <- "constant"
        ## gets the segments of the paths (linear features)
        ## within the buffer of 25 m
        sf::st_crs(bu) <- sf::st_crs(pathsMap)
        ou <- sf::st_intersection(bu, pathsMap)
        ## In cases where the st_sample did not sample exactly ndans points
        ## (can happen with st_sample)
        while (nrow(ou)<ndans) {
            ## Redo the same thing for the remaining points.
            nrest <- ndans-nrow(ou)
            ptsrest <- sf::st_sample(pathsBuffer,nrest)
            locrest <- sf::st_coordinates(ptsrest)
            dbuf <- data.frame(idb=1:nrest)
            dbuf$geom <- sf::st_buffer(ptsrest, 25)
            bu <- sf::st_as_sf(dbuf)
            sf::st_crs(bu) <- sf::st_crs(pathsMap)
            ou2 <- sf::st_intersection(bu, pathsMap)
            ou <- rbind(ou,ou2)
        }
        ## Then, sample one point on the segments for each sample.
        ## This makes sure that the points are located on the paths
        locsdans <- t(sapply(1:ndans, function(i) {
            sf::st_coordinates(sf::st_sample(sf::st_geometry(ou)[i],1))
        }))
    }

    ## Same idea for off-path area
    if (nhors>0) {
        ## samples nhors in offpath areas, get the coordinates and
        ## builds a 25 m buffer around the point.
        ptshors <- sf::st_sample(offPathsMap,nhors)
        lochors <- sf::st_coordinates(ptshors)
        dbuf <- data.frame(idb=1:nrow(lochors))
        dbuf$geom <- sf::st_buffer(ptshors, 25)
        bu <- sf::st_as_sf(dbuf)
        sf::st_agr(bu) <- "constant"
        hche <- sf::st_sf(data.frame(id=1, geom=offPathsBuffer))
        sf::st_agr(hche) <- "constant"
        ## then makes intersection with area located further from the
        ## paths (to make sure that detection area does not intersect
        ## the path area).
        sf::st_crs(bu) <- sf::st_crs(hche)
        ou <- sf::st_intersection(bu, hche)

        ## Repeats in case the sample did not result in enough points
        while (nrow(ou)<nhors) {
            nrest <- nhors-nrow(ou)
            ptsrest <- sf::st_sample(offPathsMap,nrest)
            locrest <- sf::st_coordinates(ptsrest)
            dbuf <- data.frame(idb=1:nrest)
            dbuf$geom <- sf::st_buffer(ptsrest, 25)
            bu <- sf::st_as_sf(dbuf)
            sf::st_crs(bu) <- sf::st_crs(hche)
            ou2 <- sf::st_intersection(bu, hche)
            ou <- rbind(ou,ou2)
        }
        locshors <- t(sapply(1:nhors, function(i) {
            sf::st_coordinates(sf::st_sample(sf::st_geometry(ou)[i],1))
        }))

    }

    ## Puts this in a big table
    locs <- rbind(locdans,lochors)
    ## And randomize the order of CTs
    locs <- locs[sample(1:nCT),]
    return(locs)
}
