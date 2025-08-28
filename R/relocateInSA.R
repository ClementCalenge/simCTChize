relocateInSA <-
function(xy, cch, ani, sigma, dff)
{
    ## Maximum distance from the centroid
    distance <- 1.5*dff$dmaxp[dff$sigma==sigma&dff$ani==ani]

    ## if the distance between centroid xy and study area contour is
    ## lower than this distance
    if (min(sqrt(((cch[,1]-xy[1])^2)+((cch[,2]-xy[2])^2)))<distance) {

        ## Inner buffer of -distance from this external contour of the
        ## study area
        pol <- sf::st_sfc(sf::st_polygon(list(as.matrix(cch))))
        polb <- sf::st_buffer(pol,-distance) |> sf::st_coordinates()

        ## Rediscretize the contour of this buffe.
        polb <- polb[,1:2]
        zone <- discretize(polb,length=15)

        ## finds the closest point to this centroid
        pt <- zone[which.min(sqrt(((zone[,1]-xy[1])^2)+((zone[,2]-xy[2])^2))),]
    } else {
        pt <- xy
    }
    return(pt)
}
