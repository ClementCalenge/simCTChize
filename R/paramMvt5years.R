paramMvt5years <-
function(smallHRlist, largeHRlist, north=TRUE, cch, nch, sch, dff)
{
    ## Sampling of sigmas for the five years (increasing home range):
    ma <- sapply(1:5, function(t) 3+floor((t-1)*1.75))
    sigma <- sapply(1:5, function(t) sample(1:ma[t], 1))
    ## Random sample of home-ranges:
    ani <- sapply(1:5, function(t) {
        gd <- sample(largeHRlist, floor((t-1)*6/4))
        li <- c(smallHRlist, gd)
        sa <- sample(li, 1)
        return(sa)
    })
    ## Random sample of centroid
    sfcn <- sf::st_sfc(sf::st_polygon(list(as.matrix(nch)))) ## sf of northern SA
    sfcs <- sf::st_sfc(sf::st_polygon(list(as.matrix(sch)))) ## sf of southern SA
    sfc <- sfcs
    if (north)
        sfc <- sfcn
    xys <- sf::st_coordinates(sf::st_sample(sfc,1))

    lixysim <- list()
    for (i in 1:5) {
        lixysim[[i]] <- relocateInSA(xys, cch, ani[i], sigma[i], dff)
        xys <- lixysim[[i]]
    }
    lixysim <- do.call(rbind, lixysim)
    return(list(ani=ani, sigma=sigma, lixysim=lixysim))
}
