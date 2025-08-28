covertree <-
function(xy, r, ap, d)
{
    dm <- sqrt(sum(xy^2)+(r/2)^2)       ## distance max
    ang0 <- atan2(xy[2], xy[1])         ## angle of the center of the tree
    angb <- atan2(r/2, sqrt(sum(xy^2))) ## angle between (x=distance tree, y=radius tree) and abscissa axis
    ang1 <- ang0-angb                   ## angle min from CT
    ang2 <- ang0+angb                   ## angle max from CT
    io <- d>dm&ap>ang1&ap<ang2          ## distance>distance to tree, and pixels in the direction of the tree
    return(!io)
}
