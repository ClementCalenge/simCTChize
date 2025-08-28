HSlocation <- function(area, selectedArea)
{
    if (!((inherits(area, "sf"))|(inherits(area,"sfc"))))
        stop("area should inherit the class \"sf\" or \"sfc\"")
    if (!((inherits(selectedArea, "sf"))|(inherits(area,"sfc"))))
        stop("selectedArea should inherit the class \"sf\" or \"sfc\"")

    nnoy <- sample(3:10, 1)
    centroide <- (sf::st_sample(area, 1))
    libu <- sf::st_buffer(centroide, runif(1,100,300))
    ou <- sf::st_intersection(libu, selectedArea)
    patchs <- sf::st_coordinates(sf::st_sample(ou,nnoy))
    patchs[,1] <- patchs[,1]-sf::st_coordinates(centroide)[1]
    patchs[,2] <- patchs[,2]-sf::st_coordinates(centroide)[2]
    return(list(centroide=sf::st_coordinates(centroide),
                patchs=patchs))
}
