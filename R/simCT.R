print.simCT <- function(x, ...)
{
    if (!inherits(x, "simCT"))
        stop("x should be of class simCT")
    cat("\n******* Object of class simCT:\n\n",
        "Number of simulations of a 5-year study: ", length(x$encounters),"\n",
        "Number of camera traps: ", length(x$encounters[[1]]$indicetraps[,1]),"\n",
        "Mean radius of the detection zone: ", attr(x, "CTparameters")$r, " metres\n",
        "Mean angle of the detection zone: ", attr(x, "CTparameters")$theta, " radians\n",
        "Number of simulated animals for each year:\n", sep="")
    print(sapply(x$speed[[1]],length))
    cat("Size of the object:\n")
    print(object.size(x), units="Mb")
    cat("\nThis object is a list with the following structure:\n", sep="")
    str(x,1)
    cat("See ?simulateCTStudy5years for further details on these components\n\n")
    cat("*******\n\n")
}


c.simCT <- function(...)
{
    uu <- list(...)
    if (length(uu)==1)
        return(unlist(uu, recursive=FALSE))
    if (!all(unlist(lapply(uu, function(x) inherits(x, "simCT")))))
        stop("all objects should be of class \"simCT\"")

    ## Checks that all the objects have the same attributes
    if (nrow(unique(do.call(rbind,lapply(uu, function(x) {
        unlist(attr(x, "CTparameters"))
    }))))!=1)
        stop("The different objects do not have the same CT parameters")
    if (length(unique(sapply(uu, function(x) {
        length(x$encounters[[1]]$indicetraps[,1])
    })))!=1)
        stop("The different objects do not have the same number of traps")
    if (nrow(unique(do.call(rbind,lapply(uu, function(x) {
                                      sapply(x$speed[[1]], length)
                                  }))))!=1)
        stop("The number of simulated animals during each year is not the same\nin the different objets")


    ## binds together the different objects:
    x0 <- uu[[1]]
    for (i in 2:length(uu)) {
        x0$encounters <- c(x0$encounters, uu[[i]]$encounters)
        x0$speed <- c(x0$speed, uu[[i]]$speed)
        x0$activeSpeed <- c(x0$activeSpeed, uu[[i]]$activeSpeed)
    }
    class(x0) <- class(uu[[1]])
    attr(x0, "CTparameters") <- attr(uu[[1]], "CTparameters")
    return(x0)

}
