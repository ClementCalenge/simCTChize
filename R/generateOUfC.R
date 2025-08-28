generateOUfC <-
function(pretraj,date=1:1000, start=TRUE, z0=c(0,0), v0=c(0,0))
{
    if (!inherits(pretraj,"pretraj"))
        stop("should be pretraj")
    n <- length(date)
    K <- AXES <- 2

    z <- array(0, c(n-1, AXES))
    v <- array(0, c(n-1, AXES))
    S <- pretraj$S
    G <- pretraj$G
    L <- pretraj$L
    id <- ifelse(start, 1,2)
    H <- rbind(z0,v0)

    re <- .Call("simtraj", as.double(G[1,,]), as.double(S[1,,]),
                as.double(G[2,,]), as.double(S[2,,]), as.double(H),
                as.double(L), as.integer(id-1),
                as.integer(n), PACKAGE="simCTChize")
    z <- matrix(re[[1]],ncol=2, byrow=TRUE)
    colnames(z) <- c("x","y")
    class(z) <- "boutraj"
    attr(z,"v") <- matrix(re[[2]], ncol=2, byrow=TRUE)
    return(z)
}
