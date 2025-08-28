generateCTbase <-
function(eg, orientationE, angle, depth, D1=5, D2=15)
{
    if (!inherits(eg, "data.frame"))
        stop("eg should be a data.frame")
    if (ncol(eg)!=3)
        stop("eg should have 3 columns")

    egm <- as.matrix(eg[,1:2])
    x <- c(0, depth*cos(seq(0,angle, length=100)+orientationE),0)
    y <- c(0, depth*sin(seq(0,angle, length=100)+orientationE),0)
    io <- splancs::inout(egm, cbind(x,y),bound=TRUE)
    eg <- eg[io,]
    d <- sqrt(rowSums(eg[,1:2]^2))
    ab <- trouveabo(x1=D1, x2=D2,angle)
    pde <- exp(ab[1]+ab[2]*d)
    df <- data.frame(x=eg[,1],y=eg[,2],vu=pde*eg[,3])
#    class(df) <- "PP"
    return(df)
}
