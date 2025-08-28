obstaclesQ <-
function(depth, stemsha=5000, diamMin=0.2, diamMax=0.3, res=0.1)
{
    eg <- expand.grid(seq(-depth,depth, by=res),seq(-depth,depth, by=res))
    ap <- atan2(eg[,2],eg[,1])
    d <- sqrt(rowSums(eg^2))
    eg <- as.data.frame(eg)
    names(eg) <- c("x","y")
    ##
    stems <- rpois(1,stemsha*(depth^2)/10000) ## thinning process


    if (stems>0) {
        ## random coordinates
        coo <- matrix(runif(2*stems, -depth, depth),nrow=stems)
        ## discretization
        coo <- round(coo/res)*res
        ## DiamMax
        diam <- runif(stems, diamMin, diamMax)
        remt <- 1-(((!calcCover(coo, diam, ap, d, stems, eg))*0.5)+
            ((!calcCover(coo, diam, ap, d, stems, eg))*0.5))

        } else {
        remt <- rep(TRUE, nrow(eg))
    }
    df <- data.frame(x=eg[,1],y=eg[,2],vu=as.vector(remt))
    return(df)
}
