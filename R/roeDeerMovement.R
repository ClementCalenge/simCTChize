roeDeerMovement <-
function(nj, lixy,  verbose=TRUE, complete=FALSE, lipt=NULL)
{
    if (!is.list(lixy))
        stop("lixy should be a list")
    if (!all(sapply(lixy, function(x) ncol(x)==2)))
        stop("lixy should be a list with 2-columns matrices")

    cur <- 0
    ptg <- lipt$ptg
    ptp <- lipt$ptp

    ## Sample an animal among those available
    anis <- sample(1:length(lixy),1)
    xypatchs <- lixy[[anis]]

    ## Speed and starting location at the beginning of the month
    z0=c(0,0)
    v0=c(0,0)
    zi <- z0
    vi <- v0
    z <- zi
    liz <- list(z)
    k <- 1
    lisa <- 1
    sa <- 1

    ## One starts the day on the 28th february at 17h, one relocation per second
    ## every day contains 3600*24 hours, with the following activities:
    act <- rep("hp", 3600*24*nj)
    ina <- rbinom(nj, 1, 0.85)
    if (sum(ina)>0) {
        wh <- c(1:length(ina))[ina>0]
        ## Sampling of the starting hour
        deb <- round(runif(sum(ina), (7+8)*3600, (7+15)*3600))
        ## Number of seconds since the beginning of the monitoring
        debina <- deb+(wh-1)*3600*24
        ## Duration of the inactivity phase (minimum, 0)
        durina <- pmax(round((15+7)*3600-deb+rnorm(length(deb), 0, 1200)),0)
        ## Does the inactivity ends after  17h (in other words, if I subtract
        ## 24h, is it greater than 0?)
        g <- (deb+durina)-3600*24
        ## If yes, ends at 17h exactly
        durina[g>0] <- durina[g>0]-g[g>0]
        ## add the duration
        finina <- debina+durina
        ## And define inactivity
        for (i in 1:length(debina))
            act[debina[i]:finina[i]] <- "inact"
    }

    ## During inactivity phases, patchs with a Poisson process
    ## int lambda[x] = 1 for 17h to 8h ===> lambda int dx = 1. As int dx = 15*3600
    ## lambda=1/(15*3600). So:
    ## Total number:
    npat <- rpois(1,length(act[act=="hp"])/(24*3600))
    if (npat>0) {
        wp <- sample(c(1:length(act))[act=="hp"],npat)
        durpa <- round(runif(npat,0,5*3600))
        for (i in 1:length(durpa))
            act[wp[i]:(wp[i]+durpa[i])] <- "patchact"
        ## Rewrite activity over the patches (in case of overflow)
        if (sum(ina)>0) {
            for (i in 1:length(debina))
                act[debina[i]:finina[i]] <- "inact"
        }
    }

    ## Time at which a change occurs
    wh <- which(c(FALSE, act[-1]!=act[-length(act)]))
    ## Duration of segments
    gg <- diff(c(1,wh))
    ## Add the last segment
    gg[length(gg)+1] <- length(act)-sum(gg)
    ## Which activity for which segment
    ui <- act[c(wh-1,length(act))]

    ## Data.frame with the different segments
    lp <- data.frame(act=ui, tp=0:(length(gg)-1), debut=c(1,wh), fin=c(wh+1, length(act)))
    lp$duree <- (lp$fin-lp$debut+1)/3600

    ## For each patch
    for (i in 1:nrow(lp)) {

        if (verbose)
            cat(round(100*i/nrow(lp)),"%\r")

        ## If inactivity, no change to the relocation
        if (lp$act[i]=="inact") {
            liz[[k]] <- matrix(zi,nrow=lp$fin[i]-lp$debut[i]+1, ncol=2,byrow=TRUE)
            lisa[k] <- sa
            vi <- c(0,0)

        }

        ## If outside a patch
        if (lp$act[i]=="hp") {
            ## Selection of an activity center
            sa <- sample(1:nrow(xypatchs), 1)
            xc <- xypatchs[sa,]
            ## Parameters of the movement
            pt <- ptg
            zrel <- (zi-xc)%*%diag(1/diag(pt$L))
            zb <- generateOUfC(pt, date=1:((lp$fin[i]-lp$debut[i])+2), start=FALSE, z0=zrel, v0=vi)
            zb <- t(t(zb[-1,,drop=FALSE])+xc)
            liz[[k]] <- zb
            z <- zb
            zi <- z[nrow(z),]
            lisa[k] <- sa
        }

        if (lp$act[i]=="patchact") {
            xc <- zi
            pt <- ptp
            zrel <- (zi-xc)%*%diag(1/diag(pt$L))
            zb <- generateOUfC(pt, date=1:((lp$fin[i]-lp$debut[i])+2), start=FALSE, z0=zrel, v0=vi)
            zb <- t(t(zb[-1,,drop=FALSE])+xc)
            liz[[k]] <- zb
            z <- zb
            zi <- z[nrow(z),]
            lisa[k] <- sa
        }

        k <- k+1

    }

    if (complete) {
        z <- do.call(rbind,lapply(1:length(liz), function(i) {
                               g <- as.data.frame(liz[[i]])
                               names(g) <- c("x","y")
                               g$nocluster <- lisa[i]
                               g$activite <- lp$act[i]
                               g$noburst <- lp$tp[i]
                               return(g)
                           }))
    } else {
        z <- do.call(rbind, liz)
    }

    return(z)

}
