whenDetection <-
function(movement, CT)
{
    if (!inherits(movement, "data.frame"))
        stop("movement should be a data.frame")
    if (ncol(movement)!=3)
        stop("movement should have 3 columns")

    if (!inherits(CT, "data.frame"))
        stop("CT should be a data.frame returned by organizeCT")
    if (ncol(CT)!=3)
        stop("CT should be a data.frame returned by organizeCT")
    if (is.null(attr(CT,"val")))
        stop("CT should be a data.frame returned by organizeCT")
    if (is.null(attr(CT,"cooori")))
        stop("attribute cooori should be manually set by the user.\n(see examples of the help page of whenDetection)")


    attriCT <- attr(CT,"val")
    cooori <- attr(CT,"cooori")
    contCT <- cbind(c(0, attriCT$depth*cos(seq(0,attriCT$fieldAngle,
                                                    length=100)+attriCT$angleEast),0)+cooori[1],
                    c(0, attriCT$depth*sin(seq(0,attriCT$fieldAngle,
                                                    length=100)+attriCT$angleEast),0)+cooori[2])
    rax <- range(contCT[,1])
    ray <- range(contCT[,2])
    tr <- movement[movement[,1]>=rax[1]&movement[,1]<=rax[2]&
                   movement[,2]>=ray[1]&movement[,2]<=ray[2],]

    if (nrow(tr)>1) {
        ## Speed per second, calculated from this selection
        vi <- sqrt(rowSums((tr[-1,1:2]-tr[-nrow(tr),1:2])^2))
        tr$v <- c(vi,vi[length(vi)])
    } else {
        if (nrow(tr)>0)
            tr$v <- 0
    }
    ## Remove all speeds equal to 0 (animals cannot be detected when inactive)
    if (nrow(tr)>0) {
        tr <- tr[tr$v>1e-16,,drop=FALSE]
    }

    ## Which are within the limits of the CT?
    io <- splancs::inout(as.matrix(tr[,1:2]), contCT, bound=TRUE)
    tr <- tr[io,]

    ## If there are relocations within these limits
    if (nrow(tr)>0) {

        d <- sapply(1:nrow(tr), function(i) {
            o <- which.min((CT[,1]-tr[i,1])^2 +(CT[,2]-tr[i,2])^2)
            return(o)
        })
        tr$quel <- d

        tr$Pd <- CT$vu[tr$quel]
        fac <- cumsum(c(1,(tr$date[-1]-1.5)>tr$date[-nrow(tr)]))
        tr$passage <- fac

        ## Join with the map
        spl <- split(tr,fac)

        ## Average detectability of movement = duration, close
        ## Proximity measured by map
        ## speed = max speed
        ## survival model: risk = proximity.
        ## Cox model:
        uu <- lapply(spl, function(x) {
            lvu <- runif(nrow(x))<(1-exp(-x$Pd))
            whi <- which(lvu)
            if (length(whi)>0) {
                whi <- x$date[whi]
            }
            return(whi)
        })
        res <- data.frame(noPassage=1:length(uu))
        res$beginning <- sapply(spl, function(x) x$date[1])
        res$end <- sapply(spl, function(x) x$date[nrow(x)])
        res$duration <- sapply(spl,nrow)
        res$detection <- sapply(uu,sum)>0
        res$Pdetect <- sapply(spl, function(x) 1-exp(-sum(x$Pd)))
        res$Distance
        res$When <- uu
    } else {
        res <- NULL
    }

    return(res)
}
