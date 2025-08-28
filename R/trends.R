
estimTrendIS <- function(listencounters, r, theta, detct, surfaceChize = 25979850,
                         hoursPerDay = 24, nCT = 100, duration = 30,
                         stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{
    if (!is.list(listencounters))
        stop("listencounters should be a list")
    ## other checks in estimIS
    Ni <- sapply(1:length(listencounters), function(i) {
        estimIS(listencounters[[i]], r=r, theta=theta, hoursPerDay=hoursPerDay,
                nCT=nCT, detct=detct, surfaceChize=surfaceChize,
                stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
    })

    varx <- var(c(1:length(listencounters)))
    slopei <- exp(cov(0:(length(listencounters)-1),log(Ni))/varx)-1
    res <- c(Ni, slopei)
    names(res) <- c(paste0("N[",1:length(listencounters),"]"),"lambda")
    return(res)
}


estimTrendRem <- function(listencounters, vectorSpeed, r, theta, surfaceChize = 25979850,
                          hoursPerDay = 24, Pdetect = 1,
                          nCT = 100, duration = 30,
                          stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{
    if (!is.list(listencounters))
        stop("listencounters should be a list")
    if (length(vectorSpeed)!=length(listencounters))
        stop("vectorSpeed should be a vector of the same length as listencounters")

    ## other checks in estimRem
    Nr <- sapply(1:length(listencounters), function(i) {
        estimRem(listencounters[[i]], dayrange=vectorSpeed[i], r=r, theta=theta, hoursPerDay=hoursPerDay,
                 Pdetect=Pdetect, nCT=nCT, duration=duration,
                 surfaceChize=surfaceChize, stratified=stratified,
                 whichCTpaths=whichCTpaths, surfPaths=surfPaths)
    })

    varx <- var(c(1:length(listencounters)))
    sloper <- exp(cov(0:(length(listencounters)-1),log(Nr))/varx)-1
    res <- c(Nr, sloper)
    names(res) <- c(paste0("N[",1:length(listencounters),"]"),"lambda")
    return(res)
}



bootstrapTrendRem <- function(listencounters, vectorSpeed, r, theta, surfaceChize = 25979850,
                             hoursPerDay = 24, Pdetect = 1,
                             nCT = 100, duration = 30,
                             stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187,
                             Nboot=1000,
                             verbose=TRUE)
{

    ## Bootstrap proprement dit
    pu <- t(sapply(1:Nboot, function(k) {
        if (verbose)
            cat("Bootstrap:", k,"\r")

        if (!stratified) {
            sam <- sample(1:nCT, nCT, replace=TRUE)
        } else {
            nCT1 <- sum(whichCTpaths)
            nCT2 <- sum(!whichCTpaths)
            if ((nCT1==nCT)|(nCT1==0)) {
                sam <- sample(1:nCT, nCT, replace=TRUE)
            } else {
                sam <- c(sample(c(1:nCT)[whichCTpaths], nCT1, replace=TRUE),
                         sample(c(1:nCT)[!whichCTpaths], nCT2, replace=TRUE))
            }
        }

        ## Sample
        lienb <- lapply(1:length(listencounters), function(it) {
            x <- listencounters[[it]]
            if (is.null(x)||nrow(x)==0)
                return(x)
            du <- lapply(1:nCT, function(i) c(1:nrow(x))[x$whichCT==i])
            return(x[unlist(du[sam]),])
        })

        ## result
        res <- estimTrendRem(listencounters=lienb, vectorSpeed=vectorSpeed,
                             r=r, theta=theta, surfaceChize=surfaceChize,
                             hoursPerDay = hoursPerDay, Pdetect=Pdetect,
                             nCT = nCT, duration = duration,
                             stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)

        return(res)
    }))
    return(pu)
}



bootstrapTrendIS <- function(listencounters,r, theta, detct, surfaceChize = 25979850,
                             hoursPerDay = 24, nCT = 100, duration = 30,
                             stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187,
                             Nboot=1000,
                             verbose=TRUE)
{

    ## Bootstrap proprement dit
    pu <- t(sapply(1:Nboot, function(k) {
        if (verbose)
            cat("Bootstrap:", k,"\r")

        if (!stratified) {
            sam <- sample(1:nCT, nCT, replace=TRUE)
        } else {
            nCT1 <- sum(whichCTpaths)
            nCT2 <- sum(!whichCTpaths)
            if ((nCT1==nCT)|(nCT1==0)) {
                sam <- sample(1:nCT, nCT, replace=TRUE)
            } else {
                sam <- c(sample(c(1:nCT)[whichCTpaths], nCT1, replace=TRUE),
                         sample(c(1:nCT)[!whichCTpaths], nCT2, replace=TRUE))
            }
        }

        ## Sample
        lienb <- lapply(1:length(listencounters), function(it) {
            x <- listencounters[[it]]
            if (is.null(x)||nrow(x)==0)
                return(x)
            du <- lapply(1:nCT, function(i) c(1:nrow(x))[x$whichCT==i])
            x[unlist(du[sam]),]
        })

        ## result
        res <- estimTrendIS(listencounters=lienb, r=r, theta=theta,
                            detct=detct,surfaceChize=surfaceChize,
                            hoursPerDay = hoursPerDay,
                            nCT = nCT, duration = duration,
                            stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)

        return(res)
    }))
    return(pu)
}
