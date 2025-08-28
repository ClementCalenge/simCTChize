estimatePopSize <-
    function(x, dayrangeAll, dayrangeActive, detct, r=20, theta, nCT,
             surfaceChize = 25979850, xActive=NULL, duration=30,
             stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{
    if (!inherits(x,"data.frame"))
        stop("x should be a data.frame")
    if (!all(c("noPassage", "beginning", "end", "duration", "detection",
               "Pdetect",
               "When", "whichani", "whichCT")%in%names(x)))
        stop("non convenient data for x")
    if (!is.list(x$When))
        stop("non convenient data for x")
    if (length(detct)!=nCT)
        stop("detct does not contain nCT values")

    ## combine encounters
    lireo <- x##do.call(rbind, rest)

    ## Remove inactive hours (between 8h and 17h)
    if (is.null(xActive)) {
        vh <- removeHour(lireo,8,17)
    } else {
        vh <- xActive
    }

    ## Build a matrix of encounters without detection issues
    lireot <- lireo
    lireot$detection <- TRUE

    ## Estimations
    vecEstimation <- numeric(0)

    ## Must correspond to the name of methods below
    vecEstimation[1] <- estimRem(x=lireot, dayrange=dayrangeAll, r=r, theta=theta,
                                 nCT=nCT, surfaceChize=surfaceChize, stratified=stratified,
                                 whichCTpaths=whichCTpaths, surfPaths=surfPaths)
    vecEstimation[2] <- estimRem(x=lireo, dayrange=dayrangeAll, r=r, theta=theta,
                                 nCT=nCT, surfaceChize=surfaceChize, stratified=stratified,
                                 whichCTpaths=whichCTpaths, surfPaths=surfPaths)
    vecEstimation[3] <- estimRem(x=vh, dayrange=dayrangeActive, r=r, theta=theta,
                                 hoursPerDay=15, Pdetect=mean(vh$detection),
                                 nCT=nCT, surfaceChize=surfaceChize, stratified=stratified,
                                 whichCTpaths=whichCTpaths, surfPaths=surfPaths)
    vecEstimation[4] <- estimIS(x=lireo, r=r, theta=theta, detct=rep(1,nCT),
                                nCT=nCT, surfaceChize=surfaceChize, stratified=stratified,
                                whichCTpaths=whichCTpaths, surfPaths=surfPaths)
    vecEstimation[5] <- estimIS(x=lireo, r=r, theta=theta,
                                detct=detct, nCT=nCT, surfaceChize=surfaceChize, stratified=stratified,
                                 whichCTpaths=whichCTpaths, surfPaths=surfPaths)
    vecEstimation[6] <- estimIS(x=vh, r=r, theta=theta,
                                detct=detct, hoursPerDay=15, nCT=nCT, surfaceChize=surfaceChize,
                                stratified=stratified,
                                whichCTpaths=whichCTpaths, surfPaths=surfPaths)

    names(vecEstimation) <- c("REM_ideal","REM_raw","REM_detect_active",
                              "IS_raw","IS_detect", "IS_detect_active")

    return(vecEstimation)

}
