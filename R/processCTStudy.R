
processCTStudy <- function(listSim, surfaceChize=25979850,
                           method=c("REM","IS"),
                           ideal=FALSE, detectability=TRUE,
                           active=TRUE,
                           stratified=FALSE, pathsBuffer=NULL,
                           Nboot=1000, verbose=1,
                           ibegin=1, iend=length(listSim$encounters))
{
    ## Some checks for listSim
    if (!inherits(listSim, "simCT"))
        stop("listSim should be of class simCT")
    if (!all(names(listSim)==c("encounters","speed","activeSpeed")))
        stop("listSim should have been created by simulateCTStudy5years")
    if (!all(apply(do.call(rbind,lapply(listSim$encounters, names)),2,unique)==c("creationCT","indicetraps","results")))
        stop("listSim should have been created by simulateCTStudy5years")
    if (!all(apply(do.call(rbind,lapply(listSim$encounters, function(x) names(x$creationCT))),2,unique)==c("position", "content")))
        stop("listSim should have been created by simulateCTStudy5years")
    if (!all(apply(do.call(rbind,lapply(listSim$encounters, function(x) names(x$indicetraps))),2,unique)==c("x","y","hab")))
        stop("listSim should have been created by simulateCTStudy5years")
    ## That will do.

    ## check ibegin and iend
    if (max(c(ibegin,iend))>length(listSim$encounters))
        stop("incorrect ibegin,iend (too large)")
    if (min(c(ibegin,iend))<1)
        stop("incorrect ibegin,iend (too small)")
    if (ibegin>iend)
        stop("iend should be larger than ibegin")


    if (stratified) {
        if (is.null(pathsBuffer))
            stop("pathsBuffer cannot be NULL when stratified is TRUE")
        if (!((inherits(pathsBuffer, "sf"))|(inherits(pathsBuffer,"sfc"))))
            stop("pathsBuffer should inherit the class \"sf\" or \"sfc\"")
    }

    method <- match.arg(method)

    ## Get r and theta
    r <- attr(listSim,"CTparameters")$r
    theta <- attr(listSim,"CTparameters")$theta

    ## get nCT
    nCT <- length(listSim$encounters[[1]]$indicetraps[,1])

    ## initialization of listRes
    listRes <- list()

    ## For each simulation
    for (i in ibegin:iend) {

        ## For information
        if (verbose>0)
            cat("## Simulation", i,"\n")

        ## ####################################
        ## Elements required for all methods

        ## All encounters
        listencounters <- lapply(listSim$encounters[[i]]$results,
                                 function(x) do.call(rbind,x))
        vectorSpeed <- sapply(listSim$speed[[i]], mean)

        ## During active periods
        hoursPerDay <- 24
        if (active) {
            listencounters <- lapply(listencounters, function(x)
                removeHour(x, 8, 17))

            vectorSpeed <- sapply(listSim$activeSpeed[[i]], mean)
            hoursPerDay <- 15
        }
        ## Vector of speed during all and only active periods


        ## Detection probability for each trap (for IS)
        ## Save random number generator state
        if (detectability) {
            if (method=="IS") {
                if (exists(".Random.seed", .GlobalEnv))
                    oldseed <- .GlobalEnv$.Random.seed
                else
                    oldseed <- NULL

                ## Recalculate the detection probability of associations
                habitat4sim <- listSim$encounters[[i]]$indicetraps$hab
                set.seed(listSim$encounters[[i]]$creationCT[2])
                list100CT <- lapply(1:nCT, function(j) {
                    if (verbose>1)
                        cat("Traps: ", j,"\r")
                    organiseCT(habitat4sim[j], fieldangle=theta, depth=r)
                })
                ## Restore the RNG state
                if (!is.null(oldseed))
                    .GlobalEnv$.Random.seed <- oldseed
                else
                    rm(".Random.seed", envir = .GlobalEnv)
                ## detection probability
                prop <- sapply(1:length(list100CT), function(trap)
                    mean(1-exp(-list100CT[[trap]]$vu)))
            } else {
                xox <- do.call(rbind, listencounters)
                if (!(is.null(xox)||nrow(xox)==0)) {
                    Pdetect <- mean(xox$detection)
                } else {
                    Pdetect <- 1
                }
            }
        } else {
            Pdetect <- 1
            prop <- rep(1, nCT)
        }


        if (ideal) {
            listencounters <- lapply(listencounters, function(x) {
                x$detection <- TRUE
                return(x)
            })
        }

        if (stratified) {
            ## which are on paths
            whichCTpaths <- !is.na(sf::st_join(sf::st_as_sf(listSim$encounters[[i]]$indicetraps[,c("x","y")],
                                                            coords=c("x","y"),crs=sf::st_crs(pathsBuffer)),
                                               pathsBuffer)$id)
            surfPaths <- as.numeric(sf::st_area(pathsBuffer))
        } else {
            whichCTpaths <- NULL
            surfPaths=0
        }

        if (method=="REM") {

            remest <- estimTrendRem(listencounters, vectorSpeed=vectorSpeed,
                                    r=r, theta=theta, surfaceChize=surfaceChize,
                                    hoursPerDay=hoursPerDay, Pdetect=Pdetect,nCT=nCT,
                                    stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
            bo <- bootstrapTrendRem(listencounters, vectorSpeed=vectorSpeed,
                                    r=r, theta=theta, surfaceChize=surfaceChize,
                                    hoursPerDay=hoursPerDay, Pdetect=Pdetect,Nboot=Nboot,
                                    nCT=nCT,
                                    stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths,
                                    verbose=verbose>1)
            remCI <- as.data.frame(t(apply(bo,2,quantile,c(0.05,0.95), na.rm=TRUE)))
            remSE <- apply(bo,2,sd)
            ## Change rate and CI
            remCR <- (remest[5]-remest[1])/remest[1]
            bob <- bootstrapREMcompar(listencounters[[1]],
                                      listencounters[[5]], Nboot=Nboot,
                                      dayrange=vectorSpeed[1], detection=detectability,
                                      dayrange2=vectorSpeed[5], r=r, theta=theta,
                                      hoursPerDay = hoursPerDay, surfaceChize=surfaceChize,
                                      nCT=nCT,
                                      stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
            remCRCI <- quantile(bob, c(0.05,0.95),na.rm=TRUE)
            remCRSE <- sd(na.omit(bob))
        } else {
            remest <- estimTrendIS(listencounters, r=r, theta=theta, detct=prop,
                                   surfaceChize=surfaceChize,
                                   hoursPerDay=hoursPerDay,
                                   nCT=nCT,
                                   stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
            bo <- bootstrapTrendIS(listencounters, detct=prop,
                                   r=r, theta=theta, surfaceChize=surfaceChize,
                                   hoursPerDay=hoursPerDay, Nboot=Nboot,
                                   nCT=nCT,
                                   stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths,
                                   verbose=(verbose>1))
            remCI <- as.data.frame(t(apply(bo,2,quantile,c(0.05,0.95),na.rm=TRUE)))
            remSE <- apply(bo,2,sd)
            ## Change rate and CI
            remCR <- (remest[5]-remest[1])/remest[1]
            bob <- bootstrapIScompar(listencounters[[1]],
                                     listencounters[[5]], Nboot=Nboot,
                                     r=r, theta=theta, detct=prop,
                                     hoursPerDay = hoursPerDay, surfaceChize=surfaceChize,
                                     nCT=nCT,
                                     stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
            remCRCI <- quantile(bob,
                                c(0.05,0.95),na.rm=TRUE)
            remCRSE <- sd(na.omit(bob))
        }


        dfres <- data.frame(PtEst=c(remest, remCR), EstSE=c(remSE,remCRSE),
                            EstCIl=c(remCI[,1], remCRCI[1]),
                            EstCIu=c(remCI[,2], remCRCI[2]))

        rownames(dfres) <- c(paste0("N",1:5), "lambda", "changeRate")

        listRes[[i-ibegin+1]] <- dfres

    }

    return(listRes)
}
