process_all <- function(listSim, stratified=FALSE, pathsBuffer=NULL)
{
    if (!inherits(listSim, "simCT"))
        stop("listSim should be of class simCT")
    if (!all(names(listSim) == c("encounters", "speed", "activeSpeed")))
        stop("listSim should have been created by simulateCTStudy5years")
    if (!all(apply(do.call(rbind, lapply(listSim$encounters,
        names)), 2, unique) == c("creationCT", "indicetraps",
        "results")))
        stop("listSim should have been created by simulateCTStudy5years")
    if (!all(apply(do.call(rbind, lapply(listSim$encounters,
        function(x) names(x$creationCT))), 2, unique) == c("position",
        "content")))
        stop("listSim should have been created by simulateCTStudy5years")
    if (!all(apply(do.call(rbind, lapply(listSim$encounters,
        function(x) names(x$indicetraps))), 2, unique) == c("x",
        "y", "hab")))
        stop("listSim should have been created by simulateCTStudy5years")
    if (stratified) {
        if (is.null(pathsBuffer))
            stop("pathsBuffer cannot be NULL when stratified is TRUE")
        if (!((inherits(pathsBuffer, "sf")) | (inherits(pathsBuffer,
                                                        "sfc"))))
            stop("pathsBuffer should inherit the class \"sf\" or \"sfc\"")
    }


    ## REM
    cat("### REM_raw\n\n")
    REM_raw <- processCTStudy(listSim, method="REM",
                              detectability=FALSE, active=FALSE,
                              stratified=stratified,
                              pathsBuffer=pathsBuffer)
    cat("### REM_detect\n\n")
    REM_detect <- processCTStudy(listSim, method="REM",
                                 detectability=TRUE, active=FALSE,
                                 stratified=stratified,
                                 pathsBuffer=pathsBuffer)
    cat("### REM_detect_active\n\n")
    REM_detect_active <- processCTStudy(listSim, method="REM",
                                        detectability=TRUE, active=TRUE,
                                        stratified=stratified,
                                        pathsBuffer=pathsBuffer)


    ## IS
    cat("### IS_raw\n\n")
    IS_raw <- processCTStudy(listSim, method="IS",
                             detectability=FALSE, active=FALSE,
                             stratified=stratified,
                             pathsBuffer=pathsBuffer)
    cat("### IS_detect\n\n")
    IS_detect <- processCTStudy(listSim, method="IS",
                                detectability=TRUE, active=FALSE,
                                stratified=stratified,
                                pathsBuffer=pathsBuffer)
    cat("### IS_detect_active\n\n")
    IS_detect_active <- processCTStudy(listSim, method="IS",
                                       detectability=TRUE, active=TRUE,
                                       stratified=stratified,
                                       pathsBuffer=pathsBuffer)

    roeNoHS <- list(REM_raw=REM_raw, REM_detect=REM_detect,
                    REM_detect_active=REM_detect_active,
                    IS_raw=IS_raw, IS_detect=IS_detect,
                    IS_detect_active=IS_detect_active)

    return(roeNoHS)
}
