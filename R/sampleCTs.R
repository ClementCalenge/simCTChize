sampleCTs <- function(listSim, nCT)
{
    if (!inherits(listSim, "simCT"))
        stop("listSim should be of class simCT")

    en <- listSim$encounters

    for (i in 1:length(en)) {
        en[[i]]$indicetraps <- en[[i]]$indicetraps[1:nCT,]
        for (j in 1:length(en[[i]]$results)) {
            quel <- sapply(en[[i]]$results[[j]], function(x) x$whichCT[1])<=nCT
            en[[i]]$results[[j]] <- en[[i]]$results[[j]][quel]
        }
    }

    listSim$encounters <- en

    return(listSim)
}
