Remhab <- function(re, paths=TRUE, detection=TRUE, active=TRUE, verbose=TRUE, fieldangle=0.175, depth=20)
{

    nct <- length(re$encounters[[1]]$indicetraps[, 1])
    result <- do.call(rbind,lapply(1:length(re$encounter), function(j) {
                                if (verbose)
                                    cat(j,"\r")
        listencountersb <- lapply(re$encounters[[j]]$results,
                                  function(x) do.call(rbind, x))
        if (active) {
            listencountersb <- lapply(listencountersb, function(x) removeHour(x,
                                                                              8, 17))
            vectorSpeed <- sapply(re$activeSpeed[[j]], mean)
            hoursPerDay <- 15
        } else {
            vectorSpeed <- sapply(re$speed[[j]],mean)
            hoursPerDay <- 24
        }
        if (detection) {
            Pdet <- mean(unlist(sapply(listencountersb, function(x) x$detection)))
        } else {
            Pdet <- 1
        }
        Nr <- sapply(1:length(listencountersb), function(k) {
            estimRem(listencountersb[[k]], dayrange = vectorSpeed[k],
                     r = depth, theta = fieldangle, hoursPerDay = hoursPerDay,
                     Pdetect = Pdet, nCT = nct,
                     surfaceChize = ifelse(paths,9857187,25979850-9857187))
        })
        return(Nr)
    }))
    return(result)
}






IShab <- function(re, paths=TRUE, detection=TRUE, active=TRUE, verbose=TRUE,
                prop=NULL, fieldangle=0.175, depth=20)
{

    nct <- length(re$encounters[[1]]$indicetraps[, 1])
    result <- do.call(rbind,lapply(1:length(re$encounter), function(j) {
                                if (verbose)
                                    cat(j,"\r")
        listencountersb <- lapply(re$encounters[[j]]$results,
                                  function(x) do.call(rbind, x))
        if (active) {
            listencountersb <- lapply(listencountersb, function(x) removeHour(x,
                                                                              8, 17))
            vectorSpeed <- sapply(re$activeSpeed[[j]], mean)
            hoursPerDay <- 15
        } else {
            vectorSpeed <- sapply(re$speed[[j]],mean)
            hoursPerDay <- 24
        }
        if (detection) {
            propo <- prop[[j]][1:nct]
        } else {
            Pdet <- 1
            propo <- rep(1, nct)
        }
        Nr <- sapply(1:length(listencountersb), function(k) {
            estimIS(listencountersb[[k]], r = depth, theta = fieldangle, hoursPerDay = hoursPerDay,
                    nCT = nct,
                    detct = propo, surfaceChize = ifelse(paths,9857187,25979850-9857187))
        })
        return(Nr)
    }))
    return(result)
}
