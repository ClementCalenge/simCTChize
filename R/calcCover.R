calcCover <-
function(coo, diam, ap, d, stems, eg)
{
    ## Pixels behind the trees
    lap <- lapply(1:nrow(coo), function(i) {
        covertree(coo[i,], diam[i], ap, d)
    })
    re <- Reduce("+",lap)==stems
    ## Pixels in the trees
    u <- Reduce("+",lapply(1:nrow(coo), function(i) {
                        sqrt((eg[,1]-coo[i,1])^2+
                             (eg[,2]-coo[i,2])^2)>=diam[i]/2
                    }))==stems

    rem <- matrix(re,sqrt(length(eg[,1])))
    rem2 <- matrix(u,sqrt(length(eg[,1])))
    remt <- rem&rem2
    return(remt)
}
