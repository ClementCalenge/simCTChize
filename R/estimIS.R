estimIS <-
function(x, r, theta, detct, hoursPerDay=24, nCT=100, duration=30, surfaceChize=25979850,
         stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{
    if (is.null(x))
        return(0)
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

    if (stratified) {
        if (is.null(whichCTpaths))
            stop("whichCTpaths cannot be NULL when stratified is TRUE")
        if (length(whichCTpaths)!=nCT)
            stop("whichCTpaths should be a logical vector of length nCT")

        asso <- sapply(x$When,length)
        if (length(asso)==0)
            return(0)

        ## if every trap in the same habitat => NA (cannot estimate
        ## other habitat)
        if ((sum(whichCTpaths)==0)|(sum(whichCTpaths)==nCT))
            return(NA)

        ## Two estimates for the two strata
        stato1 <- sum(sapply(c(1:nCT)[whichCTpaths], function(j) {
            nt <- sum(asso[x$whichCT==j])
            if (detct[j]==0)
                return(0)
            return(nt/(detct[j]*((theta/2)*r^2)))
        }))
        JM1 <- duration*hoursPerDay*3600*sum(whichCTpaths)
        N1 <- (stato1/JM1)*surfPaths

        stato2 <- sum(sapply(c(1:nCT)[!whichCTpaths], function(j) {
            nt <- sum(asso[x$whichCT==j])
            if (detct[j]==0)
                return(0)
            return(nt/(detct[j]*((theta/2)*r^2)))
        }))
        JM2 <- duration*hoursPerDay*3600*sum(!whichCTpaths)
        N2 <- (stato2/JM2)*(surfaceChize-surfPaths)

        return(N1+N2)
    }

    asso <- sapply(x$When,length)
    if (length(asso)==0)
        return(0)
    stato <- sum(sapply(1:nCT, function(j) {
        nt <- sum(asso[x$whichCT==j])
        if (detct[j]==0)
            return(0)
        return(nt/(detct[j]*((theta/2)*r^2)))
    }))
    JM <- duration*hoursPerDay*3600*nCT
    return(surfaceChize*(stato/JM))
}
