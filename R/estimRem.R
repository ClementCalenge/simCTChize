estimRem <-
    function(x, dayrange, r, theta, hoursPerDay=24, Pdetect=1, nCT=100, duration=30, surfaceChize=25979850,
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
    if (stratified) {
        if (is.null(whichCTpaths))
            stop("whichCTpaths cannot be NULL when stratified is TRUE")
        if (length(whichCTpaths)!=nCT)
            stop("whichCTpaths should be a logical vector of length nCT")

        ## if every trap in the same habitat => NA (cannot estimate
        ## other habitat)
        if ((sum(whichCTpaths)==0)|(sum(whichCTpaths)==nCT))
            return(NA)

        ## When Pdetect < 1, we consider that detectability is to be
        ## calculated. Two estimates for the two strata
        if (Pdetect < 0.999) {
            Pdet1 <- mean(x$detection[x$whichCT%in%c(1:nCT)[whichCTpaths]])
            Pdet2 <- mean(x$detection[!x$whichCT%in%c(1:nCT)[whichCTpaths]])
        } else {
            Pdet1 <- 1
            Pdet2 <- 1
        }
        Q1 <- sum(x$detection[x$whichCT%in%c(1:nCT)[whichCTpaths]])/Pdet1
        A1 <- (duration*sum(whichCTpaths)*hoursPerDay*3600)
        den1 <- (Q1*(3.14159265359)/(A1*dayrange*r*(2+theta)))

        Q2 <- sum(x$detection[!x$whichCT%in%c(1:nCT)[whichCTpaths]])/Pdet2
        A2 <- (duration*sum(!whichCTpaths)*hoursPerDay*3600)
        den2 <- (Q2*(3.14159265359)/(A2*dayrange*r*(2+theta)))

        den <- surfPaths*den1+(surfaceChize-surfPaths)*den2
        return(den)
    }

    Q <- sum(x$detection)/Pdetect
    A <- (duration*nCT*hoursPerDay*3600)
    return(surfaceChize*Q*(3.14159265359)/(A*dayrange*r*(2+theta)))
}
