bootstrapIScompar <-
    function(x, x2, Nboot, r, theta, detct,
             hoursPerDay=24, surfaceChize = 25979850, nCT=100,
             stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{

    ## Same principle as for the simple bootstrap: first the indices
    ## of rows of x, x2 correspond to encounters with CT
    if (!(is.null(x)||nrow(x)==0))
        du <- lapply(1:nCT, function(i) c(1:nrow(x))[x$whichCT==i])

    if (!(is.null(x2)||nrow(x2)==0))
        du2 <- lapply(1:nCT, function(i) c(1:nrow(x2))[x2$whichCT==i])

    ## resampling
    pu <- sapply(1:Nboot, function(k) {

        ## global boostrap resampling on traps
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

        ## Rebuild x,x2 and estimate population size
        ## Change rate

        if (!(is.null(x)||nrow(x)==0)) {
            xb <- x[unlist(du[sam]),]
            es <- estimIS(xb, r=r, theta=theta, detct=detct,
                          hoursPerDay=hoursPerDay, nCT=nCT, surfaceChize=surfaceChize,
                           stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
        } else {
            es <- 0
        }

        if (!(is.null(x2)||nrow(x2)==0)) {
            xb2 <- x2[unlist(du2[sam]),]
            es2 <- estimIS(xb2, r=r, theta=theta, detct=detct,
                           hoursPerDay=hoursPerDay, nCT=nCT, surfaceChize=surfaceChize,
                           stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
        } else {
            es2 <- 0
        }

        return((es2-es)/es)
    })
    return(pu)
}
