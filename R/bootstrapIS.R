bootstrapIS <-
function(x, Nboot, r, theta, detct, hoursPerDay=24, surfaceChize = 25979850, nCT=100,
         stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{
    ## for each CT, what are the rows of x
    ## corresponding to encounters with the CT?
    if (is.null(x)||nrow(x)==0)
        stop("No data in x")
    du <- lapply(1:nCT, function(i) c(1:nrow(x))[x$whichCT==i])

    pu <- sapply(1:Nboot, function(k) {

        ## bootstrap resampling of traps
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


        ## Encounters corresponding to resampled traps (if a trap is
        ## present multiple times, the rows of x will be repeated)
        xb <- x[unlist(du[sam]),]

        ## Estimation with the bootstrap sample
        if (nrow(xb)>0) {
            es <- estimIS(xb, r=r, theta=theta, detct=detct,
                          hoursPerDay=hoursPerDay, nCT=nCT,
                          stratified=stratified,
                          surfaceChize=surfaceChize,
                          whichCTpaths=whichCTpaths, surfPaths=surfPaths)
        } else {
            return(0)
        }
    })
    return(pu)
}
