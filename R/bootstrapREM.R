bootstrapREM <-
function(x, Nboot, dayrange, r, theta,
                         hoursPerDay=24, surfaceChize = 25979850,  nCT=100,
                         detection=TRUE, stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{
    if (is.null(x)||nrow(x)==0)
        stop("No data in x")

    ## Indices of rows corresponding to each trap
    du <- lapply(1:nCT, function(i) c(1:nrow(x))[x$whichCT==i])

    pu <- sapply(1:Nboot, function(k) {

        ## bootstrap resampling
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
        xb <- x[unlist(du[sam]),]

        ## Estimation
        if (detection) {
            if (nrow(xb)==0) {
                pdee <- 1
            } else {
                pdee <- mean(x$detection)
                if (pdee<1e-16)
                    pdee <- NA
            }
            es <- estimRem(x=xb, dayrange=dayrange, r=r, theta=theta,
                           hoursPerDay=hoursPerDay, surfaceChize=surfaceChize,
                           Pdetect=pdee, nCT=nCT, stratified=stratified, whichCTpaths=whichCTpaths,
                           surfPaths=surfPaths)
        } else {
            es <- estimRem(x=xb, dayrange=dayrange, r=r, theta=theta,
                           hoursPerDay=hoursPerDay, Pdetect=1,
                           nCT=nCT, surfaceChize=surfaceChize,
                           stratified=stratified,
                           whichCTpaths=whichCTpaths,
                           surfPaths=surfPaths)
        }
        return(es)
    })
    return(pu)
}
