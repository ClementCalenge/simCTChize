bootstrapREMcompar <-
function(x, x2, Nboot, dayrange, dayrange2, r,
                               theta, hoursPerDay=24, surfaceChize = 25979850,
                               nCT=100, detection=TRUE, stratified=FALSE, whichCTpaths=NULL, surfPaths=9857187)
{

    ## indice of rows of x corresponding to each PP
    if (!(is.null(x)||nrow(x)==0))
        du <- lapply(1:nCT, function(i) c(1:nrow(x))[x$whichCT==i])

    if (!(is.null(x2)||nrow(x2)==0))
        du2 <- lapply(1:nCT, function(i) c(1:nrow(x2))[x2$whichCT==i])


    pu <- sapply(1:Nboot, function(k) {

        ## Boostrap resampling (only one for the two years)
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


        if (!(is.null(x)||nrow(x)==0)) {
            xb <- x[unlist(du[sam]),]
        } else {
            xb <- x
        }

        if (!(is.null(x2)||nrow(x2)==0)) {
            xb2 <- x2[unlist(du2[sam]),]
        } else {
            xb2 <- x2
        }

        ## estimations forl the two years and rate of change.
        if (detection) {
            if ((is.null(x)||nrow(x)==0)) {
                es <- 0
            } else {
                pdee <- mean(x$detection)
                if (pdee<1e-16)
                    pdee <- NA

                es <- estimRem(x=xb, dayrange=dayrange, r=r, theta=theta,
                               hoursPerDay=hoursPerDay,
                               Pdetect=pdee, nCT=nCT, surfaceChize=surfaceChize,
                               stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
            }
            if ((is.null(x2)||nrow(x2)==0)) {
                es2 <- 0
            } else {
                pdee <- mean(x2$detection)
                if (pdee<1e-16)
                    pdee <- NA

                es2 <- estimRem(x=xb2, dayrange=dayrange2, r=r, theta=theta,
                                hoursPerDay=hoursPerDay,
                                Pdetect=pdee, nCT=nCT, surfaceChize=surfaceChize,
                                stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
            }
        } else {
            es <- estimRem(x=xb, dayrange=dayrange, r=r, theta=theta,
                           hoursPerDay=hoursPerDay,
                           Pdetect=1, nCT=nCT, surfaceChize=surfaceChize,
                           stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
            es2 <- estimRem(x=xb2, dayrange=dayrange2, r=r, theta=theta,
                            hoursPerDay=hoursPerDay,
                            Pdetect=1, nCT=nCT, surfaceChize=surfaceChize,
                            stratified=stratified, whichCTpaths=whichCTpaths, surfPaths=surfPaths)
        }

        return((es2-es)/es)
    })
    return(pu)
}
