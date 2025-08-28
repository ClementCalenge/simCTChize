present_results_CT <- function(roeNoHS, dfPopSize)
{
    if (!is.list(roeNoHS))
        stop("roeNoHS should be a list")

    if (!all(names(roeNoHS)==c("REM_raw", "REM_detect", "REM_detect_active",
                               "IS_raw", "IS_detect", "IS_detect_active")))
        stop("Non convenient names for the methods")

    if (!all(sapply(roeNoHS, function(x) all(apply(sapply(x, colnames),1,unique)==c("PtEst", "EstSE", "EstCIl", "EstCIu")))))
        stop("non convenient column names for the objects")

    if (!all(sapply(roeNoHS, function(x) all(apply(sapply(x, rownames),1,unique)==
                                             c("N1", "N2", "N3", "N4", "N5", "lambda", "changeRate")))))
        stop("non convenient rownames names for the objects")

    sel <- c(1,5,6,7)
    TrueVal <- c(dfPopSize[sel[1:2],1],
                 exp(coef(lm(log(dfPopSize[,1])~c(1:5)))[2])-1,
    (dfPopSize[5,1]-dfPopSize[1,1])/dfPopSize[1,1])

    nam <- c("REM_raw","REM_det","REM_det_act",
             "IS_raw","IS_det","IS_det_act")
    resu <- lapply(1:length(roeNoHS), function(i) {
        x <- roeNoHS[[i]]
        method <- rep(nam[i],4)
        parameter <- c("N1","N5","lam","CR")
        MeanEst <- rowMeans(sapply(x, function(y) y[sel,1]), na.rm=TRUE)
        SE <- apply(sapply(x, function(y) y[sel,1]), 1, sd, na.rm=TRUE)
        SEEst <- apply(sapply(x, function(y) y[sel,2]), 1, mean, na.rm=TRUE)
        SE.SEEst <- apply(sapply(x, function(y) y[sel,2]), 1, sd, na.rm=TRUE)
        pcover <- rowMeans(sapply(x, function(y)
            TrueVal>=y[sel,3]&TrueVal<=y[sel,4]))

        Psigla <- mean(sapply(x, function(y) y[6,4]<0))
        PsigCR <- mean(sapply(x, function(y) y[7,4]<0))
        PDecla <- mean(sapply(x, function(y) y[6,1]<0))
        PDecCR <- mean(sapply(x, function(y) y[7,1]<0))

        data.frame(method=method,Param=parameter,
                   TrueValue=round(TrueVal,4),MeanEst=round(MeanEst,3),
                   SE=round(SE,3),
                   SEEst=round(SEEst,3), SE.SEEst=round(SE.SEEst,3), Pcov=round(pcover,3),
                   PDec=c("","",round(PDecla,3),round(PDecCR,3)),
                   PsigD=c("","",round(Psigla,3),round(PsigCR,3)))
    })

    separator <- as.data.frame(t(rep("", ncol(resu[[1]]))))
    names(separator) <- names(resu[[1]])


    return(do.call("rbind",lapply(resu, function(x) {
                               rbind(x, separator)
                           })))
}
