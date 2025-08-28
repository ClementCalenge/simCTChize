removeHour <-
function(x, startHour=8, endHour=17)
{
    if (is.null(x)||nrow(x)==0)
        return(x)
    if (!inherits(x,"data.frame"))
        stop("x should be a data.frame")
    if (!all(c("noPassage", "beginning", "end", "duration", "detection",
               "Pdetect",
               "When", "whichani", "whichCT")%in%names(x)))
        stop("non convenient data for x")
    if (!is.list(x$When))
        stop("non convenient data for x")


    ## hours with modulo 24*3600, and transformed in 17h = 0h (as the
    ## day starts at 17h)
    startHourh <- (startHour+7)%%24
    endHourh <- (endHour+7)%%24
    if (endHourh==0)
        endHourh <- 24
    x2 <- x

    if (startHourh>endHourh)
        stop("startHour should be before endHour")

    ## For each encounter
    k <- 1
    vire <- numeric(0)

    for (i in 1:nrow(x)) {
        y <- x[i,]

        ## Calculation of the hour for each encounter (expressed in
        ## hour 17h=0h)
        deby <- (y$beginning%%(24*3600))/3600
        finy <- (y$end%%(24*3600))/3600

        ## If the encounter begins before the period to remove and
        ## ends before the period to remove: the end of encounter
        ## stops at the startHour
        if ((deby<startHourh)&(finy>startHourh)&(finy<endHourh)) {
            surpl <- (finy-startHourh)
            y$end <- y$end-surpl
        }

        ## if the encounter starts before the period to remove and
        ## ends after this period (animal that stays), do
        ## nothing. Just send a warning (the case is too rare to be a
        ## problem for us.
        if ((deby<startHourh)&(finy>endHourh)) {
            warning("One pb")
        }

        ## if the encounter starts during and end after the end of the
        ## period, define the beginning at the end of the period
        if ((deby>startHourh)&(deby<endHourh)&(finy>endHourh)) {
            surpl <- (endHourh-deby)
            y$beginning <- y$beginning+surpl
        }

        ## If everything happens during the period, remove
        if ((deby>startHourh)&(finy<endHourh)) {
            vire[k] <- i
            k <- k+1
        }

        ## Update association hours/dates
        u <- ((y$When[[1]]%%(24*3600))/3600)
        qu <- y$When[[1]][!((u>=startHourh)&(u<=endHourh))]
        if (length(qu)!=length(u))
            y$When[[1]] <- qu
        y$detection <- length(qu)>0
        x2[i,] <- y
    }
    x2 <- x2[-vire,]
    return(x2)
}
