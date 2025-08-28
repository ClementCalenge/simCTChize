organiseCT <-
function(habitatType, fieldangle=0.175, depth=20)
{
    ## Case: coppice
    if (is.na(habitatType))
        habitatType <- "Coppice"
    if (!habitatType%in%c("Coppice","REG","OPE"))
        stop("habitatType should be one of Coppice, REG, OPE")

    if (habitatType=="Coppice") {

        dfpb <- obstaclesQ(depth=20, stemsha=round(runif(1,50,70)),
                           diamMin=0.175, diamMax=0.275, res=0.1)
        dfbm <- obstaclesQ(depth=20, stemsha=round(runif(1,25,35)),
                           diamMin=0.275, diamMax=0.475, res=0.1)
        dfgb <- obstaclesQ(depth=20, stemsha=round(runif(1,12,17)),
                           diamMin=0.475, diamMax=1, res=0.1)
        df <- dfpb
        df$vu[df$vu&!dfbm$vu] <- 0
        df$vu[df$vu&!dfgb$vu] <- 0


        ## Simulation of camera-traps
        ## Random orientation
        angleEast <- runif(1,0,2*pi)

        D1 <- 5; D2 <- 18 ## From Howe et al.
        u <- generateCTbase(df, orientationE=angleEast, angle=fieldangle,
                            depth=depth, D1=D1, D2=D2)
    }
    if (habitatType=="REG") {
        df <- obstaclesQ(depth=20, stemsha=0.001,
                         diamMin=0.175, diamMax=0.275, res=0.1)
        df$vu[df$vu!=1] <- 1

        ## random orientation
        angleEast <- runif(1,0,2*pi)

        D1 <- 0.5; D2 <- 3 ## subjective choice
        u <- generateCTbase(df, orientationE=angleEast, angle=fieldangle, depth=depth, D1=D1, D2=D2)
    }
    if (habitatType=="OPE") {
        df <- obstaclesQ(depth=20, stemsha=0.001,
                         diamMin=0.175, diamMax=0.275, res=0.1)
        df$vu[df$vu!=1] <- 1

        ## Random orientation
        angleEast <- runif(1,0,2*pi)

        D1 <- 5; D2 <- 18 ## subjective choice
        u <- generateCTbase(df, orientationE=angleEast, angle=fieldangle, depth=depth, D1=D1, D2=D2)
    }
    attr(u,"val") <- list(angleEast=angleEast, fieldAngle=fieldangle, depth=depth)
    return(u)
}
