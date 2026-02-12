\name{stratData}
\alias{stratData}
\docType{data}
\title{
  Population Size Estimates for our Simulations of a Camera
  Trap Study on a Roe Deer Population in Two Habitat Types Based on a
  Stratified sample of 100 traps 
}
\description{
  This dataset contains the point estimates of population size for 500
  simulations of a  5-year camera trap monitoring based on 100 traps of a roe deer
  population.  The sample of camera traps was stratified: a proportion p
  of the traps was placed in the neighborhood of paths (the habitat type
  selected by the roe deer).  A vector of 19 proportions was used in this
  dataset, from 5 to 95 percent of the traps. See examples for the code
  used to create this object.
}
\usage{data("stratData")}
\format{

  This dataset is a list with 3 components: (i) the component
  \code{inPaths} contains the estimates of the number of animals in the
  paths neighborhood, (ii) \code{offPaths} contains the estimates of the
  number of animals far from the paths, (iii) \code{all} contains the
  sum of the two components.  Each component is a list with 19
  subcomponents corresponding to the 19 tested proportions of traps
  placed in the paths neighborhood.  Each subcomponent is a list of 500
  data.frames containing the estimates of population size for each year
  and each tested method. The names of the methods are, for the REM: \code{REM_raw}
  (REM which does not account for detectability or activity),
  \code{REM_det} (REM accounting for detectability), 
  \code{REM_det_act} (REM accounting for detectability and
  activity). The names are similar for the IS (but with \code{IS}
  replacing \code{REM}).  The name ends with the year number. For
  example, \code{REM_raw.1} corresponds to the estimate of the
  population size by \code{REM_raw} during year 1.

}
\source{
  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\references{
  Calenge et al. 2026. Evaluating camera trap methods for monitoring
  population trends in ungulates: insights from simulation. in prep.
}
\examples{

## A very slow code, provided for information.

## We advise the user not to try it, unless they want a coffee pause
## of one week.

\dontrun{

    ## Simulate 100 traps in the habitat far from the paths (notice
    ## propPaths=0)
    simrd5withhs0 <- simulateCTStudy5years(nCT = 100, duration = 30,
                                           niter = 500, dfPopSize, 
                                           listPatches, contourChize,
                                           contourN=northChize,
                                           contourS=southChize,
                                           dff=relationAnimalSigmaMaxd, habitatMap,
                                           listptg=mvtMatrices$listMatBetween,
                                           ptp=mvtMatrices$MatWithin,
                                           stratSampling=TRUE,
                                           propPaths=0, pathsMap=pathsMap,
                                           pathsBuffer=pathsBuffer,
                                           offPathsMap=offPathsMap,
                                           offPathsBuffer=offPathsBuffer,
                                           habitatSelection=TRUE,
                                           nofi = 0, backup = TRUE)


    ## Simulate 100 traps in the habitat close to the paths (notice
    ## propPaths=1)
    simrd5withhs <- simulateCTStudy5years(nCT = 100, duration = 30,
                                           niter = 500, dfPopSize, 
                                           listPatches, contourChize,
                                           contourN=northChize,
                                           contourS=southChize,
                                           dff=relationAnimalSigmaMaxd, habitatMap,
                                           listptg=mvtMatrices$listMatBetween,
                                           ptp=mvtMatrices$MatWithin,
                                           stratSampling=TRUE,
                                           propPaths=1, pathsMap=pathsMap,
                                           pathsBuffer=pathsBuffer,
                                           offPathsMap=offPathsMap,
                                           offPathsBuffer=offPathsBuffer,
                                           habitatSelection=TRUE,
                                           nofi = 0, backup = TRUE)

    
    ## Calculate the mean detection probability for each trap
    ## Far from paths
    prop0 <- lapply(1:500, function(j) {
    cat(j,"\r")
    habitat4sim <- simrd5withhs0$encounters[[j]]$indicetraps$hab
    set.seed(simrd5withhs0$encounters[[j]]$creationCT[2])
    list100CT <- lapply(1:100, function(k) {
        organiseCT(habitat4sim[k])
    })
    sapply(1:length(list100CT),
           function(trap)
        mean(1 -
             exp(-list100CT[[trap]]$vu)))
})

    ## Close to paths
    prop1 <- lapply(1:500, function(j) {
    cat(j,"\r")
    habitat4sim <- simrd5withhs$encounters[[j]]$indicetraps$hab
    set.seed(simrd5withhs$encounters[[j]]$creationCT[2])
    list100CT <- lapply(1:100, function(k) {
        organiseCT(habitat4sim[k])
    })
    sapply(1:length(list100CT),
           function(trap)
        mean(1 -
             exp(-list100CT[[trap]]$vu)))
    })



    
    ## The code used to generate stratData
    liche <- list()
    lipas <- list()
    litot <- list()

    for (i in 1:length(pro)) {

        cat("#############################\n## Iteration",i, "\n\n")
        cat("## rem_raw\n")
        reb <- sampleCTs(simrd5withhs, pro[i])
        rec <- sampleCTs(simrd5withhs0, 100-pro[i])

        ## REM_raw:
        rem_raw_che <- Remhab(reb, paths=TRUE, detection=FALSE, active=FALSE)
        rem_raw_pas <- Remhab(rec, paths=FALSE, detection=FALSE, active=FALSE)
        rem_raw <- rem_raw_che+rem_raw_pas

        ## REM_detect:
        cat("## rem_det\n")
        rem_det_che <- Remhab(reb, paths=TRUE, detection=TRUE, active=FALSE)
        rem_det_pas <- Remhab(rec, paths=FALSE, detection=TRUE, active=FALSE)
        rem_det <- rem_det_che+rem_det_pas

        ## REM_detect_activ
        cat("## rem_det_act\n")
        rem_det_act_che <- Remhab(reb, paths=TRUE, detection=TRUE, active=TRUE)
        rem_det_act_pas <- Remhab(rec, paths=FALSE, detection=TRUE, active=TRUE)
        rem_det_act <- rem_det_act_che+rem_det_act_pas


        ## IS_raw:
        cat("## is_raw\n")
        is_raw_che <- IShab(reb, paths=TRUE, detection=FALSE, active=FALSE, prop=prop1)
        is_raw_pas <- IShab(rec, paths=FALSE, detection=FALSE, active=FALSE, prop=prop0)
        is_raw <- is_raw_che+is_raw_pas

        ## IS_detect:
        cat("## is_det\n")
        is_det_che <- IShab(reb, paths=TRUE, detection=TRUE, active=FALSE, prop=prop1)
        is_det_pas <- IShab(rec, paths=FALSE, detection=TRUE, active=FALSE, prop=prop0)
        is_det <- is_det_che+is_det_pas

        ## IS_detect_activ
        cat("## is_det_act\n")
        is_det_act_che <- IShab(reb, paths=TRUE, detection=TRUE, active=TRUE, prop=prop1)
        is_det_act_pas <- IShab(rec, paths=FALSE, detection=TRUE, active=TRUE, prop=prop0)
        is_det_act <- is_det_act_che+is_det_act_pas

        ##
        liche[[i]] <- data.frame(rem_raw=rem_raw_che, rem_det=rem_det_che,
                                 rem_det_act=rem_det_act_che,
                                 is_raw=is_raw_che, is_det=is_det_che,
                                 is_det_act=is_det_act_che)
        lipas[[i]] <- data.frame(rem_raw=rem_raw_pas, rem_det=rem_det_pas,
                                 rem_det_act=rem_det_act_pas,
                                 is_raw=is_raw_pas, is_det=is_det_pas,
                                 is_det_act=is_det_act_pas)

        litot[[i]] <- data.frame(rem_raw=rem_raw, rem_det=rem_det,
                                 rem_det_act=rem_det_act,
                                 is_raw=is_raw, is_det=is_det,
                                 is_det_act=is_det_act)
    }

    stratData <- list(inPaths=liche, offPaths=lipas, all=litot)

    


}


}
\keyword{datasets}
