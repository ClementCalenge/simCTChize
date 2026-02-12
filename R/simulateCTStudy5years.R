simulateCTStudy5years <-
    function(nCT=100, duration=30, niter=15, dfPopSize, listPatches,
             contourChize, contourN, contourS, dff,
             habitatMap, listptg, ptp,
             stratSampling=FALSE, propPaths=NULL, pathsMap=NULL,
             pathsBuffer=NULL, offPathsMap=NULL, offPathsBuffer=NULL,
             habitatSelection=FALSE, LRroeDeer=FALSE, LRroeDeerIncreaseDV=FALSE,
             fieldangle=0.175, depth=20,
             nofi=0, backup=TRUE)
    {


        ## Checks
        if (!inherits(dfPopSize, "data.frame"))
            stop("dfPopSize should be a data.frame")
        if (!all(c("Ntot","Old","New")%in%names(dfPopSize)))
            stop("dfPopSize does not contain the correct variable names")
        if (nrow(dfPopSize)!=5)
            stop("dfPopSize should have 5 rows")
        if (ncol(dfPopSize)!=3)
            stop("dfPopSize should have 3 columns")

        if (!is.list(listPatches))
            stop("listPatches should be a list")
        if (!all(sapply(listPatches, function(x) ncol(x)==2)))
            stop("listPatches should be a list with 2-columns matrices")

        if (ncol(contourChize)!=2)
            stop("contourChize should be a matrix with two columns")
        if (ncol(contourN)!=2)
            stop("contourN should be a matrix with two columns")
        if (ncol(contourS)!=2)
            stop("contourS should be a matrix with two columns")

        if (!inherits(dff, "data.frame"))
            stop("dff should be a data.frame")

        if (!all(c("sigma","ani","dmaxp")%in%names(dff)))
            stop("dff does not contain the correct variable names")

        if (!inherits(habitatMap, "sf"))
            stop("habitatMap should inherit the class \"sf\"")

        if (!all(sapply(listptg, function(x) inherits(x, "pretraj"))))
            stop("non convenient listptg")

        if (!inherits(ptp, "pretraj"))
            stop("non convenient ptp")
        if (!is.null(pathsMap)) {
            if (!((inherits(pathsMap, "sf"))|(inherits(pathsMap,"sfc"))))
                stop("pathsMap should inherit the class \"sf\" or \"sfc\"")
        }
        if (!is.null(pathsBuffer)) {
            if (!((inherits(pathsBuffer, "sf"))|(inherits(pathsBuffer,"sfc"))))
                stop("pathsBuffer should inherit the class \"sf\" or \"sfc\"")
        }
        if (!is.null(offPathsMap)) {
            if (!((inherits(offPathsMap, "sf"))|(inherits(offPathsMap,"sfc"))))
                stop("offPathsMap should inherit the class \"sf\" or \"sfc\"")
        }
        if (!is.null(offPathsBuffer)) {
            if (!((inherits(offPathsBuffer, "sf"))|(inherits(offPathsBuffer,"sfc"))))
                stop("offPathsBuffer should inherit the class \"sf\" or \"sfc\"")
        }
        if ((!LRroeDeer)&LRroeDeerIncreaseDV)
            stop("LRroeDeerIncreaseDV cannot be TRUE if LRroeDeer is FALSE")

        redDeer <- LRroeDeer
        redDeerIncreaseDV <- LRroeDeerIncreaseDV
        ## Name backup file
        fileEncounter=paste0("fic5years-",nofi,"listSimus-CT", nCT, "-Ani.Rds")
        fileSpeed=paste0("fic5years-",nofi,"listSpeed-CT", nCT, "-Ani.Rds")
        fileActiveSpeed=paste0("fic5years-",nofi,"listActiveSpeed-CT", nCT, "-Ani.Rds")


        ## "Rush hour" (when all animals are active)
        hpoin <- ((1:(33*24*3600))%%(24*3600))/3600

        ## List of results
        listresults <- list()
        listresultsv <- list()
        listresultsv2 <- list()
        liv <- 0
        liv2 <- 0

        ## Seeds used to randomly place the traps on the area (position
        ## and content including the trees)
        seedsTrapsPos <- as.integer(runif(niter)*1e9)
        seedsTrapsCon <- as.integer(runif(niter)*1e9)

        ## if habitat selection, centroid should be at least 600 m from the border
        ## preparation of nordPaths and southPaths
        if (habitatSelection) {

            ## core map of paths (located at >600 m from the border)
            cartec <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize))))
            sf::st_crs(cartec) <- sf::st_crs(pathsBuffer)

            ## 600 m for a roe deer, 1000 m for a red deer
            if (!redDeer) {
                cartec <- sf::st_buffer(cartec,-600)
            } else {
                cartec <- sf::st_buffer(cartec,-1000)
            }
            ## The paths at the core
            corePaths <- sf::st_intersection(pathsBuffer, cartec)

            ## North and south paths roe deer
            if (!redDeer) {
                po1 <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourN))))
                sf::st_crs(po1) <- sf::st_crs(corePaths)
                northPaths <- sf::st_intersection(corePaths,po1)

                po1 <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourS))))
                sf::st_crs(po1) <- sf::st_crs(corePaths)
                southPaths <- sf::st_intersection(corePaths,po1)
            }
        } else {
            ## if no habitat selection, we just focus on a core study area
            ## eroded by 1000 m
            if (redDeer) {
                cartec <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize))))
                corePaths <- sf::st_buffer(cartec,-1000)
            }
        }

        if (redDeer) {
            ptsampa <- sf::st_coordinates(sf::st_sample(corePaths, 20000))
        }

        ## Iterations for each configuration
        for (iter in 1:niter) {

            ## Information to the user
            cat("####################################\n##\n## Iteration ", iter, "\n\n", sep="")

            ## Random sample of CTs
            set.seed(seedsTrapsPos[iter])
            if (stratSampling) {
                cojo <- as.data.frame(strsampleCT(nCT=100, propPaths=propPaths,
                                                  pathsMap=pathsMap,
                                                  pathsBuffer=pathsBuffer,
                                                  offPathsMap=offPathsMap,
                                                  offPathsBuffer=offPathsBuffer))

            } else {
                cojo <- as.data.frame(sf::st_coordinates(sf::st_sample(sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize)))),
                                                                       nCT*2)))[1:nCT,]
            }
            por <- sf::st_as_sf(cojo, coords=c("X","Y"))
            por$X <- cojo[,1]
            por$Y <- cojo[,2]
            por$idp <- 1:nrow(cojo)
            sf::st_crs(por) <- sf::st_crs(habitatMap)
            joo <- sf::st_join(por, habitatMap) ## which habitat type for each CT

            ## If the CT falls on the boundary between two habitat types,
            ## st_join returns a duplicated trap. The if belowtakes the
            ## first type
            if (any(table(joo$idp)>1)) {
                joo <- joo[!duplicated(joo$idp),]
                cojo <- data.frame(X=joo$X, Y=joo$Y)
            }
            suu <- joo$habitat

            ## Bearings of CTs with detectability function of the habitat type
            set.seed(seedsTrapsCon[iter])
            list100CT <- lapply(1:nCT, function(i) {
                cat("CTs: ", i,"\r")
                organiseCT(suu[i], fieldangle=fieldangle, depth=depth)
            })

            ## Preparation de la boucle par animal
            if (!redDeer) {
                sfcn <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourN)))) ## sf of northern SA
                sfcs <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourS)))) ## sf of southern SA

                ## differentiate small home ranges (less than 10 patches) and large home ranges
                nbrpa <- sapply(listPatches,nrow)
                listepetits <- which(nbrpa<=10)
                listegrands <- which(nbrpa>10)
            }


            ## Preparation du tableau de sorties
            ntotani <- dfPopSize$Ntot[1]+sum(dfPopSize$New)
            if (!habitatSelection) {

                if (!redDeer) {
                    paramani <- lapply(1:ntotani, function(i) {
                        north <- (runif(1)<193/300)
                        paramMvt5years(listepetits, listegrands, north, contourChize, contourN, contourS, dff)
                    })
                }
            } else {
                if (!redDeer) {
                    paramani <- lapply(1:ntotani, function(i) {
                        north <- (runif(1)<193/300)
                        minsigan <- round(4*(1:5)/5)
                        maxsigan <- round(3+7*(0:4)/4)
                        sigmaan <- sapply(1:5, function(t) sample(minsigan[t]:maxsigan[t], 1))
                        zone <- southPaths
                        if (north)
                            zone <- northPaths
                        lxyz <- HSlocation(zone, pathsBuffer)
                        return(list(lxyz=lxyz, sigmaan=sigmaan))
                    })
                }
            }
            if (redDeer) {
                if (!redDeerIncreaseDV) {
                    ## same format as roe deer when habitatSelection is TRUE
                    paramani <- lapply(1:ntotani, function(i) {

                        idp <- sample(1:20000,1)
                        ## list of possible patches +/- 2000 m from this point
                        ref <- ptsampa[idp,]
                        oxy <- ptsampa[ptsampa[,1]>(ref[1]-2000)&
                                       ptsampa[,1]<(ref[1]+2000)&
                                       ptsampa[,2]>(ref[2]-2000)&
                                       ptsampa[,2]<(ref[2]+2000),]
                        ## Random number of patches
                        np <- sample(5:10,1)
                        coo <- oxy[sample(1:nrow(oxy), np),]
                        com <- colMeans(coo)
                        coo[,1] <- coo[,1]-com[1]
                        coo[,2] <- coo[,2]-com[2]
                        cb <- coo
                        return(list(lxyz=list(centroide=com,
                                              patchs=cb), sigmaan=rep(10,5))) ## Big patch here
                    })
                } else {
                    ## XXXXXXXXXXXXXXXXXXXXXXXXXXXX c'est là que ça se joue
                    ## same format as roe deer when habitatSelection is TRUE
                    paramani <- lapply(1:ntotani, function(i) {

                        idp <- sample(1:20000,1)
                        ## list of possible patches +/- 2000 m from this point
                        ref <- ptsampa[idp,]
                        sizta <- seq(1414, 2000, length=5)
                        lcb <- lapply(1:5, function(t) {
                            oxy <- ptsampa[ptsampa[,1]>(ref[1]-sizta[t])&
                                           ptsampa[,1]<(ref[1]+sizta[t])&
                                           ptsampa[,2]>(ref[2]-sizta[t])&
                                           ptsampa[,2]<(ref[2]+sizta[t]),]
                            ## Random number of patches
                            np <- sample(5:10,1)
                            coo <- oxy[sample(1:nrow(oxy), np),]
                            coo[,1] <- coo[,1]-ref[1]
                            coo[,2] <- coo[,2]-ref[2]
                            cb <- coo
                            return(cb)
                        })
                        return(list(lxyz=list(centroide=ref,
                                              patchs=lcb), sigmaan=rep(10,5))) ## Big patch here
                    })

                }

            }

            nv <- dfPopSize$New
            nv[1] <- dfPopSize$Ntot[1]
            debut <- rep(1:5, nv)
            fin <- rep(5,length(debut))
            vivants <- rep(TRUE, length(debut))
            for (t in 2:5) {
                w <- which(debut<t)
                w2 <- which(vivants)
                w <- w[w%in%w2]
                morts <- sample(w, dfPopSize$Ntot[t-1]-dfPopSize$Old[t])
                vivants[morts] <- FALSE
                fin[morts] <- t-1
            }

            ## in the end: two informations for each animal: when it appears (debut), when it dies (fin)
            ## We give it an ID (NoAni)
            dfduree <- data.frame(NoAni=1:length(debut), debut=debut, fin=fin)
            ## and paramani for the parameters of each animal.
            ## I reframe it in a list of 5 years, with:
            ## - number
            ## - list to simulate
            quelsi <- list()
            parama <- list()
            for (t in 1:5) {
                ooo <- dfduree[dfduree$debut<=t&dfduree$fin>=t,]
                quelsi[[t]] <- ooo$NoAni
                tmp <- paramani[quelsi[[t]]]
                if ((!habitatSelection)&(!redDeer)) {
                    parama[[t]] <- lapply(tmp, function(x) {
                        return(list(ani=x$ani[t],sigma=x$sigma[t], xy=x$lixysim[t,]))
                    })
                } else {
                    if (!redDeerIncreaseDV) {
                        parama[[t]] <- lapply(tmp, function(x) {
                            return(list(xy=as.vector(x$lxyz$centroide), patchs=x$lxyz$patchs,sigma=x$sigmaan[t]))
                        })
                    } else {
                        parama[[t]] <- lapply(tmp, function(x) {
                            return(list(xy=as.vector(x$lxyz$centroide),
                                        patchs=x$lxyz$patchs[[t]],sigma=x$sigmaan[t]))
                        })

                    }
                }
            }

            ##
            listyears <- list()
            listyearsv <- list()
            listyearsva <- list()

            ## For each year
            for (t in 1:5) {
                cat("Year: ", t,"\r")
                lire <- list()
                k <- 1
                liv <- 0
                liv2 <- 0

                ## For each animal of this year
                for (iAni in 1:length(parama[[t]])) {

                    ## Get the parameters and simulate the movement
                    paAni <- parama[[t]][[iAni]]
                    if (redDeer) {
                        forptp <- listptg[[paAni$sigma]] ## corrigé ici aussi
                    } else {
                        forptp <- ptp
                    }
                    lipt <- list(ptg=listptg[[paAni$sigma]],
                                 ptp=forptp)
                    if ((!habitatSelection)&(!redDeer)) {
                        lixyt <- lapply(1:10, function(r) listPatches[[paAni$ani]])
                    } else {
                        lixyt <- list(paAni$patchs, paAni$patchs)
                    }
                    z <- roeDeerMovement(30, lixyt, lipt=lipt, verbose=FALSE)
                    z <- t(t(z)+paAni$xy)

                    ## Calculate average speed of the animal
                    vitt <- (z[-1,]-z[-nrow(z),])^2
                    vitt <- sqrt(vitt[,1]+vitt[,2])
                    liv[iAni] <- mean(vitt)

                    ## speed during rush hours
                    vv <- c(vitt,vitt[length(vitt)])
                    datee <- hpoin[1:length(vv)]
                    vv2 <- vv[datee<15]
                    liv2[iAni] <- mean(vv2)

                    ## Add the date to coordinates of the trajectory (per second)
                    z <- as.data.frame(z)
                    z$date <- 1:nrow(z)

                    ## Find the traps in the neighborhood of the trajectory
                    bbox <- range(z[,1])+c(-20,20)
                    bboy <- range(z[,2])+c(-20,20)
                    ## (add a buffer of 20 m to find CT around)
                    ## Identification of traps
                    ppcl <- which(cojo[,1]>bbox[1]&cojo[,1]<bbox[2]&
                                  cojo[,2]>bboy[1]&cojo[,2]<bboy[2])

                    listesim <- list()

                    ## If there are traps
                    if (length(ppcl)>0) {

                        ## For each traps, count the contacts and store them in lire
                        for (j in 1:length(ppcl)) {
                            o <- list100CT[[ppcl[j]]]
                            o$x <- o$x+cojo[ppcl[j],1]
                            o$y <- o$y+cojo[ppcl[j],2]
                            attr(o,"cooori") <- c(cojo[ppcl[j],1],cojo[ppcl[j],2])
                            que <- whenDetection(z, o)
                            if (!is.null(que)) {
                                que$whichani <- iAni ## bug corrected here: i -> iAni
                                que$whichCT <- ppcl[j]
                                lire[[k]] <- que
                                k <- k+1
                            }
                        }
                    }

                }
                listyears[[t]] <- lire
                listyearsv[[t]] <- liv
                listyearsva[[t]] <- liv2
            }


            ## On stocke les resultats dans des listes
            listresults[[iter]] <- list(creationCT=c(position=seedsTrapsPos[iter],
                                                     content=seedsTrapsCon[iter]),
                                        indicetraps=data.frame(x=cojo[,1],y=cojo[,2], hab=suu),
                                        results=listyears)
            listresultsv[[iter]] <- listyearsv
            listresultsv2[[iter]] <- listyearsva

            ## Et on sauve les listes
            if (backup) {
                saveRDS(listresults, file=fileEncounter)
                saveRDS(listresultsv, file=fileSpeed)
                saveRDS(listresultsv2, file=fileActiveSpeed)
            }
        }
        resu <- list(encounters=listresults, speed=listresultsv, activeSpeed=listresultsv2)
        class(resu) <- "simCT"
        attr(resu,"CTparameters") <- list(r=depth, theta=fieldangle)

        return(resu)
    }
