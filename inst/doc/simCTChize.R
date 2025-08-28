## ----setup, include=FALSE, cache=FALSE--------------------
# set global chunk options
library('knitr')
opts_chunk$set(fig.path="wapat-",
               fig.align="center",
               fig.show="hold",
               echo=TRUE,
               results="markup",
               fig.width=7,
               fig.height=7, out.width='0.6\\linewidth',
               out.height='0.6\\linewidth',
               cache=FALSE,
               dev='png',
               concordance=TRUE,
               error=FALSE)
opts_knit$set(aliases = c(h = 'fig.height',
              w = 'fig.width',
              wo='out.width',
              ho='out.height'))
options(replace.assign=TRUE,width=60)
opto <- options()
str <- options()$str
str$vec.len <- 2
options(str=str)
set.seed(9567)


## ----eval=FALSE-------------------------------------------
## install.packages("simCTChize_1.0.tar.gz", repos = NULL, type="source")


## ----eval=FALSE-------------------------------------------
## ## If devtools is not yet installed, type
## install.packages("devtools")
## 
## ## Install the package badgertub
## devtools::install_github("ClementCalenge/simCTChize", ref="main")


## ----load-package-----------------------------------------
library(simCTChize)


## ----plot-list-patches------------------------------------
plot(listPatches[[1]])


## ----calc-matrices-mvts, eval=FALSE-----------------------
## ## A list of matrices with different values of sigma comprised between
## ## 1000 and 10000 for the "between patches movement" (remember that
## ## sigma is randomly drawn between 1000 and 10000 m2 for this movement
## ## type, so that we need a list of matrices -- explaining the use of
## ## lapply here)
## listMatBetween <- lapply(seq(1000,10000, by=1000),
##                          function(x) prepareMvt(tau = c(4000,4000),
##                                                 sigma = x))
## 
## ## The list of matrices for the "patch movement"
## MatWithin <- prepareMvt(tau = c(300,300), sigma = 200)
## 
## ## We store the results in a list
## mvtMatrices <- list(listMatBetween=listMatBetween,
##                     MatWithin=MatWithin)


## ----example-movements------------------------------------
## Simulation of movement for 30 days:
set.seed(777) ## for reproducibility

## Lets say that we simulate movements between the attraction points
## of the first set of patches
setpat <- list(listPatches[[1]])


## We will simulate an animal with sigma=1000 for between-patches
## movements (first matrix of listMatBetween).
lipt <- list(ptg=mvtMatrices$listMatBetween[[1]],
             ptp=mvtMatrices$MatWithin)

## Simulation
rd <- roeDeerMovement(30, setpat, lipt = lipt, verbose=FALSE)

## the result:
plot(rd, ty="l", asp=1, xlab="X coordinate (metres)",
     ylab="Y coordinate (metres)")


## ---------------------------------------------------------
## The coordinates of the centroid
coordinatesHRcentroid <- c(0,0)

## Shift the movement to this centroid
rd[,1] <- rd[,1]+coordinatesHRcentroid[1]
rd[,2] <- rd[,2]+coordinatesHRcentroid[2]


## ---------------------------------------------------------
## Transform in data.frame and add a date
rd <- as.data.frame(rd)
rd$date <- 1:nrow(rd)


## ---------------------------------------------------------
set.seed(777)  ## for reproducibility
ct <- organiseCT("Coppice")
head(ct)


## ---------------------------------------------------------
## For a clearer depiction of the detectability:
## log scale and rescale between 0 and 1
u <- log(ct[,3]+0.001)
v <- (u-min(u))/(max(u)-min(u))

## Then show this detectability in grey levels
plot(ct[,1],ct[,2], col=grey(v), pch=16, cex=0.7, asp=1,
     xlab="X", ylab="Y", main="Detection zone of a camera trap")


## ---------------------------------------------------------
attr(ct, "cooori") <- c(0,0)                    


## ---------------------------------------------------------
u <- log(ct[,3]+0.001)
v <- (u-min(u))/(max(u)-min(u))
plot(ct[,1],ct[,2], col=grey(v), pch=16, cex=0.7, asp=1,
     xlab="X", ylab="Y", main="Detection zone of a camera trap")
lines(rd[,1:2], col="red")


## ---------------------------------------------------------
wd <- whenDetection(as.data.frame(rd), ct)
str(wd)


## ---------------------------------------------------------
u <- log(ct[,3]+0.001)
v <- (u-min(u))/(max(u)-min(u))
plot(ct[,1],ct[,2], col=grey(v), pch=16, cex=0.7, asp=1,
     xlab="X", ylab="Y", main="Detection zone of a camera trap")
lines(rd[,1:2], col="red")
tmp <- lapply(wd$When, function(x) {
    points(rd[rd$date%in%x, 1:2], pch=21, bg="yellow", col="green")
})


## ---------------------------------------------------------
set.seed(77) ## For the reproducibility
survival <- 0.7
survivalFawn <- 0.3
propParticipatingFemales <- 0.9

## What we have in March during year 1
Nmales <- 150
Nfemales <- 150
Ntot <- Nmales+Nfemales

## Reproductive females
Nfemalesrepro <- rbinom(1, Nfemales, propParticipatingFemales)

## Births: all participating females have by definition at least one fawn
## They have a second one with a probability of 0.7
Nbirths <- Nfemalesrepro+rbinom(1,Nfemalesrepro,0.7)

## Half males and half females in the births
Nyoungmales <- rbinom(1,Nbirths,0.5)
Nyoungfemales <- Nbirths-Nyoungmales

## We will need that
Nfemalesrepro <- 0
Newmales <- 0
Newfemales <- 0
Oldmales <- 0
Oldfemales <- 0


## For each year after the first one
for (i in 2:5) {

    ## Following March, survival of fawns of last year + male survival
    Newmales[i] <- rbinom(1, Nyoungmales[i-1], survivalFawn)
    Oldmales[i] <- rbinom(1, Nmales[i-1], survival)
    Nmales[i] <- Oldmales[i]+Newmales[i]

    ## Same for females
    Newfemales[i] <- rbinom(1, Nyoungfemales[i-1], survivalFawn)
    Oldfemales[i] <- rbinom(1, Nfemales[i-1], survival)
    Nfemales[i] <- Oldfemales[i]+Newfemales[i]

    ## So total population size
    Ntot[i] <- Nmales[i]+Nfemales[i]

    ## And next spring, births (same as year 1)
    Nfemalesrepro[i] <- rbinom(1, Nfemales[i], propParticipatingFemales)
    Nbirths[i] <- Nfemalesrepro[i]+rbinom(1,Nfemalesrepro[i],0.7)
    Nyoungmales[i] <- rbinom(1,Nbirths[i],0.5)
    Nyoungfemales[i] <- Nbirths[i]-Nyoungmales[i]
}

data.frame(Ntot, Old=Oldmales+Oldfemales, New=Newmales+Newfemales)


## ---------------------------------------------------------
dfPopSize


## ----eval=FALSE-------------------------------------------
## ## list of Patches used for year 1 and year 5
## patchesYear1 <- listPatches[sapply(listPatches,nrow)<=10]
## patchesYear5 <- listPatches
## 
## ## remember that the list of movement matrices contain one element per
## ## value of sigma2 comprised between 1000 and 10000. Thus:
## mvtMatricesYear1 <- mvtMatrices
## mvtMatricesYear1$listMatBetween <- mvtMatricesYear1$listMatBetween[1:3]
## mvtMatricesYear5 <- mvtMatrices
## 
## 
## ## Load adehabitatHR for home-range size estimation
## library(adehabitatHR)
## 
## ## Initialization
## simHRspeed <- list()
## 
## ## Simulation
## set.seed(777) ## for reproducibility
## 
## ## For each year
## for (year in c(1,5)) {
## 
##     homeRangeSizey <- numeric(0)
##     speedy <- numeric(0)
## 
##     ## And for each animal
##     for (i in 1:200) {
##         cat("year", year,"animal", i,"\r")
## 
##         if (year==1) {
##             lipt <- list(ptg=mvtMatricesYear1$listMatBetween[[sample(1:3,1)]],
##                          ptp=mvtMatrices$MatWithin)
##             patch <- patchesYear1
##         } else {
##             lipt <- list(ptg=mvtMatricesYear5$listMatBetween[[sample(1:10,1)]],
##                          ptp=mvtMatrices$MatWithin)
##             patch <- patchesYear5
##         }
## 
##         z <- roeDeerMovement(30, lixy=patch, verbose=FALSE, lipt=lipt)
## 
##         ## Calculation of average speed
##         speedy[i] <- mean(sqrt(rowSums((z[-1,]-z[-nrow(z),])^2)))
## 
##         ## Home-range size estimation if the animal had been monitored by
##         ## GPS with one relocation every 20 min
##         m <- as.integer(seq(1,nrow(z),by=60*20))
##         zb <- z[m,]
##         homeRangeSizey[i] <- mcp(SpatialPoints(zb))[[2]]
##     }
##     simHRspeed[[year]] <- data.frame(year=year, HRsize=homeRangeSizey,
##                                      speed=speedy)
## }
## 
## HRspeed <- do.call(rbind, simHRspeed)


## ---------------------------------------------------------
library(ggplot2)
ggplot(HRspeed, ggplot2::aes(y = HRsize, x = factor(year))) +
    geom_violin(draw_quantiles = c(0.1, 0.9)) +
    geom_boxplot(width = 0.2, fill = "grey", 
                 outlier.shape = NA) + ylab("Home-range size (hectares)") + 
    xlab("year")
dvm <- round(tapply(HRspeed$HRsize, HRspeed$year, mean), 1)
dvs <- round(tapply(HRspeed$HRsize, HRspeed$year,sd),1)


## ---------------------------------------------------------
ggplot(HRspeed, ggplot2::aes(y = speed, x = factor(year))) +
    geom_violin(draw_quantiles = c(0.1, 0.9)) +
    geom_boxplot(width = 0.2, fill = "grey", 
                 outlier.shape = NA) + ylab("Mean speed (m/s)") + 
    xlab("year")


## ---------------------------------------------------------
plot(contourChize, type="n", xlab="X coordinates (Lambert II)",
     ylab="Y coordinates (Lambert II)", main="Chize study area")
polygon(contourChize, lwd=3)
polygon(northChize, col="lightblue")
polygon(southChize, col="orange")


## ---------------------------------------------------------
library(sf)
plot(habitatMap)


## ----eval=FALSE-------------------------------------------
## sim4iter <- simulateCTStudy5years(nCT = 100, duration = 30,
##                                   niter = 4, dfPopSize,
##                                   listPatches, contourChize, contourN=northChize,
##                                   contourS=southChize,
##                                   dff=relationAnimalSigmaMaxd, habitatMap,
##                                   listptg=mvtMatrices$listMatBetween,
##                                   ptp=mvtMatrices$MatWithin, nofi = 0, backup = TRUE)


## ---------------------------------------------------------
sim4iter


## ---------------------------------------------------------
str(sim4iter$encounters,2)


## ---------------------------------------------------------
str(sim4iter$encounters[[1]]$results[[1]][[1]])


## ---------------------------------------------------------
firstyear <- sim4iter$encounters[[1]]$results[[1]]
str(head(firstyear,7),1)


## ---------------------------------------------------------
firstyeardf <- do.call(rbind, firstyear)
str(head(firstyeardf))


## ---------------------------------------------------------
(dr1 <-  mean(sim4iter$speed[[1]][[1]]))


## ---------------------------------------------------------
(dra1 <-  mean(sim4iter$activeSpeed[[1]][[1]]))


## ---------------------------------------------------------
(estd <- estimRem(firstyeardf, dr1, 20, 0.175, duration=30, nCT=100))


## ---------------------------------------------------------
(pdetect <- mean(firstyeardf$detection))


## ---------------------------------------------------------
(estd2 <- estimRem(firstyeardf, dr1, 20, 0.175,
                   Pdetect=pdetect, duration=30, nCT=100))


## ---------------------------------------------------------
firstyeardfactive <- removeHour(firstyeardf, 8,17)
## recalculate detection probability
(pdetect <- mean(firstyeardfactive$detection))
(estd3 <- estimRem(firstyeardfactive, dra1, 20, 0.175, hoursPerDay=15,
                   Pdetect=pdetect, duration=30, nCT=100))


## ---------------------------------------------------------
(edr <- sapply(1:4, function(i) {
    firstyear <- sim4iter$encounters[[i]]$results[[1]]
    firstyeardf <- do.call(rbind, firstyear)
    firstyeardfactive <- removeHour(firstyeardf, 8,17)
    dra1 <-  mean(sim4iter$activeSpeed[[i]][[1]])
    (pdetect <- mean(firstyeardfactive$detection))
    estimRem(firstyeardfactive, dra1, 20, 0.175, hoursPerDay=15,
             Pdetect=pdetect, duration=30, nCT=100)
}))


## ---------------------------------------------------------
mean(edr)


## ---------------------------------------------------------
firstyeardf <- do.call(rbind, sim4iter$encounters[[1]]$results[[1]])


## ---------------------------------------------------------
## habitat of the camera traps for the first simulation
habitat4sim <- sim4iter$encounters[[1]]$indicetraps$hab
str(habitat4sim)


## ----eval=FALSE-------------------------------------------
## ## Seed used for the "content" of the detection zone
## set.seed(sim4iter$encounters[[1]]$creationCT[2])
## 
## ## Calculate raster maps of the 100 CT
## list100CT <- lapply(1:100, function(j) {
##     cat("Traps: ", j,"\r")
##     organiseCT(habitat4sim[j])
## })
## 
## ## Calculation of the mean detectability for each trap:
## prop <- sapply(1:length(list100CT), function(trap)
##     mean(1-exp(-list100CT[[trap]]$vu)))


## ----echo=FALSE-------------------------------------------
prop <- c(0.24163462239193, 0.242044244846546, 0.242806710138144, 0.242308700145054, 
0.241819254133619, 0.00624973356465205, 0.241922261400927, 0.241983776438175, 
0.24262452288475, 0.00534244617448404, 0.241663335075927, 0.243236253746008, 
0.248644077179054, 0.242226896681013, 0.241659712462234, 0.241960303635675, 
0.242002593064575, 0.244787558483388, 0.242445296304802, 0.00676876023537776, 
0.00625215383923779, 0.241241775803697, 0.241679591202088, 0.242294145223877, 
0.242041009542621, 0.241496607279067, 0.241147758502601, 0.00545045444294345, 
0.241694998368429, 0.241951153190905, 0.241678974910832, 0.242077947019126, 
0.005956877909146, 0.00475706023537133, 0.240691234726412, 0.241593763117698, 
0.00545849574572982, 0.241616236742851, 0.231689657715438, 0.241612206188287, 
0.00541427579831701, 0.24078846893324, 0.243466170899175, 0.240546064842461, 
0.00584819836444851, 0.241990361941683, 0.241826871864555, 0.00414837009665397, 
0.235456412156215, 0.00552326729715581, 0.242500217692496, 0.00411677247595756, 
0.242182867789186, 0.241561823053323, 0.241899268955046, 0.242121963221192, 
0.00693500351689758, 0.241801653612037, 0.240539195075374, 0.00567377077677686, 
0.241708321263336, 0.241241775803697, 0.241813986018857, 0.240758080646597, 
0.242089481640825, 0.241537605059818, 0.241382742717666, 0.240927795064736, 
0.00528694583689972, 0.239158756276908, 0.241360629612083, 0.213583587163076, 
0.237317323129574, 0.242270622046679, 0.242004779768798, 0.241604888494886, 
0.241615353909812, 0.240974379027768, 0.00449515968971138, 0.241386232895008, 
0.00742022486825327, 0.242699601753129, 0.242093188734102, 0.241517587455238, 
0.24235770589451, 0.24273792414145, 0.00568599875166186, 0.241853542395015, 
0.00475798052839473, 0.241560847632903, 0.241863348389759, 0.240621151837141, 
0.117813191781167, 0.00583442855462709, 0.242383504228829, 0.245750031354992, 
0.243001233240777, 0.241578005300891, 0.234048697194414, 0.246868788738045
)



## ---------------------------------------------------------
(edi1 <- estimIS(firstyeardf, r=20, theta=0.175, detct=prop,
                 nCT=100, duration =30))


## ---------------------------------------------------------
firstyeardfactive <- removeHour(firstyeardf, 8, 17)


## ---------------------------------------------------------
(edi1 <- estimIS(firstyeardfactive, r=20, theta=0.175, detct=prop,
                 nCT=100, duration =30, hoursPerDay=15))


## ----eval=FALSE-------------------------------------------
## edi <- sapply(1:4, function(i) {
##     cat("Iteration:", i, "\n")
##     firstyeardf <- do.call(rbind, sim4iter$encounters[[i]]$results[[1]])
##     fyda <- removeHour(firstyeardf, 8, 17)
## 
##     ## habitat
##     habitat4sim <- sim4iter$encounters[[i]]$indicetraps$hab
## 
##     ## Camera traps
##     set.seed(sim4iter$encounters[[i]]$creationCT[2])
##     list100CT <- lapply(1:100, function(j) {
##         cat("Traps: ", j,"\r")
##         organiseCT(habitat4sim[j])
##     })
##     prop <- sapply(1:length(list100CT), function(trap)
##         mean(1-exp(-list100CT[[trap]]$vu)))
## 
##     ## estimation
##     estimIS(fyda, r=20, theta=0.175, detct=prop,
##             nCT=100, duration =30, hoursPerDay=15)
## 
## })


## ----echo=FALSE-------------------------------------------
edi <- c(313.389353780978, 182.209577209135, 259.040407872574, 322.561688543279
)


## ---------------------------------------------------------
edi
mean(edi)


## ---------------------------------------------------------
(estd3 <- estimRem(firstyeardf, dra1, 20, 0.175,
                   Pdetect=pdetect, duration=30, nCT=100))


## ---------------------------------------------------------
br <- bootstrapREM(firstyeardf, Nboot=1000, dra1, 20, 0.175)
head(br)


## ---------------------------------------------------------
sd(br)/mean(br)


## ---------------------------------------------------------
bi <- bootstrapIS(firstyeardfactive, Nboot=1000, r=20, theta=0.175, detct=prop,
                  hoursPerDay = 15, nCT = 100)
sd(bi)/mean(bi)


## ----eval=FALSE-------------------------------------------
## ## Elements required for year 1
## year1df <- do.call(rbind, sim4iter$encounters[[1]]$results[[1]])
## fyda1 <- removeHour(year1df, 8, 17)
## 
## ## Elements required for year 5
## year5df <- do.call(rbind, sim4iter$encounters[[1]]$results[[5]])
## fyda5 <- removeHour(year5df, 8, 17)
## 
## ## Common elements for the two years:
## ## Camera traps detection probabilities
## habitat4sim <- sim4iter$encounters[[1]]$indicetraps$hab
## set.seed(sim4iter$encounters[[1]]$creationCT[2])
## list100CT <- lapply(1:100, function(j) {
##     cat("Traps: ", j,"\r")
##     organiseCT(habitat4sim[j])
## })
## prop <- sapply(1:length(list100CT), function(trap)
##     mean(1-exp(-list100CT[[trap]]$vu)))
## 
## ## estimation year1
## estyear1 <- estimIS(fyda1, r=20, theta=0.175, detct=prop,
##                     nCT=100, duration =30, hoursPerDay=15)
## 
## ## estimation year5
## estyear5 <- estimIS(fyda5, r=20, theta=0.175, detct=prop,
##                     nCT=100, duration =30, hoursPerDay=15)
## 
## ## estimation CV year1
## byear1 <- bootstrapIS(fyda1, Nboot=1000, r=20, theta=0.175, detct=prop,
##                       hoursPerDay = 15, nCT = 100)
## cvyear1 <- sd(byear1)/mean(byear1)
## 
## 
## ## estimation CV year5
## byear5 <- bootstrapIS(fyda5, Nboot=1000, r=20, theta=0.175, detct=prop,
##                       hoursPerDay = 15, nCT = 100)
## cvyear5 <- sd(byear5)/mean(byear5)
## 


## ----echo=FALSE-------------------------------------------
## Elements required for year 1
year1df <- do.call(rbind, sim4iter$encounters[[1]]$results[[1]])
fyda1 <- removeHour(year1df, 8, 17)

## Elements required for year 5
year5df <- do.call(rbind, sim4iter$encounters[[1]]$results[[5]])
fyda5 <- removeHour(year5df, 8, 17)

## Common elements for the two years:
## Camera traps detection probabilities
prop <- c(0.24163462239193, 0.242044244846546, 0.242806710138144, 0.242308700145054, 
0.241819254133619, 0.00624973356465205, 0.241922261400927, 0.241983776438175, 
0.24262452288475, 0.00534244617448404, 0.241663335075927, 0.243236253746008, 
0.248644077179054, 0.242226896681013, 0.241659712462234, 0.241960303635675, 
0.242002593064575, 0.244787558483388, 0.242445296304802, 0.00676876023537776, 
0.00625215383923779, 0.241241775803697, 0.241679591202088, 0.242294145223877, 
0.242041009542621, 0.241496607279067, 0.241147758502601, 0.00545045444294345, 
0.241694998368429, 0.241951153190905, 0.241678974910832, 0.242077947019126, 
0.005956877909146, 0.00475706023537133, 0.240691234726412, 0.241593763117698, 
0.00545849574572982, 0.241616236742851, 0.231689657715438, 0.241612206188287, 
0.00541427579831701, 0.24078846893324, 0.243466170899175, 0.240546064842461, 
0.00584819836444851, 0.241990361941683, 0.241826871864555, 0.00414837009665397, 
0.235456412156215, 0.00552326729715581, 0.242500217692496, 0.00411677247595756, 
0.242182867789186, 0.241561823053323, 0.241899268955046, 0.242121963221192, 
0.00693500351689758, 0.241801653612037, 0.240539195075374, 0.00567377077677686, 
0.241708321263336, 0.241241775803697, 0.241813986018857, 0.240758080646597, 
0.242089481640825, 0.241537605059818, 0.241382742717666, 0.240927795064736, 
0.00528694583689972, 0.239158756276908, 0.241360629612083, 0.213583587163076, 
0.237317323129574, 0.242270622046679, 0.242004779768798, 0.241604888494886, 
0.241615353909812, 0.240974379027768, 0.00449515968971138, 0.241386232895008, 
0.00742022486825327, 0.242699601753129, 0.242093188734102, 0.241517587455238, 
0.24235770589451, 0.24273792414145, 0.00568599875166186, 0.241853542395015, 
0.00475798052839473, 0.241560847632903, 0.241863348389759, 0.240621151837141, 
0.117813191781167, 0.00583442855462709, 0.242383504228829, 0.245750031354992, 
0.243001233240777, 0.241578005300891, 0.234048697194414, 0.246868788738045
)

## estimation year1
estyear1 <- estimIS(fyda1, r=20, theta=0.175, detct=prop,
                    nCT=100, duration =30, hoursPerDay=15)

## estimation year5
estyear5 <- estimIS(fyda5, r=20, theta=0.175, detct=prop,
                    nCT=100, duration =30, hoursPerDay=15)

## estimation CV year1
byear1 <- bootstrapIS(fyda1, Nboot=1000, r=20, theta=0.175, detct=prop,
                      hoursPerDay = 15, nCT = 100)
cvyear1 <- sd(byear1)/mean(byear1)


## estimation CV year5
byear5 <- bootstrapIS(fyda5, Nboot=1000, r=20, theta=0.175, detct=prop,
                      hoursPerDay = 15, nCT = 100)
cvyear5 <- sd(byear5)/mean(byear5)



## ---------------------------------------------------------
## Year 1
estyear1
## CV year 1:
cvyear1

## Year 5
estyear5
## CV year 5:
cvyear5


## ---------------------------------------------------------
(estyear5-estyear1)/estyear1


## ---------------------------------------------------------
brc <- bootstrapIScompar(fyda1, fyda5, Nboot=1000, r=20, theta=0.175,
                         detct=prop, hoursPerDay=15, 
                         nCT=100)


## ---------------------------------------------------------
quantile(brc, c(0.05,0.95))


## ---------------------------------------------------------
## mean active speed
dra1 <-  mean(sim4iter$activeSpeed[[1]][[1]])
dra5 <-  mean(sim4iter$activeSpeed[[1]][[5]])

(esty1 <- estimRem(fyda1, dra1, 20, 0.175,
                   Pdetect=pdetect, duration=30,
                   nCT=100, hoursPerDay=15))


(esty5 <- estimRem(fyda5, dra5, 20, 0.175,
                   Pdetect=pdetect, duration=30,
                   nCT=100, hoursPerDay=15))


## ---------------------------------------------------------
(esty5-esty1)/esty1


## ---------------------------------------------------------

brcr <- bootstrapREMcompar(fyda1, fyda5, Nboot=1000, dra1, dra5,
                           r=20, theta=0.175, hoursPerDay=15,
                           nCT=100,
                           detection=TRUE)

quantile(brcr, c(0.05,0.95))


## ---------------------------------------------------------
dfPopSize


## ---------------------------------------------------------
exp(cov(log(dfPopSize$Ntot), 1:5)/var(1:5))


## ---------------------------------------------------------
1-exp(cov(log(dfPopSize$Ntot), 1:5)/var(1:5))


## ---------------------------------------------------------
listencountersAll <- lapply(sim4iter$encounters[[1]]$results,
                            function(x) do.call(rbind,x))


## ---------------------------------------------------------
listencounters <- lapply(listencountersAll, function(x)
                         removeHour(x, 8, 17))


## ----eval=FALSE-------------------------------------------
## ## Common elements for the five years:
## ## Camera traps detection probabilities
## habitat4sim <- sim4iter$encounters[[1]]$indicetraps$hab
## set.seed(sim4iter$encounters[[1]]$creationCT[2])
## list100CT <- lapply(1:100, function(j) {
##     cat("Traps: ", j,"\r")
##     organiseCT(habitat4sim[j])
## })
## prop <- sapply(1:length(list100CT), function(trap)
##     mean(1-exp(-list100CT[[trap]]$vu)))


## ---------------------------------------------------------
(tr <- estimTrendIS(listencounters, r=20, theta=0.175, detct=prop,
                    hoursPerDay=15))


## ----eval=FALSE-------------------------------------------
## set.seed(777) ## for reproducibility
## 
## bos <- bootstrapTrendIS(listencounters, r=20, theta=0.175, detct=prop,
##                         hoursPerDay=15)


## ----echo=FALSE-------------------------------------------
bos <- structure(c(277.742506633634, 281.591575099333, 278.250051230315, 
305.305555698905, 347.342318858646, 250.398036544113, 256.120416845068, 
264.349156966294, 314.366046320109, 290.64935219225, 393.068134648743, 
244.2739820423, 256.170154742235, 287.567821019956, 223.87767496715, 
145.370178548783, 184.971239877861, 215.047909525751, 214.094511312328, 
271.340994796892, 229.265179373546, 194.821773338296, 166.106477572835, 
161.108529154087, 222.281166238225, 349.125929632313, 302.166264567798, 
234.962079642861, 279.534211671766, 321.767885516748, -0.0605618415098449, 
0.0466614720168073, -0.0149633461140082, -0.0882423260416602, 
-0.121529207026278, 0.0085710256937428), dim = c(6L, 6L),
dimnames = list(
                                                              NULL, c("N[1]", "N[2]", "N[3]", "N[4]", "N[5]", "lambda")))
qutis <- structure(c(237.535094379687, 388.26108399264, 232.258397994517, 
426.690419307489, 146.10525090933, 270.830802486229, 143.32460498359, 
276.361504482177, 154.048424467334, 341.560884548941, -0.188478907125781, 
0.00816913277400306), dim = c(2L, 6L), dimnames = list(c("5%", 
"95%"), c("N[1]", "N[2]", "N[3]", "N[4]", "N[5]", "lambda")))



## ---------------------------------------------------------
head(bos)


## ----eval=FALSE-------------------------------------------
## qutis <- apply(bos,2,quantile, c(0.05, 0.95))


## ---------------------------------------------------------
qutis


## ---------------------------------------------------------
vectorSpeed <- sapply(sim4iter$activeSpeed[[1]], mean)
vectorSpeed


## ---------------------------------------------------------
estimTrendRem(listencounters, vectorSpeed, r=20, theta=0.175,
              hoursPerDay=15,
              Pdetect=mean(do.call(rbind,listencounters)$detection))


## ----eval=FALSE-------------------------------------------
## set.seed(777)
## bosr <- bootstrapTrendRem(listencounters, vectorSpeed, r=20,
##                           theta=0.175,
##                           hoursPerDay=15,
##                           Pdetect=mean(do.call(rbind,listencounters)$detection))
## citr <- apply(bosr,2,quantile,c(0.05,0.95))


## ----echo=FALSE-------------------------------------------
citr <- structure(c(229.189612195185, 356.70237825287, 196.660220336355, 
350.015057459868, 165.857184243103, 299.620880840938, 166.038554553337, 
309.209523749029, 151.665010868315, 282.704760530997, -0.155953026304002, 
0.0100588597009978), dim = c(2L, 6L), dimnames = list(c("5%", 
"95%"), c("N[1]", "N[2]", "N[3]", "N[4]", "N[5]", "lambda")))


## ---------------------------------------------------------
citr


## ----eval=FALSE-------------------------------------------
## pro <- processCTStudy(sim4iter, method="REM",
##                       detectability=TRUE, active=TRUE)


## ----echo=FALSE-------------------------------------------
pro <- list(structure(list(PtEst = c(291.6958700666, 272.113529363365, 
228.189246943455, 232.845259041381, 215.989859835811, -0.0728876999640709, 
-0.259537477213866), EstSE = c(40.4322017582373, 46.1726487695667, 
39.7705597896134, 41.8257472970153, 37.7453809837556, 0.0509589599734693, 
0.133314681480731), EstCIl = c(227.481107813367, 203.041643812575, 
163.092314533205, 168.700559580945, 156.356593889339, -0.157967373683628, 
-0.526702345973477), EstCIu = c(359.244299406308, 357.199175712971, 
296.786014429524, 306.002288776007, 277.954163887068, 0.0145953709147772, 
-0.103042769585941)), class = "data.frame", row.names = c("N1", 
"N2", "N3", "N4", "N5", "lambda", "changeRate")), structure(list(
    PtEst = c(242.961991627424, 314.033037856735, 270.365326645342, 
    242.345457281065, 180.555375868244, -0.0817511962608412, 
    -0.256857524673565), EstSE = c(40.8900244579959, 56.9311705351532, 
    42.8706327069699, 37.1127543248915, 29.2447707612931, 0.0441424529600008, 
    0.137068202191315), EstCIl = c(182.183050367462, 227.009237205818, 
    205.324611273114, 184.947848977655, 137.259478785045, -0.154282402547535, 
    -0.481659603746628), EstCIu = c(317.542096652297, 414.674518295718, 
    349.434431607659, 302.670923381771, 231.329897987406, -0.00788898738027996, 
    -0.0317430006758246)), class = "data.frame", row.names = c("N1", 
"N2", "N3", "N4", "N5", "lambda", "changeRate")), structure(list(
    PtEst = c(336.487271285668, 270.752797753506, 230.474227647653, 
    233.619875049223, 265.2364394057, -0.0604355130850734, -0.211748966336019
    ), EstSE = c(61.1206758828635, 49.2523732159654, 38.9096980051464, 
    47.458978860263, 53.4623839891058, 0.0479775668283885, 0.174129841366384
    ), EstCIl = c(240.064222818003, 194.712428676827, 169.10957260834, 
    157.836643102057, 184.534891425233, -0.135917629502052, -0.458734705672633
    ), EstCIu = c(439.673882995255, 352.295307018451, 293.337321937926, 
    311.280977926621, 356.938577038938, 0.0175244250106354, 0.113703605159715
    )), class = "data.frame", row.names = c("N1", "N2", "N3", 
"N4", "N5", "lambda", "changeRate")), structure(list(PtEst = c(414.767689989144, 
302.924481036879, 337.475304616524, 230.13749944634, 192.650790145759, 
-0.165438248035812, -0.535521221166478), EstSE = c(73.7306689800021, 
52.5054234674358, 64.9532892150813, 52.7983052330569, 44.4165005847991, 
0.0568849261471192, 0.189066979764751), EstCIl = c(301.555665426351, 
219.426066392098, 229.384192379936, 147.287999645658, 121.377054597328, 
-0.266075014937626, -0.594004948012917), EstCIu = c(537.293976572786, 
390.35508846435, 445.607673002821, 320.658249228568, 266.782531921627, 
-0.0773965554491518, 0.0137405588545651)), class = "data.frame", row.names = c("N1", 
"N2", "N3", "N4", "N5", "lambda", "changeRate")))


## ---------------------------------------------------------
pro


## ----eval=FALSE-------------------------------------------
## simrd5nohs <- simulateCTStudy5years(nCT = 100, duration = 30,
##                                     niter = 1000, dfPopSize,
##                                     listPatches, contourChize, contourN=northChize,
##                                     contourS=southChize,
##                                     dff=relationAnimalSigmaMaxd, habitatMap,
##                                     listptg=mvtMatrices$listMatBetween,
##                                     ptp=mvtMatrices$MatWithin, nofi = 0, backup = TRUE)


## ----eval=FALSE-------------------------------------------
## roeNoHS <- process_all(simrd5nohs)


## ---------------------------------------------------------
str(roeNoHS,1)


## ---------------------------------------------------------
oldopt <- options(width = 100) ## to allow a cleaner display of the results

present_results_CT(roeNoHS, dfPopSize)

options(oldopt)


## ----eval=FALSE-------------------------------------------
## simrd5nohs25 <- sampleCts(simrd5nohs, 25)


## ----eval=FALSE-------------------------------------------
## roeNoHS25 <- process_all(simrd5nohs25)


## ---------------------------------------------------------
oldopt <- options(width = 100)

present_results_CT(roeNoHS25, dfPopSize)

options(oldopt)


## ---------------------------------------------------------
plot(pathsMap)


## ---------------------------------------------------------
## Chize map
chizeMap <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize))))

## Erode this map by 600 m
erodedMap <- sf::st_buffer(chizeMap, -600)

## Intersects with areas located < 20 m from paths
sf::st_crs(pathsBuffer) <- sf::st_crs(erodedMap)
corePaths <- sf::st_intersection(st_geometry(pathsBuffer),
                                 st_geometry(erodedMap))

## Plot the result
plot(sf::st_geometry(chizeMap), col="grey")
plot(corePaths, col="red", add=TRUE)

## A random centroid will be randomly drawn from the maps in
## corePaths and patches will be drawn from pathsBuffer
set.seed(777) ## for reproducibility
(locs <- HSlocation(corePaths, pathsBuffer))


## add these points on the map:
points(sweep(locs$patchs, 2, locs$centroide, "+"), pch=21,
       bg="yellow")
points(locs$centroide[1], locs$centroide[2], pch=3, cex=2, lwd=4, col="blue")
points(locs$centroide[1], locs$centroide[2], pch=3, cex=2, lwd=2, col="green")


## ----eval=FALSE-------------------------------------------
## simrd5withhs <- simulateCTStudy5years(nCT = 100, duration = 30,
##                                       niter = 500, dfPopSize,
##                                       listPatches, contourChize, contourN=northChize,
##                                       contourS=southChize,
##                                       dff=relationAnimalSigmaMaxd, habitatMap,
##                                       listptg=mvtMatrices$listMatBetween,
##                                       ptp=mvtMatrices$MatWithin, stratSampling=TRUE,
##                                       propPaths=1, pathsMap=pathsMap,
##                                       pathsBuffer=pathsBuffer, offPathsMap=offPathsMap,
##                                       offPathsBuffer=offPathsBuffer,
##                                       habitatSelection=TRUE,
##                                       nofi = 0, backup = TRUE)


## ----eval=FALSE-------------------------------------------
## ## we use the function compute_all written above:
## roeHSBiasedSam <- process_all(simrdwithhs)


## ---------------------------------------------------------
trend1 <- exp(coef(lm(log(dfPopSize[,1])~c(1:5)))[2])-1
nam <- c("REM_raw","REM_det","REM_det_act",
         "IS_raw","IS_det","IS_det_act")
sel <- 6

oldopt <- options(width = 100)

do.call(rbind,lapply(1:length(roeHSBiasedSam), function(i) {
    x <- roeHSBiasedSam[[i]]
    method <- nam[i]
    MeanEst <- mean(sapply(x, function(y) y[sel,1]), na.rm=TRUE)
    SE <- sd(sapply(x, function(y) y[sel,1]), na.rm=TRUE)
    SEEst <- mean(sapply(x, function(y) y[sel,2]), na.rm=TRUE)
    SE.SEEst <- sd(sapply(x, function(y) y[sel,2]), na.rm=TRUE)
    pcover <- mean(sapply(x, function(y)
        trend1>=y[sel,3]&trend1<=y[sel,4]))
    
    Psigla <- mean(sapply(x, function(y) y[6,4]<0))
    PDecla <- mean(sapply(x, function(y) y[6,1]<0))
    
    data.frame(method=method,
               MeanEst=round(MeanEst,3),
               SE=round(SE,3),
               SEEst=round(SEEst,3), SE.SEEst=round(SE.SEEst,3), Pcov=round(pcover,3),
               PDec=round(PDecla,3),
               PsigD=round(Psigla,3))
}))

options(oldopt)


## ----eval=FALSE-------------------------------------------
## ## Number of simulated animals
## ntotani <- 1000
## 
## 
## ## Required for the simulation: the centroid of animals is located at
## ## at least 600 m from the boundary of the study area (to avoid
## ## movement outside the study area).
## cartec <- sf::st_sfc(sf::st_polygon(list(as.matrix(contourChize))))
## sf::st_crs(cartec) <- sf::st_crs(pathsBuffer)
## cartec <- sf::st_buffer(cartec, -600)
## 
## ## We then intersected the map of the paths neighborhood with this
## ## "eroded map"
## corePaths <- sf::st_intersection(pathsBuffer, cartec)
## po1 <- sf::st_sfc(sf::st_polygon(list(as.matrix(northChize))))
## sf::st_crs(po1) <- sf::st_crs(corePaths)
## 
## ## Because 193/300 animals were located in the northern part, we
## ## intersected this paths map with the limits of the north an south
## ## of Chize
## northPaths <- sf::st_intersection(corePaths, po1)
## po1 <- sf::st_sfc(sf::st_polygon(list(as.matrix(southChize))))
## sf::st_crs(po1) <- sf::st_crs(corePaths)
## southPaths <- sf::st_intersection(corePaths, po1)
## 
## ## Parameters of movement to simulate the increase in home range size
## ## (through the use of sigma) and the habitat selection (through
## ## HSlocation)
## paramani <- lapply(1:ntotani, function(i) {
##     cat(i,"\r")
##     north <- (runif(1) < 193/300)
##     minsigan <- round(4 * (1:5)/5)
##     maxsigan <- round(3 + 7 * (0:4)/4)
##     sigmaan <- sapply(1:5, function(t) sample(minsigan[t]:maxsigan[t],
##                                               1))
##     zone <- southPaths
##     if (north)
##         zone <- northPaths
##     lxyz <- HSlocation(zone, pathsBuffer)
##     return(list(lxyz = lxyz, sigmaan = sigmaan))
## })
## 
## ## Simulation of the movements of the 1000 animals +
## ## calculation of the proportion of relocations in paths neighborhood.
## propTimePathsHS <- matrix(0, nrow=1000, ncol=5)
## for (iAni in 1:ntotani) {
##     cat(iAni,"\r")
##     for (year in 1:5) {
## 
##         ## Simulate the movement
##         paAni <- paramani[[iAni]]
##         lipt <- list(ptg=mvtMatrices$listMatBetween[[paAni$sigmaan[year]]],
##                      ptp=mvtMatrices$MatWithin)
##         lixyt <- list(paAni$lxyz$patchs, paAni$lxyz$patchs)
##         z <- roeDeerMovement(30, lixyt, lipt = lipt,
##                              verbose = FALSE)
##         z <- t(t(z)+as.vector(paAni$lxyz$centroide))
## 
##         ## To speed up the calculation, consider one reloc every 1000
##         ## seconds
##         z <- z[seq(1,nrow(z), by=1000),]
## 
##         ## Join with the map of paths Buffer, and calculate the proportion of
##         ## relocations that are in this habitat type
##         z <- as.data.frame(z)
##         jo <- st_join(st_as_sf(z,coords=c("x","y"), crs=st_crs(pathsBuffer)),
##                       pathsBuffer)
##         propTimePathsHS[iAni, year] <- mean(!is.na(jo$id))
##     }
## }
## 


## ---------------------------------------------------------
moa <- 100*apply(propTimePathsHS,2,mean)
soa <- 100*apply(propTimePathsHS,2,mean)/sqrt(1000)

plot(1:5, moa, ylim=range(c(moa+soa, moa-soa)),
     xlab="Year",ylab="Percentage of time in paths neighborhood",
     ty="b")
arrows(1:5, moa-soa, 1:5, moa+soa, angle=90,
       length=0.05, code=3)


## ---------------------------------------------------------
## moa and soa as a proportion
moap <- moa/100
soap <- soa/100

## Real number in each habitat type
ht <- c(300,274,266,246,239)*moap
## variance of \bar{ht}
voa <- soap^2

## variance of log(ht) with the delta-method
vh <- voa/(moap^2)
    
## Weighted regression
su <- summary(lm(log(ht)~c(1:5), weight=1/vh))
cothb <- exp(round(su$coefficients[2,1],2))-1
## standard error
scothb <- round(sqrt(((su$coefficients[2,2])^2)*
                     exp(2*exp(round(su$coefficients[2,1],2)))),2)


## slope:
cothb

## standard error
scothb


## ----eval=FALSE-------------------------------------------
## listSimulated <- list()
## propsim <- c(0.25,0.5,0.75)
## 
## for (i in 1:3) {
##     listSimulated[[i]] <- simulateCTStudy5years(nCT = 100, duration = 30,
##                                                 niter = 500, dfPopSize,
##                                                 listPatches, contourChize,
##                                                 contourN=northChize,
##                                                 contourS=southChize,
##                                                 dff=relationAnimalSigmaMaxd,
##                                                 habitatMap=habitatMap,
##                                                 listptg=mvtMatrices$listMatBetween,
##                                                 ptp=mvtMatrices$MatWithin,
##                                                 stratSampling=TRUE,
##                                                 propPaths=propsim[i],
##                                                 pathsMap=pathsMap,
##                                                 pathsBuffer=pathsBuffer,
##                                                 offPathsMap=offPathsMap,
##                                                 offPathsBuffer=offPathsBuffer,
##                                                 habitatSelection=TRUE,
##                                                 nofi = 0, backup = TRUE)
## }
## 


## ----eval=FALSE-------------------------------------------
## roeStratSam <- lapply(listSimulated, function(x) {
##     process_all(x,stratified=TRUE, pathsBuffer=pathsBuffer)
## })


## ---------------------------------------------------------
oldopt <- options(width = 100)
do.call(rbind,
        lapply(1:3,
               function(i) {
                  
            df1 <- do.call(rbind,
                           lapply(1:6,
                                  function(j) {
                               data.frame(
                                   p = c(0.25, 0.5, 0.75)[i],
                                   Esp.Dech=round(mean(sapply(roeStratSam[[i]][[j]],
                                                              function(x) x[1,1])),1),
                                   SE.Dech=round(sd(sapply(roeStratSam[[i]][[j]],
                                                           function(x) x[1,1])),1),
                                   Couv.IC=round(mean(sapply(roeStratSam[[i]][[j]],
                                                             function(x)
                                   (x[1,3]<300)&(x[1,4]>300))),3),
                                   Estim.SE=round(mean(sapply(roeStratSam[[i]][[j]],
                                                              function(x) x[1,2]),
                                                       na.rm=TRUE),1),
                                   SE.Estim.SE=round(sd(sapply(roeStratSam[[i]][[j]],
                                                               function(x) x[1,2]),
                                                        na.rm=TRUE),1)
                               )
                           }))
            df1$Method <- c("REM_raw","REM_det","REM_det_act",
                            "IS_raw","IS_det","IS_det_act")
            df1 <- df1[,c(1,7,2:6)]
            if (i==3)
                return(df1)
            return(rbind(df1,
                         data.frame(p="",Method="",Esp.Dech="",SE.Dech="",
                                    Couv.IC="",Estim.SE="",SE.Estim.SE="")))
            
        }))
options(oldopt)


## ---------------------------------------------------------
oldopt <- options(width = 100)
do.call(rbind,
        lapply(1:3,
               function(i) {
                  
            df1 <- do.call(rbind,
                           lapply(1:6,
                                  function(j) {
                               data.frame(
                                   p = c(0.25, 0.5, 0.75)[i],
                                   Esp.Dech=round(mean(sapply(roeStratSam[[i]][[j]],
                                                              function(x) x[6,1])),3),
                                   SE.Dech=round(sd(sapply(roeStratSam[[i]][[j]],
                                                           function(x) x[6,1])),4),
                                   Couv.IC=round(mean(sapply(roeStratSam[[i]][[j]],
                                                             function(x)
                                   (x[6,3]< -0.054)&(x[6,4]>-0.054))),3),
                                   Estim.SE=round(mean(sapply(roeStratSam[[i]][[j]],
                                                              function(x) x[6,2]),
                                                       na.rm=TRUE),3),
                                   SE.Estim.SE=round(sd(sapply(roeStratSam[[i]][[j]],
                                                               function(x) x[6,2]),
                                                        na.rm=TRUE),3)
                               )
                           }))
            df1$Method <- c("REM_raw","REM_det","REM_det_act",
                            "IS_raw","IS_det","IS_det_act")
            df1 <- df1[,c(1,7,2:6)]

            if (i==3)
                return(df1)
            return(rbind(df1,
                         data.frame(p="",Method="", Esp.Dech="",SE.Dech="",
                                    Couv.IC="",Estim.SE="",SE.Estim.SE="")))
        }))
options(oldopt)


## ----eval=FALSE-------------------------------------------
## simrd5withhs0 <- simulateCTStudy5years(nCT = 100, duration = 30,
##                                        niter = 500, dfPopSize,
##                                        listPatches, contourChize,
##                                        contourN=northChize,
##                                        contourS=southChize,
##                                        dff=relationAnimalSigmaMaxd, habitatMap,
##                                        listptg=mvtMatrices$listMatBetween,
##                                        ptp=mvtMatrices$MatWithin,
##                                        stratSampling=TRUE,
##                                        propPaths=0, pathsMap=pathsMap,
##                                        pathsBuffer=pathsBuffer,
##                                        offPathsMap=offPathsMap,
##                                        offPathsBuffer=offPathsBuffer,
##                                        habitatSelection=TRUE,
##                                        nofi = 0, backup = TRUE)
## 


## ----eval=FALSE-------------------------------------------
## prop0 <- lapply(1:500, function(j) {
##     cat(j,"\r")
##     habitat4sim <- simrd5withhs0$encounters[[j]]$indicetraps$hab
##     set.seed(simrd5withhs0$encounters[[j]]$creationCT[2])
##     list100CT <- lapply(1:100, function(k) {
##         organiseCT(habitat4sim[k])
##     })
##     sapply(1:length(list100CT),
##            function(trap)
##         mean(1 -
##              exp(-list100CT[[trap]]$vu)))
## })
## 
## prop1 <- lapply(1:500, function(j) {
##     cat(j,"\r")
##     habitat4sim <- simrd5withhs$encounters[[j]]$indicetraps$hab
##     set.seed(simrd5withhs$encounters[[j]]$creationCT[2])
##     list100CT <- lapply(1:100, function(k) {
##         organiseCT(habitat4sim[k])
##     })
##     sapply(1:length(list100CT),
##            function(trap)
##         mean(1 -
##              exp(-list100CT[[trap]]$vu)))
## })


## ----eval=FALSE-------------------------------------------
## liche <- list()
## lipas <- list()
## litot <- list()
## 
## for (i in 1:length(pro)) {
## 
##     cat("#############################\n## Iteration",i, "\n\n")
##     cat("## rem_raw\n")
##     reb <- sampleCTs(simrd5withhs, pro[i])
##     rec <- sampleCTs(simrd5withhs0, 100-pro[i])
## 
##     ## REM_raw:
##     rem_raw_che <- Remhab(reb, paths=TRUE, detection=FALSE, active=FALSE)
##     rem_raw_pas <- Remhab(rec, paths=FALSE, detection=FALSE, active=FALSE)
##     rem_raw <- rem_raw_che+rem_raw_pas
## 
##     ## REM_detect:
##     cat("## rem_det\n")
##     rem_det_che <- Remhab(reb, paths=TRUE, detection=TRUE, active=FALSE)
##     rem_det_pas <- Remhab(rec, paths=FALSE, detection=TRUE, active=FALSE)
##     rem_det <- rem_det_che+rem_det_pas
## 
##     ## REM_detect_activ
##     cat("## rem_det_act\n")
##     rem_det_act_che <- Remhab(reb, paths=TRUE, detection=TRUE, active=TRUE)
##     rem_det_act_pas <- Remhab(rec, paths=FALSE, detection=TRUE, active=TRUE)
##     rem_det_act <- rem_det_act_che+rem_det_act_pas
## 
## 
##     ## IS_raw:
##     cat("## is_raw\n")
##     is_raw_che <- IShab(reb, paths=TRUE, detection=FALSE, active=FALSE, prop=prop1)
##     is_raw_pas <- IShab(rec, paths=FALSE, detection=FALSE, active=FALSE, prop=prop0)
##     is_raw <- is_raw_che+is_raw_pas
## 
##     ## IS_detect:
##     cat("## is_det\n")
##     is_det_che <- IShab(reb, paths=TRUE, detection=TRUE, active=FALSE, prop=prop1)
##     is_det_pas <- IShab(rec, paths=FALSE, detection=TRUE, active=FALSE, prop=prop0)
##     is_det <- is_det_che+is_det_pas
## 
##     ## IS_detect_activ
##     cat("## is_det_act\n")
##     is_det_act_che <- IShab(reb, paths=TRUE, detection=TRUE, active=TRUE, prop=prop1)
##     is_det_act_pas <- IShab(rec, paths=FALSE, detection=TRUE, active=TRUE, prop=prop0)
##     is_det_act <- is_det_act_che+is_det_act_pas
## 
##     ##
##     liche[[i]] <- data.frame(rem_raw=rem_raw_che, rem_det=rem_det_che,
##                              rem_det_act=rem_det_act_che,
##                              is_raw=is_raw_che, is_det=is_det_che,
##                              is_det_act=is_det_act_che)
##     lipas[[i]] <- data.frame(rem_raw=rem_raw_pas, rem_det=rem_det_pas,
##                              rem_det_act=rem_det_act_pas,
##                              is_raw=is_raw_pas, is_det=is_det_pas,
##                              is_det_act=is_det_act_pas)
## 
##     litot[[i]] <- data.frame(rem_raw=rem_raw, rem_det=rem_det,
##                              rem_det_act=rem_det_act,
##                              is_raw=is_raw, is_det=is_det,
##                              is_det_act=is_det_act)
## }
## 
## stratData <- list(inPaths=liche, offPaths=lipas, all=litot)


## ----fig.width=10, fig.height=10, out.width='\\linewidth', out.height='\\linewidth'----
par(mfrow=c(3,2))
un <- unique(sapply(strsplit(names(stratData$all[[1]]),"\\."),function(x) x[1]))
pro <- seq(5,95,by=5)
tmp <- lapply(1:6, function(j) {
    cv <- sapply(1:19, function(i) {
        o <- stratData$all[[i]][,paste0(un[j],".1")]
        sd(o,na.rm=TRUE)/mean(o,na.rm=TRUE)
        })
    plot(pro/100,cv, ty="b",
         xlab="Proportion of traps close to paths",
        ylab="Coefficient of variation",
        main=un[j], ylim=c(0.15,0.5))
    abline(h=seq(0.15,0.5, by=0.1),
           v=seq(0.05,0.95,by=0.1), col="grey")
    points(pro/100, cv, ty="b")
    })



## ---------------------------------------------------------
simulate_red_deer <- function()
{
    ## between 5 ànd 10 patches
    np <- sample(5:10, 1)
    ## the patches:
    cb <- cbind(runif(np,-2000,2000),
                runif(np,-2000,2000))
    ## simulation
    lipt <- list(ptg=mvtMatrices$listMatBetween[[10]],
                 ptp=mvtMatrices$listMatBetween[[10]])
    lixyt <- list(cb, cb)
    z <- roeDeerMovement(30, lixyt, lipt = lipt, 
                         verbose = FALSE)

    return(z)
}

plot(simulate_red_deer(), ty="l")


## ---------------------------------------------------------
set.seed(777)
li <- list()
for (k in 1:1000) {
    N <- 300
    nfem <- N/2
    nmal <- N/2
    nsurf  <- nsurm <- njfem <- njmal <- numeric(0)

    ## For each year
    for (i in 2:5) {

        ## Survival of previous year
        nsurf[i-1] <- rbinom(1,nfem[i-1],0.76)
        nsurm[i-1] <- rbinom(1,nmal[i-1],0.76)

        ## 0.5 young per female * sex ratio of 0.5 = 0.25
        njfem[i-1] <- rbinom(1,nsurf[i-1],0.25)
        njmal[i-1] <- rbinom(1,nsurf[i-1],0.25)

        ## Number of females = number surviving from previous year
        ## + new females
        nfem[i] <- nsurf[i-1]+njfem[i-1]
        ## same for males
        nmal[i] <- nsurm[i-1]+njmal[i-1]
    }
    N <- nfem+nmal
    li[[k]] <- data.frame(Ntot=N, Old=c(0,nsurm+nsurf), New=c(0,njfem+njmal))
}
liN <- do.call(rbind,lapply(li, function(x) x$Ntot))

## Repeated 1000 times, we select a case resulting in 240 animals
## during the last year:
whi <- c(1:nrow(liN))[liN[,5]==240][
    which.min(apply(apply(liN[liN[,5]==240,],1,diff),2,var))]

(dfPopSizeRedDeer <- li[[whi]])


## ----eval=FALSE-------------------------------------------
## simrd5nohsrd <- simulateCTStudy5years(nCT = 100, duration = 30,
##                                       niter = 1000,
##                                       dfPopSizeRedDeer,
##                                       listPatches, contourChize,
##                                       contourN=northChize,
##                                       contourS=southChize,
##                                       dff=relationAnimalSigmaMaxd,
##                                       habitatMap=habitatMap,
##                                       listptg=mvtMatrices$listMatBetween,
##                                       ptp=mvtMatrices$MatWithin,
##                                       redDeer=TRUE, nofi = 0,
##                                       backup = TRUE)


## ----eval=FALSE-------------------------------------------
## redNoHS <- process_all(simrd5nohsrd)


## ---------------------------------------------------------
oldopt <- options(width = 100)
present_results_CT(redNoHS, dfPopSizeRedDeer)
options(oldopt)


## ----eval=FALSE-------------------------------------------
## simrd5nohs25rd <- sampleCts(simrd5nohsrd, 25)


## ----eval=FALSE-------------------------------------------
## redNoHS25 <- process_all(simrd5nohs25rd)


## ---------------------------------------------------------
oldopt <- options(width = 100)
present_results_CT(redNoHS25, dfPopSizeRedDeer)
options(oldopt)



## ----eval=FALSE-------------------------------------------
## simrd5nohs735 <- simulateCTStudy5years(nCT = 100, duration = 30,
##                                        niter = 500, dfPopSize,
##                                        listPatches, contourChize, contourN=northChize,
##                                        contourS=southChize,
##                                        dff=relationAnimalSigmaMaxd, habitatMap,
##                                        listptg=mvtMatrices$listMatBetween,
##                                        ptp=mvtMatrices$MatWithin,
##                                        fieldangle=0.735, depth=20,
##                                        nofi = 0, backup = TRUE)


## ----eval=FALSE-------------------------------------------
## roeNoHS735 <- process_all(simrd5nohs735)


## ---------------------------------------------------------
str(roeNoHS735,1)


## ---------------------------------------------------------
oldopt <- options(width = 100) ## to allow a cleaner display of the results

present_results_CT(roeNoHS735, dfPopSize)

options(oldopt)


## ---------------------------------------------------------
oldopt <- options(width = 100) ## to allow a cleaner display of the results

present_results_CT(roeNoHS, dfPopSize)

options(oldopt)


## ---------------------------------------------------------
u <- sapply(roeNoHS$IS_raw, function(x) log(x[1:5,1]))
sd1 <- apply(u,1,var)
sd2 <- apply(sapply(roeHSBiasedSam$IS_raw, function(x) log(x[1:5,1])),1,var)
dv <- data.frame(year=paste0("Year ", 1:5),
                 Variance1=sd1,
                 Variance2=sd2)
names(dv) <- c("Year","Without habitat selection","With habitat selection")
dv

