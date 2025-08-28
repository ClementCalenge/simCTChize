discretize <-
function(lcl, length=1000)
{
    n <- nrow(lcl)
    nnew <- 10
    nn <- nnew*n
    toto <- .C("discretrajr", as.double(lcl[,1]), as.double(lcl[,2]),
               as.double(1:nrow(lcl)), double(nn), double(nn), as.integer(n),
               as.integer(nn), double(nn), as.double(lcl[1,1]),
               as.double(lcl[1,2]), as.double(length), as.double(1),
               integer(1), PACKAGE = "simCTChize")
    xydis <- do.call(cbind,toto[4:5])[1:(toto[[13]]-1),]
    return(xydis)
}
