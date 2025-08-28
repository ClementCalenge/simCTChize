trouveabo <-
function(x1=5, x2=18,angle=0.733)
{
    p1 <- 0.99
    p2 <- 0.01
    b <- (log(-log(1-p1))-log(-log(1-p2)))/(x1-x2)
    a <- log(-log(1-p2))-b*x2
    return(c(a,b))
}
