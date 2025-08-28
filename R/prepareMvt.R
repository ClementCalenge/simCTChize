prepareMvt <-
function(tau=c(50,30), sigma=10)
{
    MODEL <- ctmm::ctmm( tau=tau,sigma=sigma,mu=c(0,0), isotropic=TRUE)
    SIM2 <- ctmm::simulate(MODEL,t=1:5, precompute=TRUE)
    S <- get("Sigma", envir=ctmm:::Kalman.env)
    G <- get("Green", envir=ctmm:::Kalman.env)
    L <- get("Lambda", envir=ctmm:::Kalman.env)
    res <- list(S=S, G=G, L=L)
    class(res) <- "pretraj"
    return(res)
}
