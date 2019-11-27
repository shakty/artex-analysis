## By Chris: used Franks function but changed "print results" to "return vcov"
## clustered heteroskedastic robust SE's
## from http://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/
## 2012-10-09: removed "dat" from function list;
## IMHO it is not used at all; did not adjust code in other Gary*** files

library(sandwich)

getVcovCL <- function(fm, cluster){
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj  <- apply(estfun(fm), 2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    return(vcovCL) 
}


getVcovCLAdj   <- function(fm, cluster){
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    ## I'm not 100% sure if this is correct: magnifying the residuals
    ## in equation (3) by a factor sqrt(G/(G 1) )
    ## so I'm just multiplying estfun() with 
    uj  <- apply(estfun(fm) *sqrt(N/(N-1)), 2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    return(vcovCL) 
}


originalCl <- function(dat,fm, cluster){
    attach(dat, warn.conflicts = F)
    library(sandwich)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    coeftest(fm, vcovCL)
}
