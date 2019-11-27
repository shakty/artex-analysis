########################################################
## corstarsl - Print Econ Style Correlation table with mean, sd, min, max
##
## Change-Log
##		2012-XX-XX Initial version
##		2012-08-14 Added parameter controls for printing stars, mean, sd, and extremes
##		2013-03-19 Added parameter cnotrols for type of correlation; default is pearson
########################################################
corstarsl <- function(o, stars=TRUE, mean=TRUE, sd=TRUE, extremes=TRUE, method="pearson") {
  require(Hmisc)
  require(xtable)
  x <- as.matrix( o )
  R <- rcorr(x, type=method)$r
  p <- rcorr(x, type=method)$P
  
  ## define notions for significance levels; spacing is important.
  if (stars) {
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  } else {
    mystars <- ifelse(p < .001, " ", ifelse(p < .01, " ", ifelse(p < .05, " ", " ")))
  }
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  # rownames(Rnew) <- colnames(x)
  #rownames(Rnew) <- 1:length(colnames(x)) #paste(colnames(x), " ( )", sep="")
  #colnames(Rnew) <- paste(colnames(x), "", sep="")
  colnames(Rnew) <- paste("(", 1:length(colnames(x)), ")", sep="")
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  ## my Code: get means and add them to output matrix
  means  <- getAggregates(o, colMeans, c("Mean"))
  SD    <- sapply(o, sd, na.rm=TRUE)
  mins   <- getAggregates(o, min,   c("Min"))
  maxs   <- getAggregates(o, max,   c("Max"))
  
  #Rnew <- cbind(means, SD, mins, maxs, Rnew)
  if(extremes) {
    Rnew <- cbind(mins, maxs, Rnew)
  }
  if(sd) {
    Rnew <- cbind(SD, Rnew)
  }
  if(mean) {
    Rnew <- cbind(means, Rnew)
  }
  
  ## Add numbers to the rownames
  rownames(Rnew) <- paste(rownames(Rnew), " (", 1:length(rownames(Rnew)), ")", sep="")
  return(Rnew)
} 


getAggregates <- function(x, fun, label) {
  out <- c()
  for(i in 1:ncol(x)) {
    col <- round( as.matrix(fun(     x[i]    , na.rm=TRUE))[1], 3 )
    out <- rbind(out, col )
  }
  colnames(out) <- c(label)
  rownames(out) <- NULL
  return(out)
}
