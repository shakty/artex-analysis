####################################################
## Helper Function: Fit hard to converge glmer models.
## 
## Sometimes, some hard to fit glmer models do not converge properly. This code adds some robustness to make convergence more likely.
##
## Code from: http://stats.stackexchange.com/questions/158003/glmer-error-pwrssupdate-did-not-converge-in-30-iterations
## Other useful code at: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
##
## Version log
##			2016-XX-XX Created initial code; reading up on the material
##			2017-07-20 Packaged this up in a function for easier use.
####################################################

glmerSpecial <- function(formula, data, family=Gamma(link="log")) {
	## 1. set up model terms
	glmod <- glFormula(formula, data=data, family=family)

	## 1A. adjust maxit.
	glmod <- c(glmod, list(maxit=200))

	## 2.  Create the deviance function for optimizing over theta:
	devfun <- do.call(mkGlmerDevfun, glmod)

	## 3.  Optimize over theta using a rough approximation (i.e. nAGQ = 0):
	opt <- optimizeGlmer(devfun)

	## 4.  Update the deviance function for optimizing over theta and beta:
	devfun <- updateGlmerDevfun(devfun, glmod$reTrms)

	## 5.  Optimize over theta and beta:
	opt <- optimizeGlmer(devfun, stage=2)
	
	## 6.  Return result
	mkMerMod(environment(devfun), opt, glmod$reTrms, fr = glmod$fr)	
}

