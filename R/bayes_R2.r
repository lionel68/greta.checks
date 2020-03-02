# an example of a greta function 

# pull out the necessary internal functions from greta
op <- .internals$nodes$constructors$op
#' @import stats
#' @import greta
#' 
#' @title compute the Bayesian R square for a greta regression model
#' @export
#' 
#' @description Compute a Bayesian version of R-square for regression models (GLMs ...)
#' 
#' @usage ## S3 method for class 'mcmc.list'
#'  bayes_R2(y, pred, draws, summary = TRUE, probs = c(0.1, 0.9))
#' 
#' @param y a greta array, the response variables
#' @param pred a greta array, the linear predictor
#' @param draws a mcmc.list, posterior draws as returned from calling greta sampling algorithm (ie mcmc)
#' @param summary a logical, if TRUE (default) the function output summary statistics (mean, sd, 80\% credible intervals) for the R2, if FALSE the raw values are returned
#' @param probs a vector of two numeric specifying the lower and upper limits for the credible intervals (default to 0.1, 0.9), only used if summary=TRUE
#'
#' @return If summary=TRUE a 1 x C matrix is returned (C = length(probs) + 2) containing summary statistics of Bayesian R-squared values. If summary = FALSE the posterior samples of the R-squared values are returned as a numeric vector of length S (S is the number of samples)
#'
#' @details See https://github.com/jgabry/bayes_R2/blob/master/bayes_R2.pdf for a description of the computation. Note that R2 only univariate models are supported. This function is largely inspired from brms::bayes_R2.
#' 
#' 
#' @examples
#' \dontrun{
#' intercept <- normal(0, 1)
#' slope <- normal(0, 1)
#' sd_resid <- cauchy(0, 1, truncation = c(0, 100))
#' 
#' x <- runif(100)
#' y <- as_data(rnorm(100, 1 + 2 * x, 1))
#' 
#' pred <- intercept + slope * x
#' m <- model(intercept, slope, sd_resid)
#' drr <- mcmc(m)
#' 
#' bayes_R2(y, pred, drr)
#' }


bayes_R2 <- function(y, pred, draws, summary = TRUE, probs = c(0.1, 0.9)){
  
  # test that correct objects are being passed
  ## to dev
  
  # get the posterior draws for the linear predictor
  ypred <- calculate(pred, draws)
  
  # posterior residuals
  e <- -1 * sweep(as.matrix(ypred), 2, as.matrix(y))
  
  # variance in linear predictors
  var_ypred <- apply(as.matrix(ypred), 1, var)
  
  # variance in residuals
  var_e <- apply(e, 1, var)
  
  # R2 values
  post_r2 <- var_ypred / (var_ypred + var_e)
  
  # if summary=FALSE return these values
  if(summary){
    # get summary stats
    med <- median(post_r2)
    mad <- median(abs(post_r2 - med))
    ci <- quantile(post_r2, probs = probs)
    out <- matrix(c(med, mad, ci),ncol = 4,
                  dimnames = list("",
                                  c("median", "mad", "low-ci", "high-ci")))
    return(out)
    
  } else{
    return(post_r2)
  }
}
