# pull out the necessary internal functions from greta
op <- .internals$nodes$constructors$op
#' @importFrom  stats var median quantile
#' @import greta
#' 
#' @title compute the (normalized) root mean squared error for a greta regression model
#' @export
#' 
#' @description Compute the root mean squared error of a greta model
#' 
#' @param y a greta array, the response variables
#' @param pred a greta array, the linear predictor
#' @param draws a greta_mcmc_list object, posterior draws as returned from calling greta sampling algorithm (ie mcmc)
#' @param summary a logical, if TRUE (default) the function output summary statistics (mean, sd, 80% credible intervals) for the R2, if FALSE the raw values are returned
#' @param probs a vector of two numeric specifying the lower and upper limits for the credible intervals (default to 0.1, 0.9), only used if summary=TRUE
#' @param norm a logical, whether to normalize the RMSE by the mean of the response variable
#'
#' @return If summary=TRUE a 1 x C matrix is returned (C = length(probs) + 2) containing summary statistics of Bayesian R-squared values. If summary = FALSE the posterior samples of the R-squared values are returned as a numeric vector of length S (S is the number of samples)
#'
#' @details 
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
#' distribution(y) <- normal(pred, sd_resid)
#' 
#' m <- model(intercept, slope, sd_resid)
#' drr <- mcmc(m)
#' 
#' rmse(y, pred, drr)
#' }


rmse <- function(y, pred, draws, summary = TRUE, probs = c(0.1, 0.9),
                 norm = FALSE){
  
  # get the posterior draws for the linear predictor
  ypred <- calculate(pred, values = draws)
  
  # posterior residuals
  e <- -1 * sweep(as.matrix(ypred), 2, as.matrix(y))
  
  # calculate per posterior draws root mean squared error or
  # a normalized version of it
  if(!norm){
    post_rmse <- apply(e, 1, function(x) sqrt(mean(x ** 2)))
  }
  else{
    post_rmse <- apply(e, 1, function(x) sqrt(mean(x ** 2)) / mean(as.numeric(y)))
  }
  
  
  if(summary){
    # get summary stats
    med <- median(post_rmse)
    mad <- median(abs(post_rmse - med))
    ci <- quantile(post_rmse, probs = probs)
    out <- matrix(c(med, mad, ci),ncol = 4,
                  dimnames = list("",
                                  c("median", "mad", "low-ci", "high-ci")))
    return(out)
    
  } else{
    return(post_rmse)
  }
}
  
  