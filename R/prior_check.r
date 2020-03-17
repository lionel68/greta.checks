# prior predictive checks
# pull out the necessary internal functions from greta
op <- .internals$nodes$constructors$op

#' @import greta
#' 
#' @title Prior predictive checks for greta models
#' 
#' @description Perform prior predictive checks on a greta model object
#' 
#' @param y A greta array of the response variable
#' @param fun A character, the name of the function to apply to the simulated response values, default is "mean". Custom functions can also be passed, see examples.
#' @param probs A vector of two numeric, the lower and upper bound of the predictive interval
#' @param nsim A numeric, the number of simulation draws, default is 100
#' 
#' @return A character string of the form: XX of the nsim simulated response from the prior distributions had a fun value between XX and XX.
#' 
#' @details Prior predictive checks allow a better tuning of the prior distribution of the model parameters by checking simulated new draws of the response. For instance, if we want to model the speed of migratory birds, we do not expect the maximum value of simulated draws from the priors to be beyond 100 of km/h.
#' 
#' @usage prior_check(y, fun = "mean", probs = c(0.1, 0.9), nsim = 100)
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' # a simple lm example
#' intercept <- normal(0, 1)
#' slope <- normal(0, 1)
#' sd_resid <- cauchy(0, 1, truncation = c(0, 100))
#' x <- runif(100)
#' y <- as_data(rnorm(100, 1 + 2 * x, 1))
#' linpred <- intercept + slope * x
#' distribution(y) <- normal(linpred, sd_resid)
#' prior_check(y)
#' 
#' # can also use custom function, like counting number 
#' # of zero observations to check for zero-inflation
#' count0 <- function(x){
#' sum(x==0)
#' }
#' 
#' # a poisson regression
#' intercept <- normal(0, 1)
#' slope <- normal(0, 1)
#' x <- runif(100)
#' y <- as_data(rpois(100, exp(0.001 + 1 * x, 1))
#' linpred <- intercept + slope * x
#' distribution(y) <- poisson(linpred)
#' prior_check(y, fun = "count0")
#' 
#' }
#' 

prior_check <- function(y, fun = "mean",
                        probs = c(0.1, 0.9), nsim = 100){
  # get the summary functions to use
  sum_fun <- match.fun(fun)
  # draw prior distributions
  sims <- calculate(y, nsim = nsim)
  # summarize prior distribution of response
  sum_prior <- apply(sims[[1]], 1, sum_fun)
  # get the quantiles
  qq <- quantile(sum_prior, probs = probs)
  # quantile range
  r_qq <- (probs[2] - probs[1]) * 100
  # output
  out <- paste0(r_qq, "% of the ", nsim, " simulated response draws from the prior distributions had a ",fun, " value between ", round(qq[1], 2), " and ", round(qq[2], 2),".")
  
  return(out)  
}

