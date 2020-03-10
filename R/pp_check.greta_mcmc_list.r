
# pull out the necessary internal functions from greta
op <- .internals$nodes$constructors$op

#' @import greta
#'
#' @title Posterior Predictive Checks for \code{greta_mcmc_list} object
#'
#' @aliases pp_check
#' 
#' @description Perform posterior predictive checks with the help of the package \code{bayesplot}
#'
#' @param object A greta_mcmc_list object obtained from the greta::mcmc function
#' @param y A greta array of the response variable, see example
#' @param nsim A numeric, the number of posterior simulation to draw, default is 10
#' @param type A character string, the type of posterior predictive plot to draw. See \code{\link[bayesplot:PPC-overview]{PPC-overview}} for some options, or check the example below.
#' @param ... Further arguments passed to ppc_* functions, check the possibilities there.
#' 
#' @return A ggplot object that can be further modified using the \pkg{ggplot2} package.
#' 
#' @details For a detailed explanation of each of the implemented ppc functions, see \code{\link[bayesplot:PPC-overview]{PPC-overview}}. 
#' Note that LOO and Discrete ppc_* function are not yet available for greta models.
#'
#' @examples 
#' \dontrun{
#' x <- runif(100, -2, 2)
#' y <- rnorm(100, 1 + 2 * x, 1)
#' # need the pass the response vector as greta array
#' y <- as_data(y)
#' 
#' intercept <- normal(0, 2)
#' slope <- normal(0, 1)
#' linpred <- intercept + slope * x
#' sd_res <- cauchy(0, 2, truncation = c(0, Inf))
#'
#' distribution(y) <- normal(linpred, sd_res)
#' m <- model(intercept, slope, sd_res)
#' d <- mcmc(m, warmup = 10, n_samples = 10)
#' 
#' # default use
#' pp_check(d, y)
#' # check some options
#' pp_check(d, y, nsim = 4, type = "scatter_avg")
#' pp_check(d, y, type = "hist")
#' pp_check(d, y, type = "error_scatter") 
#' }
#'
#' @importFrom bayesplot pp_check
#' @export pp_check
#' @export


pp_check.greta_mcmc_list <- function(object, y, nsim = 10, type = "dens_overlay", ...){
  # compute posterior predictive values
  yrep <- calculate(y, values = object, nsim = nsim)[[1]][,,1]
  # turn response into a vector
  y_vec <- as.numeric(y)
  
  # remove some type of pp plots that are currently not making sense
  if(type %in% c("bars", "bars_grouped", "rootgram")){
    stop("Discrete ppc function not supported")
  }
  
  if(length(grep("loo", type)) != 0){
    stop("LOO ppc function are not supported")
  }
  
  # the ppc function to apply
  ppc_fun <- get(paste0("ppc_", type), asNamespace("bayesplot"))
  
  # call the function
  ppc_fun(y_vec, yrep, ...)
}
