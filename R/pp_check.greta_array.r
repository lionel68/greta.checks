
# pull out the necessary internal functions from greta
op <- .internals$nodes$constructors$op

#' @import greta
#'
#' @title Prior Predictive Checks for \code{greta_array} object
#'
#' @aliases pp_check
#' 
#' @description Perform prior predictive checks with the help of the package \code{bayesplot}
#'
#' @param y A greta_array object generated from the greta::distributions function
#' @param nsim A numeric, the number of prior simulations per draw, default is 100
#' @param ndraw A numeric, the number of draws, default is 10
#' @param ... Further arguments passed to bayesplot::ppc_dens_overlay, check the possibilities there.
#' 
#' @return A ggplot object that can be further modified using the \pkg{ggplot2} package.
#' 
#' @examples 
#' \dontrun{
#' x <- normal(0, 2)
#' 
#' # default use
#' pp_check(x)
#' }
#'
#' @importFrom bayesplot ppc_dens_overlay
#' @export ppc_dens_overlay
#' @export

pp_check.greta_array <- function(y, 
                                 nsim = 100, 
                                 ndraw = 10,
                                 ...){
  # draw prior distributions
  sims <- replicate(n_realisations, calculate(y, nsim = nsim)[[1]][, , 1])

  # call the function
  # retrive data and attributes from the ggplot object
  p <- ppc_dens_overlay(sims[,1], t(sims[,-1]), ...)
  # and then manually plot the densities
  ggplot(p$data) +
    geom_density(aes(value, group = rep_id)) +
    p$coordinates +
    p$theme 
}
