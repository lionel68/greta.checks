# pull out the necessary internal functions from greta
op <- .internals$nodes$constructors$op

#' @import greta
#'
#' @title Convert a \code{greta_mcmc_list} to a \code{DHARMa} object
#' 
#' @description Create a \code{DHARMa} object from a fitted greta model.
#'
#' @param object A greta_mcmc_list object obtained from the greta::mcmc function
#' @param y A greta array of the response variable, see example
#' @param linpred A greta array of the expected value of y (typically the linear predictor in GLMM)
#' @param nsim A numeric, the number of posterior simulation to draw, default is 10
#' @param integerResponse A logical, whether the response variable is of integer type (binomial or poisson distribution). 
#' If TRUE, noise will be added to the residuals to maintain uniform expectations. Default is FALSE.
#' 
#' 
#' @return A DHARMa object that allows plenty of model checks methods from the \pkg{DHARMa} package.
#' 
#' @details For hierarchical models the linear predictor can be set conditional to the random terms, or unconditional to them. Best practice seems to be to define the linear predictor (the predictions) unconditional from the random effects (see \href{https://github.com/florianhartig/DHARMa/issues/43}{DHARMa issues 43}).
#' DHARMa objects come with many methods to check (among other): model miss-fit, overdispersion, spatial or temporal autocorrelation ... Check the \code{DHARMa} manual for examples and options.
#' 
#' @examples 
#' \dontrun{
#' 
#' x <- runif(100, -2, 2)
#' y <- rnorm(100, 1 + 2 * x, 1)
#' y <- as_data(y)
#'
#' intercept <- normal(0, 1)
#' slope <- normal(0, 1)
#' linpred <- intercept + slope * x
#' sd_res <- cauchy(0, 1, truncation = c(0, 50))
#'
#' distribution(y) <- normal(linpred, sd_res)

#' m <- model(intercept, slope, sd_res)
#' d <- mcmc(m)
#' 
#' sims <- simulate_residual(d, y, linpred)
#' plot(sims)
#' }
#' 
#' @importFrom DHARMa createDHARMa
#' @export

simulate_residual <- function(object, y, linpred, nsim = 10,
                               integerResponse = FALSE){
  # draw new response variable
  yrep <- calculate(y, values = object, nsim = nsim)[[1]][,,1]
  
  # fitted response
  mu <- calculate(linpred, values = object, nsim = nsim)[[1]][,,1]
  
  # create a DHARMa object
  dh <- createDHARMa(simulatedResponse = t(yrep),
                     observedResponse = as.numeric(y),
                     integerResponse = integerResponse,
                     fittedPredictedResponse = apply(mu, 2, median))
  
  return(dh)
}