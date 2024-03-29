% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_residual.r
\name{simulate_residual}
\alias{simulate_residual}
\title{Convert a \code{greta_mcmc_list} to a \code{DHARMa} object}
\usage{
simulate_residual(object, y, linpred, nsim = 10, integerResponse = FALSE)
}
\arguments{
\item{object}{A greta_mcmc_list object obtained from the greta::mcmc function}

\item{y}{A greta array of the response variable, see example}

\item{linpred}{A greta array of the expected value of y (typically the linear predictor in GLMM)}

\item{nsim}{A numeric, the number of posterior simulation to draw, default is 10}

\item{integerResponse}{A logical, whether the response variable is of integer type (binomial or poisson distribution).
If TRUE, noise will be added to the residuals to maintain uniform expectations. Default is FALSE.}
}
\value{
A DHARMa object that allows plenty of model checks methods from the \pkg{DHARMa} package.
}
\description{
Create a \code{DHARMa} object from a fitted greta model.
}
\details{
For hierarchical models the linear predictor can be set conditional to the random terms, or unconditional to them. Best practice seems to be to define the linear predictor (the predictions) unconditional from the random effects (see \href{https://github.com/florianhartig/DHARMa/issues/43}{DHARMa issues 43}).
DHARMa objects come with many methods to check (among other): model miss-fit, overdispersion, spatial or temporal autocorrelation ... Check the \code{DHARMa} manual for examples and options.
}
\examples{
\dontrun{

x <- runif(100, -2, 2)
y <- rnorm(100, 1 + 2 * x, 1)
y <- as_data(y)

intercept <- normal(0, 1)
slope <- normal(0, 1)
linpred <- intercept + slope * x
sd_res <- cauchy(0, 1, truncation = c(0, 50))

distribution(y) <- normal(linpred, sd_res)
m <- model(intercept, slope, sd_res)
d <- mcmc(m)

sims <- simulate_residual(d, y, linpred)
plot(sims)
}

}
