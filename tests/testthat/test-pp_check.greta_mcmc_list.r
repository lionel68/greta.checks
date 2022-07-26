test_that("pp_check.greta_mcmc_list return a ggplot object", {
  
  check_tf_version <- greta::.internals$checks$check_tf_version
  skip_if_not(check_tf_version())
  
  set.seed(1234)
  
  intercept <- normal(0, 1)
  slope <- normal(0, 1)
  sd_resid <- cauchy(0, 1, truncation = c(0, 100))
  
  x <- runif(100)
  y <- as_data(rnorm(100, 1 + 2 * x, 1))
  
  linpred <- intercept + slope * x
  
  distribution(y) <- normal(linpred, sd_resid)
  
  m <- model(intercept, slope, sd_resid)
  draws <- mcmc(m, warmup = 10, n_samples = 10)
  
  expect_s3_class(pp_check(draws, y), "gg")
  
  expect_error(pp_check(draws, y, type = "loo_pit"), "LOO ppc function are not supported", fixed = TRUE)
  
  expect_error(pp_check(draws, y, type = "bars"), "Discrete ppc function not supported", fixed = TRUE)
  
})