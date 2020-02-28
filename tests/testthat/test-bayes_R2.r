test_that("bayes_R2 gives correct format", {
  
  check_tf_version <- greta::.internals$utils$misc$check_tf_version
  skip_if_not(check_tf_version())
  
  intercept <- normal(0, 1)
  slope <- normal(0, 1)
  sd_resid <- cauchy(0, 1, truncation = c(0, 100))
   
  x <- runif(100)
  y <- as_data(rnorm(100, 1 + 2 * x, 1))
   
  pred <- intercept + slope * x
  m <- model(intercept, slope, sd_resid)
  drr <- mcmc(m, warmup = 10, n_samples = 10)

  # compare
  expect_is(class(bayes_R2(y, pred, drr)), "matrix")
  
})