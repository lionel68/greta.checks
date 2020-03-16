test_that("prior_check works as it should", {
  
  check_tf_version <- greta::.internals$utils$misc$check_tf_version
  skip_if_not(check_tf_version())
  
  set.seed(1234)
  
  intercept <- normal(0, 1)
  slope <- normal(0, 1)
  sd_resid <- cauchy(0, 1, truncation = c(0, 100))
  
  x <- runif(100)
  y <- as_data(rnorm(100, 1 + 2 * x, 1))
  
  linpred <- intercept + slope * x
  
  distribution(y) <- normal(linpred, sd_resid)

  # results 
  expect_error(prior_check(y), NA)
  
  # compare
  #expect_equal(round(r2[,1], 2), 0.02)
  
})