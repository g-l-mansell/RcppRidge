context("MVN sampling")

#test a 2D MVN with a plot

test_that("2D rmvn_omp produces good samples", {
  samples <- rmvn_omp(500L, c(1, 2), sigma = matrix(c(2,2,2,4), nrow=2))
  plot(samples)
  
  #means of the two variables are close to the given 1, 2 
  expect_lt(abs(mean(samples[,1]) - 1), 0.1)
  expect_lt(abs(mean(samples[,2]) - 2), 0.1)
  
  #variances of the two variables are close to the given 2, 4 
  expect_lt(abs(var(samples[,1]) - 2), 0.5)
  expect_lt(abs(var(samples[,2]) - 4), 0.5)
})
  
  