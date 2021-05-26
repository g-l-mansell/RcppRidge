context("parallel regression")

#create a dataset of with two different regression groups to test par_reg and predict_groups

n <- 50
idx <- c(rep(1, 50), rep(2, 50))
y <- c(2 * 1:n, 1:n) + rnorm(2*n, 0, 1)
x_1 <- rnorm(n*2, 3, 1)
x_2 <- c((0.5 * 1:n), (3 + 1:n)) + rnorm(n*2, 0.5, 0.5)
X <- matrix(c(x_1, x_2), ncol=2)
#plot(x_1, y, col=idx)
#plot(x_2, y, col=idx)


test_that("parallel ridge regression works well on sample dataset", {
  
  lambdas <- exp(seq(-4,4,length=20))
  res <- par_reg(X, as.matrix(y), lambdas, idx)
  betas <- res$betas
  
  #expect that random x_1 has coefficients near 0
  expect_equal(betas[1,1], 0, tolerance = 1)
  expect_equal(betas[1,2], 0, tolerance = 1)
  
  #expect that predicting x_2 has coefficients near the real values (1 and 4)
  expect_equal(betas[2,1], 4, tolerance = 0.5)
  expect_equal(betas[2,2], 1, tolerance = 0.5)
})


test_that("prediction function for multiple groups works", {
  
  lambdas <- exp(seq(-4,4,length=20))
  res <- par_reg(X, as.matrix(y), lambdas, idx)
  betas <- res$betas
  
  mix <- sample(1:(2*n), 2*n)
  X_test <- X[mix,]
  y_test <- y[mix]
  idx_y <- idx[mix]
  
  idx_y  <- as.numeric(factor(idx_y, levels=levels(factor(idx))))
  y_pred <- predict_groups(X_test, betas, idx_y)
  
  # plot(X_test[,1], y_pred, col=idx_y)
  # plot(X_test[,2], y_pred, col=idx_y)
  
  expect_equal(y_test, c(y_pred), tolerance = 10)
})
