context("RidgeRegression")

#create a dataset with two with one correlated and one uncorrelated variables to check ridge regression works

n <- 50
y <- (2 * 1:n) + rnorm(n, 1, 1)
x_1 <- rnorm(n, 3, 1)
x_2 <- 0.5 * 1:n + rnorm(n, 0.5, 0.5)
X <- matrix(c(x_1, x_2), ncol=2)
plot(x_1, y)
plot(x_2, y)

test_that("k-means correctly finds 2 clusters", {
  beta <- fit_rr(X, y, 1)
})