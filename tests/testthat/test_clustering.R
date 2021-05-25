context("Clustering")

#create a dataset with two clear clusters and check our clustering functions work

n <- 50
y <- c(rep(1, n), rep(2, n))
x_1 <- c(rnorm(n, 5, 1), rnorm(n, 10, 1))
x_2 <- c(rnorm(n, 5, 1), rnorm(n, 10, 1))
X <- matrix(c(x_1, x_2), ncol=2)
#plot(x_1, x_2, col=y)

test_that("k-means correctly finds 2 clusters", {
  y_pred <- k_means(X, 2)
  compare_lables <- table(y_pred, y) 
  #we expect this to be a table with elements (50, 0 // 0, 50) or (0, 50 // 50, 0)
  expect_equal(max(compare_lables),  50)
  expect_equal(min(compare_lables),  0)
})

test_that("spectral clustering correctly finds 2 clusters", {
  y_pred <- spectralClustering(X, k=2)
  compare_lables <- table(y_pred, y) 
  #we expect this to be a table with elements (50, 0 // 0, 50) or (0, 50 // 50, 0)
  expect_equal(max(compare_lables),  50)
  expect_equal(min(compare_lables),  0)
})

##pca function doesnt currently work
