context("regression")

#create a dataset with two with one correlated and one uncorrelated variables to check ridge regression works

n <- 50
y <- (2 * 1:n) + rnorm(n, 0, 1)
x_1 <- rnorm(n, 3, 1)
x_2 <- 0.5 * 1:n + rnorm(n, 0.5, 0.5)
X <- matrix(c(x_1, x_2), ncol=2)
#plot(x_1, y)
#plot(x_2, y)

test_that("ridge regression works as expected on simple dataset", {
  beta <- fit_rr(X, as.matrix(y), 1)
  y_pred <- predict_rr(X, beta)
  
  #check the largest prediction error is lower than expected from noise
  expect_lt(max(abs(y-y_pred)), 8)
  
  #check beta for the random variable is lower than the predicting variable
  expect_lt(beta[1], beta[2])
})


# compare ocv results to R function
ocvR <- function(X, y, lam){
  A <- X %*% solve(crossprod(X) + diag(ncol(X))*lam, t(X))
  mu.hat <- A %*% y
  return(mean((y-mu.hat)^2/(1-diag(A))^2))
}

test_that("ocv once gives the same result as R", {
  ocv <- get_ocv_once(X, as.matrix(y), 1)
  ocv2 <- ocvR(X, y, 1)
  expect_equal(ocv, ocv2)
})


test_that("optim rr gives the same results as R and get_ocv_once", {
  lambdas <- exp(seq(-4,4,length=20))
  ocvs <- optim_rr(X, as.matrix(y), lambdas)
  
  ocvs2 <- numeric(20)
  for(i in 1:20){
    ocvs2[i] <- ocvR(X, y, lambdas[i])
  }
  
  #plot(log(lambdas), ocvs, type="l", ylim=range(ocvs, ocvs2))
  #lines(log(lambdas), ocvs2, col=2)
  all.equal(ocvs, ocvs2)

  opt_lam <- lambdas[which.min(ocvs)]
  opt_ocv <- get_ocv_once(X, as.matrix(y), opt_lam)
  expect_equal(opt_ocv, min(ocvs))
})

