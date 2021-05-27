# Fit ridge regression model to the total demand data
library(tidyverse)
library(RcppRidge)
load("data/Irish_Totals.RData")

#list of lambdas to test
lambdas <- exp(seq(-2,4,length=20))

#create group index, as we want to fit a model to each time of day separately
idx <- as.numeric(as.factor(X[,"tod"]))

#use par_reg to fit 48 models in parallel
res <- par_reg(X, as.matrix(y), lambdas, idx)

#extract results, betas is a ncol(X)x48 matrix with fitted betas
#and lambdas is length-48 vector of fitted regularisation parameter for each group
betas <- res$betas
opt_lams <- res$lambdas
vars <- res$variances

#now predict total demand for the test set
idx_y  <- as.numeric(factor(X_test[, "tod"], levels=levels(factor(X[,"tod"]))))
y_pred <- predict_groups(X_test, betas, idx_y)

#View the predictions (looks fine)
par(mfrow=c(3, 4))
par(mar=c(4,4,2,1))
for(i in 1:12){
  plot(0:47, y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand",
       ylim=range(y_test, y_pred), main=paste("month", i))
  lines(0:47, y_pred[(i*48-47):(i*48)], col=2)
}


#sample from the posterior distribution of beta to get a credible interval for y predictions
#for each timepoint, sample betas from the posterior and predict y
sig <- sd(y)
p <- ncol(X)
n <- nrow(X_test)
n_samp <- 1000
y_pred_mat <- matrix(NA, nrow=length(y_test), ncol=n_samp)

for(i in 1:48){
  #get matrix of X_test just for this tod
  tod <- unique(X[,"tod"])[i]
  sub <- X_test[,"tod"] == tod
  X_sub <- X_test[sub,]
  
  #sample betas
  beta <- betas[,i]
  Sig <- vars[, , i]
  betas_samp <- rmvn_omp(n_samp, mu=beta, sigma=Sig)
  
  #make new y predictions for the test sub with each sampled beta
  for(j in 1:n_samp){
    y_pred_mat[sub,j] <- predict_rr(X_sub, betas_samp[j,])
  }
}

#for each y take 2.5 and 97.5th percentile values
y_int <- matrix(NA, nrow=n, ncol=3)
colnames(y_int) <- c("lower", "mean", "upper")
y_int[,2] <- rowMeans(y_pred_mat)
for(j in 1:n){
  y_int[j,c(1, 3)] <- quantile(y_pred_mat[j,], c(.025, .975))
}

#add back on the mean
y_int <- y_int + mean_y
y_pred <- y_pred + mean_y
y_test <- y_test + mean_y

#plot the predictions with the upper and lower interval
par(mfrow=c(3, 4))
par(mar=c(4,4,2,1))
for(i in 1:12){
  #plot observed demand
  plot(1:48, y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand",
       ylim=range(y_test, y_int, na.rm=T), main=paste("month", i))
  #plot predicted demand in red (these two lines should be identical)
  lines(1:48, y_int[(i*48-47):(i*48),2], col=2)
  lines(1:48, y_pred[(i*48-47):(i*48)], col=2)
  #plot interval with red dashed lines
  lines(1:48, y_int[(i*48-47):(i*48),1], col=2, lty=2)
  lines(1:48, y_int[(i*48-47):(i*48),3], col=2, lty=2)
}


#how many of the true values fall outside of the interval?
sum(y_test < apply(y_int, 1, min) | y_test > apply(y_int, 1, max)) / length(y_test)
#35%


save(X_test, y_test, y_pred, y_int, file="data/Total_Predictions.RData")
