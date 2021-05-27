# Fit ridge regression model to the total demand data
library(tidyverse)
library(RcppRidge)
load("data/Irish_Totals.RData")
source("analysis/0_utilityfunctions.R")

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
plot_monthly_predictions(y_test, cbind(NA, y_pred, NA))


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

png("analysis/figures/MontlyPredictionsInterval.png", width=800, height=500)
plot_monthly_predictions(y_test, y_int)
dev.off()

MSE <- mean(abs(y_test - y_pred)) / nrow(X_test)
#[1] 0.1422841
