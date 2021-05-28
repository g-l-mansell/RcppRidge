# Fit ridge regression model to the total demand data
library(tidyverse)
library(RcppRidge)
load("data/Irish_Processed.RData")
source("analysis/0_utilityfunctions.R")


### Create dataset of total demand per time
df <- extra %>%
  mutate(demand = rowSums(cust)) %>%
  as.data.frame()

#split input and response
X <- df[, c(2, 3, 5)]
y <- df$demand

#subtract mean so we dont need an intercept
mean_y <- mean(y)
y <- y - mean(y)

#feature transform
X <- feature_transform(X)

#remove the last day as a test
y_test <- y[df$testSet]
X_test <- X[df$testSet,]

y <- y[!df$testSet]
X <- X[!df$testSet,]

#list of lambdas to test
lambdas <- exp(seq(-2,4,length=20))

#create group index, as we want to fit a model to each time of day separately
idx <- df[!df$testSet,"tod"]

#use par_reg to fit 48 models in parallel
res <- par_reg(X, as.matrix(y), lambdas, idx)
betas <- res$betas # px48 matrix with fitted betas for each group
opt_lams <- res$lambdas # length-48 vector of fitted lambda for each group
vars <- res$variances # pxpx48 array of covariance matrix for beta|y

#now predict total demand for the test set
idx_y <- df[df$testSet,"tod"]
y_pred <- predict_groups(X_test, betas, idx_y)

#view the predictions
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
  sub <- df[df$testSet, "tod"] == (i-1)
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

#png("analysis/figures/MontlyPredictionsInterval.png", width=800, height=500)
plot_monthly_predictions(y_test, y_int)
#dev.off()

(MSE <- mean(abs(y_test - y_pred)) / nrow(X_test))
#[1] 0.1422841



#test plotting betas for each of the 48 models
colnames(betas) <- 1:48 #seq(0, 23.5, 0.5)
pd <- as.data.frame(betas) %>%
  mutate(var=factor(colnames(X), levels=colnames(X))) %>%
  pivot_longer(cols=-"var", names_to="tod", values_to="coeff") %>%
  mutate(tod=as.numeric(tod))

ggplot(pd, aes(x=var, y=coeff)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(aes(colour=tod), size=0.8, alpha=0.8) +
  labs(x=NULL, y="beta", colour="Model") +
  theme_minimal() +
  scale_colour_viridis_c()
ggsave("analysis/figures/Totals_Betas_48.png", height=4, width=7)



#test plotting betas_samp for one model
#for each y take 2.5 and 97.5th percentile values
beta_int <- matrix(NA, nrow=p, ncol=3)
colnames(beta_int) <- c("lower", "mean", "upper")
beta_int[,2] <- colMeans(betas_samp)
for(j in 1:p){
  beta_int[j,c(1, 3)] <- quantile(betas_samp[,j], c(.025, .975))
}
rownames(beta_int) <- colnames(X)

par(mfrow=c(1,1))
library(forestplot)
png("analysis/figures/Total_Betas_Intervals.png", width=400, height=500)
forestplot(beta_int, col=fpColors(box = "darkblue"), 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "Arial"), ticks = gpar(cex=1)))
dev.off()
#boxplot(betas_samp, names=colnames(X), ylab="Regression Coefficient", cex.axis=0.8)
