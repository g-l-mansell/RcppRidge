# Load relevant code
load("data/Irish_Processed.Rdata")
load("data/clusters.Rdata")
source("analysis/0_utilityfunctions.R")
# load required packages
library(RcppRidge)
library(matrixStats)
library(tidyverse)
library(lubridate)
# set seed to ensure reproducibility
set.seed(59)
MAE <- vector(length =length(unique(clusters))) # store MAE output
for (clust in unique(clusters)) {
  # Create the subsample of data from ID's in each cluster
  samp <- cust[,names(clusters[clusters == clust])]
  print(clust) # Used to get waypoints during the fitting proces
  
  # Create dataframe of households in the cluster
  df <- select(extra, -date) %>%
    cbind(samp) %>%
    pivot_longer(names_to = "ID", c(-time, -toy, -dow, -tod, -temp, -testSet, -dateTime), values_to = "demand") %>%
    left_join(surv, by = "ID")  %>%
    mutate(windows = as.numeric(windows),
           tariff = as.numeric(tariff),
           stimulus = as.numeric(stimulus),
           class = as.numeric(class))
  # One hot encode relevant factor variables, importantly day of week
  df <- one_hot_encode(df)
  #saveRDS(df, "df.rds")
  
  # Create data matrix X removing covariates we don't want
  X <- df %>%
    select(-time, -ID, -demand, -testSet, -dateTime) %>%
    mutate_if(is.factor, ~as.numeric(.)-1) %>%
    as.data.frame()
  
  # Create the response vector
  y <- df %>% pull(demand)
  
  # Store columns that are part of the test set
  testset <- df$testSet
  # No longer needed
  rm(samp)
  #feature transform numerical columns of X
  cols <- c(1, 3, 4, 5, 7, 11) # Relevant columns for feature transforms
  # Add feature transforms if less than 0.9 correlation
  for(i in cols){
    name <- colnames(X)[i]
    col <- paste0(name,"^2")
    if(cor(X[,i], X[,i]^2) < 0.9) X[,col] <- X[,i]^2
    
    col <- paste0(name,"^3")
    if(cor(X[,i], X[,i]^3) < 0.9) X[,col] <- X[,i]^2
    
    for(j in cols[cols>i]){
      if(i != j){
        col <- paste0(name, colnames(X)[j])
        if(cor(X[,i], X[,j]) < 0.9) X[,col] <- X[,i]*X[,j]
      }
    }
  }
  
  # No longer needed
  rm(cols)
  
  #standardise
  X <- as.matrix(X)
  #X <- scale(X)
  
  # scale response data
  mean_y <- mean(y)
  
  y <- y - mean(y) 
  
  #remove the last day of each month as a test set
  y_test <- y[testset]
  X_test <- X[testset,]
  
  y <- y[!testset]
  X <- X[!testset,]
  
  ## Create a model with totals
  ## Create model by household
  M <- 20 # try 20 lambdas
  y_pred <- rep(NA, length(y_test))
  lambdas <- exp(seq(-5,5,length=M))
  
  # Store tod ids for use in the parallel regression
  idx <- as.numeric(as.factor(X[,"tod"]))
  # out stores betas/lambdas/variance matrices
  out <- par_reg(X, as.matrix(y), lambdas, idx)
  
  # Save relevant output
  saveRDS(out, paste0("analysis/cluster_data/clust", clust, ".rds"))
  
  betas <- out$betas
  out_lambdas <- out$lambdas
  #now use betas to predict each tod
  y_pred <- rep(NA, length(y_test))
  tods <- unique(X[,"tod"])
  
  # Predict on last month
  idx_y  <- as.numeric(factor(X_test[, "tod"], levels=levels(factor(X[,"tod"]))))
  y_pred <- RcppRidge::predict_groups(X_test, betas, idx_y)
  # bootstrap interval results
  interval_results <- boostrap_intervals(out, X_test)
  # Create relevant values
  y_lower <- interval_results[[1]][,1]
  y_upper <- interval_results[[1]][,2]
  y_pred <- y_pred + mean_y
  y_test <- y_test + mean_y
  y_lower <- y_lower + mean_y
  y_upper <- y_upper + mean_y
  
  saveRDS(interval_results[[2]], paste0("analysis/cluster_data/beta_ints_cluster", clust, ".rds"))
  
  # Need the month covariate to create summary test data frame
  df <- df %>% mutate(month = month(dateTime, label = TRUE))
  #plot totalof cluster predictions
  
  # Results data
  res <- df %>%
    filter(testSet) %>%
    cbind(y_test, y_pred, y_lower, y_upper) %>%
    group_by(tod, month) %>%
    summarise(y_test = sum(y_test),
              y_pred = sum(y_pred),
              y_lower = sum(y_lower),
              y_upper = sum(y_upper),
              cluster = clust)
  
  saveRDS(res, paste0("analysis/cluster_data/plot_cluster", clust, ".rds"))
  
  # Create a plot showing each month of predicting daily demand profile on test set
  monthlyplot <- ggplot(res) + 
    geom_line(aes(x = tod/2, y = y_test, color = "True value")) +
    geom_line(aes(x = tod/2, y = y_pred, color = "Prediction")) + 
    geom_line(aes(x = tod/2, y = y_lower, color = "Interval")) +
    geom_line(aes(x = tod/2, y = y_upper, color = "Interval")) +
    facet_wrap(vars(month)) +
    xlab("Time of Day (hour)") +
    ylab("Demand (kWh)")
  
  ggsave(paste0("analysis/figures/monthly_predictions_cluster", clust, ".png"), monthlyplot)
  
  # Store the MAE for this cluster
  MAE[clust] <- mean(abs(res$y_test-res$y_pred))/sum(clusters == clust)
  
  # tidy up
  rm(res)
  rm(X)
  rm(y)
  gc()
}

