# Load relevant code
load("data/Irish_Processed.Rdata")
load("data/clusters.Rdata")
source("code/bootstrap_intervals.R")
library(RcppRidge)
library(matrixStats)

rm(Irish)
rm(df_cluster_1)
rm(df_cluster_2)
rm(df_cluster_3)
rm(df_cluster_4)
rm(df_cluster_5)
gc()

set.seed(59)
for (clust in unique(clusters)) {
  # Create the subsample of data from ID's in each cluster
  samp <- cust[,names(clusters[clusters == clust])]
  print(clust)
  # Proceed in same way that we fit 100 customers
  df <- select(extra, -date) %>%
    cbind(samp) %>%
    pivot_longer(names_to = "ID", c(-time, -toy, -dow, -tod, -temp, -testSet, -dateTime), values_to = "demand" ) %>%
    left_join(surv, by = "ID") %>%
    mutate(windows = as.numeric(windows),
           tariff = as.numeric(tariff),
           stimulus = as.numeric(stimulus),
           class = as.numeric(class))
  
  df <- one_hot_encode(df)
  #saveRDS(df, "df.rds")
  X <- df %>%
    select(-time, -ID, -demand, -testSet, -dateTime) %>%
    mutate_if(is.factor, ~as.numeric(.)-1) %>%
    as.data.frame()
  
  y <- df %>% pull(demand)
  
  testset <- df$testSet
  #rm(df)
  rm(samp)
  #feature transform numerical columns of X
  cols <- c(1, 3, 4, 5, 7, 11)
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
  
  rm(cols)
  
  #standardise
  X <- as.matrix(X)
  #X <- scale(X)
  mean_y <- mean(y)
  
  y <- y - mean(y) 
  
  #remove the last day as a test
  y_test <- y[testset]
  X_test <- X[testset,]
  
  y <- y[!testset]
  X <- X[!testset,]
  
  ## Create a model with totals
  ## Create model by household
  M <- 20
  y_pred <- rep(NA, length(y_test))
  lambdas <- exp(seq(-5,5,length=M))
  
  idx <- as.numeric(as.factor(X[,"tod"]))
  out <- par_reg(X, as.matrix(y), lambdas, idx)
  
  saveRDS(out, paste0("data/clusterparams/cluster", clust, ".rds"))
  
  betas <- out$betas
  out_lambdas <- out$lambdas
  #now use betas to predict each tod
  y_pred <- rep(NA, length(y_test))
  tods <- unique(X[,"tod"])
  
  # Predict on last month
  idx_y  <- as.numeric(factor(X_test[, "tod"], levels=levels(factor(X[,"tod"]))))
  y_pred <- RcppRidge::predict_groups(X_test, betas, idx_y)
  interval_results <- boostrap_intervals(out, X_test)
  y_lower <- interval_results[[1]][,1]
  y_upper <- interval_results[[1]][,2]
  y_pred <- y_pred + mean_y
  y_test <- y_test + mean_y
  y_lower <- y_lower + mean_y
  y_upper <- y_upper + mean_y
  
  df <- df %>% mutate(month = month(dateTime, label = TRUE))
  #plot totalof cluster predictions
  
  res <- df %>%
    filter(testSet) %>%
    cbind(y_test, y_pred, y_lower, y_upper) %>%
    group_by(tod, month) %>%
    summarise(y_test = sum(y_test),
              y_pred = sum(y_pred),
              y_lower = sum(y_lower),
              y_upper = sum(y_upper),
              cluster = clust)
  
  saveRDS(res, paste0("data/plot_cluster", clust))
  
  monthlyplot <- ggplot(res) + 
    geom_line(aes(x = tod/2, y = y_test, color = "True value")) +
    geom_line(aes(x = tod/2, y = y_pred, color = "Prediction")) + 
    geom_line(aes(x = tod/2, y = y_lower, color = "Interval")) +
    geom_line(aes(x = tod/2, y = y_upper, color = "Interval")) +
    facet_wrap(vars(month)) +
    xlab("Time of Day (hour)") +
    ylab("Demand (kWh)")
  
  ggsave(paste0("plots/monthly_predictions_cluster", clust, ".png"), monthlyplot)
  
  png(paste0("plots/cluster",  clust, "model.png"), width=1000, height=800)
  par(mfrow=c(3, 4))
  par(mar=c(2,2,1,1))
  for(i in 1:12){
    plot(0:47, res$y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", ylim=range(res$y_test, res$y_pred)) 
    lines(0:47, res$y_pred[(i*48-47):(i*48)], col=2)
  }
  dev.off()
  
  (MAE <- mean(abs(res$y_test-res$y_pred)))
  print(MAE)
  saveRDS(betas, paste0("data/betas", clust, ".rds"))
  rm(res)
  rm(X)
  rm(y)
  gc()
}

