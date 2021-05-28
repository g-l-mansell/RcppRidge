# Place to store useful functions for the project, that are too specific to be added to the package


#' Our feature transform function
#' 
#'@param X dataframe of sample (so named columns can be easily added) 
#'Factor columns will be one-hot encoded
#'Numeric columns will be squared, cubed, and have first order interactions
#'@return data matrix of transformed variables
feature_transform <- function(X){
  
  facs <- sapply(X, is.factor)
  if(any(facs)){
    X_num <- X[,!facs, drop=F]
    X_facs <- X[,facs, drop=F]
  } else {
    X_num <- X
  }

  #(not adding columns if correlation is >90%)
  cols <- 1:ncol(X_num)
  for(i in cols){
    name <- colnames(X_num)[i]
    
    #add col squared
    col <- paste0(name,"^2")
    if(cor(X_num[,i], X_num[,i]^2) < 0.9) X_num[,col] <- X_num[,i]^2
    
    #add col cubed
    col <- paste0(name,"^3")
    if(cor(X_num[,i], X_num[,i]^3) < 0.9) X_num[,col] <- X_num[,i]^2
    
    #add first order interactions
    for(j in cols[cols>i]){
      col <- paste0(name, colnames(X_num)[j])
      if(cor(X_num[,i], X_num[,j]) < 0.9) X_num[,col] <- X_num[,i]*X_num[,j]
    }
  }
  
  #standardise
  X_out <- as.matrix(X_num)
  X_out <- scale(X_out)
  
  #one hot encode factors
  if(any(facs)){
    for(i in 1:ncol(X_facs)){
      oh <- model.matrix(~0+X_facs[,i])
      attr(oh, "dimnames")[[2]] <- levels(X_facs[,i])
      X_out <- cbind(X_out, oh)
    }
  }
  
  return(X_out)
}

plot_monthly_predictions <- function(y_test, y_int){
  par(mfrow=c(3, 4))
  par(mar=c(4,4,2,1))
  times <- seq(0, 23.5, 0.5)
  for(i in 1:12){
    day_idx <- (i*48-47):(i*48)
    plot(x=range(times), y=range(y_test, y_int, na.rm=T), type="n", 
         main=format(ISOdate(2004,i,1),"%b"), xlab="Time of Day", ylab="Demand (kWh)")
    
    #plot predicted demand in red
    lines(times, y_int[day_idx,2], col=2)
    #plot interval with red dashed lines
    lines(times, y_int[day_idx,1], col=2, lty=2)
    lines(times, y_int[day_idx,3], col=2, lty=2)
    #plot true demand in black last
    lines(times, y_test[day_idx])
  }
}


#' Performs a parametric bootstrap to work out confidence intervals
#' 
#'@param out a list containing betas, variance and mu
#'@param X_test test data frame
#'@return list containing confidence intervals for the ys and the beta parameters
boostrap_intervals <- function(out, X_test) {
  beta_confidence <- array(dim = c(nrow(out$betas), 3, 48))
  y_confidence <- matrix(nrow = nrow(X_test), ncol = 2)
  colnames(y_confidence) <- c("lower", "upper")
  colnames(beta_confidence) <- c("lower", "mean", "upper")
  p <- nrow(out$betas)
  for (i in 1:48) {
    betas_samp <- rmvn_omp(10000, mu=out$betas[,i], sigma=out$variances[,,i])
    
    #for each parameter take 2.5 and 97.5th percentile values
    beta_mat <- matrix(NA, nrow=nrow(out$betas), ncol=3)
    colnames(beta_mat) <- c("lower", "mean", "upper")
    beta_mat[,2] <- out$betas[,i]
    
    for(j in 1:p){
      beta_mat[j,c(1,3)] <- quantile(betas_samp[,j], c(.025, .975))
    }
    
    beta_confidence[,,i] <- beta_mat
    
    tmp_X_test <- X_test[X_test[,"tod"] == (i-1),]
    
    tmp_preds <- tmp_X_test %*% t(betas_samp)
    
    tmp_quants <- rowQuantiles(tmp_preds, probs = c(.025, .975))
    
    y_confidence[X_test[,"tod"] == (i-1),] <- tmp_quants
  }
  return(list(y_confidence, beta_confidence))
}

one_hot_encode <- function(dataframe) {
  dataframe <- data.frame(dataframe)
  ids <- NULL
  for (j in 1:ncol(dataframe)) {
    if (is.factor(data.frame(dataframe)[,j])) {
      if (length(levels(dataframe[,j])) > 2) {
        ids <- append(ids, j)
        for (i in levels(dataframe[,j])) {
          dataframe[,paste0(names(dataframe)[j], "_", i)] = as.numeric(dataframe[,j] == i)
        }
        
      }
      
    }
  }
  
  dataframe <- select(dataframe, -ids)
  return(as_tibble(dataframe))
}

#etc.