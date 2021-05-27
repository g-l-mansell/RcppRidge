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


plot_monthly_demand <- function(){
  
}

#etc.