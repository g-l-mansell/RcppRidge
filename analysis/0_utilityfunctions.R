# Place to store useful functions for the project, that are too specific to be added to the package

#Our feature transform for the dataset of demand + extra
#'@param X dataframe (so named columns can be easily added)
#'@param factors list of columns to be treated as factors, all other columns are treated as numeric
#'@return data matrix of scaled transform variables
feature_transform <- function(X, factors=NULL){
  
  if(!is.null(factors)){
    cols <- which(!colnames(X) %in% factors)
  } else {
    cols <- 1:ncol(X)
  }

  #(not adding columns if correlation is >90%)
  for(i in cols){
    name <- colnames(X)[i]
    
    #add col squared
    col <- paste0(name,"^2")
    if(cor(X[,i], X[,i]^2) < 0.9) X[,col] <- X[,i]^2
    
    #add col cubed
    col <- paste0(name,"^3")
    if(cor(X[,i], X[,i]^3) < 0.9) X[,col] <- X[,i]^2
    
    #add first order interactions
    for(j in cols[cols>i]){
      col <- paste0(name, colnames(X)[j])
      if(cor(X[,i], X[,j]) < 0.9) X[,col] <- X[,i]*X[,j]
    }
  }
  
  ## Standardise and split training and test
  X <- as.matrix(X)
  X <- scale(X)
  return(X)
}

#our feature transform for the dataset of demand + extra + surv
feature_transform_surv <- function(X){
  
  
}

plot_monthly_demand <- function(){
  
}

#etc.