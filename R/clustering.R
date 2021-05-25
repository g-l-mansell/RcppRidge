#' Our implementation of principal component analysis (PCA)
#' 
#' Given an data matrix x, a linear projection is applied to maximise sample variation.
#' The first two prinicpal components are returned.
#' 
#' @param x numeric matrix of data, where rows are samples
#' @param sigma 
#' @return dataframe of sample projections onto PC1 and PC2
#' @export
pca <- function(x, sigma = 1.5) {
  nodes <- dim(x)[1]
  
  dmat <- dist(x)
  
  S <- apply(dmat, 1:2, function(x) x / sigma)
  
  S_degree <- rowSums(S)
  normalised_Laplacian <- diag(S_degree^(-1/2)) %*% S %*% diag(S_degree^(-1/2))
  
  ev <- eigen(normalised_Laplacian)
  ev_vector <- ev$vectors[,1:3]
  
  for(i in 1:nodes){
    ev_vector[i,] <- ev_vector[i,] / sqrt(sum(ev_vector[i,]^2))
  }
  
  EP=eigen(var(X))
  PC=as.matrix(X) %*% as.matrix(EP$vectors)
  
  # Return the first two principle components for ploting
  return(data.frame(PC[, 1], PC[, 2]))
}



#' Our implementation of the k-means algorithm
#' 
#' Given an data matrix x, samples are clustered into a given number of groups.
#' 
#' @param x numeric matrix of data, where rows are samples
#' @param centers the number of groups
#' @return vector of integers indicating the group allocations
#' @export
k_means <- function(x, centers = 5) {
  n <- nrow(x)
  
  centroid <- x[sample(n,centers),]
  dist_to_centroid <- matrix(NA,ncol=centers,nrow=n)
  
  centroid_new <- matrix(0,nrow=centers,ncol=ncol(x))
  
  while(all(centroid != centroid_new)){
    centroid_new <- centroid
    
    for(i in 1:n){
      for(j in 1:centers){
        dist_to_centroid[i,j] <- sqrt(sum((x[i,] - centroid[j,])^2))
      }
    }
    
    category <- rep(NA,n)
    
    for(i in 1:n){
      category[i] <- which.min(dist_to_centroid[i,])
    }
    
    for(i in 1:centers){
      centroid[i,] <- colMeans(x[which(category==i),])
    }
  }
  return(category)
}

#' Our implementation of spectral clustering
#' 
#' Given a data matrix x, samples are clustered into k groups using a spectral (eigen-) decomposition of the graph Laplacian.
#' Uses the implementation of kmeans from this package `k_means`.  
#' 
#' @param x numeric matrix of data, where rows are samples
#' @param c 
#' @param k the number of groups
#' @return vector of groups
#' @export
spectralClustering <- function(x, c = 1, k = 10) {
  
  # Create distance matrix
  d <- as.matrix(dist(x, method="euclidean"))
  
  # Create similarity matrix from the distance matrix
  S <- apply(d, 1:2, function(x) exp(-x^2/c))
  
  # Sum over columns
  g <- apply(S, 2, sum)
  
  # Laplacian
  L <- diag(g) - S
  
  L.eig <- eigen(L)
  p <- length(L.eig$values)
  
  Z <- L.eig$vectors[, 1:k]
  #Z <- L.eig$vectors[, max(c((p - k + 1), 0)):p]

  clusters <- k_means(Z, centers = k)
  
  return(clusters)
}
