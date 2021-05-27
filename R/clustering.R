#' Our implementation of principal component analysis (PCA)
#'
#' Given an data matrix x, a linear projection is applied to maximise sample variation.
#' The first two prinicpal components are returned.
#'
#' @param x numeric matrix of data, where rows are samples
#' @return a list containing a matrix with the princple components and a vector containing the weight of each principle component
#' @export
pca <- function(x) {
  S <- scale(x)

  EP = eigen(var(S))
  PC = as.matrix(S) %*% as.matrix(EP$vectors)

  # pcw1 <- EP$values[1] / sum(EP$values)
  # pcw2 <- EP$values[2] / sum(EP$values)

  energy <- sapply(EP$values, function(x) x / sum(EP$values))

  out <- list(data.frame(PC), energy)
  names(out) <- c("PC", "energy")

  return(out)
}



#' Our implementation of the k-means algorithm
#' 
#' Given an data matrix x, samples are clustered into a given number of groups.
#' 
#' @param x numeric matrix of data, where rows are samples
#' @param centers the number of groups
#' @return returns a list containing: a vector (called clusters) that contains the clusters each row belongs to and a vector (called wcss) containing the within-cluster sum of squares for each cluster.
#' @export
k_means <- function(x, centers = 5) {
  if (centers == 1) {
    mean <- colMeans(x)
    # calculate wcss
    wcss <- 0
    for (i in 1:nrow(x)) {
      wcss <- wcss + sqrt(sum((x[i, ] - mean)^2))
    }

    return(list(clusters = rep(1, nrow(x)), wcss = wcss))
  }

  n <- nrow(x)

  centroid <- x[sample(n,centers),]
  dist_to_centroid <- matrix(NA,ncol=centers,nrow=n)

  centroid_new <- matrix(0,nrow=centers,ncol=ncol(x))

  while(all(centroid != centroid_new)){
    centroid_new <- centroid

    for(i in 1:n){
      for(j in 1:centers){
        dist_to_centroid[i,j] <- sqrt(sum((x[i,] - centroid[j, ])^2))
      }
    }
    
    cluster <- rep(NA,n)
    
    for(i in 1:n){
      cluster[i] <- which.min(dist_to_centroid[i,])
    }
    
    for(i in 1:centers){
      centroid[i,] <- colMeans(x[which(cluster==i),])
    }
  }

  # calculate wcss
  wcss <- rep(0, centers)
  for (i in 1:nrow(x)) {
    wcss[cluster[i]] <- wcss[cluster[i]] + sqrt(sum((x[i, ] - centroid[cluster[i], ])^2))
  }

  out <- list(clusters = cluster, wcss = wcss)

  return(out)
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

  clusters <- k_means(Z, centers = k)
  
  return(clusters)
}