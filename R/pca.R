#' PCA
#' 
#' @param x First value
#' @param sigma
#' @return PC1 and 2
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

