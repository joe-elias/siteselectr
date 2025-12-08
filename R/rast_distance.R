#' The rast_distance() function measures the initial dissimilarity present in the raster stack data without considering any chosen sampling sites.
#'
#' This function uses the Mahalanobis distance calculation to measure multivariate dissimilarity.
#'
#' 'data' refers to the product of the rast_matrix() function - the matrix of numeric values extracted from the stack of raster images.
#'
#' The covariance matrix included in the distance calculation uses an added diagonal matrix of 0.01 to avoid singularity issues.
#'
#' The product from the first_distance() function should be applied to the __ funciton.

rast_distance<-function(data){
  cov_matrix <- cov(data, use="pairwise.complete.obs") +
    diag(0.01, ncol(data))
  center_mean <- colMeans(data, na.rm = TRUE)

  distance_matrix <- mahalanobis(data, center_mean, cov_matrix)
  distance_matrix <- as.numeric(distance_matrix)
  return(distance_matrix)
}
