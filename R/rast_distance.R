#' This function measures the initial dissimilarity present in raster data without considering any chosen sampling sites. Used in the site_select() function.
#'
#' This function uses the Mahalanobis distance (MD) calculation to measure multivariate dissimilarity.
#'
#' @param data the product of the rast_matrix() function - the matrix of numeric values extracted from the stack of raster images.
#'
#' The covariance matrix included in the distance calculation uses an added diagonal matrix of 0.01 to avoid singularity issues.
#'
#' @return a MD distance matrix measuring dissimilarity of a raster stack.
#' @export

rast_distance<-function(data){
  cov_matrix <- cov(data, use="pairwise.complete.obs") +
    diag(0.01, ncol(data))
  center_mean <- colMeans(data, na.rm = TRUE)

  distance_matrix <- mahalanobis(data, center_mean, cov_matrix)
  distance_matrix <- as.numeric(distance_matrix)
  return(distance_matrix)
}
