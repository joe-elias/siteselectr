#' This function is a modified version of rast_distance() and the wmahalanobis function from the WMDB package (Wu, 2012).

#' @param weighted_layer the layer(s) in the raster stack to be weighted - numbers corresponding to a subset of the raster stack.
#' use c() to combine multiple layers before using them in the 'weighted layer' parameter.
#'
#' @param magnitude should be a weight value assigned to focal layers - relative to all other layers which are by default weighted at 0.001.
#' A large weight value would be >0.5.
#'
#' @param stack raster stack.
#'
#' @return a measure of the initial dissimilarity present in raster data without considering any chosen sampling sites.
#' Example:
#' rast_distance_wt(stack=r_stack, weighted_layer=c(1, 3), magnitude=0.5)
#'
#' @export


rast_distance_wt <- function(stack, weighted_layer, magnitude){
  # 1. initialize weight matrix and raster matrix
  dat <- as.matrix(stack)

  t <- rep(0.001, ncol(dat))

  # 2. apply weights to specific layers
  for (n in unique(weighted_layer)){
    t[n]<-t[n]+magnitude
  }

  weight<-diag(length(t))
  diag(weight)<-t

  # 3. weighted mahalanobis calculation
  cov=cov(dat, use="pairwise.complete.obs") +
    diag(0.01, ncol(dat))
  center=colMeans(dat, na.rm = TRUE)
  distance_matrix <- wt_distance(dat, center, cov, weight)
  return(distance_matrix)
}
