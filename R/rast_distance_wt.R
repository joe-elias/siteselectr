#' modified version of rast_distance() and the wmahalanobis function from the WMDB package (Wu, 2012).

#' 'weighted_layer' refers to the layer(s) in the raster stack to be weighted.
#' use c() to combine multiple layers before using them in the 'weighted layer' parameter.
#'
#' 'magnitude' should be a weight value assigned to focal layers - relative to all other layers which are by default weighted at 0.001.
#' A large weight value would be >0.5.


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
