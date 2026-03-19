#' Modified version of new_distance function, based on the wmahalanobis function in the WMDB package (Wu, 2012).
#' Used within the site_select_wt() function.
#'
#' @param stack raster stack.
#' @param weighted_layer a single or mutliple numbers refering to the subset of the raster stack (which layer) to be weighted.
#' @param cells selected cells, 'dat' to the raster matrix.
#' @param magnitude a weight 0.1-1.0 to place on the weighted layer.
#' All other layers are set to 0.001. Weighted layer is weighted proportionally to other layers.
#'
#' @return New distance metrics measured between the original distance matrix - see rast_distance_wt() function - and the original distance matrix minus already selected cells.
#'
#' Example:
#' weighted_cells<-site_select_wt(long=12, lat=10, num=15, stack=r_stack, weighted_layer = c(1, 3), magnitude = 0.5)
#'
#' @export

new_distance_wt<- function(stack, weighted_layer, cells, magnitude, dat) {
  dat <- as.matrix(stack)

  # create weight vector
  t <- rep(0.001, ncol(dat))

  # apply weights to specific layers
  for (n in unique(weighted_layer)){
    t[n]<-t[n]+magnitude
  }

  weight<-diag(length(t))
  diag(weight)<-t

  ref<-stack[[1]]

  # define baseline and comparison groups
  baseline <- dat[c(cells), ]
  comparison <- dat[-c(cells), ]

  weight <- weight

  # calculate distance matrix
  center<-colMeans(baseline, na.rm=T)
  cov <- var(baseline, na.rm=T) + diag(0.01, nrow(var(baseline, na.rm=T)))

  new_matrix_wt <- wt_distance(comparison, center, cov, weight)

  return(new_matrix_wt)
}
