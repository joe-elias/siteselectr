#' modified version of new_distance function, based on the wmahalanobis function in the WMDB package (Wu, 2012).

#' 'cells' refers to the already selected cells, 'dat' to the raster matrix.

new_distance_wt<- function(cells, dat) {

  ref<-stack[[1]]

  baseline <- dat[c(cells), ]
  comparison <- dat[-c(cells), ]

  # calculate distance matrix
  center<-colMeans(baseline, na.rm=T)
  cov <- var(baseline, na.rm=T) + diag(0.01, nrow(var(baseline, na.rm=T)))

  new_matrix_wt <- wt_distance(comparison, center, cov, weight)

  return(new_matrix_wt)
}
