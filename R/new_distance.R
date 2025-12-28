#' Create a new matrix using the baseline (with chosen cells) and comparison (without chosen cells) matrices.
#'
#' calculates Mahalanobis distance between a comparison and baseline set of cells.
#'
#' 'data' refers to the raw data matrix of combined raster layers.
#'
#' 'cells' refers to the cells already chosen
#'

new_distance<-function(data, cells){
  baseline<-baseline(data, cells)
  comparison<-comparison(data, cells)

  reg.cov<-cov(baseline)+diag(0.01, nrow(cov(baseline)))

  new_matrix<-stats::mahalanobis(comparison, apply(baseline, 2, mean, na.rm=F), reg.cov)
}
