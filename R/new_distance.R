#' This function creates a new matrix using the baseline (with selected cells) and comparison (without selected cells) matrices.
#'
#' @param data the raw data matrix of combined raster layers.
#'
#' @param cells refers to the cells already selected, or legacy sites that might exist and converted to cells that correspond to the raster stack.
#' @return a distance matrix that measures the dissimilarity between the original distance matrix - see rast_distance() function - and the original distance matrix minus already selected cells.
#' @export

new_distance<-function(data, cells){
  baseline<-baseline(data, cells)
  comparison<-comparison(data, cells)

  reg.cov<-cov(baseline)+diag(0.01, nrow(cov(baseline)))

  new_matrix<-stats::mahalanobis(comparison, apply(baseline, 2, mean, na.rm=F), reg.cov)
}
