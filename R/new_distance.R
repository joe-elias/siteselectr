#' This function creates a new matrix using the available (with selected cells) and selected (without selected cells) matrices.
#'
#' @param data the raw data matrix of combined raster layers.
#'
#' @param cells refers to the cells already selected, or legacy sites that might exist and converted to cells that correspond to the raster stack.
#' @return a distance matrix that measures the dissimilarity between the original distance matrix - see rast_distance() function - and the original distance matrix minus already selected cells.
#' @export

new_distance<-function(data, cells){
  available<-available(data, cells)
  selected<-selected(data, cells)

  reg.cov<-cov(available)+diag(0.01, nrow(cov(available)))

  new_matrix<-stats::mahalanobis(selected, apply(available, 2, mean, na.rm=F), reg.cov)
}
