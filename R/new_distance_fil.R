#' This function creates a new matrix using the available (with selected cells) and selected (without selected cells) matrices.
#' Modified for the filter function - site_select_fil - where the resulting dm is masked by the filter layer.
#'
#' @param data the raw data matrix of combined raster layers.
#'
#' @param cells refers to the cells already selected, or legacy sites that might exist and converted to cells that correspond to the raster stack.
#' @return a distance matrix that measures the dissimilarity between the original distance matrix - see rast_distance() function - and the original distance matrix minus already selected cells.
#'
#' @export

new_distance_fil<-function(data, cells, filter_val){
  available<-available(data, cells)
  selected<-selected(data, cells)

  reg.cov<-cov(available)+diag(0.01, nrow(cov(available)))

  new_matrix<-stats::mahalanobis(selected, apply(available, 2, mean, na.rm=F), reg.cov)

  filter_new <- filter_val[-cells]

  new_matrix<-new_matrix*filter_new
  new_matrix[new_matrix==0]<-NA

  return(new_matrix)
}
