#' This is an optional function to visualize the initial dissimilarity present in the first_distance() function.
#'
#' The function takes a reference_raster to fill with the values from the distance matrix.
#' The function uses a random sublayer of the raster stack (e.g., r_stack[[]]) to ensure the distance matrix is projected to the correct resolution and extent.
#' @param raster_stack raster stack.
#' @param dist_mat distance matrix.
#' @return a heat map of distance values visually projected onto the spatial extent of the supplied rasters.
#' @export


heat_distance<-function(raster_stack, dist_mat){

  ref_layer<-r_stack[[1]]
  values(ref_layer) <- NA

  mahal_raster <- ref_layer
  values(mahal_raster) <- dist_mat

  mahal_trim<-terra::trim(mahal_raster)
}
