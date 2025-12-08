#' This is an optional function to visualize the initial dissimilarity present in the first_distance() function.
#'
#' The function takes a reference_raster to fill with the values from the distance matrix.
#'
#' Input the assigned name to the raster stack - the output of rast_match() function.
#'
#' The function uses a random sublayer of the raster stack (e.g., r_stack[[]]) to ensure the distance matrix is projected to the correct resolution and extent.
#'
#' The output is a heat map of distance values visually projected onto the spatial extent of the supplied rasters.

heat_distance<-function(stack){

  ref_layer<-r_stack[[1]]
  values(ref_layer) <- NA

  mahal_raster <- ref_layer
  values(mahal_raster) <- distance_matrix

  mahal_trim<-terra::trim(mahal_raster)

  plot(mahal_trim)
}
