#' This function takes a classified logical raster, like land cover data (e.g., NLCD), masks a desired class, and converts it to a continuous data.
#'
#' 'cls' is the class values for the user's desired variable.
#' For instance, a single value (e.g, 21) could represent low urban cover in a classified logical raster.
#' Or, enter c(21, 22, 23) for 'cls' to mask low, medium, and high urban cover under a single raster object.
#'
#' The 'extent' parameter crops the logical raster to a desired spatial constraint, either from the sample_ext() function or another vector boundary.
#'
#' Create a raster template for the 'shell' parameter that includes a desired number of columns and rows, extent, and crs.
#' To easily create a raster template copy information from an existent raster.
#' Example - for raster 'x' :
#' shell = rast(ncol=ncol(x), nrow=nrow(x), ext = ext(x), crs = crs(x))
#'
#' The proportion of the occurrence of the raster is calculated in each cell of the raster template.


log_to_prop <- function(rast, cls, extent, shell){
  terra::crop(rast, extent)
  obj<-rast %in% cls
  obj_proj<-project(obj, shell)
  return(obj_proj)
}
