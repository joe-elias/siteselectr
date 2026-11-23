#' This function takes vectorized spatial data (e.g., points and lines) and converts it to a raster image.
#'
#' @param vect the vector (line or point)
#' @param extent either from the sample_ext() function or an existing polygon. Crops the vect parameter.
#' @param res the desired resolution that the vector is rasterized.
#'
#' @return a 'distance to vector" raster where each raster cell is the distance from the nearest line or point.
#' @export


vect_to_rast <- function(vect, extent, res){
  obj<-sf::st_crop(vect, extent)
  rast_obj<-rast(st_as_sf(obj), res=res)
  euclid_obj<- distance(rasterize(obj, rast_obj))
  plot(euclid_obj)
  return(euclid_obj)
}

