#' This function takes vectorized spatial data (e.g., points and lines).
#'
#' The vector is cropped to a desired extent - either from the sample_ext() function or an existing polygon.
#'
#' The vector is projected to




vect_to_rast <- function(vect, extent, res){
  obj<-sf::st_crop(vect, extent)
  rast_obj<-rast(st_as_sf(obj), res=res)
  euclid_obj<- distance(rasterize(obj, rast_obj))
  plot(euclid_obj)
  return(euclid_obj)
}

