#' This function converts the resampled raster list into a geospatial object.
#'
#' Input the product of the raster_match() function.

rast_stack<-function(resampled, extent){
  r_stack<-terra::rast(resampled)
  r_stack_clip<-terra::mask(r_stack, extent)
  r_stack_trim<-terra::trim(r_stack_clip)
}
