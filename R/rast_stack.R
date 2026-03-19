#' Converts the resampled raster list from the rast_match function into a single geospatial object.
#'
#' @param resampled The resampled rasters from the rast_match function.
#' @param extent The vectorized polygon from the sample_ext function to crop the raster stack. description.
#'
#' @export

rast_stack<-function(resampled, extent){
  r_stack<-terra::rast(resampled)
  r_stack_clip<-terra::mask(r_stack, extent)
  r_stack_trim<-terra::trim(r_stack_clip)
}
