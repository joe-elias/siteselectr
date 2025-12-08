#' Used to collect a set of rasters into a list to be used in the smooth_raster() function.
#'
#' Input any variable number of rasters by their assigned object name.

rast_collect<-function(...){
  r_list<-list(...)
  return(r_list)
}
