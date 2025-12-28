#' Used to collect a set of rasters into a list.
#'
#' Input any number of raster objects and combine them using list().

rast_collect<-function(...){
  r_list<-list(...)
  return(r_list)
}
