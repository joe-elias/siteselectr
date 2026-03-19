#' This function collects a set of rasters into a list.
#'
#' @param ... any number of raster images.
#' @return a list of rasters.
#'
#' @export

rast_collect<-function(...){
  r_list<-list(...)
  return(r_list)
}
