#' This function wraps two numeric values into a spatial object and returns a cell value corresponding to sampling extent
#'

cell_wrap<-function(x, y, raster_stack){
  x<-x
  y<-y
  xy1<-cbind(x, y)
  cell1<-terra::cellFromXY(raster_stack[[1]], xy1)
}
