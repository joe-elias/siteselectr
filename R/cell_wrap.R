#' This function wraps two numeric values into a spatial object and returns a cell value corresponding to sampling extent
#' @param x latitude value
#' @param y longitude value
#' @param raster_stack raster stack.
#'
#' @return a cell value corresponding to a raster stack from a given set of coordinates.
#' @export

cell_wrap<-function(x, y, raster_stack){
  x_val<-x
  y_val<-y
  xy1<-cbind(x_val, y_val)
  cell1<-terra::cellFromXY(raster_stack[[1]], xy1)
}

getAnywhere(cell_wrap)
find("cell_wrap")
