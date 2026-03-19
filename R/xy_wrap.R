#' similar to cell_wrap, xy_wrap takes a cell and produces a set of coordinates that are usable for site_select()
#'
#'@param cell a list of cells.
#'@param raster_stack raster stack that corresponds to the geographic coordinates of the cells.
#'@return list of geographic coordinates corresponding to the cells in the raster stack.
#'
#'@export

xy_wrap<-function(cell, raster_stack){
  xy<-terra::xyFromcell(raster_stack, cell)
}
