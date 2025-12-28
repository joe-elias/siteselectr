#' similar to cell_wrap, xy_wrap takes a cell and produces a set of coordinates that are usable for site_select()
#'

cell_wrap<-function(cell, raster_stack){
  xy<-terra::XYFromcell(raster_stack, cell)
}
