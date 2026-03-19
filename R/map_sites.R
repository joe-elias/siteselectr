#' Uses the cells produced from site_select and plots them on the first layer of
#' the raster stack.
#'
#' @param cells list of cells produced from site_select.
#' @param stack raster stack data.
#' @return a plot of cells chosen from site_select.
#'
#' @export


map_sites<-function(cells, stack){
  site_xy<-terra::xyFromCell(stack[[1]], cells)

  plot(stack[[1]])
  points(site_xy, col='red', pch=19, add=T)

  return(site_xy)
}
