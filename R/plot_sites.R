#' This function uses the cells chosen in the site_select() function.
#'
#' Converts cells to geographic coordinates and plots on the first layer of the raster stack.
#'
#' 'cells' refers to the output of site_select().
#'
#' 'stack', refers to the raster stack used as a reference to extract the geographic locations.


plot_sites<-function(cells, stack){
  site_xy<-terra::xyFromCell(stack[[1]], cells)

  plot(stack[[1]])
  points(site_xy, col='red', pch=19)
  return(site_xy)

}
