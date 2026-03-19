#' This function creates a matrix with the values of the sites at each layer of raster data.
#'
#' @param sites the site coordinates from the plot_sites() function.
#'
#' @param stack raster stack.
#' @return the values of each variable in the raster stack at each selected cell during site selection.
#' @export

site_val<-function(sites, stack){
  site_val<-data.frame(terra::extract(stack, sites))
  sites_mat<-matrix()
  return(site_val)
}
