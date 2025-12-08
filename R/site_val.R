#' Creates a matrix with the values of the sites at each layer of raster data.
#'
#' The matrix is the correct format to use in ggbiplot which is the suggested visual to view representativeness of sites relative to the raw raster data.
#'
#' Input is 'sites', the site coordinates from the plot_sites() function.
#'
#' 'Stack' refers to the output of rast_stack().

site_val<-function(sites, stack){
  site_val<-data.frame(terra::extract(stack, sites))
  sites_mat<-matrix()
  return(site_val)
}
