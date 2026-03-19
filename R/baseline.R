#' Create a baseline matrix to use in the site select function.
#'
#' @param data raw data matrix of combined raster layers.
#'
#' @param cells refers to the cells already chosen.
#' @return baseline matrix of the original raster stack data.
#' @export

baseline<-function(data, cells){
  base<-data[c(cells), ]
  return(base)
}
