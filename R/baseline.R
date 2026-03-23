#' Create a available matrix to use in the site select function.
#'
#' @param data raw data matrix of combined raster layers.
#'
#' @param cells refers to the cells already chosen.
#' @return available matrix of the original raster stack data.
#' @export

available<-function(data, cells){
  base<-data[c(cells), ]
  return(base)
}
