#' Create a comparison  matrix to use in the site select function.
#'
#' Produces a matrix of the original cell values except those chosen already for sampling sites.
#'
#' @param data data matrix of combined raster layers.
#'
#' @param cells refers to the cells already chosen.
#'
#' @return a comparison matrix for the site_select() function. A matrix of data param with selected cells excluded.
#' @export

comparison<-function(data, cells){
  comp<-data[-c(cells), ]
  return(comp)
}

