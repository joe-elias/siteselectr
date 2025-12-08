#' Create a baseline matrix to use in the site select function.
#'
#' Produces a matrix of the original cell values.
#'
#' df' refers to the raw data matrix of combined raster layers.
#'
#' 'cells' refers to the cells already chosen.

baseline<-function(data, cells){
  base<-data[c(cells), ]
  return(base)
}
