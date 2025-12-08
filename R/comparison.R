#' Create a comparison  matrix to use in the site select function.
#'
#' Produces a matrix of the original cell values except those chosen already for sampling sites.
#'
#' 'df' refers to the raw data matrix of combined raster layers.
#'
#' 'cells' refers to the cells already chosen.

comparison<-function(data, cells){
  comp<-data[-c(cells), ]
  return(comp)
}

