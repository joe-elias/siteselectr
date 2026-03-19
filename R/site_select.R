#' This function creates a network of field sites based on multivariate GIS data and a starting
#' location.
#'
#' @param long longitude value for the first site.
#' @param lat latitude value for the first site.
#'
#' @param num  number of sites to create the network.
#'
#' @param stack raster stack of raster layers. Product of the rast_stack function.
#'
#' @return list of selected cells.
#' @export

site_select<-function(long, lat, num, stack){

  # create a matrix from the raster stack
  data<-as.matrix(stack)

  # create ref layer
  ref<-stack[[1]]
  values(ref)<-NA

  # first site selection
  cell1<-terra::cellFromXY(ref, cbind(long, lat))
  cell2<-max_cell(rast_distance(data))
  cells<-c(cell1, cell2)

  n<-num-length(cells)

  # use cell 1 and 2 to pick other sites, each iteration will calculate a new distance matrix between chosen and unchosen cells.
  for (i in 1:n){
    new_dist<-new_distance(data, cells)
    tmp.r<-as.matrix(ref)
    tmp.r[!is.na(tmp.r)] <- NA
    tmp.r[-c(cells)] <- new_dist

    max.diff<-which.max(tmp.r)

    cells<-c(cells, max.diff)
  }
  return(cells)
}

