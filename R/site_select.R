#' Choose a set of sites from the new distance matrix.
#' see comparison(), baseline(), & new_distance() functions.
#'
#' 'num' refers the number of rounds of site selection.
#'
#' 'cells' refers to the starting location and cell with the highest dissimilarity
#'
#' 'stack' refers to the output from the rast_stack() function - the raster stack resampled to the same extent and resolution.

site_select<-function(long, lat, num,
                      data, stack){

  # create ref layer
  ref<-stack[[1]]
  values(ref)<-NA

  # first site selection
  cell1<-terra::cellFromXY(ref, cbind(long, lat))
  cell2<-max_cell(first_distance(data))
  cells<-c(cell1, cell2)

  # use cell 1 and 2 to pick other sites, each iteration will calculate a new distance matrix between chosen and unchosen cells.
  for (i in 1:num){
    new_dist<-new_distance(data, cells)
    tmp.r<-as.matrix(ref)
    tmp.r[!is.na(tmp.r)] <- NA
    tmp.r[-c(cells)] <- new_dist

    max.diff<-which.max(tmp.r)

    cells<-c(cells, max.diff)
  }
  return(cells)
}

