#' Choose the maximum different site to the starting location.
#'
#' Using an x, y coordinate - chooses the cell from the original distance matrix with the highest dissimilarity to the starting location.
#'
#' 'matrix' refers to the output from the rast_mat() function - the raster stack resampled and in matrix-value form.
#'
#' See: first_distance() function for original distance matrix.
#'

max_cell<-function(distance_matrix){

  cell.dat<-distance_matrix

  max.diff<-which.max(cell.dat)

  return(max.diff)
}


