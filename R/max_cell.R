#' This function chooses the maximum different site to the starting location based on the MD.
#'
#' @param distance_matrix calculated from rast_distance() function.
#' @return a cell in the distance matrix with maximum dissimilarity (i.e., highest value).
#' @export

max_cell<-function(distance_matrix){

  cell.dat<-distance_matrix

  max.diff<-which.max(cell.dat)

  return(max.diff)
}


