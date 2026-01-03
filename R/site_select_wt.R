#' Choose a set of sites from the new distance matrix.
#' see comparison(), baseline(), & new_distance() functions.
#'
#' 'num' refers the number of rounds of site selection added to existing cells/sites.
#'
#' 'cells' refers to the starting location and cell with the highest dissimilarity
#'
#' 'stack' refers to the output from the rast_stack() function - the raster stack resampled to the same extent and resolution.
#'
#' 'weighted layer' is the layer number in the raster stack to be weighted. This can be multiple numbers in c().
#'
#' if magnitude = NULL, default is 0.1 weight.

site_select_wt <- function(long, lat, num, stack, weighted_layer, magnitude){
  # 1. extract matrix and make ref layer
  dat <- as.matrix(stack)
  ref<-stack[[1]]
  values(ref)<-NA

  # 2. first cell
  cells<-terra::extract(stack, cbind(long, lat))

  # 3. initial weighted matrix
  wt_mat<- rast_distance_wt(stack, weighted_layer, magnitude)

  # 4. choose most dissimilar site
  cell2<-max_cell(wt_mat)
  cells<-c(cell1, cell2)

  # 5. calculate new matrix and choose a new cell - repeat for desired iterations
  n=num-length(cells)

  for (i in 1:n){
    new_dist<-new_distance_wt(cells=cells, dat=dat)
    tmp.r<-as.matrix(ref)
    tmp.r[!is.na(tmp.r)] <- NA
    tmp.r[-c(cells)] <- new_dist

    max.diff<-which.max(tmp.r)

    cells<-c(cells, max.diff)
  }
  return(cells)
}
