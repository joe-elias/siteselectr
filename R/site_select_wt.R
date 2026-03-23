#' This function chooses a set of sites from the new distance matrix.
#'
#' @param num the number of rounds of site selection added to existing cells/sites.
#'
#' @param long the starting location.
#' @param lat the starting location.
#'
#' @param stack refers to the output from the rast_stack() function - the raster stack resampled to the same extent and resolution.
#'
#' @param weighted_layer is the layer number in the raster stack to be weighted. This can be multiple numbers in c().
#'
#' @param magnitude if magnitude = NULL, default is 0.1 weight.
#' @export

site_select_wt <- function(long, lat, num, stack, weighted_layer, magnitude){

  # 1. extract matrix and make ref layer
  dat <- as.matrix(stack)
  ref<-stack[[1]]
  values(ref)<-NA

  # 2. first cell
  cell1<-cellFromXY(stack[[1]], cbind(long, lat))

  # 3. initial weighted matrix
  wt_mat<- rast_distance_wt(stack=stack,
                            weighted_layer = weighted_layer,
                            magnitude =magnitude)

  # 4. choose most dissimilar site
  cell2<-max_cell(wt_mat)
  sites<-c(cell1, cell2)

  # 5. calculate new matrix and choose a new cell - repeat for desired iterations
  n=num-length(sites)

  for (i in 1:n){
    new_dist<-new_distance_wt(stack=stack,
                              weighted_layer=weighted_layer,
                              cells=sites,
                              magnitude=magnitude)

    tmp.r<-as.matrix(ref)
    tmp.r[!is.na(tmp.r)] <- NA
    tmp.r[-c(sites)] <- new_dist

    max.diff<-which.max(tmp.r)

    sites<-c(sites, max.diff)

  }

  return(sites)

}
