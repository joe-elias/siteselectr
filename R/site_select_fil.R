#' This function is a modified version of site_select that is capable of masking to a set of vectorized polygons within the sampling extent (e.g., public land).
#' Masking occurs either before or after the Mahalanobis calculation.
#'
#' @param long longitude value for the first site.
#' @param lat latitude value for the first site.
#'
#' @param num number of sites to create the network.
#' @param stack raster stack of raster layers. Product of the rast_stack function.
#'
#' @param type mask data either 'after' or 'before' the initial Mahalanobis distance calculation.
#'
#' @return list of selected cells.
#' @export

site_select_fil<-function(long, lat, num, stack, filter, type){

  if(type == "before"){
    # mask before MD
    stack <- mask(stack, filter)
    data <-as.matrix(stack)
    dm <- rast_distance(data)

    # resume workflow for "before" MD filtering
    # create ref layer
    ref<-stack[[1]]
    values(ref)<-NA

    # first site selection
    cell1<-terra::cellFromXY(ref, cbind(long, lat))
    cell2<-max_cell(dm)
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

  if(type == "after"){

    data <- as.matrix(stack)
    dm <- rast_distance(data)

    # mask after MD
    r <- stack[[1]] * 0
    r <- rasterize(filter, r, background = 0, field = 1)
    r_val <- values(r)

    dm <- dm * r_val
    dm[dm==0]<-NA


    # create ref layer
    ref<-stack[[1]]
    values(ref)<-NA

    # first site selection
    cell1<-terra::cellFromXY(ref, cbind(long, lat))
    cell2<-max_cell(dm)
    cells<-c(cell1, cell2)

    n<-num-length(cells)

    # use cell 1 and 2 to pick other sites, each iteration will calculate a new distance matrix between chosen and unchosen cells.
    for (i in 1:n){
      new_dist<-new_distance_fil(data = data,
                                 cells = cells,
                                 filter_val = r_val)

      tmp.r<-as.matrix(ref)
      tmp.r[!is.na(tmp.r)] <- NA
      tmp.r[-c(cells)] <- new_dist

      max.diff<-which.max(tmp.r)

      cells<-c(cells, max.diff)
    }
    return(cells)
  }
}

