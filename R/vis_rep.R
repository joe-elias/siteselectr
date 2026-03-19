#' This function takes two layers of raster data to compare to the site values.
#' A good indicator of representative site selection is broad coverage of the mean, min, and max raster data.
#'
#' @param values output of the site_val() function. The values for each raster layer at each selected site.
#' @param stack raster stack.
#'
#' @param layer1 first layer to compare. Input a number corresponding to a subset of the raster stack.
#' @param layer2 second layer to compare.  Input a number corresponding to a subset of the raster stack.
#'
#' @return scatter plot of raw raster data with the selected sites overlaid.
#' @export

vis_rep<-function(values, stack, layer1, layer2){
  plot(stack[[layer1]], stack[[layer2]], col="lightgray")
  points(values[,layer1], values[,layer2], col="red", pch=19, cex=1.0)
}
