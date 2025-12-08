#' This function takes two layers of raster data to compare to the site values.
#'
#' A good indicator of representative site selection is broad coverage of the mean, min, and max raster data.
#'
#' Input the raster stack - output of rast_stck() - and site values - output of site_val().
#'
#' 'layer1' and 'layer2' refers to the y and x axis of the plot and the subset of layers used in the raster stack.

vis_rep<-function(values, stack, layer1, layer2){
  plot(stack[[layer1]], r_stack[[layer2]], col="lightgray")
  points(values[,layer1], values[,layer2], col="red", pch=19, cex=1.0)
}
