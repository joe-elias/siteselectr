#' This function makes a simple correlation plot to visualize the collinearity env. variables.
#'
#' Input is the site values produced from the site_val() function.
#'
#' Output of cor_plot() is a matrix of correlation values for the raster data and a heat map visualizing environmental collinearity between site values.

vis_cor<-function(values){
  cor_val<-cor(values)
  palette <- colorRampPalette(c("#56B4E9", "white", "orangered"))(100)

  heatmap(cor_val,
          col = palette,
          symm = TRUE, # Ensures symmetry for correlation matrix
          margins = c(8, 8))
  return(cor_val)
}




