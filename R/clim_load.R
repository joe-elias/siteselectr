#' this function takes NetCDF data which contains monthly means condensed into a single file.
#' clim_load() takes an nc file and uses climetric to calculate annual means for the geospatial analysis in other function of SiteSelectR.
#' the extent parameter refers to the sampling extent - see the sample_ext() function.
#' the extent crops new raster of annual averages to the desired sampling extent.
#'
#' only works with the climetric package:
#' devtools::install_github("elizagrames/climetric")
#'
#' returns a single cropped object of mean annual climate that can be saved as a TIFF file for further geospatial analysis.

clim_load<-function(nc, extent){
x<-climetric::brick_nc(nc)
ext.region<-raster::extent(x)
dim_region<-dim(x)[1:2]
raster_mean <- climetric::extract_data(x, "mean", "annual",
                                       writefile = FALSE, layer_type = "month")
mean_raster <- rast(array(as.numeric(raster_mean$annual$mean), dim = dim_region),
                    extent = ext.region, crs = "+proj=longlat +datum=WGS84 +no_defs")

# Align and crop raster with sample_extent
poly <- st_transform(polygon_sf, crs(mean_raster))
crop_raster <- crop(mean_raster, poly) %>%
  project(.,"EPSG:4326")  # Ensure CRS consistency

plot(crop_raster)
return(crop_raster)
}
