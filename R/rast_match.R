#' Align raster data to the same extent and resolution.
#'
#' 'reference_layer' refers to the raster that everything will be projected.
#' 'extent' refers to the spatial boundary that the rasters will be cropped. This is the output of sample_ext().
#'
#' Rasters need to be aligned and projected to the same resolution and CRS before condensing into a raster stack and calculating a distance matrix.


rast_match<-function(raster_list, reference_layer, extent){
  lapply(raster_list, function(x){
    extent <- st_transform(extent, crs(x))
    crop_rast <- project(crop(x, extent), "EPSG:4326")

    crs(crop_rast) <- "EPSG:4326"

    rast_resampled <- terra::resample(crop_rast, reference_layer)
  })
}
