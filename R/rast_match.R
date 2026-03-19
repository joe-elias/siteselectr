#' Resample raster data to the same extent and resolution.
#'
#' @param rast_list List of rasters from the rast_collect function.
#' @param reference_layer A target layer for resampling. Written as a subset of
#' the object.
#'
#' @param extent The product of sample_ext - a vectorized polygon to crop the rasters.
#'
#' @export

rast_match<-function(rast_list, reference_layer, extent){
  lapply(rast_list, function(x){
    extent <- sf::st_transform(extent, crs(x))
    crop_rast <- terra::project(crop(x, extent), "EPSG:4326")

    crs(crop_rast) <- "EPSG:4326"

    rast_resampled <- terra::resample(crop_rast, reference_layer)
  })
}
