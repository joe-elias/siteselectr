#' The functions is meant for those interested in creating a simple sampling circumfrence around a central location as sampling extent.
#'
#' The sampling extent is used in the raster_match() function.
#'
#' x=latitude, y=longitude, 'radius' refers to the radius of the sampling circumfrence.

sample_ext<-function(x, y, radius){
  sf_use_s2(FALSE)

  center_loc<-sf::st_as_sf(data.frame(lat=x, long=y, radius),
                           coords=c("long", "lat"))
  sf::st_crs(center_loc)<- sf::st_crs("+proj=latlon") # assign crs

  sample_extent <- sf::st_buffer(sf::st_transform(center_loc,
                                                  sf::st_crs("+proj=merc")),
                                 radius)

  sample_extent_proj<-sf::st_transform(sample_extent,
                                   crs="+proj=longlat +datum=WGS84 +no_defs")
  return(sample_extent_proj)
}

