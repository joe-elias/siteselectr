#' The functions is meant for those interested in calculating a sampling circle around a central location.
#'
#' Whether using the sample_ext() function or a pre-loaded shapefile, a sample extent is needed for site_select().
#'
#' x=latitude, y=longitude, 'radius'=desired radius of the circumference, and crs requires an EPSG code.

sample_ext<-function(x, y, radius, crs){
  sf_use_s2(FALSE)

  center_loc<-sf::st_as_sf(data.frame(lat=x, long=y, radius),
                           coords=c("long", "lat"))

  sf::st_crs(center_loc)<- sf::st_crs(crs)

  sample_extent <- sf::st_buffer(sf::st_transform(center_loc,
                                                  sf::st_crs("+proj=merc")),
                                 radius)

  sample_extent_proj<-sf::st_transform(sample_extent,
                                   crs=crs)
  return(sample_extent_proj)
}

