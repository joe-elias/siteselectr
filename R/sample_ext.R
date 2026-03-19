#' Create a sampling circle around a central location
#'
#' This function is meant for users who want to calculate a sampling circle
#' around a central point. Whether using `sample_ext` directly or supplying a pre‑loaded shapefile, a sample extent is required for `site_select`.
#'
#' @param x Latitude of the center point.
#' @param y Longitude of the center point.
#' @param radius Desired radius of the sampling circle (in the units of the CRS).
#' @param crs EPSG code defining the coordinate reference system (CRS)
#'
#' @return A polygon (circle) representing the sampling extent. Used in the site_select() function.
#'
#' Example:
#' sample_ext(lat, long, 100000 #meters, crs=4326)
#'
#' @export

sample_ext<-function(x, y, radius, crs){
  sf::sf_use_s2(FALSE)

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

