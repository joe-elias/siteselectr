#' This function is modified from the wmahalanobis function from the WMDB package where this approach was originally developed.
#'
#' Wu, B., & Csárdi, G. (2010). WMDB: Discriminant Analysis Methods by Weight Mahalanobis Distance and Bayes* (Version 1.0) R package. CRAN. https://CRAN.R-project.org/package=WMDB
#'
#' @param data raster stack data.
#' @param center center mean.
#' @param cov covariance matrix.
#' @param weight weighted diagonal matrix.
#'
#' @return a matrix of dissimilarity values.
#' nested in the rast_distance_wt() function
#' @export


wt_distance <- function (data, center, cov, weight) {

  if (is.vector(data))
    data = matrix(data, ncol = length(data))

  else data = as.matrix(data)

  data <- sweep(data, 2, center)
  cov <- weight %*% solve(cov)

  retval <- diag(data %*% cov %*% t(data))

  retval
}
