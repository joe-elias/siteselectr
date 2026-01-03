#' modified from the wmahalanobis function from the WMDB package
#'
#' Wu, B., & Csárdi, G. (2010). WMDB: Discriminant Analysis Methods by Weight Mahalanobis Distance and Bayes* (Version 1.0) R package. CRAN. https://CRAN.R-project.org/package=WMDB


wt_distance <- function (data, center, cov, weight) {

  if (is.vector(data))
    data = matrix(x, ncol = length(data))

  else data = as.matrix(data)

  data <- sweep(data, 2, center)
  cov <- weight %*% solve(cov)

  retval <- diag(data %*% cov %*% t(data))

  retval
}
