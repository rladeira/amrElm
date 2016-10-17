#' Cosine similarity implementation from http://brainchronicle.blogspot.com.br/2012/06/rcpp-vs-r-implementation-of-cosine.html
#'
#' @param X numerical data matrix - patterns are in the lines, attributes are in the columns
#' @return Cosine similarity matrix
#'


cosine <- function(X) {

  X <- t(X)
  y <- t(X) %*% X

  # Code above is a faster implementation
  # for res <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))

  res_a <- diag(y)
  res_a <- sqrt(res_a)
  res_b <- t(res_a)
  res <- res_a %*% res_b
  res <- y / res

  return(res)
}
