#' Cosine similarity implementation from http://brainchronicle.blogspot.com.br/2012/06/rcpp-vs-r-implementation-of-cosine.html
#' 
#' @param X numerical data matrix - patterns are in the lines, attributes are in the columns
#' @return Cosine similarity matrix 
cosine <- function( X ) {
  
  X <- t(X)  
  y <- t(X) %*% X
  
  #Code above is a faster implementation for res <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  
  res.a <- diag(y) 
  res.a <- sqrt(res.a) 
  res.b <- t(res.a) 
  res <- res.a %*% res.b 
  res <- y/res 
    
  return(res)
}