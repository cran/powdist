#' @name PowerNormal
#' @examples
#' rpnorm(5, 2, 3, 4)
#' @export
rpnorm = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qpnorm(n, lambda, mu, sigma)
  return(x)
}
