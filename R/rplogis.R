#' @name PowerLogistic
#' @examples
#' rplogis(5, 2, 3, 4)
#' @export
rplogis = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qplogis(n, lambda, mu, sigma)
  return(x)
}
