#' @name ReversalPowerLogistic
#' @examples
#' rrplogis(5, 2, 3, 4)
#' @export
rrplogis = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrplogis(n, lambda, mu, sigma)
  return(x)
}
