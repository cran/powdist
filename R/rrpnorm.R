#' @name ReversalPowerNormal
#' @examples
#' rrpnorm(5, 2, 3, 4)
#' @export
rrpnorm = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpnorm(n, lambda, mu, sigma)
  return(x)
}
