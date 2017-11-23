#' @name ReversalPowerExponentialPower
#' @examples
#' rrpexpow(5, 2, 3, 4, 1)
#' @export
rrpexpow = function(n, lambda = 1, mu= 0, sigma = 1, k = 0){
  n = runif(n)
  x = qrpexpow(n, lambda, mu, sigma, k)
  return(x)
}
