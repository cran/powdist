#' @name ReversalPowerReversalGumbel
#' @examples
#' rrprgumbel(5, 2, 3, 4)
#' @export
rrprgumbel = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrprgumbel(n, lambda, mu, sigma)
  return(x)
}
