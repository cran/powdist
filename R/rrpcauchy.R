#' @name ReversalPowerCauchy
#' @examples
#' rrpcauchy(5, 2, 3, 4)
#' @export
rrpcauchy = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpcauchy(n, lambda, mu, sigma)
  return(x)
}
