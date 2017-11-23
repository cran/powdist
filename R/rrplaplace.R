#' @name ReversalPowerLaplace
#' @examples
#' rrplaplace(5, 2, 3, 4)
#' @export
rrplaplace = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrplaplace(n, lambda, mu, sigma)
  return(x)
}
