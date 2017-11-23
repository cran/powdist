#' @name ReversalPowerT
#' @examples
#' rrpt(5, 2, 3, 4, 1)
#' @export
rrpt = function(n, lambda = 1, mu= 0, sigma = 1, df){
  n = runif(n)
  x = qrpt(n, lambda, mu, sigma, df)
  return(x)
}
