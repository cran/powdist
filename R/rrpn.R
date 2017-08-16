#' @name revpn
#' @examples
#' rrpn(5, 2, 3, 4)
#' @export
rrpn = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpn(n, lambda, mu, sigma)
  return(x)
}
