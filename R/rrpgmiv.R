#' @name revpgmiv
#' @examples
#' rrpgmiv(5, 2, 3, 4)
#' @export
rrpgmiv = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpgmiv(n, lambda, mu, sigma)
  return(x)
}
