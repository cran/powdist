#' @name revpgmav
#' @examples
#' rrpgmav(5, 2, 3, 4)
#' @export
rrpgmav = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpgmav(n, lambda, mu, sigma)
  return(x)
}
