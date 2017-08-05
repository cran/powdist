#' @name pn
#' @examples
#' rpl(5, 2, 3, 4)
#' @export
rpn = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpn(n, lambda, mu, sigma)
  return(x)
}
