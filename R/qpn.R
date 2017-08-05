#' @name pn
#' @examples
#' qpn(0.2, 1, 3, 4)
#' @export
qpn <- function(p, lambda, mu = 0, sigma = 1){
  q = qnorm(p**(1/lambda))* sigma + mu
  return(q)
}
