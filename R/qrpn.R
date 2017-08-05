#' @name revpn
#' @examples
#' qrpn(0.2, 1, 3, 4)
#' @export
qrpn <- function(p, lambda, mu = 0, sigma = 1){
  q = -qnorm((1-p)**(1/lambda))* sigma + mu
  return(q)
}
