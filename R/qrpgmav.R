#' @name revpgmav
#' @examples
#' qrpgmav(0.2, 1, 3, 4)
#' @export
qrpgmav <- function(p, lambda, mu = 0, sigma = 1){
  q = -qgumbel((1-p)**(1/lambda))* sigma + mu
  return(q)
}
