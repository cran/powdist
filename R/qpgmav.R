#' @name pgmav
#' @examples
#' qpgmav(0.2, 1, 3, 4)
#' @export
qpgmav <- function(p, lambda, mu = 0, sigma = 1){
  q = qgumbel(p**(1/lambda))* sigma + mu
  return(q)
}
