#' @name pc
#' @examples
#' qpc(0.2, 1, 3, 4)
#' @export
qpc <- function(p, lambda, mu = 0, sigma = 1){
  q = qcauchy(p**(1/lambda))* sigma + mu
  return(q)
}
