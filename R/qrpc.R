#' @name revpc
#' @examples
#' qrpc(0.2, 1, 3, 4)
#' @export
qrpc <- function(p, lambda, mu = 0, sigma = 1){
  q = -qcauchy((1-p)**(1/lambda))* sigma + mu
  return(q)
}
