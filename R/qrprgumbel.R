#' @name ReversalPowerReversalGumbel
#' @examples
#' qrprgumbel(0.2, 1, 3, 4)
#' @export
qrprgumbel <- function(p, lambda = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = -qGU((1-p)**(1/lambda))* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = -qGU((1-(1-p))**(1/lambda))* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = -qGU((1-exp(p))**(1/lambda))* sigma + mu
  } else {q = -qGU((1-(1-exp(p)))**(1/lambda))* sigma + mu}
  return(q)
}
