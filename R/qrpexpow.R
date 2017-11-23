#' @name ReversalPowerExponentialPower
#' @examples
#' qrpexpow(0.2, 1, 3, 4, 1)
#' @export
qrpexpow <- function(p, lambda = 1, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = -qnormp((1-p)**(1/lambda), p = (2/(k+1)))* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = -qnormp((1-(1-p))**(1/lambda), p = (2/(k+1)))* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = -qnormp((1-exp(p))**(1/lambda), p = (2/(k+1)))* sigma + mu
  } else {q = -qnormp((1-(1-exp(p)))**(1/lambda), p = (2/(k+1)))* sigma + mu}
  return(q)
}
