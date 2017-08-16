#' @name pn
#' @examples
#' qpn(0.2, 1, 3, 4)
#' @export
qpn <- function(p, lambda = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = qnorm(p**(1/lambda))* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = qnorm((1-p)**(1/lambda))* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = qnorm(exp(p)**(1/lambda))* sigma + mu
  } else {q = qnorm((1-exp(p))**(1/lambda))* sigma + mu}
  return(q)
}
