#' @name pl
#' @examples
#' qpl(0.2, 1, 3, 4)
#' @export
qpl <- function(p, lambda = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = qlogis(p**(1/lambda))* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = qlogis((1-p)**(1/lambda))* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = qlogis(exp(p)**(1/lambda))* sigma + mu
  } else {q = qlogis((1-exp(p))**(1/lambda))* sigma + mu}
  return(q)
}
