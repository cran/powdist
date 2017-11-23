#' @name PowerLaplace
#' @examples
#' qplaplace(0.2, 1, 3, 4)
#' @export
qplaplace <- function(p, lambda = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = qlaplace(p**(1/lambda))* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = qlaplace((1-p)**(1/lambda))* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = qlaplace(exp(p)**(1/lambda))* sigma + mu
  } else {q = qlaplace((1-exp(p))**(1/lambda))* sigma + mu}
  return(q)
}
