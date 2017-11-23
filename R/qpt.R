#' @name PowerT
#' @examples
#' qpt(0.2, 1, 3, 4, 1)
#' @export
qpt <- function(p, lambda = 1, mu = 0, sigma = 1, df, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = qt(p**(1/lambda), df)* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = qt((1-p)**(1/lambda), df)* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = qt(exp(p)**(1/lambda), df)* sigma + mu
  } else {q = qt((1-exp(p))**(1/lambda), df)* sigma + mu}
  return(q)
}
