#' @name ReversalGumbel
#' @examples
#' qrgumbel(0.2, 3, 4)
#' @export
qrgumbel <- function(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = qGU(p)* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = qGU((1-p))* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = qGU(exp(p))* sigma + mu
  } else {q = qGU(1-exp(p))* sigma + mu}
  return(q)
}
