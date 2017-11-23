#' @name Gumbel
#' @examples
#' qgumbel(0.2, 3, 4)
#' @export
qgumbel <- function(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = -log ( - log (p))*sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = -log ( - log (1-p))* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = -log ( - log (exp (p)))* sigma + mu
  } else {q = -log ( - log (1-exp (p)))* sigma + mu}
  return(q)
}
