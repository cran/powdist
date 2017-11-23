#' @name ExponentialPower
#' @examples
#' qexpow(0.2, 3, 4, 1)
#' @export
qexpow <- function(p, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE){
  if (lower.tail == TRUE & log.p == FALSE) {
    q = qnormp(pr = p, p = (2/(k+1)) )* sigma + mu
  } else if (lower.tail == FALSE & log.p == FALSE) {
    q = qnormp(pr = 1-p, p = (2/(k+1)) )* sigma + mu
  } else if (lower.tail == TRUE & log.p == TRUE) {
    q = qnormp(pr = exp(p), p = (2/(k+1)) )* sigma + mu
  } else {q = qnormp(pr = 1-exp(p), p = (2/(k+1)) )* sigma + mu}
  return(q)
}
