#' @name pc
#' @examples
#' ppc(1, 1, 3, 4)
#' @export
ppc <- function(q, lambda = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  p = pcauchy(q, mu, sigma)**lambda
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}
