#' @name PowerExponentialPower
#' @examples
#' ppexpow(1, 1, 3, 4, 1)
#' @export
ppexpow <- function(q, lambda = 1, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE){
  p = pnormp(((q-mu)/sigma), p = (2/(k+1)))**lambda
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}
