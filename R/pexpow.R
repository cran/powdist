#' @name ExponentialPower
#' @examples
#' pexpow(1, 3, 4, 1)
#' @export
pexpow <- function(q, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE){
  p = pnormp(((q-mu)/sigma), p = (2/(k+1)))
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}
