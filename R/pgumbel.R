#' @name Gumbel
#' @examples
#' pgumbel(1, 3, 4)
#' @export
pgumbel <- function(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  p = exp ( - exp ( - (q-mu)/sigma ) )
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}
