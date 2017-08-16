#' @name pl
#' @examples
#' ppl(1, 1, 3, 4)
#' @export
ppl <- function(q, lambda = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  p = plogis(q, mu, sigma)**lambda
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}
