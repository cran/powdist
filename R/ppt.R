#' @name PowerT
#' @examples
#' ppt(1, 1, 3, 4, 1)
#' @export
ppt <- function(q, lambda = 1, mu = 0, sigma = 1, df, lower.tail = TRUE, log.p = FALSE){
  p = pt(((q-mu)/sigma), df)**lambda
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}
