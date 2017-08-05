#' @name pl
#' @examples
#' ppl(1, 1, 3, 4)
#' @export
ppl <- function(q, lambda, mu = 0, sigma = 1){
  p = plogis(q, mu, sigma)
  return(p**lambda)
}