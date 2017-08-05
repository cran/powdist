#' @name revpl
#' @examples
#' prpl(1, 1, 3, 4)
#' @export
prpl <- function(q, lambda, mu = 0, sigma = 1){
  p = plogis(-q, mu, sigma)
  return(1-p**lambda)
}
