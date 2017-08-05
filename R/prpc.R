#' @name revpc
#' @examples
#' prpc(1, 1, 3, 4)
#' @export
prpc <- function(q, lambda, mu = 0, sigma = 1){
  p = pcauchy(-q, mu, sigma)
  return(1-p**lambda)
}
