#' @name pc
#' @examples
#' ppc(1, 1, 3, 4)
#' @export
ppc <- function(q, lambda, mu = 0, sigma = 1){
  p = pcauchy(q, mu, sigma)
  return(p**lambda)
}
