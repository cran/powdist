#' @name pn
#' @examples
#' ppn(1, 1, 3, 4)
#' @export
ppn <- function(q, lambda, mu = 0, sigma = 1){
  p = pnorm(q, mu, sigma)
  return(p**lambda)
}
