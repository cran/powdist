#' @name pgmiv
#' @examples
#' ppgmiv(1, 1, 3, 4)
#' @export
ppgmiv <- function(q, lambda, mu = 0, sigma = 1){
  p = 1-pgumbel(-q, mu, sigma)
  return(p**lambda)
}
