#' @name pgmav
#' @examples
#' ppgmav(1, 1, 3, 4)
#' @export
ppgmav <- function(q, lambda, mu = 0, sigma = 1){
  p = pgumbel(q, mu, sigma)
  return(p**lambda)
}
