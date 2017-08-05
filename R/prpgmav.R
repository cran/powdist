#' @name revpgmav
#' @examples
#' prpgmav(1, 1, 3, 4)
#' @export
prpgmav <- function(q, lambda, mu = 0, sigma = 1){
  p = pgumbel(-q, mu, sigma)
  return(1-p**lambda)
}

