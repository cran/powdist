#' @name revpgmiv
#' @examples
#' prpgmiv(1, 1, 3, 4)
#' @export
prpgmiv <- function(q, lambda, mu = 0, sigma = 1){
  p = 1-pgumbel(q, mu, sigma)
  return(1-p**lambda)
}
