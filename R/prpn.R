#' @name revpn
#' @examples
#' prpn(1, 1, 3, 4)
#' @export
prpn <- function(q, lambda, mu = 0, sigma = 1){
  p = pnorm(-q, mu, sigma)
  return(1-p**lambda)
}
