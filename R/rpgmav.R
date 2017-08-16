#' @name pgmav
#' @examples
#' rpgmav(5, 2, 3, 4)
#' @export
rpgmav = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qpgmav(n, lambda, mu, sigma)
  return(x)
}
