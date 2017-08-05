#' @name pgmiv
#' @examples
#' rpgmiv(5, 2, 3, 4)
#' @export
rpgmiv = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpgmiv(n, lambda, mu, sigma)
  return(x)
}
