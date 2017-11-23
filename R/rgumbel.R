#' @name Gumbel
#' @examples
#' rgumbel(5, 3, 4)
#' @export
rgumbel = function(n, mu= 0, sigma = 1){
  n = runif(n)
  x = qgumbel(n, mu, sigma)
  return(x)
}
