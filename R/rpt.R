#' @name PowerT
#' @examples
#' rpt(5, 2, 3, 4, 1)
#' @export
rpt = function(n, lambda = 1, mu= 0, sigma = 1, df){
  n = runif(n)
  x = qpt(n, lambda, mu, sigma, df)
  return(x)
}
