#' @name ExponentialPower
#' @examples
#' rexpow(5, 3, 4, 1)
#' @export
rexpow = function(n, mu= 0, sigma = 1, k = 0){
  n = runif(n)
  x = qexpow(n, mu, sigma, k)
  return(x)
}
