#' @name PowerLaplace
#' @examples
#' rplaplace(5, 2, 3, 4)
#' @export
rplaplace = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qplaplace(n, lambda, mu, sigma)
  return(x)
}
