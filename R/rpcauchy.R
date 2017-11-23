#' @name PowerCauchy
#' @examples
#' rpcauchy(5, 2, 3, 4)
#' @export
rpcauchy = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qpcauchy(n, lambda, mu, sigma)
  return(x)
}
