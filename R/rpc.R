#' @name pc
#' @examples
#' rpc(5, 2, 3, 4)
#' @export
rpc = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpc(n, lambda, mu, sigma)
  return(x)
}
