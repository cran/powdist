#' @name revpc
#' @examples
#' rrpc(5, 2, 3, 4)
#' @export
rrpc = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpc(n, lambda, mu, sigma)
  return(x)
}
