#' @name PowerReversalGumbel
#' @examples
#' rprgumbel(5, 2, 3, 4)
#' @export
rprgumbel = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qprgumbel(n, lambda, mu, sigma)
  return(x)
}
