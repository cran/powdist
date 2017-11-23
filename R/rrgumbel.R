#' @name ReversalGumbel
#' @examples
#' rprgumbel(5, 3, 4)
#' @export
rrgumbel = function(n, mu= 0, sigma = 1){
  n = runif(n)
  x = qrgumbel(n, mu, sigma)
  return(x)
}
