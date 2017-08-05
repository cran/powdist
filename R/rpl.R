#' @name pl
#' @examples
#' rpl(5, 2, 3, 4)
#' @export
rpl = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpl(n, lambda, mu, sigma)
  return(x)
}