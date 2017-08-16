#' @name revpl
#' @examples
#' rrpl(5, 2, 3, 4)
#' @export
rrpl = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpl(n, lambda, mu, sigma)
  return(x)
}
