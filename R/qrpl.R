#' @name revpl
#' @examples
#' qrpl(0.2, 1, 3, 4)
#' @export
qrpl <- function(p, lambda, mu = 0, sigma = 1){
  q = -qlogis((1-p)**(1/lambda))* sigma + mu
  return(q)
}
