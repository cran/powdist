#' @title The Exponential Power Distribution
#' @name ExponentialPower
#' @description Density, distribution function,
#' quantile function and random generation for
#' the exponential power distribution with parameters mu, sigma and k.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param k shape parameter.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @references Lemonte A. and Bazán J.L.
#' @importFrom stats runif
#' @importFrom normalp dnormp
#' @importFrom normalp pnormp
#' @importFrom normalp qnormp
#' @details The Exponential distribution has density
#'
#' \eqn{f\left(x\right)=\left[\frac{e^{-\left(\frac{x-\mu}{\sigma}\right)}}{\left(1+e^{-\left(\frac{x-\mu}{\sigma}\right)}\right)^{2}}\right]}{f(x)=[exp(-(x-\mu)/\sigma)/(1+exp(-(x-\mu)/\sigma)))^2]},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and k the shape parameter.
#'
#' @examples
#' dexpow(1, 3, 4, 1)
#' @export
dexpow <- function(x, mu = 0, sigma = 1, k = 0, log = FALSE){
  d = dnormp( ((x-mu)/sigma), p = (2/(k+1)) )
  if (log == TRUE) {
    d = log( dnormp( ((x-mu)/sigma), p = (2/(k+1)) ) )
  }
return(d)
}

