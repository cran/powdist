#' @title The Power Exponential Power Distribution
#' @name PowerExponentialPower
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power exponential power distribution with parameters mu, sigma, lambda and k.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param k,lambda shape parameters.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @references Lemonte A. and Bazán J.L.
#' @importFrom stats runif
#' @importFrom normalp dnormp
#' @importFrom normalp pnormp
#' @importFrom normalp qnormp
#' @details The power exponential power distribution has density
#'
#' \eqn{f\left(x\right)=\frac{\lambda}{\sigma}\left[\frac{e^{-\left(\frac{x-\mu}{\sigma}\right)}}{\left(1+e^{-\left(\frac{x-\mu}{\sigma}\right)}\right)^{2}}\right]\left[\frac{e^{\left(\frac{x-\mu}{\sigma}\right)}}{1+e^{\left(\frac{x-\mu}{\sigma}\right)}}\right]^{\lambda-1}}{f(x)=[\lambda/\sigma][exp(-(x-\mu)/\sigma)/(1+exp(-(x-\mu)/\sigma)))^2][exp((x-\mu)/\sigma)/(1+exp((x-\mu)/\sigma)]^(\lambda-1)},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and \eqn{\lambda>0} and k the shape parameters.
#'
#' @examples
#' dpexpow(1, 1, 3, 4, 1)
#' @export
dpexpow <- function(x, lambda = 1, mu = 0, sigma = 1, k = 0, log = FALSE){
  d = (lambda/sigma) * dnormp( ((x-mu)/sigma), p = (2/(k+1)) ) * (pnormp( ((x-mu)/sigma), p = (2/(k+1)) )**(lambda-1))
  if (log == TRUE) {
    d = log( (lambda/sigma) * dnormp( ((x-mu)/sigma), p = (2/(k+1)) ) * (pnormp( ((x-mu)/sigma), p = (2/(k+1)) )**(lambda-1)) )
  }
return(d)
}

