#' @title The Power Reversal Laplace Distribution
#' @name ReversalPowerLaplace
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power reversal Laplace distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @importFrom stats runif
#' @importFrom rmutil dlaplace
#' @importFrom rmutil plaplace
#' @importFrom rmutil qlaplace
#' @details The reversal power Laplace distribution has density
#'
#' \eqn{f(x)=\lambda\left[\frac{1}{2}+\frac{\left(1-e^{\frac{\left|x-\mu\right|}{\sigma}}\right)}{2}\textrm{sign}\left(-\frac{x-\mu}{\sigma}\right)\right]^{\lambda-1}\left[\frac{e^{-\frac{\left|x-\mu\right|}{\sigma}}}{2\sigma}\right]}{a},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and \eqn{\lambda>0} the shape parameter.
#'
#' @examples
#' drplaplace(1, 1, 3, 4)
#' @export
drplaplace <- function(x, lambda = 1, mu = 0, sigma = 1, log = FALSE){
  d = (lambda/sigma) * dlaplace((x-mu)/sigma) * (plaplace((-x+mu)/sigma)**(lambda-1))
  if (log == TRUE) {
    d = log( (lambda/sigma) * dlaplace((x-mu)/sigma) * (plaplace((-x+mu)/sigma)**(lambda-1)) )
  }
return(d)
}

