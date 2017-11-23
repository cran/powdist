#' @title The Gumbel Distribution
#' @name Gumbel
#' @description Density, distribution function,
#' quantile function and random generation for
#' the Gumbel distribution with parameters mu and sigma.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @importFrom stats runif
#' @importFrom gamlss.dist dGU
#' @importFrom gamlss.dist pGU
#' @importFrom gamlss.dist qGU
#' @details The Gumbel distribution has density
#'
#' \eqn{f(x)=\left[\frac{1}{\sigma}e^{\left(-\frac{x-\mu}{\sigma}\right)-e^{\left(-\frac{x-\mu}{\sigma}\right)}}\right]}{f(x)=(1/\sigma)exp(-(x-\mu)/\sigma-exp(-(x-\mu)/\sigma))},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether and \eqn{\sigma^2>0} is the scale parameter.
#'
#' @examples
#' dgumbel(1, 3, 4)
#' @export
dgumbel <- function(x, mu = 0, sigma = 1, log = FALSE){
  d =  (1/sigma) * exp( -((x-mu)/sigma) - exp(-(x-mu)/sigma) )
  if (log == TRUE) {
    d = log( (1/sigma) * exp( -((x-mu)/sigma) - exp(-(x-mu)/sigma) ) )
  }
  return(d)
}

