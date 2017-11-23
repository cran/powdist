#' @title The Power Reversal Student t Distribution
#' @name ReversalPowerT
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power reversal Student t distribution with parameters mu, sigma, lambda and df.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @param df degrees of freedom (> 0, maybe non-integer). df = Inf is allowed.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @importFrom stats runif
#' @importFrom stats dt
#' @importFrom stats pt
#' @importFrom stats qt
#' @details The reversal power Student t distribution has density
#'
#' \eqn{f(x)=[\lambda/\sigma][f((x-\mu)/\sigma)][F((x-\mu)/\sigma)] ^(\lambda-1)},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and \eqn{\lambda>0} the shape parameter.
#'
#' @examples
#' drpt(1, 1, 3, 4, 1)
#' @export
drpt <- function(x, lambda = 1, mu = 0, sigma = 1, df, log = FALSE){
  d = (lambda/sigma) * dt( ((x-mu)/sigma), df) * (pt( ((-x+mu)/sigma), df)**(lambda-1))
  if (log == TRUE) {
    d = log( (lambda/sigma) * dt( ((x-mu)/sigma), df) * (pt( ((-x+mu)/sigma), df)**(lambda-1)) )
  }
return(d)
}

