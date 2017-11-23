#' @title The Power Logistic Distribution
#' @name PowerLogistic
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power logistic distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @references Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{https://repositorio.ufscar.br/handle/ufscar/9016}.
#' @references Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 16. Wiley, New York.
#' @references Lemonte, A. J. and Bazán, J. L. (2017) New links for binary regression: an application to coca cultivation in Peru. \emph{TEST}.
#' @references Nadarajah, S. (2009) The skew logistic distribution. \emph{AStA Advances in Statistical Analysis}, \strong{93}, 187-203.
#' @references Prentice, R. L. (1976) A Generalization of the probit and logit methods for dose-response curves. \emph{Biometrika}, \strong{32}, 761-768.
#' @importFrom stats runif
#' @importFrom stats dlogis
#' @importFrom stats plogis
#' @importFrom stats qlogis
#' @details The power Logistic distribution has density
#'
#' \eqn{f(x)=\lambda \left [\frac{1}{1+e^{-\left ( \frac{x-\mu}{\sigma} \right )} }\right ]^{\lambda-1}\left[\frac{ e^{-\left ( \frac{x-\mu}{\sigma} \right )} }{\sigma\left ( 1+e^{-\left ( \frac{x-\mu}{\sigma} \right )}   \right )^2}\right]}{f(x)=[\lambda/\sigma][exp(-(x-\mu)/\sigma)/(1+exp(-(x-\mu)/\sigma)))^2][exp((x-\mu)/\sigma)/(1+exp((x-\mu)/\sigma)]^(\lambda-1)},
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and \eqn{\lambda>0} the shape parameter.
#'
#' @examples
#' dplogis(1, 1, 3, 4)
#' @export
dplogis <- function(x, lambda = 1, mu = 0, sigma = 1, log = FALSE){
  d = (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((x-mu)/sigma)**(lambda-1))
  if (log == TRUE) {
    d = log( (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((x-mu)/sigma)**(lambda-1)) )
  }
return(d)
}

