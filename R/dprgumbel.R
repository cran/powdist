#' @title The Power Reversal-Gumbel Distribution
#' @name PowerReversalGumbel
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power Reversal-Gumbel distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @references Abanto -Valle, C. A., Bazán, J. L. and Smith, A. C. (2014) \emph{State space mixed models for binary responses with skewed inverse links using JAGS}. Rio de Janeiro, Brazil.
#' @references Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{https://repositorio.ufscar.br/handle/ufscar/9016}.
#' @references Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @importFrom stats runif
#' @importFrom gamlss.dist dGU
#' @importFrom gamlss.dist pGU
#' @importFrom gamlss.dist qGU
#' @details The power reverlsa-Gumbel distribution has density
#'
#' \eqn{f(x)=\lambda \left[1-e^{-e^{\left(\frac{x-\mu}{\sigma}\right)}}\right]^{\lambda-1}\left[\frac{1}{\sigma}e^{\left(\frac{x-\mu}{\sigma}\right)-e^{\left(\frac{x-\mu}{\sigma}\right)}} \right]}{f(x)=[\lambda/\sigma][exp(-(x-\mu)/\sigma-exp(-(x-\mu)/\sigma))][exp(-exp(-(x-\mu)/\sigma))]^(\lambda-1)},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and \eqn{\lambda>0} the shape parameter.
#'
#' @examples
#' dprgumbel(1, 1, 3, 4)
#' @export
dprgumbel <- function(x, lambda = 1, mu = 0, sigma = 1, log = FALSE){
  d = (lambda/sigma) * dGU((x-mu)/sigma) * ( pGU((x-mu)/sigma) **(lambda-1))
  if (log == TRUE) {
    d = log( (lambda/sigma) * dGU((x-mu)/sigma) * ( pGU((x-mu)/sigma) **(lambda-1)) )
  }
  return(d)
}

