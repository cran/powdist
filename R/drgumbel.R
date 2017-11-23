#' @title The Reversal-Gumbel Distribution
#' @name ReversalGumbel
#' @description Density, distribution function,
#' quantile function and random generation for
#' the Reversal-Gumbel distribution with parameters mu and sigma.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @references Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{https://repositorio.ufscar.br/handle/ufscar/9016}.
#' @references Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @importFrom stats runif
#' @importFrom gamlss.dist dGU
#' @importFrom gamlss.dist pGU
#' @importFrom gamlss.dist qGU
#' @details The reversal-Gumbel distribution has density
#'
#' \eqn{f(x)=\left[\frac{1}{\sigma}e^{\left(\frac{x-\mu}{\sigma}\right)-e^{\left(\frac{x-\mu}{\sigma}\right)}}\right]}{f(x)=exp(-(x-\mu)/\sigma-exp(-(x-\mu)/\sigma))},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether and \eqn{\sigma^2>0} is the scale parameter.
#'
#' @examples
#' drgumbel(1, 3, 4)
#' @export
drgumbel <- function(x, mu = 0, sigma = 1, log = FALSE){
  d =  dGU((x-mu)/sigma)
  if (log == TRUE) {
    d = log( dGU((x-mu)/sigma) )
  }
  return(d)
}

