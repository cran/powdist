#' @title The Reversal Power Normal Distribution
#' @name revpn
#' @description Density, distribution function,
#' quantile function and random generation for
#' the reversal power normal distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{http://conteudo.icmc.usp.br/pessoas/jlbazan/download/thesis.pdf}.
#' @references Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @references Bazán, J. L., Romeo, J. S. and Rodrigues, J. (2014) Bayesian skew-probit regression for binary response data. \emph{Brazilian Journal of Probability and Statistics}. \strong{28}(4), 467–482.
#' @importFrom stats runif
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @examples
#' drpn(1, 1, 3, 4)
#' @export
drpn <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((-x-mu)/sigma)**(lambda-1))
  return(d)
}

