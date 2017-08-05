#' @title The Power Cauchy Distribution
#' @name pc
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power Cauchy distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{http://conteudo.icmc.usp.br/pessoas/jlbazan/download/thesis.pdf}.
#' @references Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @importFrom stats runif
#' @importFrom stats dcauchy
#' @importFrom stats pcauchy
#' @importFrom stats qcauchy
#' @examples
#' dpc(1, 1, 3, 4)
#' @export
dpc <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dcauchy((x-mu)/sigma) * (pcauchy((x-mu)/sigma)**(lambda-1))
  return(d)
}

