#' @title The Power Logistic Distribution
#' @name pl
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power logistic distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{http://conteudo.icmc.usp.br/pessoas/jlbazan/download/thesis.pdf}.
#' @references Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 16. Wiley, New York.
#' @references Nadarajah, S. (2009) The skew logistic distribution. \emph{AStA Advances in Statistical Analysis}, \strong{93}, 187-203.
#' @references Prentice, R. L. (1976) A Generalization of the probit and logit methods for dose-response curves. \emph{Biometrika}, \strong{32}, 761-768.
#' @importFrom stats runif
#' @importFrom stats dlogis
#' @importFrom stats plogis
#' @importFrom stats qlogis
#' @examples
#' dpl(1, 1, 3, 4)
#' @export
dpl <- function(x, lambda, mu = 0, sigma = 1){
d = (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((x-mu)/sigma)**(lambda-1))
return(d)
}

