#' @title The Power Normal Distribution
#' @name PowerNormal
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power normal distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @references Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{https://repositorio.ufscar.br/handle/ufscar/9016}.
#' @references Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @references Bazán, J. L., Romeo, J. S. and Rodrigues, J. (2014) Bayesian skew-probit regression for binary response data. \emph{Brazilian Journal of Probability and Statistics}. \strong{28}(4), 467–482.
#' @references Gupta, R. D. and Gupta, R. C. (2008) Analyzing skewed data by power normal model. \emph{Test} \strong{17}, 197–210.
#' @references Kundu, D. and Gupta, R. D. (2013) Power-normal distribution. \emph{Statistics} \strong{47}, 110–125.
#' @importFrom stats runif
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @details The power Normal distribution has density
#'
#' \eqn{f(x)=\lambda \left [ \Phi \left ( \frac{x-\mu}{\sigma} \right ) \right]^{\lambda - 1} \left[\frac{e^{ -\frac{1}{2}\left ( \frac{x-\mu}{\sigma} \right )^2}}{\sigma\sqrt{2\pi}} \right]}{f(x)=[\lambda/\sigma][exp(-((x-\mu)/\sigma)^2)/\sqrt(2\pi)][\Phi((x-\mu)/\sigma)]^(\lambda-1)},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and \eqn{\lambda>0} the shape parameter.
#'
#' @examples
#' dpnorm(1, 1, 3, 4)
#' @export
dpnorm <- function(x, lambda = 1, mu = 0, sigma = 1, log = FALSE){
  d = (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((x-mu)/sigma)**(lambda-1))
  if (log == TRUE) {
    d = log( (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((x-mu)/sigma)**(lambda-1)) )
  }
  return(d)
}

