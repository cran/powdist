% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drgumbel.R, R/prgumbel.R, R/qrgumbel.R,
%   R/rrgumbel.R
\name{ReversalGumbel}
\alias{ReversalGumbel}
\alias{drgumbel}
\alias{ReversalGumbel}
\alias{prgumbel}
\alias{ReversalGumbel}
\alias{qrgumbel}
\alias{ReversalGumbel}
\alias{rrgumbel}
\title{The Reversal-Gumbel Distribution}
\usage{
drgumbel(x, mu = 0, sigma = 1, log = FALSE)

prgumbel(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)

qrgumbel(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE)

rrgumbel(n, mu = 0, sigma = 1)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{mu, sigma}{location and scale parameters.}

\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}

\item{lower.tail}{logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].}

\item{p}{vector of probabilities.}

\item{n}{number of observations.}
}
\description{
Density, distribution function,
quantile function and random generation for
the Reversal-Gumbel distribution with parameters mu and sigma.
}
\details{
The reversal-Gumbel distribution has density

\eqn{f(x)=\left[\frac{1}{\sigma}e^{\left(\frac{x-\mu}{\sigma}\right)-e^{\left(\frac{x-\mu}{\sigma}\right)}}\right]}{f(x)=exp(-(x-\mu)/\sigma-exp(-(x-\mu)/\sigma))},

where \eqn{-\infty<\mu<\infty} is the location paramether and \eqn{\sigma^2>0} is the scale parameter.
}
\examples{
drgumbel(1, 3, 4)
prgumbel(1, 3, 4)
qrgumbel(0.2, 3, 4)
rprgumbel(5, 3, 4)
}
\references{
Anyosa, S. A. C. (2017) \emph{Binary regression using power and reversal power links}. Master's thesis in Portuguese. Interinstitutional Graduate Program in Statistics. Universidade de São Paulo - Universidade Federal de São Carlos. Available in \url{https://repositorio.ufscar.br/handle/ufscar/9016}.

Bazán, J. L., Torres -Avilés, F., Suzuki, A. K. and Louzada, F. (2017) Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
}
