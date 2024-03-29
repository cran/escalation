% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{utility}
\alias{utility}
\title{Utility score of each dose.}
\usage{
utility(x, ...)
}
\arguments{
\item{x}{Object of class \code{\link{selector}}}

\item{...}{arguments passed to other methods}
}
\value{
a numerical vector
}
\description{
Get the derived utility score of each dose under investigation. Some models,
particularly phase I/II models or efficacy-toxicity designs, specify
algorithms to calculate utility. If no utility algorithm is specified for a
design, this function will return a vector of NAs.
}
\examples{
efftox_priors <- trialr::efftox_priors
p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
                   beta_mean = 1.5482, beta_sd = 3.5018,
                   gamma_mean = 0.7367, gamma_sd = 2.5423,
                   zeta_mean = 3.4181, zeta_sd = 2.4406,
                   eta_mean = 0, eta_sd = 0.2,
                   psi_mean = 0, psi_sd = 1)
real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
model <- get_trialr_efftox(real_doses = real_doses,
                           efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                           p_e = 0.1, p_t = 0.1,
                           eff0 = 0.5, tox1 = 0.65,
                           eff_star = 0.7, tox_star = 0.25,
                           priors = p, iter = 1000, chains = 1, seed = 2020)
x <- model \%>\% fit('1N 2E 3B')
utility(x)
}
