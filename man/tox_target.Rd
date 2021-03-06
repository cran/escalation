% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{tox_target}
\alias{tox_target}
\title{Target toxicity rate}
\usage{
tox_target(x, ...)
}
\arguments{
\item{x}{Object of type \code{\link{selector}}.}

\item{...}{Extra args are passed onwards.}
}
\value{
numeric
}
\description{
Get the target toxicity rate, if supported. NULL if not.
}
\examples{
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
model <- get_dfcrm(skeleton = skeleton, target = target)
fit <- model \%>\% fit('1NNN 2NTN')
fit \%>\% tox_target()
}
