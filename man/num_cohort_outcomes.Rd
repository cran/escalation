% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/num_cohort_outcomes.R
\name{num_cohort_outcomes}
\alias{num_cohort_outcomes}
\title{Number of different possible outcomes for a cohort of patients}
\usage{
num_cohort_outcomes(num_patient_outcomes, cohort_size)
}
\arguments{
\item{num_patient_outcomes}{integer, number of distinct possible outcomes for
each single patient}

\item{cohort_size}{integer, number of patients in the cohort}
}
\value{
integer, number of distinct possible cohort outcomes
}
\description{
Number of different possible outcomes for a cohort of patients, each of
which will experience one of a number of discrete outcomes. For instance, in
a typical phase I dose-finding trial, each patient will experience:
no-toxicity (N); or toxicity (T). The number of possible outcomes per patient
is two. For a cohort of three patients, the number of cohort outcomes is
four: NNN, NNT, NTT, TTT. Consider a more complex example: in a seamless
phase I/II trial with efficacy and toxicity outcomes, an individual patient
will experience one of four distinct outcomes: efficacy only (E); toxicity
only (T); both efficacy and toxicity (B) or neither. How many different
outcomes are there for a cohort of three patients? The answer is 20 but it is
non-trivial to see why. This convenience function calculates that number
using the formula for the number of combinations with replacement,
}
\examples{
# As described in example, N or T in a cohort of three:
num_cohort_outcomes(num_patient_outcomes = 2, cohort_size = 3)
# Also described in example, E, T, B or N in a cohort of three:
num_cohort_outcomes(num_patient_outcomes = 4, cohort_size = 3)
}
