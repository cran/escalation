## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)

## ----message=FALSE------------------------------------------------------------
library(escalation)

## -----------------------------------------------------------------------------
num_sims <- 20

## -----------------------------------------------------------------------------
set.seed(123)

sims <- get_three_plus_three(num_doses = 5) %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox)

## -----------------------------------------------------------------------------
sims

## -----------------------------------------------------------------------------
prob_recommend(sims)

## -----------------------------------------------------------------------------
summary(num_patients(sims))

## -----------------------------------------------------------------------------
summary(n_at_recommended_dose(sims))

## -----------------------------------------------------------------------------
n_at_dose(sims) %>% head(10)

## -----------------------------------------------------------------------------
prob_administer(sims)

## -----------------------------------------------------------------------------
tox_at_dose(sims) %>% head(10)

## -----------------------------------------------------------------------------
summary(num_tox(sims))

## ----rows.print = 12----------------------------------------------------------
library(tibble)

as_tibble(sims) %>% head(12)

## ----fig.width=7, fig.height=5, message=FALSE---------------------------------
library(dplyr)
library(ggplot2)

as_tibble(sims) %>% 
  filter(recommended) %>% 
  ggplot(aes(x = dose, fill = dose)) + 
  geom_bar()

## -----------------------------------------------------------------------------
target <- 0.25

## -----------------------------------------------------------------------------
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)

## -----------------------------------------------------------------------------
sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox)

sims

## -----------------------------------------------------------------------------
sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.8) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox)

sims

## -----------------------------------------------------------------------------
summary(n_at_recommended_dose(sims))

## -----------------------------------------------------------------------------
sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.8) %>% 
  demand_n_at_dose(n = 6, dose = 'recommended') %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox)

## -----------------------------------------------------------------------------
summary(n_at_recommended_dose(sims))

## -----------------------------------------------------------------------------
sims

## -----------------------------------------------------------------------------
patient_arrivals_func <- function(current_data) cohorts_of_n(n = 2)

model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.8) %>% 
  demand_n_at_dose(n = 6, dose = 'recommended')

sims <- model %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox,
                  sample_patient_arrivals = patient_arrivals_func)

## -----------------------------------------------------------------------------
cohorts_of_n(n = 5)

## -----------------------------------------------------------------------------
set.seed(123)

sims <- model %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox,
                  previous_outcomes = '1NTN')

## -----------------------------------------------------------------------------
prob_recommend(sims)

## -----------------------------------------------------------------------------
previous_outcomes <- data.frame(
  patient = 1:3,
  cohort = c(1, 1, 1),
  tox = c(0, 1, 0),
  dose = c(1, 1, 1)
)

set.seed(123)
sims <- model %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox,
                  previous_outcomes = previous_outcomes)

## -----------------------------------------------------------------------------
prob_recommend(sims)

## ----eval = FALSE-------------------------------------------------------------
# sims <- model %>%
#   simulate_trials(num_sims = num_trials, true_prob_tox = true_prob_tox,
#                   next_dose = 5)

## -----------------------------------------------------------------------------
sims <- get_three_plus_three(num_doses = 5) %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox,
                  return_all_fits = TRUE)

## -----------------------------------------------------------------------------
sapply(sims$fits, length)

## ----rows.print = 12----------------------------------------------------------
as_tibble(sims) %>% head(12)

## -----------------------------------------------------------------------------
sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 99) %>%
  simulate_trials(num_sims = 1, true_prob_tox = true_prob_tox,
                  i_like_big_trials = TRUE)

## -----------------------------------------------------------------------------
num_patients(sims)

