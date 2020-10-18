## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(escalation)

dose <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200, 250)
outcomes <- '1NNN 2NNNN 3NNNN 4NNNN 7TT'

## -----------------------------------------------------------------------------
model <- get_trialr_nbg(real_doses = dose, d_star = 250, target = 0.3,
                        alpha_mean = 2.15, alpha_sd = 0.84,
                        beta_mean = 0.52, beta_sd = 0.8,
                        seed = 2020)

## -----------------------------------------------------------------------------
fit <- model %>% fit(outcomes)
fit

## -----------------------------------------------------------------------------
model2 <- model %>% select_dose_by_cibp(a = 0.3)

## -----------------------------------------------------------------------------
fit2 <- model2 %>% fit(outcomes)

## -----------------------------------------------------------------------------
fit2 %>% recommended_dose()

## ---- message=FALSE-----------------------------------------------------------
paths1 <- model %>% get_dose_paths(cohort_sizes = c(3, 3), next_dose = 2)
graph_paths(paths1)

## ---- message=FALSE-----------------------------------------------------------
paths2 <- model2 %>% get_dose_paths(cohort_sizes = c(3, 3), next_dose = 2)
graph_paths(paths2)

## -----------------------------------------------------------------------------
dose <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200, 250)
model <- get_trialr_nbg(real_doses = dose, d_star = 250, target = 0.3,
                        alpha_mean = 2.15, alpha_sd = 0.84,
                        beta_mean = 0.52, beta_sd = 0.8,
                        seed = 2020) %>% 
  stop_when_too_toxic(dose = 1, tox_threshold = 0.3, confidence = 0.8) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 9) %>% 
  stop_at_n(n = 24)

## -----------------------------------------------------------------------------
num_sims <- 10

## -----------------------------------------------------------------------------
sc1 <- c(0.01, 0.03, 0.10, 0.17, 0.25, 0.35, 0.45, 0.53, 0.60, 0.65, 0.69, 
         0.72, 0.75, 0.79, 0.80)

## -----------------------------------------------------------------------------
set.seed(123)
sims <- model %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = sc1, next_dose = 1)

sims

