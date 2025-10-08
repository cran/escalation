## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----message=FALSE------------------------------------------------------------
library(escalation)

model <- get_mtpi(num_doses = 5, target = 0.3, alpha = 1, beta = 1, 
                  epsilon1 = 0.05, epsilon2 = 0.05, exclusion_certainty = 0.95)

fit <- model %>% fit('1NNT') 

## -----------------------------------------------------------------------------
fit %>% recommended_dose()

## -----------------------------------------------------------------------------
fit %>% dose_admissible()

## -----------------------------------------------------------------------------
fit <- model %>% fit('1NNT 1NNN') 

## -----------------------------------------------------------------------------
fit

## -----------------------------------------------------------------------------
fit <- model %>% fit('1NNT 1NNN 1NNN 2TTT') 

## -----------------------------------------------------------------------------
fit %>% prob_tox_exceeds(threshold = 0.25)

## -----------------------------------------------------------------------------
fit %>% dose_admissible()

## -----------------------------------------------------------------------------
model <- get_tpi(num_doses = 5, target = 0.25, k1 = 1, k2 = 1.5, 
                 exclusion_certainty = 0.95) %>%
  stop_at_n(n = 12) %>%
  select_mtpi_mtd(exclusion_certainty = 0.95)

outcomes <- '1NNN 2NTN 2NNN 3NTT'
model %>% fit(outcomes) %>% recommended_dose()

## ----message=FALSE------------------------------------------------------------
paths <- model %>% get_dose_paths(cohort_sizes = c(3), next_dose = 2)

library(dplyr)
as_tibble(paths) %>% select(outcomes, next_dose) %>% print(n = 100)

## -----------------------------------------------------------------------------
cohort_sizes <- c(3, 3)
paths <- model %>% get_dose_paths(cohort_sizes = cohort_sizes, next_dose = 2)
graph_paths(paths)

## -----------------------------------------------------------------------------
model <- get_mtpi(num_doses = 8, target = 0.25, 
                  epsilon1 = 0.05, epsilon2 = 0.05, 
                  exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30)

## -----------------------------------------------------------------------------
num_sims <- 50

## -----------------------------------------------------------------------------
sc1 <- c(0.05, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)

## -----------------------------------------------------------------------------
set.seed(123)
sims <- model %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = sc1, next_dose = 1)

sims

