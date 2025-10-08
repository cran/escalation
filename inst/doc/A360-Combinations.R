## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

## ----message=FALSE------------------------------------------------------------
library(escalation)

num_doses <- c(3, 5)
target <- 0.3

## -----------------------------------------------------------------------------
boin_fitter <- get_boin_comb(
  num_doses = num_doses, target = target
)

## -----------------------------------------------------------------------------
model <- get_boin_comb(
  num_doses = num_doses, target = target, 
  p.saf = 0.18, p.tox = 0.42
)

## -----------------------------------------------------------------------------
outcomes <- "1.1NNN 2.1NNNNNNT 2.2NNNNNT"

## -----------------------------------------------------------------------------
set.seed(2025)
fit <- model %>% fit(outcomes)

## -----------------------------------------------------------------------------
fit %>% continue()

## -----------------------------------------------------------------------------
fit %>% recommended_dose()

## -----------------------------------------------------------------------------
model <- get_boin_comb(num_doses = num_doses, target = target)  %>%
   stop_at_n(n = 12) %>%
   select_boin_comb_mtd()

outcomes <- '1.1NNN 1.2NTN 1.2NNN 2.1TNT'
model %>% fit(outcomes) %>% recommended_dose()

## -----------------------------------------------------------------------------
set.seed(2025)
model <- get_boin_comb(num_doses = num_doses, target = target)  %>%
   stop_when_n_at_dose(n = 3, dose = "recommended")

outcomes <- '1.1NNN 2.1NTN'
x <- model %>% fit(outcomes) 
recommended_dose(x)

## -----------------------------------------------------------------------------
continue(x)

## -----------------------------------------------------------------------------
set.seed(2025)
model <- get_boin_comb(num_doses = num_doses, target = target)  %>%
   stop_when_n_at_dose(n = 3, dose = c(2, 1))

outcomes <- '1.1NNN 2.1NTN'
x <- model %>% fit(outcomes) 
recommended_dose(x)
continue(x)

## ----error=TRUE---------------------------------------------------------------
try({
cohort_sizes <- c(3, 3)
paths <- model %>% 
  get_dose_paths(
    cohort_sizes = cohort_sizes, 
    previous_outcomes = '1NNN', next_dose = 2
  )
})

## -----------------------------------------------------------------------------
model <- get_boin_comb(num_doses = num_doses, target = target)  %>%
   stop_at_n(n = 12) %>%
   select_boin_comb_mtd()

## -----------------------------------------------------------------------------
true_prob_tox <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5,
                          0.25, 0.35, 0.45, 0.55, 0.65,
                          0.3, 0.4, 0.5, 0.6, 0.7),
                        nrow = num_doses[1],
                        ncol = num_doses[2],
                        byrow = TRUE)
true_prob_tox

## -----------------------------------------------------------------------------
set.seed(2025)
sims <- model %>%
  simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)

## -----------------------------------------------------------------------------
sims

## -----------------------------------------------------------------------------
num_patients(sims)

## -----------------------------------------------------------------------------
num_doses(sims)

## -----------------------------------------------------------------------------
dose_indices(sims)

## -----------------------------------------------------------------------------
dose_strings(sims)

## -----------------------------------------------------------------------------
recommended_dose(sims, dose_string = TRUE)

## -----------------------------------------------------------------------------
recommended_dose(sims, dose_string = FALSE)

## -----------------------------------------------------------------------------
n_at_dose(sims)

## -----------------------------------------------------------------------------
n_at_dose(sims, dose = "recommended")

## -----------------------------------------------------------------------------
n_at_recommended_dose(sims)

## -----------------------------------------------------------------------------
tox_at_dose(sims)

## -----------------------------------------------------------------------------
num_tox(sims)

## -----------------------------------------------------------------------------
eff_at_dose(sims) %>% head(2)

## -----------------------------------------------------------------------------
num_eff(sims)

## -----------------------------------------------------------------------------
prob_recommend(sims)

## -----------------------------------------------------------------------------
prob_administer(sims, method = 0)

## -----------------------------------------------------------------------------
prob_administer(sims, method = 1)

## -----------------------------------------------------------------------------
trial_duration(sims)

## -----------------------------------------------------------------------------
summary(sims)

## -----------------------------------------------------------------------------
tibble::as_tibble(sims)

## -----------------------------------------------------------------------------
model1 <- get_boin_comb(
  num_doses = num_doses, target = target, 
  p.saf = 0.6 * target, p.tox = 1.4 * target
) %>% 
  stop_at_n(n = 12) %>%
   select_boin_comb_mtd()

model2 <- get_boin_comb(
  num_doses = num_doses, target = target, 
  p.saf = 0.5 * target, p.tox = 1.5 * target
) %>% 
  stop_at_n(n = 12) %>%
   select_boin_comb_mtd()

designs <- list(
  model1 = model1,
  model2 = model2
)

set.seed(2025)
sims <- simulate_compare(
  designs = designs,
  num_sims = 10,
  true_prob_tox = true_prob_tox
)
convergence_plot(sims)

## -----------------------------------------------------------------------------
tibble::as_tibble(sims)

