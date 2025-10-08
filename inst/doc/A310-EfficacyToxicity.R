## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

## -----------------------------------------------------------------------------
outcomes <- '2TEB'

## ----message=FALSE------------------------------------------------------------
library(escalation)

outcomes <- "1NNN 2EBT"

## -----------------------------------------------------------------------------
p <- trialr::efftox_priors(
  alpha_mean = -7.9593, alpha_sd = 3.5487,
  beta_mean = 1.5482, beta_sd = 3.5018,
  gamma_mean = 0.7367, gamma_sd = 2.5423,
  zeta_mean = 3.4181, zeta_sd = 2.4406,
  eta_mean = 0, eta_sd = 0.2,
  psi_mean = 0, psi_sd = 1
)
real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
num_doses <- length(real_doses)

et_model <- get_trialr_efftox(
  real_doses = real_doses,
  efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
  p_e = 0.1, p_t = 0.1,
  eff0 = 0.5, tox1 = 0.65,
  eff_star = 0.7, tox_star = 0.25,
  priors = p, 
  # Low MCMC sample size purely for speed in vignette:
  iter = 1000, chains = 1, seed = 2020
)

## -----------------------------------------------------------------------------
et_fit <- et_model %>% fit(outcomes)

## -----------------------------------------------------------------------------
et_fit

## -----------------------------------------------------------------------------
recommended_dose(et_fit)

## -----------------------------------------------------------------------------
continue(et_fit)

## -----------------------------------------------------------------------------
dose_admissible(et_fit)

## -----------------------------------------------------------------------------
tox_skeleton = c(0.08, 0.15, 0.22, 0.29, 0.36)

eff_skeletons = matrix(nrow = 9, ncol = num_doses)
eff_skeletons[1,] <- c(0.60, 0.50, 0.40, 0.30, 0.20)
eff_skeletons[2,] <- c(0.50, 0.60, 0.50, 0.40, 0.30)
eff_skeletons[3,] <- c(0.40, 0.50, 0.60, 0.50, 0.40)
eff_skeletons[4,] <- c(0.30, 0.40, 0.50, 0.60, 0.50)
eff_skeletons[5,] <- c(0.20, 0.30, 0.40, 0.50, 0.60)
eff_skeletons[6,] <- c(0.30, 0.40, 0.50, 0.60, 0.60)
eff_skeletons[7,] <- c(0.40, 0.50, 0.60, 0.60, 0.60)
eff_skeletons[8,] <- c(0.50, 0.60, 0.60, 0.60, 0.60)
eff_skeletons[9,] <- c(0.60, 0.60, 0.60, 0.60, 0.60)
eff_skeleton_weights = rep(1, nrow(eff_skeletons))

wt_model <- get_wages_and_tait(
  tox_skeleton = tox_skeleton,
  eff_skeletons = eff_skeletons,
  tox_limit = 0.3, eff_limit = 0.5,
  num_randomise = 20
)

## -----------------------------------------------------------------------------
wt_fit <- wt_model %>% fit(outcomes)

## -----------------------------------------------------------------------------
recommended_dose(wt_fit)

## -----------------------------------------------------------------------------
continue(wt_fit)

## -----------------------------------------------------------------------------
dose_admissible(wt_fit)

## -----------------------------------------------------------------------------
is_randomising(wt_fit)

## -----------------------------------------------------------------------------
b_model <- get_boin12(
  num_doses = 5, phi_t = 0.3, phi_e = 0.5, u2 = 40, u3 = 60, n_star = 6
)

## -----------------------------------------------------------------------------
b_fit <- b_model %>% fit(outcomes)

## -----------------------------------------------------------------------------
recommended_dose(b_fit)

## -----------------------------------------------------------------------------
continue(b_fit)

## -----------------------------------------------------------------------------
dose_admissible(b_fit)

## -----------------------------------------------------------------------------
b_model2 <- get_boin12(
  num_doses = 5, phi_t = 0.3, phi_e = 0.5, u2 = 40, u3 = 60, n_star = 6
) %>%
  stop_at_n(n = 12) %>%
  select_boin12_obd()

outcomes <- '1NNN 2NTN 2NNN 3BEN'
b_model2 %>% fit(outcomes) %>% recommended_dose()

## -----------------------------------------------------------------------------
boin12_rds(
 sample_sizes = c(0, 3, 6, 9),
 phi_t = 0.35,
 phi_e = 0.25,
 u1 = 100,
 u2 = 40,
 u3 = 60,
 u4 = 0,
 c_t = 0.95,
 c_e = 0.9,
 prior_alpha = 1,
 prior_beta = 1
)

## -----------------------------------------------------------------------------
cohort_sizes <- c(2, 2)
outcomes <- "1NN"

et_paths <- et_model %>% 
  get_dose_paths(
    cohort_sizes = cohort_sizes, 
    previous_outcomes = outcomes,
    next_dose = 2
  )

graph_paths(et_paths)

## -----------------------------------------------------------------------------
wt_paths <- wt_model %>% 
  get_dose_paths(
    cohort_sizes = cohort_sizes, 
    previous_outcomes = outcomes,
    next_dose = 2
  )

graph_paths(wt_paths)

## -----------------------------------------------------------------------------
b_paths <- b_model %>% 
  get_dose_paths(
    cohort_sizes = cohort_sizes, 
    previous_outcomes = outcomes,
    next_dose = 2
  )

graph_paths(b_paths)

## -----------------------------------------------------------------------------
n_sim <- 20
true_prob_tox = c(0.02, 0.12, 0.17, 0.38, 0.55)
true_prob_eff = c(0.25, 0.27, 0.52, 0.54, 0.54)

## -----------------------------------------------------------------------------
et_model <- get_trialr_efftox(
  real_doses = real_doses,
  efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
  p_e = 0.1, p_t = 0.1,
  eff0 = 0.5, tox1 = 0.65,
  eff_star = 0.7, tox_star = 0.25,
  priors = p, 
  # Low MCMC sample size purely for speed in vignette:
  iter = 2000, chains = 1, 
) %>% 
  dont_skip_doses() %>% 
  stop_at_n(n = 12)

set.seed(2025)
et_sims <- et_model %>% 
  simulate_trials(
    num_sims = n_sim, 
    true_prob_tox = true_prob_tox,
    true_prob_eff = true_prob_tox
  )
et_sims

## -----------------------------------------------------------------------------
wt_model <- get_wages_and_tait(
  tox_skeleton = tox_skeleton,
  eff_skeletons = eff_skeletons,
  tox_limit = 0.3, eff_limit = 0.5,
  num_randomise = 20
) %>% 
  dont_skip_doses() %>% 
  stop_at_n(n = 12)

set.seed(2025)
wt_sims <- wt_model %>% 
  simulate_trials(
    num_sims = n_sim, 
    true_prob_tox = true_prob_tox,
    true_prob_eff = true_prob_tox
  )
wt_sims

## -----------------------------------------------------------------------------
b_model <- get_boin12(
  num_doses = 5, phi_t = 0.3, phi_e = 0.5, u2 = 40, u3 = 60, n_star = 6
) %>%
  dont_skip_doses() %>% 
  stop_at_n(n = 12) %>% 
  select_boin12_obd()
  
set.seed(2025)
b_sims <- b_model %>% 
  simulate_trials(
    num_sims = n_sim, 
    true_prob_tox = true_prob_tox,
    true_prob_eff = true_prob_tox
  )
b_sims

## -----------------------------------------------------------------------------
designs <- list(
  EffTox = et_model,
  WagesTait = wt_model,
  BOIN12 = b_model
)

sims <- simulate_compare(
  designs = designs, 
  num_sims = n_sim, 
  true_prob_tox = true_prob_tox,
  true_prob_eff = true_prob_tox
)
convergence_plot(sims)

