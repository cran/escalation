## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(escalation)

target <- 0.25
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)

designs <- 
  list(
    "3+3" = get_three_plus_three(num_doses = 5),
    "CRM" = get_dfcrm(skeleton = skeleton, target = target) %>%
      stop_at_n(n = 12),
    "Stopping CRM" = get_dfcrm(skeleton = skeleton, target = target) %>%
      stop_at_n(n = 12) %>%
      stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.8)
  )

## -----------------------------------------------------------------------------
num_sims <- 20
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)

sims <- simulate_compare(
  designs, 
  num_sims = num_sims, 
  true_prob_tox = true_prob_tox
)

## ---- fig.width=7, fig.height=5-----------------------------------------------
convergence_plot(sims)

## ---- fig.width=7, fig.height=10, message=FALSE-------------------------------
library(dplyr)
library(ggplot2)

as_tibble(sims) %>% 
  filter(n %% 5 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )

## -----------------------------------------------------------------------------
num_doses <- 5

designs <- list(

  "BOIN12 v1" = get_boin12(num_doses = num_doses,
                          phi_t = 0.35, phi_e = 0.25,
                          u2 = 40, u3 = 60,
                          c_t = 0.95, c_e = 0.9) %>%
    stop_when_n_at_dose(n = 12,  dose = 'any') %>%
    stop_at_n(n = 36),

  "BOIN12 v2" = get_boin12(num_doses = num_doses,
                          phi_t = 0.35, phi_e = 0.25,
                          u2 = 40, u3 = 60,
                          c_t = 0.85, c_e = 0.8
                          ) %>%
    stop_when_n_at_dose(n = 12,  dose = 'any') %>%
    stop_at_n(n = 36)
)

## -----------------------------------------------------------------------------
true_prob_tox <- c(0.05, 0.10, 0.15, 0.18, 0.45)
true_prob_eff <- c(0.40, 0.50, 0.52, 0.53, 0.53)

set.seed(2024)
x <- simulate_compare(
  designs = designs, 
  num_sims = 50, 
  true_prob_tox = true_prob_tox, 
  true_prob_eff = true_prob_eff, 
  return_patient_samples = TRUE
)

## -----------------------------------------------------------------------------
ps <- attr(x, "patient_samples")

## -----------------------------------------------------------------------------
length(ps)

## -----------------------------------------------------------------------------
ps[[1]]$tox_u

## -----------------------------------------------------------------------------
ps[[1]]$eff_u

## -----------------------------------------------------------------------------
z <- get_potential_outcomes(
  patient_samples = ps,
  true_prob_tox = true_prob_tox,
  true_prob_eff = true_prob_eff
)

## -----------------------------------------------------------------------------
z[[1]]

## -----------------------------------------------------------------------------
num_sims <- 10
ps <- lapply(1:num_sims, function(x) PatientSample$new())

## -----------------------------------------------------------------------------
set.seed(2024)
for(i in seq_len(num_sims)) {
  tox_u_new <- runif(n = 50)
  eff_u_new <- runif(n = 50)
  ps[[i]]$set_eff_and_tox(tox_u = tox_u_new, eff_u = eff_u_new)
}

## -----------------------------------------------------------------------------
x1 <- simulate_compare(
  designs = designs, 
  num_sims = length(ps), 
  true_prob_tox = true_prob_tox, 
  true_prob_eff = true_prob_eff, 
  patient_samples = ps
)

## -----------------------------------------------------------------------------
x2 <- simulate_compare(
  designs = designs, 
  num_sims = length(ps), 
  true_prob_tox = true_prob_tox, 
  true_prob_eff = true_prob_eff, 
  patient_samples = ps
)

## -----------------------------------------------------------------------------
all(
  recommended_dose(x1[["BOIN12 v1"]]) == 
    recommended_dose(x2[["BOIN12 v1"]])
)

## -----------------------------------------------------------------------------
ps <- CorrelatedPatientSample$new(num_patients = 100, rho = 0.5)

## -----------------------------------------------------------------------------
cor(ps$tox_u, ps$eff_u)

## -----------------------------------------------------------------------------
tox <- sapply(seq_len(100), function(i) ps$get_patient_tox(i, prob_tox = 0.3))
eff <- sapply(seq_len(100), function(i) ps$get_patient_eff(i, prob_eff = 0.4))
cor(tox, eff)

