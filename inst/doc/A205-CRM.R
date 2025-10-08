## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
skeleton <- c(0.05, 0.12, 0.25, 0.40, 0.55)
target <- 0.25
a0 <- 3
beta_sd <- sqrt(1.34)

## ----message=FALSE------------------------------------------------------------
library(escalation)
model1 <- get_dfcrm(skeleton = skeleton, target = target, model = 'logistic', 
                    intcpt = a0, scale = beta_sd)

## -----------------------------------------------------------------------------
model2 <- get_trialr_crm(skeleton = skeleton, target = target, model = 'logistic', 
                         a0 = a0, beta_mean = 0, beta_sd = beta_sd)

## -----------------------------------------------------------------------------
outcomes <- '3N 5N 5T 3N 4N'

fit1 <- model1 %>% fit(outcomes)
fit2 <- model2 %>% fit(outcomes)

## -----------------------------------------------------------------------------
fit1 %>% recommended_dose()
fit2 %>% recommended_dose()

## -----------------------------------------------------------------------------
fit1 %>% summary()

## -----------------------------------------------------------------------------
fit2 %>% summary()

## -----------------------------------------------------------------------------
model3 <- get_dfcrm(skeleton = skeleton, target = target, model = 'empiric', 
                    scale = beta_sd)

model4 <- get_trialr_crm(skeleton = skeleton, target = target, model = 'empiric', 
                         beta_sd = beta_sd)

## -----------------------------------------------------------------------------
fit3 <- model3 %>% fit(outcomes)
fit4 <- model4 %>% fit(outcomes) 

## -----------------------------------------------------------------------------
fit3 %>% summary()

## -----------------------------------------------------------------------------
fit4 %>% summary()

## -----------------------------------------------------------------------------
model5 <- get_trialr_crm(skeleton = skeleton, target = target, model = 'logistic2', 
                         alpha_mean = 0, alpha_sd = 2, beta_mean = 0, beta_sd = 1)
fit5 <- model5 %>% fit(outcomes)
fit5 %>% summary()

## -----------------------------------------------------------------------------
fit1 %>% continue()
fit5 %>% continue()

## -----------------------------------------------------------------------------
fit1 %>% dose_admissible()

## -----------------------------------------------------------------------------
model6 <- get_trialr_crm(skeleton = skeleton, target = 0.3, model = 'empiric', 
                         beta_sd = 1) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 6)

fit6 <- model6 %>% fit('2NNN 3TTT 2NTN')

fit6 %>% continue()
fit6 %>% recommended_dose()

## -----------------------------------------------------------------------------
model7 <- get_trialr_crm(skeleton = skeleton, target = 0.3, model = 'empiric', 
                         beta_sd = 1) %>% 
  stop_when_too_toxic(dose = 1, tox_threshold = 0.3, confidence = 0.8)

fit7 <- model7 %>% fit('1NTT 1TTN')

fit7 %>% continue()
fit7 %>% recommended_dose()
fit7 %>% dose_admissible()

## -----------------------------------------------------------------------------
model8 <- get_trialr_crm(skeleton = skeleton, target = 0.3, model = 'empiric', 
                         beta_sd = 1) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 6) %>% 
  stop_when_too_toxic(dose = 1, tox_threshold = 0.3, confidence = 0.8)


## -----------------------------------------------------------------------------
fit7 %>% 
  prob_tox_samples(tall = TRUE) %>% 
  head()

## ----message=FALSE------------------------------------------------------------
library(ggplot2)
library(dplyr)

fit7 %>% 
  prob_tox_samples(tall = TRUE) %>% 
  mutate(.draw = .draw %>% as.integer()) %>% 
  filter(.draw <= 200) %>% 
  ggplot(aes(dose, prob_tox)) + 
  geom_line(aes(group = .draw), alpha = 0.2)

## ----message=FALSE------------------------------------------------------------
skeleton <- c(0.05, 0.12, 0.25, 0.40, 0.55)
target <- 0.25
beta_sd <- 1

model <- get_dfcrm(skeleton = skeleton, target = target, model = 'empiric', 
                   scale = beta_sd)
paths <- model %>% get_dose_paths(cohort_sizes = c(3, 3), next_dose = 2)
graph_paths(paths)

## -----------------------------------------------------------------------------
model <- get_dfcrm(skeleton = skeleton, target = target, model = 'empiric', 
                   scale = beta_sd) %>% 
  stop_when_too_toxic(dose = 1, tox_threshold = target, confidence = 0.8) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 9) %>% 
  stop_at_n(n = 24)

## -----------------------------------------------------------------------------
num_sims <- 50

## -----------------------------------------------------------------------------
sc1 <- c(0.25, 0.5, 0.6, 0.7, 0.8)

## -----------------------------------------------------------------------------
set.seed(123)
sims <- model %>%
  simulate_trials(num_sims = num_sims, true_prob_tox = sc1, next_dose = 1)

sims

## -----------------------------------------------------------------------------
get_dfcrm(skeleton = skeleton, target = target, model = 'empiric', 
          scale = beta_sd) %>% 
  stop_when_too_toxic(dose = 1, tox_threshold = target, confidence = 0.8) %>% 
  stop_at_n(n = 24) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc1, next_dose = 1)


