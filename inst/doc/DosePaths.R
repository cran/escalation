## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(escalation)

## -----------------------------------------------------------------------------
paths <- get_three_plus_three(num_doses = 5, allow_deescalate = TRUE) %>% 
  get_dose_paths(cohort_sizes = c(3, 3))

## -----------------------------------------------------------------------------
paths

## ---- fig.width=7, fig.height=7-----------------------------------------------
if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths)
}

## -----------------------------------------------------------------------------
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

## -----------------------------------------------------------------------------
paths <- get_dfcrm(skeleton = skeleton, target = target) %>%
  get_dose_paths(cohort_sizes = c(3, 3))

## ---- fig.width=7, fig.height=7-----------------------------------------------
if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, viridis_palette = 'magma')
}

## -----------------------------------------------------------------------------
paths <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.9) %>% 
  get_dose_paths(cohort_sizes = c(3, 3))

## ---- fig.width=7, fig.height=7-----------------------------------------------
if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, viridis_palette = 'inferno')
}

## ---- fig.width=7, fig.height=7-----------------------------------------------
paths <- get_boin(num_doses = 4, target = target) %>% 
  get_dose_paths(cohort_sizes = rep(2, 4))

if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, RColorBrewer_palette = 'YlOrRd')
}

## ---- fig.width=7, fig.height=7-----------------------------------------------
paths <- get_boin(num_doses = 4, target = target) %>% 
  get_dose_paths(cohort_sizes = c(3, 1, 2))

if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, RColorBrewer_palette = 'Blues')
}

## ---- fig.width=7, fig.height=7-----------------------------------------------
paths <- get_boin(num_doses = 4, target = target) %>% 
  get_dose_paths(cohort_sizes = rep(1, 4))

if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, RColorBrewer_palette = 'RdPu')
}

## ---- fig.width=7, fig.height=7-----------------------------------------------
paths <- get_boin(num_doses = 4, target = target) %>% 
  get_dose_paths(cohort_sizes = rep(3, 2), previous_outcomes = '1NNN 2TNT')

if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, viridis_palette = 'viridis')
}

## ---- fig.width=7, fig.height=7-----------------------------------------------
paths <- get_boin(num_doses = 4, target = target) %>% 
  get_dose_paths(cohort_sizes = rep(3, 2))

if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, viridis_palette = 'viridis')
}

## ---- fig.width=7, fig.height=7-----------------------------------------------
paths <- get_three_plus_three(num_doses = 5, allow_deescalate = TRUE) %>% 
  get_dose_paths(cohort_sizes = c(3, 3), next_dose = 3)

if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths, viridis_palette = 'plasma')
}

## -----------------------------------------------------------------------------
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

paths <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.9) %>% 
  get_dose_paths(cohort_sizes = rep(3, 4))

## -----------------------------------------------------------------------------
true_prob_tox <- skeleton

x <- paths %>% calculate_probabilities(true_prob_tox = true_prob_tox)
x

## -----------------------------------------------------------------------------
true_prob_tox <- c(0.45, 0.6, 0.68, 0.75, 0.81)

x <- paths %>% calculate_probabilities(true_prob_tox = true_prob_tox)
x

## -----------------------------------------------------------------------------
paths <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.9) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 9) %>% 
  get_dose_paths(cohort_sizes = rep(3, 4))

x <- paths %>% calculate_probabilities(true_prob_tox = true_prob_tox)
x

## -----------------------------------------------------------------------------
paths <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.9) %>% 
  stop_when_n_at_dose(dose = 'recommended', n = 12) %>% 
  get_dose_paths(cohort_sizes = rep(3, 4))

x <- paths %>% calculate_probabilities(true_prob_tox = true_prob_tox)
x

## -----------------------------------------------------------------------------
num_dose_path_nodes(num_patient_outcomes = 2, cohort_sizes = rep(3, 5))

## -----------------------------------------------------------------------------
num_dose_path_nodes(num_patient_outcomes = 2, cohort_sizes = rep(3, 5)) %>% 
  sum

## -----------------------------------------------------------------------------
num_dose_path_nodes(num_patient_outcomes = 2, cohort_sizes = rep(3, 8)) %>% 
  sum

