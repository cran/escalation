
test_that('CorrelatedPatientSample works like it should.', {

  ps <- CorrelatedPatientSample$new(num_patients = 10, rho = -0.5)

  expect_equal(
    ps$num_patients,
    10
  )
  expect_true(ps$can_grow)

  # Test toxicity and efficacy events at infinite time (the default)
  tox_u7 <- ps$get_tox_u(i = 7)
  eff_u3 <- ps$get_eff_u(i = 3)
  t7 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_tox(i = 7, prob_tox = x)
  )
  e3 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_eff(i = 3, prob_eff = x)
  )
  # TODO At most one turn

  # Test toxicity and efficacy events at specified time
  tox_time7 <- ps$tox_time[7]
  expect_equal(
    ps$get_patient_tox(i = 7, prob_tox = tox_u7 * 1.01, time = tox_time7),
    1
  )
  expect_equal(
    ps$get_patient_tox(i = 7, prob_tox = tox_u7 * 1.01, time = 0.99 * tox_time7),
    0
  )
  eff_time3 <- ps$eff_time[3]
  expect_equal(
    ps$get_patient_eff(i = 3, prob_eff = eff_u3 * 1.01, time = eff_time3),
    1
  )
  expect_equal(
    ps$get_patient_eff(i = 3, prob_eff = eff_u3 * 1.01, time = 0.99 * eff_time3),
    0
  )

  # Expand sample manually
  ps$expand_to(num_patients = 12)
  expect_equal(
    ps$num_patients,
    12
  )
  expect_true(ps$can_grow)
  # Expansion should not affect what was already known
  t7_v2 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_tox(i = 7, prob_tox = x)
  )
  expect_equal(t7, t7_v2)
  e3_v2 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_eff(i = 3, prob_eff = x)
  )
  expect_equal(e3, e3_v2)

  # Expand sample implicitly
  z <- ps$get_patient_eff(i = 15, 0.1)
  expect_equal(
    ps$num_patients,
    15
  )
  expect_true(ps$can_grow)
  # Expansion should not affect what was already known
  t7_v3 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_tox(i = 7, prob_tox = x)
  )
  expect_equal(t7, t7_v3)
  e3_v3 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_eff(i = 3, prob_eff = x)
  )
  expect_equal(e3, e3_v3)


  # Extract tox_u and eff_u
  tox_u <- ps$tox_u
  eff_u <- ps$eff_u
  expect_equal(
    tox_u7,
    tox_u[7]
  )
  expect_equal(
    eff_u3,
    eff_u[3]
  )

  # Correlations in largish sample
  ps$expand_to(100)
  expect_lt(
    cor(ps$tox_u, ps$eff_u),
    0
  )
  tox_events <- ps$get_patient_tox(i = seq_len(ps$num_patients), prob_tox = 0.5)
  eff_events <- ps$get_patient_eff(i = seq_len(ps$num_patients), prob_eff = 0.3)
  expect_lt(
    cor(tox_events, eff_events),
    0
  )

  # Set tox_u and eff_u, without specifying event times
  tox_u_new <- runif(n = 20)
  eff_u_new <- runif(n = 20)
  ps$set_eff_and_tox(tox_u_new, eff_u_new)
  expect_equal(
    ps$num_patients,
    20
  )
  expect_false(ps$can_grow) # Expansion is now prevented
  # What was known is now erased
  tox_u7_v2 <- ps$get_tox_u(i = 7)
  expect_true(tox_u7 != tox_u7_v2)
  eff_u3_v2 <- ps$get_eff_u(i = 3)
  expect_true(eff_u3 != eff_u3_v2)
  # Further expansion is now not possible
  expect_error(ps$expand_to(25))
  expect_error(ps$get_tox_u(i = 25))
  expect_error(ps$get_patient_tox(i = 25, prob_tox = 0.1))
  expect_error(ps$get_patient_eff(i = 25, prob_eff = 0.1))

  # Set tox_u and eff_u manually, plus event times
  tox_time <- rexp(n = 20)
  eff_time <- rexp(n = 20, rate = 0.5)
  ps$set_eff_and_tox(tox_u = tox_u_new, eff_u = eff_u_new,
                     tox_time = tox_time, eff_time = eff_time)
  expect_equal(
    ps$num_patients,
    20
  )
  expect_false(ps$can_grow) # Expansion is now prevented
  # What was known is now erased
  tox_u7_v4 <- ps$get_tox_u(i = 7)
  expect_true(tox_u7 != tox_u7_v4)
  eff_u3_v4 <- ps$get_eff_u(i = 3)
  expect_true(eff_u3 != eff_u3_v4)
  # Further expansion is now not possible
  expect_error(ps$expand_to(25))
  expect_error(ps$get_tox_u(i = 25))
  expect_error(ps$get_patient_tox(i = 25, prob_tox = 0.1))
  expect_error(ps$get_eff_u(i = 25))
  expect_error(ps$get_patient_eff(i = 25, prob_eff = 0.1))

  expect_equal(
    ps$get_patient_tox(
      i = 7,
      prob_tox = 1.01 * ps$tox_u[7],
      time = 1.01 * ps$tox_time[7]
    ),
    1
  )
  expect_equal(
    ps$get_patient_tox(
      i = 7,
      prob_tox = 1.01 * ps$tox_u[7],
      time = 0.99 * ps$tox_time[7]
    ),
    0
  )
  expect_equal(
    ps$get_patient_tox(
      i = 7,
      prob_tox = 0.99 * ps$tox_u[7],
      time = 1.01 * ps$tox_time[7]
    ),
    0
  )
  expect_equal(
    ps$get_patient_tox(
      i = 7,
      prob_tox = 0.99 * ps$tox_u[7],
      time = 0.99 * ps$tox_time[7]
    ),
    0
  )

  expect_equal(
    ps$get_patient_eff(
      i = 3,
      prob_eff = 1.01 * ps$eff_u[3],
      time = 1.01 * ps$eff_time[3]
    ),
    1
  )
  expect_equal(
    ps$get_patient_eff(
      i = 3,
      prob_eff = 1.01 * ps$eff_u[3],
      time = 0.99 * ps$eff_time[3]
    ),
    0
  )
  expect_equal(
    ps$get_patient_eff(
      i = 3,
      prob_eff = 0.99 * ps$eff_u[3],
      time = 1.01 * ps$eff_time[3]
    ),
    0
  )
  expect_equal(
    ps$get_patient_eff(
      i = 3,
      prob_eff = 0.99 * ps$eff_u[3],
      time = 0.99 * ps$eff_time[3]
    ),
    0
  )
})
