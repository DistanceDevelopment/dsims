library(dsims)
library(testthat)

context("Uniform key function analyses")

test_that("Uniform analyses can be created and covariates are rejected", {
  analysis <- make.ds.analysis(dfmodel = ~1, key = "unif", truncation = 50)
  expect_s4_class(analysis, "DS.Analysis")
  expect_equal(analysis@key, "unif")

  # "uf" is accepted for consistency with make.detectability
  analysis <- make.ds.analysis(dfmodel = ~1, key = "uf", truncation = 50)
  expect_equal(analysis@key, "unif")

  expect_error(make.ds.analysis(dfmodel = ~size, key = "unif", truncation = 50),
               "Covariates cannot be included in the model when the uniform key function is used")
  expect_error(make.ds.analysis(dfmodel = list(~1, ~size), key = c("hn", "unif"), truncation = 50),
               "Covariates cannot be included")

  analysis <- make.ds.analysis(dfmodel = ~1, key = c("hn", "unif"), truncation = 50, criteria = "AIC")
  expect_equal(analysis@key, c("hn", "unif"))

  expect_error(make.ds.analysis(dfmodel = ~1, key = "gamma", truncation = 50),
               "All key function values should be 'hn', 'hr' or 'unif'.")
})

test_that("Uniform analysis of uniform detections gives the strip estimate", {
  detect <- make.detectability(key.function = "uf", scale.param = 1, truncation = 50)
  set.seed(42)
  sim <- make.simulation(reps = 10,
                         detectability = detect,
                         ds.analysis = make.ds.analysis(dfmodel = ~1, key = "unif", truncation = 50))
  sim <- run.simulation(sim, run.parallel = FALSE, counter = FALSE)
  reps <- 1:10
  results <- sim@results
  ok <- !is.na(results$individuals$D[1, "Estimate", reps])
  expect_true(all(ok))
  # All animals in the strip are detected so the estimated average p is 1
  expect_equal(unname(results$Detection[1, "Pa", reps]), rep(1, 10))
  # Density estimate equals n / covered area
  n <- results$individuals$summary[1, "n", reps]
  covered.area <- results$individuals$summary[1, "CoveredArea", reps]
  expect_equal(unname(results$individuals$D[1, "Estimate", reps]), unname(n / covered.area))
  # Unbiased within Monte Carlo error
  sim.sum <- summary(sim, description.summary = FALSE)
  expect_lt(abs(sim.sum@individuals$N$percent.bias), 5)

  # Model selection between half-normal and uniform runs
  sim <- make.simulation(reps = 3,
                         detectability = detect,
                         ds.analysis = make.ds.analysis(dfmodel = ~1, key = c("hn", "unif"),
                                                        truncation = 50, criteria = "AIC"))
  sim <- run.simulation(sim, run.parallel = FALSE, counter = FALSE)
  expect_true(all(sim@results$Detection[1, "SuccessfulModels", 1:3] >= 1))
  expect_true(all(sim@results$Detection[1, "SelectedModel", 1:3] %in% c(1, 2)))
})

test_that("Uniform analysis underestimates when detection in the strip is below 1", {
  # g(0) < 1 cannot be estimated from the distances, so the uniform analysis
  # underestimates abundance by about (1 - scale.param)
  detect <- make.detectability(key.function = "uf", scale.param = 0.9, truncation = 50)
  set.seed(42)
  sim <- make.simulation(reps = 10,
                         detectability = detect,
                         ds.analysis = make.ds.analysis(dfmodel = ~1, key = "unif", truncation = 50))
  sim <- run.simulation(sim, run.parallel = FALSE, counter = FALSE)
  sim.sum <- summary(sim, description.summary = FALSE)
  expect_equal(sim.sum@individuals$N$percent.bias, -10, tolerance = 5, scale = 1)
})
