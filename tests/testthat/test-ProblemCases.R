library(dsims)
library(testthat)

context("Robustness")

test_that("Test problem cases: e.g. no/insufficient detections", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Set up simulation
  region <- make.region()
  density <- make.density(region)
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          N = 1)
  detect <- make.detectability(key.function = "hn",
                               scale.param = 0.01,
                               truncation = 0.01)
  design <- make.design(region = region,
                        transect.type = "line",
                        samplers = 2,
                        truncation = 0.01)
  analysis <- make.ds.analysis(dfmodel = ~1,
                               key = "hn",
                               truncation = 0.01)
  sim <- make.simulation(reps = 5,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)

  set.seed(923)
  expect_warning(run.survey(sim), "No detections")

  densities <- get.densities(density)
  densities <- rep(0, length(densities))
  densities[550] <- 1
  density <- set.densities(density, densities)

  set.seed(634)
  test <- expect_warning(run.survey(sim), "No detections")
  expect_true(nrow(test@population@population) == 1)

})

