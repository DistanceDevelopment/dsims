library(dsims)
library(testthat)

context("Basic line transect example")

test_that("Test creation and data generation", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Set up simulation
  region <- make.region()
  density <- make.density(region)
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          N = 500)
  detect <- make.detectability(key.function = "hn",
                               scale.param = 25,
                               truncation = 50)
  design <- make.design(region = region,
                        transect.type = "line",
                        samplers = 20,
                        truncation = 50)
  analysis <- make.ds.analysis(dfmodel = ~1,
                               key = "hn",
                               truncation = 50)
  sim <- make.simulation(reps = 5,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)

  survey <- run.survey(sim)
  expect_true(class(survey@transect) == "Line.Transect")

  sim <- run.simulation(sim)
  temp <- summary(sim, description.summary = FALSE)

})
