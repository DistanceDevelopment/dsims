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

  sim.serial <- run.simulation(sim, counter = FALSE)
  #summary(sim.serial, description.summary = FALSE)

  sim.para <- run.simulation(sim, run.parallel = TRUE, counter = FALSE)
  # summary(sim.para)

})

test_that("Test uf detectability key function", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Set up simulation
  region <- make.region()
  density <- make.density(region)
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          N = 500)
  detect <- make.detectability(key.function = "uf",
                               scale.param = 1,
                               truncation = 30)

  pop <- generate.population(pop.desc, region = region,
                             detectability = detect)
  # Check certain detection within truncation
  expect_true(all(pop@population$scale.param == 1))

  design <- make.design(region = region,
                        transect.type = "line",
                        samplers = 20,
                        truncation = 30)
  analysis <- make.ds.analysis(dfmodel = ~1,
                               key = "hn",
                               truncation = 30)
  sim <- make.simulation(reps = 5,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)

  survey <- run.survey(sim)
  expect_equal(length(survey@dists.in.covered), nrow(survey@dist.data))

})

