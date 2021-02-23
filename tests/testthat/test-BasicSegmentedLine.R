library(dsims)
library(testthat)

context("Basic segmented line transect example")

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
                        design = "segmentedgrid",
                        seg.length = 50,
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
  expect_true(class(survey@transect) == "Segment.Transect")

  undebug(single.sim.loop)
  undebug(store.dht.results)
  sim.serial <- run.simulation(sim)
  #summary(sim.serial, description.summary = FALSE)


  sim.para <- run.simulation(sim, run.parallel = TRUE)
  # summary(sim.para)

})
