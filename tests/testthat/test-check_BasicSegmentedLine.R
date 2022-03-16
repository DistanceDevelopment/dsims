library(dsims)
library(testthat)

context("Segmented line transect example, hr detectability, binned analysis")

test_that("Test creation and data generation", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Set up simulation
  region <- make.region()
  density <- make.density(region)
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          N = 1000)

  detect <- make.detectability(key.function = "hr",
                               scale.param = 25,
                               shape.param = 3,
                               truncation = 50)

  design <- make.design(region = region,
                        transect.type = "line",
                        design = "segmentedgrid",
                        seg.length = 100,
                        design.angle = 0,
                        samplers = 20,
                        truncation = 50)

  analysis.bin <- make.ds.analysis(dfmodel = ~1,
                                   key = "hn",
                                   cutpoints = seq(0, 50, length = 6),
                                   truncation = 50)

  sim <- make.simulation(reps = 4,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis.bin)

  set.seed(747)
  survey <- run.survey(sim)
  expect_true(inherits(survey@transect, "Segment.Transect"))
  expect_true("shape.param" %in% names(survey@population@population))
  expect_true(all(survey@population@population$shape.param == 3))

  test <- analyse.data(analysis.bin, survey)
  if(!is.null(test$model)){
    expect_true("distbegin" %in% names(test$model$ddf$data))
  }

  sim.serial <- run.simulation(sim, counter = FALSE)
  sum.sim <- summary(sim.serial, description.summary = FALSE)

  #sim.para <- run.simulation(sim, run.parallel = TRUE, max.cores = 2, counter = FALSE)
  #sum.para <- summary(sim.para, description.summary = FALSE)

})
