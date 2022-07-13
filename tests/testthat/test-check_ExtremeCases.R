library(dsims)
library(testthat)

context("Extreme Scenarios")

test_that("Test very few or no detections", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Set up simulation
  region <- make.region()
  density <- make.density(region)
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          N = 1)
  detect <- make.detectability(key.function = "hn",
                               scale.param = 0.000000001,
                               truncation = 0.00001)
  design <- make.design(region = region,
                        transect.type = "line",
                        samplers = 1,
                        truncation = 0.000001)
  analysis <- make.ds.analysis(dfmodel = ~1,
                               key = "hn",
                               truncation = 0.000001)
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)
  set.seed(777)
  survey <- run.survey(sim)

  expect_warning(results <- analyse.data(sim@ds.analysis, survey),
                 "No detections, cannot fit model.")
  
  # Now test 1 detection
  # Leave population size as 1 and make detectability 100%
  detect <- make.detectability(key.function = "hn",
                               scale.param = 5000,
                               truncation = 50)
  design <- make.design(region = region,
                        transect.type = "line",
                        samplers = 20,
                        truncation = 50)
  analysis <- make.ds.analysis(dfmodel = ~1,
                               key = "hn",
                               truncation = 50)
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)
  
  survey <- run.survey(sim)
  
  results <- analyse.data(sim@ds.analysis, survey, warnings = list())
  
  # Check warnings
  expect_equal(results$warnings$message[[1]],
                    "Low number of detections (<20), models may become unstable or give errors especially if complex models are being fitted.")
  expect_equal(results$warnings$message[[2]],
                    "simpleWarning in qt(ci.width, estimate.table$df): NaNs produced\n (Model number: 1)")
  
  # Test with 2 detections & half normal
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          N = 2)
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)
  
  survey <- run.survey(sim)
  
  expect_warning(tmp <- analyse.data(sim@ds.analysis, survey),
                 "Low number of detections \\(2\\), models may become unstable or give errors especially if complex models are being fitted.")
  
  
  # Test with 2 detections & half normal
  analysis <- make.ds.analysis(dfmodel = ~1,
                               key = "hr",
                               truncation = 50)
  
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)
  
  survey <- run.survey(sim)
  
  # Takes too long to run
  #results <- analyse.data(sim@ds.analysis, survey, warnings = list())
  
  # Add in cluster size and height and fit a hr with 4 detections
  covs <- list()
  covs$size <- list(list(distribution = "poisson", lambda = 25))
  covs$height <- list(list(distribution = "normal", mean = 125, sd =25))
  # 4 detectionn
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          covariates = covs,
                                          N = 4)
  analysis <- make.ds.analysis(dfmodel = ~size+height,
                               key = "hr",
                               truncation = 50)
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)
  
  survey <- run.survey(sim)
  
  results <- analyse.data(sim@ds.analysis, survey, warnings = list(), i = 1)
  
  # Turn it into a warning to allow comparison
  expect_warning(warning(results$warnings$message[[1]]),
               "Low number of detections \\(<20\\), models may become unstable or give errors especially if complex models are being fitted.")

})

