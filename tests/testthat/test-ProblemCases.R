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
  #expect_warning(run.survey(sim), "No detections")

  densities <- get.densities(density)
  densities <- rep(0, length(densities))
  densities[550] <- 1
  density <- set.densities(density, densities)

  set.seed(634)
  test <- run.survey(sim)
  expect_true(nrow(test@population@population) == 1)

})

test_that("Test segmented line sims run.", {
  
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
  
  set.seed(748)
  
  sim.serial <- run.simulation(sim, counter = FALSE)
  sum.sim <- summary(sim.serial, description.summary = FALSE)
  expect_s4_class(sum.sim, "Simulation.Summary")
  
  sim.para <- run.simulation(sim, run.parallel = TRUE, max.cores = 2, counter = FALSE)
  sum.para <- summary(sim.para, description.summary = FALSE)
  expect_s4_class(sum.para, "Simulation.Summary")
  
})

test_that("AICc simulation", {
  
  analyses <- make.ds.analysis(key = c("hn", "hr"),
                               criteria = "AICc")
  sim <- make.simulation(reps = 1, 
                         ds.analysis = analyses)
  
  sim <- run.simulation(sim)
 
  expect_s4_class(sim, "Simulation")
})

test_that("Check cannot get detections greater than truncation", {
  
  outer <- matrix(c(0,0,15,0,15,10,0,10,0,0),ncol=2, byrow=TRUE)
  pol1 <- sf::st_polygon(list(outer))
  pol2 <- sf::st_polygon(list(outer + 15))
  pol3 <- sf::st_polygon(list(outer + 30))
  sfc <- sf::st_sfc(pol1,pol2,pol3)
  strata.names <- c("SW", "central", "NE")
  mp1 <- sf::st_sf(strata = strata.names, geom = sfc)
  
  region <- make.region(region.name = "study.area", 
                        strata.name = strata.names, 
                        shape = mp1)
  
  density <- make.density(region = region,
                          x.space = 0.25,
                          constant = rep(1,3))
  
  popdesc <- make.population.description(region = region,
                                         density = density,
                                         N = rep(100,3),
                                         fixed.N = TRUE)
  
  design <- make.design(region,
                        spacing = 1,
                        truncation = 0.5)
  
  detect <- make.detectability(scale.param = 0.5,
                               truncation = 0.5)
  
  suppressWarnings(sim <- make.simulation(reps = 10,
                                          design = design,
                                          population.description = popdesc,
                                          detectability = detect))
  
  
  survey <- run.survey(sim)
  # Check all distances are less than truncation distance (Issue #76)
  expect_true(all(survey@dist.data$distance <= 0.5))
})