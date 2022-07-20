library(dsims)
library(testthat)

context("Covariate Checks")

test_that("Can deal with different covariate options", {

  #Set up simulation
  outer1 = matrix(c(0,0,0,500,1000,500,1000,0,0,0),ncol=2, byrow=TRUE)
  outer2 = matrix(c(1000,0,1000,1000,2000,1000,2000,0,1000,0),ncol=2, byrow=TRUE)
  pol1 <- sf::st_polygon(list(outer1))
  pol2 <- sf::st_polygon(list(outer2))
  sfc <- sf::st_sfc(pol1,pol2)
  strata.names <- c("West", "East")
  mp1 <- sf::st_sf(strata = strata.names, geom = sfc)

  region <- make.region(region.name = "main",
                        strata.name = c("West", "East"),
                        shape = mp1)

  density <- make.density(region = region, x.space = 20, constant = 10)
  density <- add.hotspot(density, c(500,250), 200, 200)

  covariate.list <- list()
  covariate.list$height <- list(distribution = "normal", mu = 1.7, sigma = 0.25)
  expect_error(pop.descrp <- make.population.description(region = region,
                                                         density = density,
                                                         covariates = covariate.list,
                                                         N = c(250,500)),
               "You have not supplied all the required parameters \\(mean, sd\\) for the following covariate distribution: normal")
  
  # Check that means <= 1 are not permitted
  covs <- list()
  covs$size <- list(list(distribution = "ztruncpois", mean = 2),
                    list(distribution = "ztruncpois", mean = 1))
  expect_error(make.population.description(region = region,
                                         density = density,
                                         covariates = covs,
                                         fixed.N = FALSE),
               "The mean parameter for covariate size and strata 2 must be greater than 1.")
  
  covs <- list()
  covs$size <- list(list(distribution = "ztruncpois", mean = 0.5),
                    list(distribution = "ztruncpois", mean = 2))
  expect_error(make.population.description(region = region,
                                           density = density,
                                           covariates = covs,
                                           fixed.N = FALSE),
               "The mean parameter for covariate size and strata 1 must be greater than 1.")
  

  covariate.list <- list()
  covariate.list$height <- list(distribution = "normal", mean = 1.7, sd = 0.25)
  pop.descrp <- make.population.description(region = region,
                                            density = density,
                                            covariates = covariate.list,
                                            N = c(250,500))

  cov.params <- list(height = log(1.75))

  detect <- make.detectability(key.function = "hn",
                               scale.param = 4,
                               truncation = 25,
                               cov.param = cov.params)

  design <- make.design(region = region,
                        transect.type = "line",
                        design = "systematic",
                        spacing = 75,
                        truncation = 25)

  analyses <- make.ds.analysis(dfmodel = list(~1, ~height),
                               key = c("hn","hn"),
                               truncation = 25)

  sim <- make.simulation(reps = 10,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analyses)

  survey <- run.survey(sim)

  summary.sim <- summary(sim, description.summary = FALSE)
  expect_true(inherits(summary.sim, "Simulation.Summary"))

})

test_that("Can run simulation with cluster size", {
  
  #Set up simulatio
  region <- make.region(region.name = "main")
  
  density <- make.density(region = region)
  
  covariate.list <- list()
  covariate.list$size <- list(distribution = "ztruncpois", mean = 5)
  pop.descrp <- make.population.description(region = region,
                                            density = density,
                                            covariates = covariate.list,
                                            N = 250)
  
  detect <- make.detectability()
  
  design <- make.design(region = region)
  
  expect_error(make.ds.analysis(dfmodel = list(~1),
                               key = c("hn","hn"),
                               truncation = 50),
               "All models must be unique, there appears to be the same combination of key function and dfmodel entered more than once.")
  
  analyses <- make.ds.analysis(dfmodel = list(~1, ~size),
                               key = c("hn","hn"),
                               truncation = 50)
  expect_equivalent(analyses@dfmodel, list(~1, ~size))
  
  expect_error(make.ds.analysis(dfmodel = list(~size, ~size),
                               key = c("hr","hr"),
                               truncation = 50),
               "All models must be unique, there appears to be the same combination of key function and dfmodel entered more than once.")
  
  analyses <- make.ds.analysis(dfmodel = list(~1, ~size,~1, ~size),
                               key = c("hn","hn","hr","hr"),
                               truncation = 50)
  expect_equivalent(analyses@dfmodel, list(~1, ~size,~1, ~size))
  
  expect_error(make.ds.analysis(dfmodel = list(~1, ~1, ~1),
                                key = c("hn","hr", "hn"),
                                truncation = 50),
               "All models must be unique, there appears to be the same combination of key function and dfmodel entered more than once.")
  
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = "hn",
                               truncation = 50)
  
  sim <- make.simulation(reps = 3,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analyses)
  
  survey <- run.survey(sim)
  
  sim.tmp <- run.simulation(sim)
  expect_true(inherits(sim.tmp, "Simulation"))
  
  summary.sim <- summary(sim.tmp, description.summary = FALSE)
  expect_true(inherits(summary.sim, "Simulation.Summary"))
  
  expect_s3_class(summary.sim@expected.size, "data.frame")
  
  # Test new code to check it works in the rare situation that two models have identical AIC values
  # Cannot test all code but should now run for repeats of the same model and other code is the same.
  
  analyses@dfmodel <- list(~1, ~1)
  analyses@key <- c("hn", "hn")
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analyses)
  sim.check <- run.simulation(sim)
  
  expect_equal(sim.check@warnings$message[[1]],
               "Two or more models had the same information criterion value and the same number of parameters, taking the first model of: models 1, 2")
  
})

test_that("Size biased testing", {
  
  region <- make.region()
  
  density <- make.density(region = region, x.space = 20, constant = 10)
  
  design <- make.design()
  
  covariate.list <- list()
  covariate.list$size <- data.frame(level = c(5, 25),
                                    prob = c(0.5, 0.5))
  pop.descrp <- make.population.description(region = region,
                                            density = density,
                                            covariates = covariate.list,
                                            N = c(2500))
  cov.param <- list()
  cov.param$size <- c(log(1.075))
  detect <- make.detectability(key.function = "hn",
                               scale.param = 5,
                               cov.param = cov.param,
                               truncation = 50)
  
  analyses <- make.ds.analysis()
  
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analyses)
  
  survey <- run.survey(sim)
  
  pop <- survey@population
  
  # Check that the scale parameters are different for the two sets
  # of cluster sizes
  index <- which(pop@population$size == 5)
  expect_equal(pop@population$scale.param[index], 
               rep(exp(log(5)+5*log(1.075)), length(index)))
  index <- which(pop@population$size == 25)
  expect_equal(pop@population$scale.param[index], 
               rep(exp(log(5)+25*log(1.075)), length(index)))
               
  dist.data <- survey@dist.data
  percent.detected <- round(table(dist.data$size)/table(pop@population$size)*100,2)
  
  # Check that the percent of the clusters of 5 detected is less than the 25's
  expect_lt(percent.detected[1], percent.detected[2])
  expect_gt(mean(dist.data$size), mean(c(5,25)))
  
  # analyses <- make.ds.analysis(dfmodel = list(~size),
  #                              truncation = 30)
  # sim <- make.simulation(reps = 50,
  #                        design = design,
  #                        population.description = pop.descrp,
  #                        detectability = detect,
  #                        ds.analysis = analyses)
  # 
  # sim<- run.simulation(sim)
  # summary(sim)
  
})

