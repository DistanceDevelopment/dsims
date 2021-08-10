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
  expect_true("Simulation.Summary" %in% class(summary.sim))

})
