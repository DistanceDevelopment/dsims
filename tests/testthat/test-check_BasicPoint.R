library(dsims)
library(testthat)

context("Basic point transect example")

test_that("Can create object or return correct error messages", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Set up simulation
  outer = matrix(c(0,0,0,500,2000,500,2000,0,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(500,300,500,350,1500,350,1500,300,500,300),ncol=2, byrow=TRUE)
  pol1 = list(outer, hole1)
  mp = list(pol1)
  mp1 = sf::st_multipolygon(mp)

  region <- make.region(region.name = "main",
                        shape = mp1)
  # Create density
  density <- make.density(region = region, x.space = 20, constant = 10)
  # Add hotspot
  density <- add.hotspot(density, c(500,250), 200, 200)

  # Population description
  covariate.list <- list()
  covariate.list$size <- list(distribution = "poisson", lambda = 25)
  pop.descrp <- make.population.description(region = region,
                                            density = density,
                                            covariates = covariate.list,
                                            N = 300)

  cov.params <- list(size = log(1.02))

  detect <- make.detectability(key.function = "hn",
                               scale.param = 25,
                               truncation = 75,
                               cov.param = cov.params)


  # Make the design
  design <- make.design(region = region,
                        transect.type = "point",
                        design = "systematic",
                        samplers = 30,
                        truncation = 75)

  # Make analyses
  analyses <- make.ds.analysis(dfmodel = ~1,
                               key = "hr",
                               truncation = 75,
                               er.var = "P3")

  # Make.simulation
  sim <- make.simulation(reps = 1,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analyses)

  # Create a test survey
  survey <- run.survey(sim)
  expect_true(inherits(survey@transect, "Point.Transect"))

  # test running the simulation
  #set.seed(555)
  #test <- run.simulation(sim, counter = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


})
