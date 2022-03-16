library(dsims)
library(testthat)

context("Analysis Option Checks")

test_that("Can create objects or return correct error / warning messages", {

  #Set up simulation
  outer = matrix(c(0,0,0,500,2000,500,2000,0,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(500,300,500,350,1500,350,1500,300,500,300),ncol=2, byrow=TRUE)
  pol1 = list(outer, hole1)
  mp = list(pol1)
  mp1 = sf::st_multipolygon(mp)

  region <- make.region(region.name = "main",
                        shape = mp1)

  density <- make.density(region = region, x.space = 20, constant = 10)
  density <- add.hotspot(density, c(500,250), 200, 200)

  covariate.list <- list()
  covariate.list$size <- list(distribution = "poisson", lambda = 25)
  covariate.list$sex <- data.frame(level = c("male", "female"), prob = c(0.5,0.5))
  pop.descrp <- make.population.description(region = region,
                                            density = density,
                                            covariates = covariate.list,
                                            N = 250)
  cov.params <- list(size = log(1.05),
                     sex = data.frame(level = c("male", "female"),
                                      param = c(log(1), log(1.5))))

  detect <- make.detectability(key.function = "hn",
                               scale.param = 5,
                               truncation = 50,
                               cov.param = cov.params)

  design <- make.design(region = region,
                        transect.type = "line",
                        design = "systematic",
                        samplers = 20,
                        truncation = 50)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test anaysis options

  analyses <- make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
                               key = c("hn","hn","hn"),
                               truncation = 50)

  sim <- make.simulation(reps = 10,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analyses)

  survey <- run.survey(sim)

  # Basic multi-model selection test
  sel.model <- analyse.data(analyses, survey@dist.data)
  expect_equal(length(sel.model$model$ddf$fitted), nrow(survey@dist.data[!is.na(survey@dist.data$object),]))

  # Test optim method

  # Test different er.var estimator
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = c("hn"),
                               er.var = "R2",
                               truncation = 50)
  fit.R2 <- analyse.data(analyses, survey@dist.data)
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = c("hn"),
                               er.var = "S2",
                               truncation = 50)
  fit.S2 <- analyse.data(analyses, survey@dist.data)
  
  expect_true(fit.R2$model$dht$individuals$N$se > fit.S2$model$dht$individuals$N$se)
  
  # Test default truncation distance (should be 50)
  
  analyses <- make.ds.analysis()
  expect_equal(analyses@truncation, list(50))


})
