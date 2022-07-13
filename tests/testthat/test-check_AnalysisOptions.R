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
  cov.params <- list(size = log(1.1),
                     sex = data.frame(level = c("male", "female"),
                                      param = c(log(1), log(1.75))))

  detect <- make.detectability(key.function = "hn",
                               scale.param = 5,
                               truncation = 75,
                               cov.param = cov.params)

  design <- make.design(region = region,
                        transect.type = "line",
                        design = "systematic",
                        samplers = 20,
                        truncation = 75)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test anaysis options

  analyses <- make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
                               key = c("hn","hn","hn"),
                               truncation = 75)

  sim <- make.simulation(reps = 10,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analyses)

  survey <- run.survey(sim)
  
  pop <- survey@population
  pop.data <- pop@population
  
  # Check the scale parameter is calculated correctly
  indiv.size <- pop.data[1,"size"]
  indiv.sex <- ifelse(pop.data[1,"sex"] == "male", 0, 1)
  
  scale.param <- pop@detectability@scale.param
  size.param <- pop@detectability@cov.param$size
  sex.param <- pop@detectability@cov.param$sex[2,2]
  
  expect_equal(pop.data[1,"scale.param"],
               exp(log(scale.param)+indiv.size*size.param+indiv.sex*sex.param))

  # Basic multi-model selection test
  sel.model <- analyse.data(analyses, survey@dist.data, warnings = list())
  expect_equal(length(sel.model$model$ddf$fitted), nrow(survey@dist.data[!is.na(survey@dist.data$object),]))

  # Test optim method

  # Test different er.var estimator
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = c("hn"),
                               er.var = "R2",
                               truncation = 75)
  fit.R2 <- analyse.data(analyses, survey@dist.data)
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = c("hn"),
                               er.var = "S2",
                               truncation = 75)
  fit.S2 <- analyse.data(analyses, survey@dist.data)
  
  expect_true(fit.R2$dht$individuals$N$se > fit.S2$dht$individuals$N$se)
  
  # Test default truncation distance (should be 50)
  
  analyses <- make.ds.analysis()
  expect_equal(analyses@truncation, list(50))


})
