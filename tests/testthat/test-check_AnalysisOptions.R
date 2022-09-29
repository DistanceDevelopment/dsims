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

test_that("Grouping strata", {
  
  outer <- matrix(c(0,0,15,0,15,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 <- matrix(c(2,2,2,3,3,3,3,2,2,2),ncol=2, byrow=TRUE)
  hole2 <- matrix(c(5,5,5,6,7,6,8,5.5,7,5,5,5),ncol=2, byrow=TRUE)
  pol1 <- sf::st_polygon(list(outer, hole1*1.5, hole2))
  pol2 <- sf::st_polygon(list(outer + 15, hole2*1.5 + 12))
  pol3 <- sf::st_polygon(list(outer + 30, hole2*2.5 + 20))
  sfc <- sf::st_sfc(pol1,pol2,pol3)
  strata.names <- c("SW", "central", "NE")
  mp1 <- sf::st_sf(strata = strata.names, geom = sfc)
  
  region <- make.region(region.name = "study.area", 
                        strata.name = strata.names, 
                        shape = mp1)
  plot(region)
  
  density <- make.density(region,
                          x.space = 0.5,
                          constant = rep(1,3))
  
  design <- make.design(region,
                        design = "systematic",
                        samplers = 30,
                        truncation = 0.5)
  
  pop.descript <- make.population.description(region,
                                              density,
                                              N = c(200,200,200))
  
  detect <- make.detectability(scale.param = 0.25,
                               truncation = 0.5)
  
  grp.strat <- data.frame(design.id = c("SW", "central", "NE"),
                          analysis.id = rep("global",3))
  
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = "hn",
                               truncation = 0.5,
                               group.strata = grp.strat)
  
  sim <- make.simulation(reps = 3,
                         design = design,
                         population.description = pop.descript,
                         detectability = detect,
                         ds.analysis = analyses)
  
  test <- run.simulation(sim)
  tmp <- summary(test, description.summary = FALSE)
  
  expect_null(tmp@expected.size$Truth)
  true.individsN <- 600
  expect_equal(tmp@individuals$N$Truth, true.individsN)
  
  
  # Test when cluster size is in the simulation
  covs <- list()
  covs$size <- list(list(distribution = "poisson", lambda = 25))
  
  pop.descript <- make.population.description(region,
                                              density,
                                              covariates = covs,
                                              N = c(200,200,200))
  
  sim <- make.simulation(reps = 3,
                         design = design,
                         population.description = pop.descript,
                         detectability = detect,
                         ds.analysis = analyses)
  
  test <- run.simulation(sim)
  tmp <- summary(test, description.summary = FALSE)
  
  true.es <-25
  true.clusterN <-  600
  expect_equal(tmp@expected.size$Truth, true.es)
  expect_equal(tmp@clusters$N$Truth,true.clusterN)
  expect_equal(tmp@individuals$N$Truth, true.es*true.clusterN)
  
  # Test when only some are grouped
  grp.strat <- data.frame(design.id = c("SW", "central", "NE"),
                          analysis.id = c(rep("S_Central",2), "NE"))
  
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = "hn",
                               truncation = 0.5,
                               group.strata = grp.strat)
  
  sim <- make.simulation(reps = 3,
                         design = design,
                         population.description = pop.descript,
                         detectability = detect,
                         ds.analysis = analyses)
  
  test <- run.simulation(sim)
  tmp <- summary(test, description.summary = FALSE)
  
  true.es <- c(25,25,25)
  true.clusterN <-  c(200,400,600)
  expect_equal(tmp@expected.size$Truth, true.es)
  expect_equal(tmp@clusters$N$Truth,true.clusterN)
  expect_equal(tmp@individuals$N$Truth, true.es*true.clusterN)
  
  
  # Modify cluster size values to check 
  
  covs <- list()
  covs$size <- list(list(distribution = "poisson", lambda = 25),
                    list(distribution = "poisson", lambda = 50),
                    list(distribution = "poisson", lambda = 100))
  
  pop.descript <- make.population.description(region,
                                              density,
                                              covariates = covs,
                                              N = c(200,300,500))
  
  grp.strat <- data.frame(design.id = c("SW", "central", "NE"),
                          analysis.id = c(rep("S_Central",2), "NE"))
  
  analyses <- make.ds.analysis(dfmodel = list(~1),
                               key = "hn",
                               truncation = 0.5,
                               group.strata = grp.strat)
  
  
  sim <- make.simulation(reps = 3,
                         design = design,
                         population.description = pop.descript,
                         detectability = detect,
                         ds.analysis = analyses)
  
  test <- run.simulation(sim)
  tmp <- summary(test, description.summary = FALSE)
  
  true.es <- c(100, 25*0.4 + 0.6*50, 25*0.2 + 0.3*50 + 0.5*100)
  true.clusterN <-  c(500,500,1000)
  expect_equal(tmp@expected.size$Truth, true.es)
  expect_equal(tmp@clusters$N$Truth,true.clusterN)
  expect_equal(tmp@individuals$N$Truth, true.es*true.clusterN)
})
