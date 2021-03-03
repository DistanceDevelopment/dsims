library(dsims)
library(testthat)

context("Stratified line transect example")

test_that("Test stratified options and generating by density", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set up region
  s1p1 = matrix(c(0,0,0,1.75,2,1.75,2,0,0,0),ncol=2, byrow=TRUE)
  s1p2 = matrix(c(1,2,1,2.5,3.8,2.5,3.8,2,1,2),ncol=2, byrow=TRUE)
  s2p1 = matrix(c(2,0,2,0.5,4,0.5,4,0,2,0),ncol=2, byrow=TRUE)
  s3p1 = matrix(c(4,0,4,2,5,2,5,0,4,0),ncol=2, byrow=TRUE)
  mpol1 = sf::st_multipolygon(list(sf::st_polygon(list(s1p1)),
                                  sf::st_polygon(list(s1p2))))
  mpol2 = sf::st_multipolygon(list(sf::st_polygon(list(s2p1))))
  mpol3 = sf::st_multipolygon(list(sf::st_polygon(list(s3p1))))
  sfc <- sf::st_sfc(mpol1,mpol2,mpol3)
  strata.names <- c("low", "high", "med")
  mp1 <- sf::st_sf(strata = strata.names, geom = sfc)

  region <- make.region(region.name = "main",
                        strata.name = strata.names,
                        shape = mp1)

  # Density surface
  density <- make.density(region = region,
                          x.space = 0.05,
                          constant = c(100,550,300))


  density.summary <- summary(density)
  expect_equal(density.summary@summary$ave.D, c(100,550,300))
  expect_equal(region@area*c(100,550,300), density.summary@summary$ave.N)

  # Population description
  pop.desc <- make.population.description(region = region,
                                         density = density,
                                         fixed.N = FALSE)

  # Detectability
  detect <- make.detectability(key.function = "hn",
                               scale.param = 0.05,
                               truncation = 0.1)

  # design
  design <- make.design(region = region,
                        transect.type = "line",
                        design = c("segmentedgrid","systematic","systematic"),
                        spacing = c(0.3,0.2,0.2),
                        design.angle = c(90,0,90),
                        seg.length = c(0.1, NA, NA),
                        seg.threshold = 0.5,
                        truncation = 0.1)

  # analysis
  group.strata <- data.frame(design.id = c("low","high","med"),
                             analysis.id = c("low", "med/high", "med/high"))

  analysis <- make.ds.analysis(dfmodel = ~1,
                               key = "hn",
                               group.strata = group.strata,
                               truncation = 0.1)

  sim <- make.simulation(reps = 5,
                         design = design,
                         population.description = pop.desc,
                         detectability = detect,
                         ds.analysis = analysis)

  survey <- run.survey(sim)

  # Get an example dataset
  dist.data <- survey@dist.data
  region.table <- unique(dist.data[,c("Region.Label", "Area")])
  sample.table <- unique(dist.data[, c("Sample.Label", "Region.Label", "Effort")])
  obs.table <- na.omit(unique(dist.data[, c("object", "Region.Label","Sample.Label")]))

  # Test function modify.strata.for.analysis
  check.strata.group <- modify.strata.for.analysis(group.strata,
                                                   obs.table,
                                                   sample.table,
                                                   region.table)

  new.obs <- check.strata.group$obs.table
  new.samp <- check.strata.group$sample.table
  new.reg <- check.strata.group$region.table

  # Check new region table
  expect_equal(region.table$Area[region.table$Region.Label == "low"],
               new.reg$Area[new.reg$Region.Label == "low"])
  expect_equal(region.table$Area[region.table$Region.Label == "med"]+region.table$Area[region.table$Region.Label == "high"], new.reg$Area[new.reg$Region.Label == "med/high"])

  # Check new sample table
  expect_equal(nrow(sample.table[sample.table$Region.Label %in% c("med","high"),]),
               nrow(new.samp[new.samp$Region.Label == "med/high",]))
  expect_equal(nrow(sample.table), nrow(new.samp))

  # Check new obs table
  expect_equal(nrow(obs.table[obs.table$Region.Label %in% c("med","high"),]),
               nrow(new.obs[new.obs$Region.Label == "med/high",]))
  expect_equal(nrow(obs.table), nrow(new.obs))

  # test running the simulation
  sim <- run.simulation(sim)

  sim.summary <- summary(sim, description.summary = FALSE)

  D.summary <- density.summary@summary
  expect_equal(sum(D.summary$ave.N[2:3])/sum(D.summary$area[2:3]),sim.summary@individuals$D$Truth[2])
  expect_equal(sum(D.summary$ave.N[2:3]),sim.summary@individuals$N$Truth[2])
  expect_equal(sum(D.summary$ave.N[1:3])/sum(D.summary$area[1:3]),sim.summary@individuals$D$Truth[3])

})

test_that("Test donttest example", {

  s1 = matrix(c(0,0,0,2,1,2,1,0,0,0),ncol=2, byrow=TRUE)
  s2 = matrix(c(1,0,1,2,2,2,2,0,1,0),ncol=2, byrow=TRUE)
  pol1 = sf::st_polygon(list(s1))
  pol2 = sf::st_polygon(list(s2))
  sfc <- sf::st_sfc(pol1,pol2)
  strata.names <- c("low", "high")
  sf.pol <- sf::st_sf(strata = strata.names, geom = sfc)

  region <- make.region(region.name = "Multi-strata Eg",
                        strata.name = strata.names,
                        shape = sf.pol)

  density <- make.density(region = region,
                          x.space = 0.2,
                          constant = c(10,80))

  covs <- list()
  covs$size <- list(list(distribution = "poisson", lambda = 25),
                    list(distribution = "poisson", lambda = 15))
  covs$sex <- data.frame(level = rep(c("male", "female"),2),
                         prob = c(0.5, 0.5, 0.6, 0.4),
                         strata = c(rep("low",2),rep("high",2)))

  # Define the population description (this time using the density to determine
  # the population size)
  popdesc <- make.population.description(region = region,
                                         density = density,
                                         covariates = covs,
                                         fixed.N = FALSE)

  # define the detecability (see make.detectability to alter detection function
  # for different covariate values)
  detect <- make.detectability(key.function = "hn", scale.param = 25, truncation = 50)

  # generate an example population
  # high <- low <- numeric()
  # for(i in 1:250){
  #   pop <- generate.population(popdesc, region = region, detectability = detect)
  #   tbl.results <- table(pop@population$Region.Label)
  #   high[i] <- tbl.results[["high"]]
  #   low[i] <- tbl.results[["low"]]
  # }
  #
  # mean(high) # Expected 160 (2*80)
  # [1] 159.82
  # mean(low) # Expected 20 (2*10)
  # [1] 19.845
  pop <- generate.population(popdesc, region = region, detectability = detect)

  expect_true(all(pop@population$scale.param == pop@population$scale.param[1]))

})
