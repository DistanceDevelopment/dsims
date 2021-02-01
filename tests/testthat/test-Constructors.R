library(DSsim)
library(testthat)

context("Constructor Checks")

test_that("Can create object or return correct error messages", {

  #Set up data
  outer = matrix(c(0,0,0,500,2000,500,2000,0,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(500,300,500,350,1500,350,1500,300,500,300),ncol=2, byrow=TRUE)
  pol1 = list(outer, hole1)
  mp = list(pol1)
  mp1 = sf::st_multipolygon(mp)

  region <- make.region(region.name = "main",
                        shape = mp1)

  # Quick area calculation check
  expect_equal(region@area, 2000*500-50*1000)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test density surface creation
  density <- make.density(region.obj = region, x.space = 10, constant = 10)
  expect_equal(all(density@density.surface[[1]]$density == 10), TRUE)

  # Add hotspot
  density <- add.hotspot(density, c(500,250), 200, 200)

  # Fit a gam to the density surface
  ddata <- density@density.surface[[1]]
  fit.gam <- mgcv::gam(density~s(x,y), data = ddata)

  # Try creating a density object from the gam results
  density2 <- make.density(region, x.space = 10, density.gam = fit.gam)

  # Check can feed in density grid values directly
  density.grid <- density2@density.surface
  density3 <- make.density(region, density.surface = density.grid, x.space = 10)
  #Check these two things are identical
  expect_identical(density2, density3)

  # Check non equal values for x.space and y.space work
  density2 <- make.density(region, x.space = 10, y.space = 100, density.gam = fit.gam)
  x.vals <- sort(unique(density2@density.surface[[1]]$x))
  y.vals <- sort(unique(density2@density.surface[[1]]$y))
  expect_equal(x.vals[2]-x.vals[1], 10)
  expect_equal(y.vals[2]-y.vals[1], 100)

  # Test failure when nothing is supplied
  expect_that(make.density(region, x.space = 10, constant = 0),
              throws_error("All strata must have some cells with non-zero density. Check that you have correctly specified your density grid. Large grid spacing may also generate this error."))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test population description creation


  # Test giving wrong parameter for distribution
  covariate.list <- list()
  covariate.list$size <- list(list("poisson", list(mu = 25)))

  expect_error(make.population.description(region.obj = region,
                                           density.obj = density,
                                           covariates = covariate.list,
                                           N = 250),
               "The distribution parameter for covariate size and strata 1 should be lambda.")

  # Test giving wrong columns in covariate dataframe
  covariate.list <- list()
  covariate.list$sex <- list(data.frame(cov = c("male", "female"), prob = c(0.5,0.5)))

  expect_error(make.population.description(region.obj = region,
                                           density.obj = density,
                                           covariates = covariate.list,
                                           N = 250),
               "The data.frame for covariate sex and strata 1 should have 2 columns: level and prob.")

  expect_error(make.population.description(region.obj = region,
                                           density.obj = density,
                                           N = c(250,200)),
               "You have not supplied the correct number of constants for population size N (one for each strata).", fixed = TRUE)

  expect_error(make.population.description(region.obj = region,
                                           density.obj = density,
                                           N = -100),
               "You must provide a positive, non-zero abundance", fixed = TRUE)


  # This should work below
  covariate.list <- list()
  # Animal height is generated from a lognormal distribution for both strata
  covariate.list$size <- list(list("poisson", list(lambda = 25)))
  # Animal sex is discrete/categorical, there are more females than males in strata 1 and equal
  # numbers in strata 2
  covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.5,0.5)))

  pop.descrp <- make.population.description(region.obj = region,
                                            density.obj = density,
                                            covariates = covariate.list,
                                            N = 250)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test detectability creation

  cov.params <- list(size = log(1.05),
                     sex = data.frame(level = c("male", "female"),
                                      param = c(log(1), log(1.5))))

  expect_error(make.detectability(key.function = "zz",
                                  scale.param = 5,
                                  truncation = 25,
                                  cov.param = cov.params),
               "Unsupported key function, please select hn, hr or uf.")


  cov.params <- list(size = log(1.05),
                     sex = data.frame(level = c("male", "female"),
                                      param = c(log(1), log(1.5))),
                     height = NULL)

  expect_error(make.detectability(key.function = "hn",
                               scale.param = 5,
                               truncation = 25,
                               cov.param = cov.params),
               "List element height of the cov.param list does not contain any values.")

  cov.params <- list(size = log(1.05),
                     sex = data.frame(factor = c("male", "female"),
                                      param = c(log(1), log(1.5))))

  expect_error(make.detectability(key.function = "hn",
                                  scale.param = 5,
                                  truncation = 25,
                                  cov.param = cov.params),
               "The dataframe for covariate 'sex' has missing columns: level")

  cov.params <- list(size = log(1.05),
                     sex = data.frame(level = rep(c("male", "female"),2),
                                      param = rep(c(log(1), log(1.5)),2),
                                      strata = c("A", "A", "B", "B"),
                                      ignore = rep(10,4)))

  expect_warning(test <- make.detectability(key.function = "hn",
                                             scale.param = 5,
                                             truncation = 25,
                                             cov.param = cov.params),
                  "The dataframe for covariate 'sex' has unrecognised columns: ignore. These will be ignored.")
  tmp <- data.frame(level = rep(c("male", "female"),2),
                    param = rep(c(log(1), log(1.5)),2),
                    strata = c("A", "A", "B", "B"))
  expect_equal(test@cov.param$sex, tmp)

  cov.params <- list(size = log(1.05))
  cov.params[[2]] <- data.frame(factor = c("male", "female"),
                                param = c(log(1), log(1.5)))

  expect_error(make.detectability(key.function = "hn",
                                  scale.param = 5,
                                  truncation = 25,
                                  cov.param = cov.params),
               "Not all the elements of the cov.param list are named. Please provide names for all elements.")

  expect_warning(test <- make.detectability(key.function = "hn",
                                            scale.param = 5,
                                            shape.param = 1,
                                            truncation = 25),
                 "You have selected the hn key function and supplied a shape parameter value, this will be ignored.")

  expect_error(make.detectability(key.function = "hr",
                                  scale.param = 5,
                                  truncation = 25),
               "You have selected the hazard rate model but not supplied a shape parameter.")

  expect_error(make.detectability(key.function = "hr",
                                  scale.param = 5,
                                  shape.param = c(1,1.5),
                                  truncation = 25),
               "The same number of values must be provided for both the shape and scale parameters or only one value supplied for either the shape or scale parameter.")

  cov.params <- list(size = log(1.05),
                     sex = data.frame(level = c("male", "female"),
                                      param = c(log(1), log(1.5))))

  detect <- make.detectability(key.function = "hn",
                               scale.param = 5,
                               truncation = 25,
                               cov.param = cov.params)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test analysis creation

  # Truncation checks
  expect_error(make.ds.analysis(dsmodel = list(~1, ~size, ~size+sex),
                                key = rep("hn",3),
                                cutpoints = seq(0,25, length = 4),
                                truncation = "15%"),
               "Truncation cannot be supplied as a percentage with binned data.")

  expect_error(make.ds.analysis(dsmodel = list(~1, ~size, ~size+sex),
                                key = rep("hn",3),
                                cutpoints = seq(0,25, length = 4),
                                truncation = list(left="1",right="15%")),
               "Truncation cannot be supplied as a percentage with binned data.")

  expect_error(make.ds.analysis(dsmodel = list(~1, ~size, ~size+sex),
                                key = rep("hn",3),
                                cutpoints = seq(0,25, length = 4),
                                truncation = list(left="1",righ="15%")),
               "Truncation must be supplied as a single number/string or a list with elements \"left\" and \"right\".")

  expect_error(make.ds.analysis(dsmodel = list(~1, ~size, ~size+sex),
                                key = rep("hn",3),
                                cutpoints = seq(0,25, length = 4),
                                truncation = c("1","15%")),
               "Truncation must be supplied as a single number/string or a list with elements \"left\" and \"right\".")









  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test simulation creation
  #check number of shape / scale parameters is equal to number of strata - what else needs to be equal to strata numbers?


})
