library(dsims)
library(testthat)
library(mgcv)

context("Constructor Checks")

test_that("Can create objects or return correct error / warning messages", {

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
  density <- make.density(region = region, x.space = 20, constant = 10)
  expect_equal(all(density@density.surface[[1]]$density == 10), TRUE)

  # Add hotspot
  density <- add.hotspot(density, c(500,250), 200, 200)

  # Fit a gam to the density surface
  ddata <- density@density.surface[[1]]
  fit.gam <- gam(density~s(x,y), data = ddata)

  # Try creating a density object from the gam results
  density2 <- make.density(region, x.space = 20, fitted.model = fit.gam)

  # Check can feed in density grid values directly
  density.grid <- get.density.surface(region, x.space = 20, y.space = 20, constant = 10)
  density3 <- make.density(region, density.surface = density.grid, x.space = 20)
  density4 <- make.density(region, x.space = 20, constant = 10)
  #Check these two things are identical
  expect_identical(density3, density4)

  # Check non equal values for x.space and y.space work
  density2 <- make.density(region, x.space = 20, y.space = 100, fitted.model = fit.gam)
  x.vals <- sort(unique(density2@density.surface[[1]]$x))
  y.vals <- sort(unique(density2@density.surface[[1]]$y))
  expect_equal(x.vals[2]-x.vals[1], 20)
  expect_equal(y.vals[2]-y.vals[1], 100)

  # Test failure when nothing is supplied
  expect_that(make.density(region, x.space = 20, constant = 0),
              throws_error("All strata must have some cells with non-zero density. Check that you have correctly specified your density grid. Large grid spacing may also generate this error."))

  # Test creating a density grid with a formula
  density2 <- make.density(region, x.space = 20, y.space = 100,
                           density.formula = "sin(x/250)+1+0.002*y")
  x <- density2@density.surface[[1]]$x[100]
  y <- density2@density.surface[[1]]$y[100]
  density.val <- density2@density.surface[[1]]$density[100]
  expect_equal(density.val, sin(x/250)+1+0.002*y)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test population description creation


  # Test giving wrong parameter for distribution
  covariate.list <- list()
  covariate.list$size <- list(distribution = "poisson", mu = 25)

  expect_error(make.population.description(region = region,
                                           density = density,
                                           covariates = covariate.list,
                                           N = 250),
               "You have not supplied all the required parameters (lambda) for the following covariate distribution: poisson", fixed = TRUE)


  # Test giving wrong columns in covariate dataframe
  covariate.list <- list()
  covariate.list$sex <- list(data.frame(cov = c("male", "female"), prob = c(0.5,0.5)))

  expect_error(make.population.description(region = region,
                                           density = density,
                                           covariates = covariate.list,
                                           N = 250),
               "Covariate dataframes must contain the columns 'level' and 'prob'.")

  expect_error(make.population.description(region = region,
                                           density = density,
                                           N = c(250,200)),
               "You have not supplied the correct number of constants for population size N (one for each strata).", fixed = TRUE)

  expect_error(make.population.description(region = region,
                                           density = density,
                                           N = -100),
               "You must provide a positive, non-zero abundance", fixed = TRUE)


  # This should work below
  covariate.list <- list()
  # Animal height is generated from a lognormal distribution for both strata
  covariate.list$size <- list(distribution = "poisson", lambda = 25)
  # Animal sex is discrete/categorical, there are more females than males in strata 1 and equal
  # numbers in strata 2
  covariate.list$sex <- data.frame(level = c("male", "female"), prob = c(0.5,0.5))

  pop.descrp <- make.population.description(region = region,
                                            density = density,
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

  expect_warning(expect_error(make.detectability(key.function = "hn",
                                  scale.param = 5,
                                  truncation = 25,
                                  cov.param = cov.params),
               "The dataframe for covariate 'sex' has missing columns: level"), "The dataframe for covariate 'sex' has unrecognised columns: factor. These will be ignored.")

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
  cov.params[[2]] <- data.frame(level = c("male", "female"),
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
  # expect_error(make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
  #                               key = rep("hn",3),
  #                               cutpoints = seq(0,25, length = 4),
  #                               truncation = "15%"),
  #              "Truncation cannot be supplied as a percentage with binned data.")

  expect_error(make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
                                key = rep("hn",3),
                                cutpoints = seq(0,25, length = 4),
                                truncation = list(left=1,right=25)),
               "Truncation must be supplied as a single numeric value giving the absolute truncation distance.")

  # expect_error(make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
  #                               key = rep("hn",3),
  #                               cutpoints = seq(0,25, length = 4),
  #                               truncation = list(left="1",righ="15%")),
  #              "Truncation must be supplied as a single number/string or a list with elements \"left\" and \"right\".")

  # expect_error(make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
  #                               key = rep("hn",3),
  #                               cutpoints = seq(0,25, length = 4),
  #                               truncation = c("1","15%")),
  #              "Truncation must be supplied as a single number/string or a list with elements \"left\" and \"right\".")

  expect_error(make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
                                key = c("hn","zz","uf"),
                                truncation = 25),
               "All key function values should be either 'hn' or 'hr'.")

  expect_error(make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
                                key = c("hn","hr","hr"),
                                truncation = 25,
                                criteria = "QIC"),
               "This selection criteria is not currently supported, please select from 'AIC', 'BIC' or 'AICc'.")

  expect_error(make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
                                key = c("hn","hr","hr"),
                                truncation = 25,
                                er.var = "ZZ",
                                criteria = "AIC"),
               "The er.var argument must be one of: 'R2', 'R3', 'R4', 'S1', 'S2', 'O1', 'O2', 'O3', 'P2', 'P3'.")

  analysis <- make.ds.analysis(dfmodel = list(~1, ~size, ~size+sex),
                               key = c("hn","hr","hr"),
                               truncation = 25)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a design - no need to test as tested in dssd

  design <- make.design(region = region,
                        transect.type = "line",
                        design = "systematic",
                        samplers = 20,
                        truncation = 25)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test simulation creation

  sim <- make.simulation(reps = 10,
                         design = design,
                         population.description = pop.descrp,
                         detectability = detect,
                         ds.analysis = analysis)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test anaysis options

  survey <- run.survey(sim)

})
