library(dsims)
library(testthat)

context("Population generator and zero detections")

# Small line transect simulation, identical to the one in
# fixtures/make-reference-d668415.R
make.test.sim <- function(reps = 5, population.generator = NULL,
                          design = NULL, zero.n = "fail", detect.scale = 25){
  region <- make.region()
  dens <- make.density(region = region, x.space = 50, constant = 1)
  pd <- make.population.description(region = region, density = dens, N = 300,
                                    fixed.N = TRUE)
  det <- make.detectability(scale.param = detect.scale, truncation = 50)
  if(is.null(design)){
    design <- make.design(region = region, transect.type = "line",
                          design = "systematic", samplers = 10,
                          truncation = 50)
  }
  ana <- make.ds.analysis(truncation = 50, zero.n = zero.n)
  make.simulation(reps = reps, design = design, population.description = pd,
                  detectability = det, ds.analysis = ana,
                  population.generator = population.generator)
}

run.serial <- function(sim, seed = 123){
  set.seed(seed)
  suppressMessages(run.simulation(sim, run.parallel = FALSE, counter = FALSE))
}

# Returns a generator which records the populations it returns
make.recorder <- function(gen.fun){
  env <- new.env()
  env$pops <- list()
  list(env = env,
       generator = function(simulation, rep){
         pop <- gen.fun(simulation, rep)
         env$pops[[rep]] <- pop
         pop
       })
}

pop.data <- function(pop){
  # population data.frame of a Population (or of a generator list return)
  if(is.list(pop)) pop <- pop$population
  pop@population
}

test_that("(a) Without a population generator results match dsims d668415", {
  ref <- readRDS(test_path("fixtures", "reference-d668415.rds"))
  sim <- run.serial(make.test.sim())
  for(n in c("summary", "N", "D")){
    expect_equal(sim@results$individuals[[n]], ref$individuals[[n]])
  }
  # The existing Detection columns are unchanged, ZeroDetections is new
  old.cols <- dimnames(ref$Detection)[[2]]
  expect_equal(sim@results$Detection[, old.cols, , drop = FALSE], ref$Detection)
  expect_equal(unname(sim@results$Detection[1, "ZeroDetections", 1:5]), rep(0, 5))
  # Realised number of individuals in the design region is recorded
  expect_equal(unname(sim@results$individuals$true.N[1, "N", 1:5]), rep(300, 5))
  expect_null(sim@results$truth)
  expect_null(sim@population.generator)
})

test_that("(b) The RNG state is saved and restored around the generator", {
  # Populations from a generator returning generate.population(simulation)
  # are the populations the default would generate in each repetition
  rec <- make.recorder(function(simulation, rep) generate.population(simulation))
  sim.a <- run.serial(make.test.sim(population.generator = rec$generator))
  sim.default <- make.test.sim()
  set.seed(123)
  seeds <- dsims:::make.rep.seeds(5)
  old.seed <- .Random.seed
  for(i in 1:5){
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    expect_identical(pop.data(rec$env$pops[[i]]),
                     generate.population(sim.default)@population)
  }
  assign(".Random.seed", old.seed, envir = globalenv())
  # The same populations from a generator which uses many more random
  # numbers (and a different RNG kind), or none at all, give identical
  # results: the generator does not change the dsims stream
  gen.b <- function(simulation, rep){
    pop <- generate.population(simulation)
    RNGkind("Mersenne-Twister")
    set.seed(rep)
    runif(1000)
    pop
  }
  sim.b <- run.serial(make.test.sim(population.generator = gen.b))
  fixed.pops <- rec$env$pops
  gen.c <- function(simulation, rep) fixed.pops[[rep]]
  sim.c <- run.serial(make.test.sim(population.generator = gen.c))
  expect_identical(sim.a@results, sim.b@results)
  expect_identical(sim.a@results, sim.c@results)
  # The user's RNG state is not changed by the generator
  sim.b.def <- make.test.sim(population.generator = gen.b)
  set.seed(5)
  expected <- runif(1)
  set.seed(5)
  pop <- dsims:::run.population.generator(sim.b.def, 1)
  expect_identical(runif(1), expected)
})

test_that("(c) Simulations with different designs see identical populations", {
  gen.fun <- function(simulation, rep){
    set.seed(1000 + rep)
    generate.population(simulation)
  }
  rec1 <- make.recorder(gen.fun)
  rec2 <- make.recorder(gen.fun)
  des2 <- make.design(region = make.region(), transect.type = "line",
                      design = "random", samplers = 6, truncation = 50)
  sim1 <- run.serial(make.test.sim(population.generator = rec1$generator))
  sim2 <- run.serial(make.test.sim(population.generator = rec2$generator,
                                   design = des2), seed = 999)
  for(i in 1:5){
    expect_identical(pop.data(rec1$env$pops[[i]]), pop.data(rec2$env$pops[[i]]))
  }
  # but the populations differ between repetitions
  expect_false(identical(pop.data(rec1$env$pops[[1]]), pop.data(rec1$env$pops[[2]])))
  # and the surveys differ between designs
  expect_false(identical(sim1@results$individuals$N, sim2@results$individuals$N))
})

test_that("(d) The population generator works in parallel", {
  skip_on_cran()
  skip_if(parallelly::availableCores() < 3, "Not enough cores")
  # Generator made by a factory so the data it needs (here the number of
  # individuals to keep) travels with it to the workers
  make.gen <- function(keep){
    function(simulation, rep){
      set.seed(rep)
      pop <- generate.population(simulation)
      pop@population <- pop@population[seq_len(keep), ]
      pop@N <- keep
      list(population = pop, truth = list(N = 2 * keep, D = 2 * keep / 1e6))
    }
  }
  sim <- make.test.sim(reps = 4, population.generator = make.gen(250))
  sim.serial <- run.serial(sim, seed = 42)
  set.seed(42)
  sim.para <- suppressMessages(run.simulation(sim, run.parallel = TRUE,
                                              max.cores = 2, counter = FALSE))
  expect_identical(sim.serial@results, sim.para@results)
  expect_equal(unname(sim.para@results$individuals$true.N[1, "N", 1:4]), rep(250, 4))
  expect_equal(unname(sim.para@results$truth[1, "N", 1:4]), rep(500, 4))
  expect_false(mirai::daemons_set(.compute = "dsims"))
})

test_that("(e) zero.n = 'zero' records zero detection repetitions as 0", {
  # Empty populations in even repetitions
  gen <- function(simulation, rep){
    pop <- generate.population(simulation)
    if(rep %% 2 == 0){
      pop@population <- pop@population[0, ]
      pop@N <- 0
    }
    pop
  }
  sim.zero <- run.serial(make.test.sim(population.generator = gen, zero.n = "zero"))
  res <- sim.zero@results
  for(i in c(2, 4)){
    expect_equal(unname(res$individuals$N[1, c("Estimate", "lcl"), i]), c(0, 0))
    expect_true(all(is.na(res$individuals$N[1, c("se", "cv", "ucl"), i])))
    expect_equal(unname(res$individuals$D[1, "Estimate", i]), 0)
    expect_equal(unname(res$individuals$summary[1, "n", i]), 0)
    expect_equal(unname(res$Detection[1, "ZeroDetections", i]), 1)
    expect_equal(unname(res$individuals$true.N[1, "N", i]), 0)
  }
  expect_equal(unname(res$Detection[1, "ZeroDetections", c(1, 3, 5)]), c(0, 0, 0))
  # Effort and covered area are recorded as for the other repetitions
  summ <- res$individuals$summary
  expect_equal(unname(summ[1, "Effort", 2]), unname(summ[1, "Effort", 1]))
  expect_equal(unname(summ[1, "CoveredArea", 1:5]),
               unname(2 * 50 * summ[1, "Effort", 1:5]))
  expect_equal(unname(summ[1, "Area", 2]), 1e6)
  # Counted as successful and included in the mean
  sum.zero <- summary(sim.zero, description.summary = FALSE)
  expect_equal(sum.zero@failures, 0)
  expect_equal(sum.zero@zero.detection.reps, 2)
  expect_equal(sum.zero@individuals$N$mean.Estimate,
               mean(res$individuals$N[1, "Estimate", 1:5]))
  expect_equal(sum.zero@individuals$N$sd.of.means,
               sd(res$individuals$N[1, "Estimate", 1:5]))
  expect_output(show(sum.zero), "Zero Detection Repetitions")
  # With the default, the same repetitions are failures
  sim.fail <- run.serial(make.test.sim(population.generator = gen))
  sum.fail <- summary(sim.fail, description.summary = FALSE)
  expect_equal(sum.fail@failures, 2)
  expect_true(all(is.na(sim.fail@results$individuals$N[1, "Estimate", c(2, 4)])))
  # Zero detections with animals present (detection probability ~ 0)
  sim.nodet <- run.serial(make.test.sim(reps = 2, zero.n = "zero", detect.scale = 0.001))
  expect_equal(unname(sim.nodet@results$individuals$N[1, "Estimate", 1:2]), c(0, 0))
  expect_equal(unname(sim.nodet@results$individuals$true.N[1, "N", 1:2]), c(300, 300))
  # Invalid values are rejected
  expect_error(make.ds.analysis(zero.n = "skip"))
})

test_that("(f) Per-repetition truth is used by summary()", {
  gen <- function(simulation, rep){
    pop <- generate.population(simulation)
    list(population = pop, truth = c(N = 1000 + 100 * rep, D = (1000 + 100 * rep) / 4e6))
  }
  sim <- run.serial(make.test.sim(population.generator = gen))
  truth.N <- 1000 + 100 * (1:5)
  expect_equal(unname(sim@results$truth[1, "N", 1:5]), truth.N)
  expect_equal(unname(sim@results$truth[1, "N", "mean"]), mean(truth.N))
  summ <- summary(sim, description.summary = FALSE)
  est <- sim@results$individuals$N[1, "Estimate", 1:5]
  lcl <- sim@results$individuals$N[1, "lcl", 1:5]
  ucl <- sim@results$individuals$N[1, "ucl", 1:5]
  expect_equal(summ@individuals$N$Truth, mean(truth.N))
  expect_equal(summ@individuals$N$percent.bias,
               (mean(est) - mean(truth.N)) / mean(truth.N) * 100)
  expect_equal(summ@individuals$N$RMSE, sqrt(mean((est - truth.N)^2)))
  expect_equal(summ@individuals$N$CI.coverage.prob, mean(lcl < truth.N & ucl > truth.N))
  expect_equal(summ@individuals$D$Truth, mean(truth.N / 4e6))
  expect_match(summ@truth.source[["N"]], "population.generator")
  expect_output(show(summ), "per-repetition truth from population.generator")
  # Without truth the population description is used
  summ.default <- summary(run.serial(make.test.sim()), description.summary = FALSE)
  expect_equal(summ.default@individuals$N$Truth, 300)
  expect_equal(summ.default@truth.source[["N"]], "population description")
  # Only D supplied: N truth from the population description
  gen.D <- function(simulation, rep){
    list(population = generate.population(simulation), truth = list(D = 1e-4))
  }
  summ.D <- summary(run.serial(make.test.sim(population.generator = gen.D)),
                    description.summary = FALSE)
  expect_equal(summ.D@individuals$N$Truth, 300)
  expect_equal(summ.D@individuals$D$Truth, 1e-4)
  expect_equal(summ.D@truth.source[["N"]], "population description")
})

test_that("Invalid generator output gives clear errors", {
  sim <- make.test.sim(population.generator = function(simulation, rep) generate.population(simulation))
  check <- function(gen, pattern){
    sim@population.generator <- gen
    expect_error(dsims:::run.population.generator(sim, 1), pattern)
  }
  set.seed(1)
  good <- generate.population(sim)
  check(function(simulation, rep) data.frame(x = 1), "Population object")
  check(function(simulation, rep) list(pop = good), "Population object")
  check(function(simulation, rep) list(population = good, truth = list(M = 3)), "'N' and/or 'D'")
  check(function(simulation, rep) list(population = good, truth = c(N = -1)), "non-negative")
  check(function(simulation, rep) stop("boom"), "failed in repetition 1: boom")
  bad <- good
  bad@population$scale.param <- NULL
  check(function(simulation, rep) bad, "missing the column\\(s\\) 'scale.param'")
  bad <- good
  bad@population$Region.Label <- "elsewhere"
  check(function(simulation, rep) bad, "do not match the strata names")
  bad <- good
  bad@population$x[3] <- 5000
  check(function(simulation, rep) bad, "outside this stratum of the design region")
  # Generator errors in a simulation skip the repetition
  sim@population.generator <- function(simulation, rep) stop("boom")
  set.seed(1)
  expect_message(sim.run <- run.simulation(sim, run.parallel = FALSE, counter = FALSE),
                 "population.generator failed")
  expect_true(all(is.na(sim.run@results$individuals$N[1, "Estimate", 1:5])))
  # make.simulation checks the argument
  expect_error(make.test.sim(population.generator = "gen"), "NULL or a function")
  expect_error(make.test.sim(population.generator = function(x) x), "two arguments")
})

test_that("Zero detections and realised N with points and strata", {
  # Two strata, point transects, empty population in repetition 2
  shape <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0),c(1000,0),c(1000,500),c(0,500),c(0,0)))),
                      sf::st_polygon(list(rbind(c(1000,0),c(2000,0),c(2000,500),c(1000,500),c(1000,0)))))
  region <- make.region(region.name = "study", strata.name = c("B", "A"), shape = shape)
  dens <- make.density(region = region, x.space = 50, constant = 1)
  pd <- make.population.description(region = region, density = dens, N = c(200, 400))
  det <- make.detectability(scale.param = 25, truncation = 50)
  des <- make.design(region = region, transect.type = "point", design = "systematic",
                     samplers = 40, truncation = 50)
  ana <- make.ds.analysis(truncation = 50, er.var = "P3", zero.n = "zero")
  gen <- function(simulation, rep){
    pop <- generate.population(simulation)
    if(rep == 2){
      pop@population <- pop@population[0, ]
      pop@N <- 0
    }
    pop
  }
  sim <- make.simulation(reps = 3, design = des, population.description = pd,
                         detectability = det, ds.analysis = ana,
                         population.generator = gen)
  sim <- run.serial(sim)
  res <- sim@results
  expect_equal(dimnames(res$individuals$true.N)[[1]], c("A", "B", "Total"))
  expect_equal(unname(res$individuals$true.N[, "N", 1]), c(400, 200, 600))
  expect_equal(unname(res$individuals$true.N[, "N", 2]), c(0, 0, 0))
  expect_equal(unname(res$Detection[1, "ZeroDetections", 1:3]), c(0, 1, 0))
  expect_equal(unname(res$individuals$N[, "Estimate", 2]), c(0, 0, 0))
  summ <- res$individuals$summary
  # Point transect effort and covered area as calculated by dht
  for(i in 1:3){
    expect_equal(unname(summ[, "CoveredArea", i]), unname(pi * 50^2 * summ[, "Effort", i]))
    expect_equal(unname(summ["Total", "Effort", i]), sum(summ[c("A", "B"), "Effort", i]))
    expect_equal(unname(summ[, "Area", i]), c(5e5, 5e5, 1e6))
  }
  expect_equal(unname(summ[, "k", 2]), unname(summ[, "Effort", 2]))
})
