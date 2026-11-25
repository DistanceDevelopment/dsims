library(dsims)
library(testthat)

context("Parallel processing")

test_that("Results are reproducible in serial and in parallel", {
  skip_on_cran()
  sim <- make.simulation(reps = 4)
  RNGkind("Mersenne-Twister")

  set.seed(101)
  sim.serial <- run.simulation(sim, run.parallel = FALSE, counter = FALSE)
  # User's RNG kind is not changed
  expect_equal(RNGkind()[1], "Mersenne-Twister")

  set.seed(101)
  sim.para <- run.simulation(sim, run.parallel = TRUE, max.cores = 2, counter = FALSE)

  expect_equal(sim.serial@results$individuals$N, sim.para@results$individuals$N)
  expect_equal(sim.serial@results$Detection, sim.para@results$Detection)
  # dsims shuts down the daemons it started
  expect_false(mirai::daemons_set(.compute = "dsims"))
})

test_that("Daemons set up by the user are used and left running", {
  skip_on_cran()
  sim <- make.simulation(reps = 2)
  mirai::daemons(2)
  on.exit(mirai::daemons(0))
  sim.para <- run.simulation(sim, counter = FALSE)
  expect_s4_class(sim.para, "Simulation")
  expect_true(mirai::daemons_set())
  expect_false(mirai::daemons_set(.compute = "dsims"))
})

test_that("A failing repetition is skipped without stopping the simulation", {
  sim <- make.simulation(reps = 3)
  sim.loop <- dsims:::single.sim.loop
  local_mocked_bindings(single.sim.loop = function(i, ...){
    if(i == 2) stop("test failure")
    sim.loop(i, ...)
  })
  expect_message(sim.res <- run.simulation(sim, run.parallel = FALSE, counter = FALSE),
                 "Error in repetition, repetition skipped: test failure")
  expect_true(all(is.na(sim.res@results$individuals$N[, "Estimate", 2])))
  expect_false(any(is.na(sim.res@results$individuals$N[, "Estimate", c(1, 3)])))
  expect_equal(sim.res@warnings$index[[1]], 2)
})

test_that("Errors returned by mirai are reported", {
  skip_on_cran()
  mirai::daemons(1, .compute = "test")
  on.exit(mirai::daemons(0, .compute = "test"))
  err <- mirai::mirai(stop("worker failure"), .compute = "test")[]
  expect_equal(dsims:::failed.rep.message(err), "worker failure")
})

test_that("Progress counter shows elapsed and remaining time", {
  expect_equal(dsims:::duration.string(65), "1m 05s")
  expect_equal(dsims:::duration.string(2*3600 + 5*60), "2h 05m")
  expect_equal(dsims:::duration.string(86400 + 2*3600 + 5*60), "1d 02h 05m")
  progress.file <- tempfile()
  expect_message(dsims:::report.progress(5, 10, Sys.time() - 10, TRUE, progress.file),
                 "5 out of 10 reps")
  expect_equal(scan(progress.file, quiet = TRUE), 50)
})
