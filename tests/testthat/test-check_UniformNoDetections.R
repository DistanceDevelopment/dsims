library(dsims)
library(testthat)

context("Uniform detection function with no individuals near the transects")

test_that("simulate.detections handles empty possible distances for all keys", {
  empty <- data.frame()
  for(key in c("uf", "hn")){
    detect <- make.detectability(key.function = key, scale.param = ifelse(key == "uf", 1, 10), truncation = 10)
    out <- dsims:::simulate.detections(empty, detect)
    expect_equal(nrow(out), 0)
  }
})

test_that("A uniform-key repetition with no individuals near the transects is a zero estimate, not an error", {
  region <- make.region(region.name = "study area",
                        shape = sf::st_polygon(list(matrix(c(0,0,1000,0,1000,1000,0,1000,0,0), ncol = 2, byrow = TRUE))))
  design <- make.design(region = region, transect.type = "line", design = "systematic",
                        spacing = 500, truncation = 5, edge.protocol = "minus")
  density <- make.density(region = region, x.space = 100, constant = 1)
  popdesc <- make.population.description(region = region, density = density, N = 1, fixed.N = TRUE)
  detect <- make.detectability(key.function = "uf", scale.param = 1, truncation = 5)
  analysis <- make.ds.analysis(dfmodel = ~1, key = "unif", truncation = 5, zero.n = "zero")
  sim <- make.simulation(reps = 20, design = design, population.description = popdesc,
                         detectability = detect, ds.analysis = analysis)
  set.seed(42)
  sim <- run.simulation(sim, run.parallel = FALSE, counter = FALSE)
  errors <- grepl("Error in repetition", unlist(sim@warnings$message))
  expect_false(any(errors))
  zero <- sim@results$Detection[1, "ZeroDetections", 1:20]
  expect_true(sum(zero == 1, na.rm = TRUE) > 0)
  expect_true(all(sim@results$individuals$N[1, "Estimate", which(zero == 1)] == 0))
})
