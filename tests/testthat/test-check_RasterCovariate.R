context("Raster-based spatial covariate detectability (cov.surface)")

# ---------------------------------------------------------------------------
# Helper: build a minimal single-stratum region covering [0,1] x [0,1]
# ---------------------------------------------------------------------------
make_test_region <- function(){
  pol <- sf::st_polygon(list(matrix(c(0,0, 0,1, 1,1, 1,0, 0,0), ncol = 2, byrow = TRUE)))
  sfc <- sf::st_sfc(pol)
  sf.pol <- sf::st_sf(strata = "main", geom = sfc)
  make.region(region.name = "test", strata.name = "main", shape = sf.pol)
}

# ---------------------------------------------------------------------------
# 1. Canopy raster reduces scale.param at higher cover values
# ---------------------------------------------------------------------------
test_that("Canopy raster covariate: higher cover yields smaller scale.param", {
  skip_if_not_installed("terra")

  region <- make_test_region()

  # Two-zone raster: left half (x < 0.5) canopy = 0.2, right half = 0.8
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  m <- matrix(c(rep(0.2, 50), rep(0.8, 50)), nrow = 10, ncol = 10, byrow = FALSE)
  terra::values(r) <- as.vector(t(m))

  rast_path <- tempfile(fileext = ".tif")
  on.exit(unlink(rast_path), add = TRUE)
  terra::writeRaster(r, rast_path, overwrite = TRUE)

  # Slope = -1 on log scale: higher canopy -> smaller sigma -> lower detectability
  detect <- make.detectability(
    key.function = "hn",
    scale.param  = 25,
    cov.param    = list(canopy = -1),
    cov.surface  = list(canopy = rast_path),
    truncation   = 50
  )

  pop.data <- data.frame(
    individual   = 1:4,
    x            = c(0.1, 0.3, 0.6, 0.8),
    y            = c(0.5, 0.5, 0.5, 0.5),
    Region.Label = "main"
  )

  result <- dsims:::calculate.scale.param(pop.data, detect, region)

  # Expected: sigma = exp(log(25) + (-1) * canopy_value)
  low_scale  <- exp(log(25) + (-1) * 0.2)  # left-zone animals
  high_scale <- exp(log(25) + (-1) * 0.8)  # right-zone animals

  # Animals 1 & 2 are in low-canopy zone; 3 & 4 are in high-canopy zone
  expect_lt(result$scale.param[3], result$scale.param[1],
            label = "high-canopy scale.param < low-canopy scale.param")
  expect_equal(result$scale.param[1], low_scale,  tolerance = 0.05)
  expect_equal(result$scale.param[2], low_scale,  tolerance = 0.05)
  expect_equal(result$scale.param[3], high_scale, tolerance = 0.05)
  expect_equal(result$scale.param[4], high_scale, tolerance = 0.05)
})

# ---------------------------------------------------------------------------
# 2. File path and in-memory SpatRaster give identical results
# ---------------------------------------------------------------------------
test_that("File path and in-memory SpatRaster produce the same scale.param", {
  skip_if_not_installed("terra")

  region <- make_test_region()

  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- seq(0, 1, length.out = 25)

  rast_path <- tempfile(fileext = ".tif")
  on.exit(unlink(rast_path), add = TRUE)
  terra::writeRaster(r, rast_path, overwrite = TRUE)

  pop.data <- data.frame(
    individual   = 1:3,
    x            = c(0.1, 0.5, 0.9),
    y            = c(0.5, 0.5, 0.5),
    Region.Label = "main"
  )

  detect_path <- make.detectability(cov.param   = list(canopy = -0.5),
                                    cov.surface = list(canopy = rast_path))
  detect_mem  <- make.detectability(cov.param   = list(canopy = -0.5),
                                    cov.surface = list(canopy = r))

  res_path <- dsims:::calculate.scale.param(pop.data, detect_path, region)
  res_mem  <- dsims:::calculate.scale.param(pop.data, detect_mem,  region)

  expect_equal(res_path$scale.param, res_mem$scale.param, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 3. Validation: cov.surface name not in cov.param triggers an error
# ---------------------------------------------------------------------------
test_that("make.detectability errors when cov.surface name absent from cov.param", {
  skip_if_not_installed("terra")

  rast_path <- tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- 1:4
  on.exit(unlink(rast_path), add = TRUE)
  terra::writeRaster(r, rast_path, overwrite = TRUE)

  expect_error(
    make.detectability(
      cov.param   = list(size = log(1.1)),      # 'size', not 'canopy'
      cov.surface = list(canopy = rast_path)    # 'canopy' has no cov.param slope
    ),
    regexp = "cov.surface entry 'canopy' has no matching"
  )
})

# ---------------------------------------------------------------------------
# 4. Validation: non-character / non-SpatRaster entry triggers an error
# ---------------------------------------------------------------------------
test_that("make.detectability errors when cov.surface element is wrong type", {
  expect_error(
    make.detectability(
      cov.param   = list(canopy = -0.5),
      cov.surface = list(canopy = 42)           # numeric scalar is invalid
    ),
    regexp = "must be a file path"
  )
})

# ---------------------------------------------------------------------------
# 5. Validation: non-existent file path triggers an error
# ---------------------------------------------------------------------------
test_that("make.detectability errors when cov.surface file path does not exist", {
  expect_error(
    make.detectability(
      cov.param   = list(canopy = -0.5),
      cov.surface = list(canopy = "/does/not/exist/canopy.tif")
    ),
    regexp = "does not exist"
  )
})

# ---------------------------------------------------------------------------
# 6. Existing behaviour unchanged when cov.surface is empty (regression guard)
# ---------------------------------------------------------------------------
test_that("calculate.scale.param unchanged when no cov.surface is supplied", {
  region <- make_test_region()

  detect <- make.detectability(
    key.function = "hn",
    scale.param  = 25,
    cov.param    = list(size = log(1.02)),
    truncation   = 50
  )

  pop.data <- data.frame(
    individual   = 1:3,
    x            = c(0.2, 0.5, 0.8),
    y            = c(0.5, 0.5, 0.5),
    Region.Label = "main",
    size         = c(10L, 20L, 30L)
  )

  result <- dsims:::calculate.scale.param(pop.data, detect, region)

  expected <- exp(log(25) + log(1.02) * c(10, 20, 30))
  expect_equal(result$scale.param, expected, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# 7. Integration: run.simulation uses raster covariate each replicate
# ---------------------------------------------------------------------------
test_that("run.simulation completes with cov.surface raster (2 reps)", {
  skip_if_not_installed("terra")
  skip_if_not_installed("Distance")

  region <- make_test_region()

  # Flat-gradient raster (constant value = 0.5, so slope effect is fixed)
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- 0.5
  rast_path <- tempfile(fileext = ".tif")
  on.exit(unlink(rast_path), add = TRUE)
  terra::writeRaster(r, rast_path, overwrite = TRUE)

  density <- make.density(region = region, x.space = 0.1, constant = 200)

  popdesc <- make.population.description(
    region    = region,
    density   = density,
    fixed.N   = TRUE,
    N         = 200
  )

  detect <- make.detectability(
    key.function = "hn",
    scale.param  = 0.05,
    cov.param    = list(canopy = -0.5),
    cov.surface  = list(canopy = rast_path),
    truncation   = 0.1
  )

  design <- make.design(
    region         = region,
    transect.type  = "line",
    design         = "systematic",
    samplers       = 5,
    truncation     = 0.1
  )

  analysis <- make.ds.analysis(
    dfmodel    = list(~1),
    key        = "hn",
    truncation = 0.1,
    criteria   = "AIC"
  )

  sim <- make.simulation(
    reps                   = 2,
    design                 = design,
    population.description = popdesc,
    detectability          = detect,
    ds.analysis            = analysis
  )

  # Should run without error and return a Simulation object with results
  result <- run.simulation(sim, run.parallel = FALSE, counter = FALSE)
  expect_s4_class(result, "Simulation")
})
