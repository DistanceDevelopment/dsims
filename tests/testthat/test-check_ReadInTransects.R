library(dsims)
library(testthat)
library(sf)

context("Processing transects from shapefile")

test_that("Can process point transect shapes", {
  
  # Create tests for processing read in transects
  
  pt1 = st_point(c(1,1))
  pt2 = st_point(c(2,2))
  (sfc = st_sfc(pt1, pt2))
  points = st_sf(data.frame(transect=1:2, geom=sfc))
  plot(points)
  
  outer = matrix(c(0,0,
                   2,0,
                   2,3,
                   0,3,
                   0,0),ncol=2, byrow=TRUE)
  pts = list(outer)
  pl1 = st_polygon(pts)
  poly_sfc = st_sfc(pl1)
  poly_sf = st_sf(data.frame(strata = "main", geom = poly_sfc))
  
  region <- make.region(region.name = "main",
                        shape = poly_sf)
  
  design <- make.design(region = region,
                        transect.type = "point",
                        design = "random",
                        samplers = 2,
                        truncation = 0.5)
  
  test <- process.point.transects(points, design)
  
  transects <- test$transects
  
  # The covered area should be 1.5 times the area of a circle of radius 0.5
  # (One of the points is entirely in the region the other only half in)
  expect_equal(transects@cov.area, 1.5*pi*0.5^2, tolerance = 0.001)
  expect_equal(transects@samp.count, 2)
  expect_equal(transects@edge.protocol, "minus")
  expect_s4_class(transects, "Point.Transect")
  
})

test_that("Can process line transect shapes", {
  
  # Create tests for processing read in transects
  pts = matrix(c(0,0,0,1), ncol = 2)
  ls1 = st_linestring(pts)
  pts = matrix(c(0.5,1,0.5,1), ncol = 2)
  ls2 = st_linestring(pts)
  (sfc = st_sfc(ls1, ls2))
  lines = st_sf(data.frame(transect=1:2, geom=sfc))
  plot(lines)
  
  outer = matrix(c(0,0,
                   2,0,
                   2,3,
                   0,3,
                   0,0),ncol=2, byrow=TRUE)
  pts = list(outer)
  pl1 = st_polygon(pts)
  poly_sfc = st_sfc(pl1)
  poly_sf = st_sf(data.frame(strata = "main", geom = poly_sfc))
  
  
  region <- make.region(region.name = "main",
                        shape = poly_sf)
  
  design <- make.design(region = region,
                        transect.type = "line",
                        design = "systematic",
                        samplers = 2,
                        truncation = 0.5)
  
  test <- process.line.transects(lines, design)
  
  transects <- test$transects
  
  # The covered area should be 1 x 0.5 for the vertical transect and
  # length of second sqrt(0.5^2+0.5^2) multiplied by the width 2*0.5
  # for the other
  expect_equal(transects@cov.area, 0.5*1 + sqrt(0.5^2+0.5^2), tolerance = 0.001)
  expect_equal(transects@samp.count, 2)
  expect_equal(transects@edge.protocol, "minus")
  expect_s4_class(transects, "Line.Transect")
  
  # Test that the information gets passed on for segmented grid
  design <- make.design(region = region,
                        transect.type = "line",
                        design = "segmentedgrid",
                        samplers = 2,
                        seg.length = 15,
                        truncation = 0.5)
  
  test <- process.seg.transects(lines, design)
  
  transects <- test$transects
  
  # The covered area should be 1 x 0.5 for the vertical transect and
  # length of second sqrt(0.5^2+0.5^2) multiplied by the width 2*0.5
  # for the other
  expect_equal(transects@cov.area, 0.5*1 + sqrt(0.5^2+0.5^2), tolerance = 0.001)
  expect_equal(transects@samp.count, 2)
  expect_equal(transects@edge.protocol, "minus")
  expect_s4_class(transects, "Segment.Transect")
})
