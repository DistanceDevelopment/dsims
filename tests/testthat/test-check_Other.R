library(dsims)
library(testthat)
library(Distance)

context("Other tests")

test_that("Output checks", {
  
  test<-capture.output(description.summary())
  expect_equal(test[2], "GLOSSARY")
  
})

test_that("AICc calculations", {
  
  dists <- c(102, 10, -14, -61, -148, 66, -94, -24, -105, -106, -8, -217, 61, 67, 157, 167, 45, 107, -42, 65, -18, 12, -47, -3, -34, -1, 152, 35, 86, -33, 184, -130, -42, 3, -2, 207, -88, 268, -135, -127, 65, 85, 121, -2, -85, 26, -310, -146, 55, -104, -142, 88, 125, 11, -62, 15, 78, -239, 123, 113)
  dist.data <- data.frame(Region.Label = rep("A", 60),
                          Area = rep(1000,3),
                          Sample.Label = rep(1:20,3),
                          Effort = rep(10,60),
                          distance = abs(dists))
  
  model <- Distance::ds(dist.data)
  
  aicc <- AICc(model)
  n = 60
  K = 1
  AIC.small.samp.adj <- (2*K*(K+1))/(n-K-1)
  
  expect_equal(aicc, AIC(model)$AIC+AIC.small.samp.adj)
  expect_lt(AIC(model)$AIC, aicc)

})

test_that("Accumulate warnings", {
  
  w1 <- list(message = list("Warning too few detections"),
             index = list(c(1,4)),
             counter = list(2))
  w2 <- list(message = list("Warning too few detections",
                            "Warning number 2"),
             index = list(c(3,10,15),
                          c(6)),
             counter = list(3, 1))
  w3 <- list(message = list("Warning too many detections",
                            "Warning number 2"),
             index = list(c(3,10,15),
                          c(10,12)),
             counter = list(3, 2))
  warnings.list <- list(w1, w2, w3)
  
  warnings <- dsims:::accumulate.warnings(warnings.list)
  
  expect_equal(warnings$message[[3]], "Warning too many detections")
  expect_equal(warnings$counter[[3]], 3)
  expect_equal(warnings$index[[1]], c(1,3,4,10,15))
})

# Had to comment these out as S4 plotting methods cannot be found in this environment.
# test_that("Survey plotting options", {
#   
#   sim <- make.simulation()
#   
#   survey <- run.survey(sim)
#   
#   # Two plots should be produced by default
#   test <- plot(survey)
#   expect_equal(length(test), 2)
#   
#   expect_error(plot(survey, type = "population"),
#                "Plotting argument type not recognised. Please use 'survey', 'distances' or 'all' when Region not supplied.")
#   
#   test <- plot(survey, type = "survey")
#   expect_s3_class(test, "ggplot")
#   test <- plot(survey, type = "distances")
#   expect_s3_class(test, "ggplot")
#   
#   # Four plots should be produced by default
#   region <- sim@design@region
#   test <- plot(survey, region)
#   expect_equal(length(test), 4)
#   
#   test <- plot(survey, region, type = "transects")
#   expect_s3_class(test, "ggplot")
#   test <- plot(survey, region, type = "population")
#   expect_s3_class(test, "ggplot")
#   test <- plot(survey, region, type = "survey")
#   expect_s3_class(test, "ggplot")
#   test <- plot(survey, region, type = "distances")
#   expect_s3_class(test, "ggplot")
#   
# })