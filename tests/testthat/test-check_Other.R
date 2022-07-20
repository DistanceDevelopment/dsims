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
  
  aicc <- dsims:::AICc(model)
  n = 60
  K = 1
  AIC.small.samp.adj <- (2*K*(K+1))/(n-K-1)
  
  expect_equal(aicc, AIC(model)$AIC+AIC.small.samp.adj)
  expect_lt(AIC(model)$AIC, aicc)

})