# dssd
Distance Sampling Simulations
=============================

[![R-CMD-check](https://github.com/DistanceDevelopment/dsims/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/DistanceDevelopment/dsims/actions/workflows/check-standard.yaml)
[![CRAN (RStudio Mirror) Downloads](http://cranlogs.r-pkg.org/badges/dsims)](https://www.r-pkg.org/pkg/dsims)
[![CRAN Version](http://www.r-pkg.org/badges/version/dsims)](https://www.r-pkg.org/pkg/dsims)
[![Codecov test coverage](https://app.codecov.io/gh/DistanceDevelopment/dsims/branch/master/graph/badge.svg)](https://app.codecov.io/gh/DistanceDevelopment/dsims?branch=master)

`dssd` is a package for designing distance sampling surveys. It provides a number of designs including systematic point transect, parallel line transect, zigzag line transect and segment line transect designs.

# Using `dsims`

There is currently one vignette within the dsims package to help you get started using `dsims`:
  - GettingStarted: "Getting Started with dsims"

# Getting `dsims`

The easiest way to get `dsims` is to install it from CRAN within R-studio or the R interface. We endeavour to make all new functionality available on CRAN in a timely manor. However, if you wish to download the development version with the latest updates immediately you can do this using Hadley Wickham's `devtools` package:

      install.packages("devtools")

then install `dsims` from github:

      library(devtools)
      install_github("DistanceDevelopment/dsims")
