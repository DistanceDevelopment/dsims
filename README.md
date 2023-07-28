# dsims
Distance Sampling Simulations
=============================

[![CRAN (RStudio Mirror) Downloads](http://cranlogs.r-pkg.org/badges/dsims)](https://www.r-pkg.org/pkg/dsims)
[![CRAN Version](http://www.r-pkg.org/badges/version/dsims)](https://www.r-pkg.org/pkg/dsims)
[![Codecov test coverage](https://app.codecov.io/gh/DistanceDevelopment/dsims/branch/master/graph/badge.svg)](https://app.codecov.io/gh/DistanceDevelopment/dsims?branch=master)

`dsims` is a package for simulating distance sampling surveys to allow users to optimise survey design for studies with particular properties.

# Using `dsims`

There is currently one vignette within the dsims package to help you get started using `dsims`:
  - GettingStarted: "Getting Started with dsims"

# Getting `dsims`

We typically aim to keep `dsims` on CRAN, so it can be readily installed from within R-Studio or the R interface.  However, at present there is an issue with our package `dssd` on which `dsims` relies (see [issue in dssd](https://github.com/DistanceDevelopment/dssd/issues/94) ) that prevents this.  Therefore to obtain `dsims` at present, please use the following code.  

      # First, ensure you have a copy of the devtools package
      if (system.file(package = "devtools") == "") install.packages("devtools")
      # then ensure you have a copy of the dssd package:
      devtools::install_github("DistanceDevelopment/dssd", build_vignettes = TRUE)
      # finally install dsims from github:
      devtools::install_github("DistanceDevelopment/dsims", build_vignettes = TRUE)

### Troubleshooting tip

During installation of packages, you may get the message "These packages have more recent versions available. It is recommended to update all of them. Which would you like to update?" and then a list of packages. We recommend you typically choose the option "CRAN packages only".  Note you may then get the message that some packages cannot be installed because they are already loaded.  In this case, a solution may be to note which packages these are, to open an R console (rather than R Studio) and to use the `Packages | Update packages` menu option (or the `update.packages` function) to update these packages.

<!-- 
The easiest way to get `dsims` is to install it from CRAN within R-studio or the R interface. We endeavour to make all new functionality available on CRAN in a timely manor. However, if you wish to download the development version with the latest updates immediately you can do this using Hadley Wickham's `devtools` package:

      install.packages("devtools")

then install `dsims` from github:

      library(devtools)
      install_github("DistanceDevelopment/dsims", build_vignettes = TRUE)
-->
