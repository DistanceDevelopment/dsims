% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dsims-package.R
\name{dsims-package}
\alias{dsims-package}
\alias{dsims}
\title{Distance Sampling Simulations 'dsims'}
\description{
Runs simulations of distance sampling surveys to help users optimise
their survey designs for their particular study.
}
\details{
The full process involves defining the study region, a description of the
population of interest (including its distribution within the study region),
a survey design, a detection process and one or more models to fit to the
resulting data. The simulation engine will then use this information to
generate both a population and a set of transects and simulate the detection
process. The resulting data will be analysed and the estimates stored.
By repeating this many times we can test the accuracy and precision of
our estimates from various survey designs given our particular population
of interest.

This package interfaces with the survey design package 'dssd' to create the survey
regions, designs and generate the survey transects. While the 'DSsim' simulation
package relied on survey transects already being contained in shapefiles within
the supplied directory, dsims will generate the survey transects directly in R.

 The main functions in this package are: \link{make.density}, \link{make.population.description}, \link{make.detectability}, \link{make.ds.analysis}, \link{make.simulation}, \link{run.survey} and \link{run.simulation}. See also \link[dssd]{make.region} and \link[dssd]{make.design} in the dssd package for examples of how to define study regions and designs.

Further information on distance sampling methods and example code is 
available at \url{https://distancesampling.org/}. Specifically, see our 
website for vignettes / example code at \url{https://distancesampling.org/resources/vignettes.html}.

For help with distance sampling and this package, there is a Google Group \url{https://groups.google.com/forum/#!forum/distance-sampling}.
}
\author{
Laura Marshall <lhm@st-and.ac.uk>
"_PACKAGE"
}
