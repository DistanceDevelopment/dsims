# dsims 1.0.6

Bug Fixes

* Fixed bug when simulation checks generated an error for projected shapes. Issue #94

Enhancements

* Updated web site links in package documentation.

# dsims 1.0.5

Bug Fixes

* Fixed bug which generated NA's as scale parameters when factor covariates were included. Issue #89
* Simulation validation checks: consistency in truncation distances (Issue #76), consistency in region (Issue #88)

# dsims 1.0.4

Bug Fixes

* Fixed a bug when generating the simulation summary which meant that only the first value of mean.k and n.miss.dists was repeated rather than including all values in the summary tables. Issue #84
* The make.simulation function now throws an error if the P2 ER variance estimator is used with line transects designs (rather than after the simulation has completed). Issue #61 

# dsims 1.0.3

Bug Fixes

* Simulations were crashing if there were zero detections - now fixed and warnings displayed instead. Issue #77
* Errors were also occurring when there were no individuals generated in a stratum, now fixed. Issue #80
* Detections are no longer permitted across stratum boundaries - this was causing errors due to NA area values in the data. This is inline with expected protocols on surveys. Issue #81
* Remove dependence on sp and rgeos. Issue #42

# dsims 1.0.2

Bug Fixes

* Fixed transparency issue with detection distance histograms when saving to wmf (generated a warning in Distance for Windows)
* Only print summary table for individuals if animals occur as individuals (and not as clusters)
* Updated references to examples
* Fixed grouped strata bugs
* Can now read transect shapefiles in from file and will convert all to one strata if global region used. This allows regional simulations from stratified designs in distance for windows.


# dsims 1.0.1

New Features

* Added save.sim.results function so that simulation results can be written to .txt files. This is mainly useful for Distance for Windows users as R users would probably prefer to just save the whole simulation object to file. 
* Can write the simulation progress to file - this allows the simulation progress to be displayed when simulations are being run from Distance for Windows using dsims.
* Add segmented trackline design as an option in simulation summary (currently these design can only be generated inside Distance for Windows for use in simulations).
 
Bug Fixes

* Partial fix to the bug relating to grouping strata at the analysis stage. Strata grouping should now work when detections are of individuals. Still needs to be fixed for when clusters are present. 

# dsims 1.0.0

New Features

* Reading transects from file - this functionality is primarily envisioned for use from within Distance for Windows.

Enhancements

* New routine which will generate covariate values from a zero-truncated Poisson distribution for non integer values.
* There is now no lower limit on the number of detections in simulations as this was introducing bias. There is now a warning system in place. Very low numbers of detections may cause issues fitting. There must be more detections than there are parameters in the model for the model to have a chance of fitting successfully. Note that distance sampling good practice recommends  minimum of 60-80 detections for estimating the detection function for line transects and more for points.
* Improved histogram.N.ests function will now plot either a histogram of estimates of individuals or clusters. It also provides the use.max.reps argument so that the plot can be consistent with the option selected for the simulation summary.

Bug Fixes

* Fixed simulations where cluster size was included - there was a formatting change in mrds output tables.
* Added a check for repeat model definitions.
* Add code to deal with equal model criteria values.
* Fixed bug when no simulation repetitions had been successful
* AICc method fixed
* Warning indexes from parallel runs are now fixed

# dsims 0.2.2 / 0.2.3

Bug Fixes

* Minor modifications to stay CRAN compliant.

# dsims 0.2.1

New Features

* Now interfaces with new syntax in Distance >= 1.0.5 (it will remain backwards compatible with older versions of Distance for this release)

Bug Fixes

* Plus sampling simulations now issue a warning and modify to minus sampling - these should not have run in previous versions.
* Fixed default simulation truncation distance to 50 in the analyses (will fix dssd to be consistent with this in release 0.3.2)
* Fixed the recording of warning / error indexing in parallel simulations

# dsims 0.2.0

New Features

* Delta selection criteria is now recorded as the difference in information criteria between the top 2 best fitting models as determined by the information criteria.]
* The iteration numbers generating warnings or errors are now stored and displayed so user can choose what to do with these results.

Bug Fixes

* Fixed missing RMSE values
* Fix strata re-ordering for cluster size
* Models with -Inf information criteria no longer selected
* Models with dht = NULL are no longer selected
* Models which predict detection values < 1 no longer cause errors and are correctly excluded.
* Detectibility parameters for continuous covariates are now checked and validated.
* Fix situation where all reps are to be excluded due to problematic model fitting.
* There was a bug in the underlying code on windows machines that meant that segmented lines were not being clipped properly. The dependencies of sf have not been updated and the issue fixed. Please update sf if you run into missing segment transects.

# dsims 0.1.0

Enhancements

* Introducing the new Distance Sampling Simulation package. # dsims is our latest simulation package which interfaces with dssd so designs can be generated within R, thus making the simulation process a lot easier. # dsims also makes use of ggplot to produce cleaner looking graphics.
* Region and Design: # dsims can make use of the region creation and all the designs currently in dssd.
* Density: # dsims can generate density objects from constant values for each strata, from fitted mgcv gam objects with x and y as explantory covariates and from formulas of x and y.
* Density: Density grids are stored as sf polygons with their associated x, y central coordinates and density value
* Population Description: Populations can either be created with fixed population sizes or based on the densities in the density grid.
* Population Description: Both discrete and continuous individual level covariates can be included in the population
* Detectablity: The detectability of the population can be described by either half normal, hazard rate or uniform detection shapes. Parameters can vary by stratum
* Detectablity: Covariate parameters can be included to modify the scale parameter for each individual based on their covariate values.
* Analyses:A number of detection function analyses can be incorporated in a simulation and the model with the lowest criterion (AIC / AICc / BIC) will be selected.
* Analyses:Defining analyses is based on the arguments which are passed to our Distance R library.
* Simulations: Simulations can be run in serial or parallel and their progress is output.
* Simulations: The function run.survey can be used to create a single instance of a survey and check the simulation setup.

