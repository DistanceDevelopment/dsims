Here I am creating a list of tests which should be implemented:

General Testing:

- Basic single strata shape no holes simulation - check it runs
- Single strata shape with hole - check it runs
- Complex multi-strata shape - check it runs
- Line transect simulation - check it runs
- Point transect simulation - check it runs
- Segmented line transect simulation - check it runs
- Half-normal detectability - check it runs and produces half-normal shape
- Hazard-rate detectability - check it runs and produces hazard-rate shape
- Uniform detection function - check it runs and produces uniform detection probability
- test that calling ds directly gives same results as calling it through generic tests
- test that with size in the model all sizes have the same scale params and check ordering tmp <- unique(eg.zigzag.survey@population@population[,c("size","scale.param")])
expect_equal(order(tmp$size), order(tmp$scale.param))

Input testing:

- Copy all imput testing across
- Test single versus multiple strata

Special specific testing:

- Create a test transect set and population to verify calculations in cal.perp.dists() - tests should cover horizontal, vertical and another set of transcts at an angle other than these options.

- Check that dsims correctly re-numbers sightings of the same object detected from 2 different transects. Most extreme test case here would be largely overlapping covered areas and a uniform detection probability of 1.

- Check what happens when running simulation in serial and parallel and loading the data from file... I think there might be an output conflict to the console stream between the progress counter and a message about analysing datasets.

- Test what happens for the flat file data format when there are no detections on some transects

- Test matching of strata covariates (it was looking for numbers but now looks for stratum names in the info)

- Test binned data

- Test grouped strata analysis

- Implement and test when only some of the points record distances (not in beta version)

- Unit testing, check that add.summary.results calculates correct summary results depending on use.max.reps

- Strata specific versus global detection function parameters. (Cluster sizes parameter values can be defined here. Each list entry will either be a data.frame containing 2 or 3 columns: level, param and where desired strata. If the region has multiple strata but this column is omitted then the values will be assumed to apply globally. )

- Test generate.pop.D





