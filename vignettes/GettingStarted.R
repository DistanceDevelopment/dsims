## ----region, fig.align='center', fig.cap="Figure 1: The study region.", fig.width=3.8, fig.height=4----
library(dsims)
# Find the file path to the example shapefile in dssd
shapefile.name <- system.file("extdata", "StAndrew.shp", package = "dssd")
# Create the survey region object
region <- make.region(region.name = "St Andrews bay",
                      shape = shapefile.name,
                      units = "m")
plot(region)

## ----density, fig.align='center', fig.cap="Figure 2: A density map representing a plausible distributions of animals within the study region.", fig.width=4, fig.height=4----

# We first create a flat density grid
density <- make.density(region = region,
                        x.space = 500,
                        constant = 1)

# Now we can add some high and low points to give some spatial variability
density <- add.hotspot(object = density,
                       centre = c(-170000, 6255000),
                       sigma = 8000,
                       amplitude = 4)

density <- add.hotspot(object = density,
                       centre = c(-160000, 6275000),
                       sigma = 6000,
                       amplitude = 4)

density <- add.hotspot(object = density,
                       centre = c(-155000, 6260000),
                       sigma = 3000,
                       amplitude = 2)

density <- add.hotspot(object = density,
                       centre = c(-150000, 6240000),
                       sigma = 10000,
                       amplitude = -0.9)

density <- add.hotspot(object = density,
                       centre = c(-155000, 6285000),
                       sigma = 10000,
                       amplitude = -1)

# I will choose to plot in km rather than m (scale = 0.001)
plot(density, region, scale = 0.001)




## ----densitygam, fig.align='center', fig.cap="Figure 3: A density map representing a plausible distributions of animals within the study region.", fig.width=4, fig.height=4----

# First extract the data above - this is simple in this case as we only have a single strata
# Multi-strata regions will involve combining the density grids for each strata into a 
# single dataset.
density.data <- density@density.surface[[1]]
head(density.data)

# Fit a simple gam to the data
library(mgcv)
fit.gam <- gam(density ~ s(x,y), data = density.data, family = gaussian(link="log"))

# Use the gam object to create a density object
gam.density <- make.density(region = region,
                            x.space = 500,
                            fitted.model = fit.gam)

plot(gam.density, region, scale = 0.001)

## ----popdesc------------------------------------------------------------------

# Create a covariate list describing the distribution of cluster sizes
covariates <- list(size = list(distribution = "ztruncpois", mean = 3))

# Define the population description
pop.desc <- make.population.description(region = region,
                                        density = gam.density,
                                        covariates = covariates,
                                        N = 300,
                                        fixed.N = TRUE)


## ----designs------------------------------------------------------------------
parallel.design <- make.design(region = region, 
                               design = "systematic",
                               spacing = 2500,
                               edge.protocol = "minus",
                               design.angle = 90,
                               truncation = 750)

zigzag.design <- make.design(region = region, 
                             design = "eszigzag",
                             spacing = 2233,
                             edge.protocol = "minus",
                             design.angle = 0,
                             bounding.shape = "convex.hull",
                             truncation = 750)


## ----seed, echo=FALSE---------------------------------------------------------
set.seed(476)

## ----paralleltransects, fig.align='center', fig.cap="Figure 4: An example set of transects generated from the systematic parallel line design plotted within the study region.", fig.width=3.8, fig.height=4, fig.align='center'----
p.survey <- generate.transects(parallel.design)
plot(region, p.survey)

## ----zigzagtransects, fig.align='center', fig.cap="Figure 5: An example set of transects generated from the systematic parallel line design plotted within the study region.", fig.width=3.8, fig.height=4, fig.align='center'----
z.survey <- generate.transects(zigzag.design)
plot(region, z.survey)

## ----detect, fig.align='center', fig.cap="Figure 6: Plot of the detection function for the mean group size (solid line) and for the 2.5 and 97.5 percentile values  of group size (dashed lines) for this population. ", fig.width=6, fig.height=4----

# Define the covariate parameters on the log scale
cov.param <- list(size = log(1.08))

# Create the detectability description
detect <- make.detectability(key.function = "hn",
                             scale.param = 300,
                             cov.param = cov.param,
                             truncation = 750)

# Plot the simulation detection functions
plot(detect, pop.desc)


## ----analysis-----------------------------------------------------------------

analyses <- make.ds.analysis(dfmodel = list(~1, ~1, ~size),
                             key = c("hn", "hr", "hn"),
                             truncation = 750,
                             er.var = "R2",
                             criteria = "AIC")


## ----simulation---------------------------------------------------------------

sim.parallel <- make.simulation(reps = 999,
                                design = parallel.design,
                                population.description = pop.desc,
                                detectability = detect,
                                ds.analysis = analyses)

sim.zigzag <- make.simulation(reps = 999,
                              design = zigzag.design,
                              population.description = pop.desc,
                              detectability = detect,
                              ds.analysis = analyses)


## ----parallel.survey, fig.align='center', fig.cap="Figure 7: Example survey from systematic parallel design. Panels showing: top left - transects, top right - population, bottom left - transects, population and survey detections (cyan dots), bottom right -  histogram of detection distances", fig.width=6, fig.height=6----
# Generate a single instance of a survey: a population, set of transects 
# and the resulting distance data
eg.parallel.survey <- run.survey(sim.parallel)

# Plot it to view a summary
plot(eg.parallel.survey, region)


## ----zigzag.survey, fig.align='center', fig.cap="Figure 8: Example survey from equal spaced zigzag design. Panels showing: top left - transects, top right - population, bottom left - transects, population and survey detections (cyan dots), bottom right -  histogram of detection distances", fig.width=6, fig.height=6----
# Generate a single instance of a survey: a population, set of transects 
# and the resulting distance data
eg.zigzag.survey <- run.survey(sim.zigzag)

# Plot it to view a summary
plot(eg.zigzag.survey, region)


## ----runsim, eval=FALSE-------------------------------------------------------
#  
#  # Running the simulations
#  sim.parallel <- run.simulation(sim.parallel)
#  sim.zigzag <- run.simulation(sim.zigzag)
#  

## ----simresults, echo=FALSE---------------------------------------------------
load("files/sim.parallel.ROBJ")
load("files/sim.zigzag.ROBJ")

