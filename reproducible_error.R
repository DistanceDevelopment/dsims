library(dsims)

northsea <- make.region(region.name = "minkes",
                        shape =  "shapefiles/Strataprj.shp",
                        strata.name = c("South", "North"),
                        units = "km")

constant <- make.density(region = northsea, x.space = 10, constant = 1)

minkepop <- make.population.description(region = northsea,
                                        density = constant,
                                        N = c(1065, 1935))

coverage.grid <- make.coverage(northsea, n.grid.points = 100)

equal.cover <- make.design(region = northsea,
                           transect.type = "line",
                           design = "systematic",
                           samplers=40,
                           design.angle = c(50, 40),
                           truncation = 0.8,
                           coverage.grid = coverage.grid)

strat.specific.or.not <- make.ds.analysis(dfmodel = list(~1, ~Region.Label),
                                          key = "hn",
                                          criteria = "AIC",
                                          truncation = 0.8)

detect <- make.detectability(key.function = "hn",
                             scale.param = c(0.6, 0.24),
                             truncation = 0.8)

sim <- make.simulation(reps =1,
                       design = equal.cover,
                       population.description = minkepop,
                       detectability = detect,
                       ds.analysis = strat.specific.or.not)

#Add following command to start of single.simulation.loop function: set.seed(566)
devtools::load_all()

sim <- run.simulation(sim)
