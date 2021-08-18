

## ----------------------------------------------------------------------------------------
library(dsims)
#library(leaflet)
#library(knitr)
#library(sf)


## ----studyarea---------------------------------------------------------------------------
northsea <- make.region(region.name = "minkes",
                        shape = "/Users/lhm/Documents/GitHub/dsims/dsims/shapefiles/Strataprj.shp",
                        strata.name = c("South", "North"),
                        units = "km")
plot(northsea)


## ----popspec-----------------------------------------------------------------------------
areas <- northsea@area
prop.south <- areas[1]/sum(areas)
prop.north <- areas[2]/sum(areas)
total.abundance <- 3000
abund.south <- round(total.abundance * prop.south)
abund.north <- round(total.abundance * prop.north)
constant <- make.density(region = northsea, x.space = 10, constant = 1)
minkepop <- make.population.description(region = northsea,
                                        density = constant,
                                        N = c(abund.south, abund.north))
plot(constant, northsea)

## ----survdesign--------------------------------------------------------------------------
coverage.grid <- make.coverage(northsea, n.grid.points = 100)
equal.cover <- make.design(region = northsea,
                           transect.type = "line",
                           design = "systematic",
                           samplers=40,
                           design.angle = c(50, 40),
                           truncation = 0.8,
                           coverage.grid = coverage.grid)



## ----whatanalysis------------------------------------------------------------------------
pooled.hn <- make.ds.analysis(dfmodel = list(~1),
                              key = "hn",
                              criteria = "AIC",
                              truncation = 0.8)
strat.specific.or.not <- make.ds.analysis(dfmodel = list(~1, ~Region.Label),
                                          key = "hn",
                                          criteria = "AIC",
                                          truncation = 0.8)


## ----sigma-------------------------------------------------------------------------------
delta.multiplier <- c(seq(from=0.4, to=0.8, by=0.2),
                      seq(from=0.85, to=1.15, by=0.1),
                      seq(from=1.2, to=2.4, by=0.2))
sigma.south <- 0.6
north.sigma <- sigma.south*delta.multiplier


## ----sigmarange--------------------------------------------------------------------------
hn <- function(sigma, x) {return(exp(-x^2/(2*sigma^2)))}
for (i in seq_along(north.sigma)) {
  curve(hn(north.sigma[i],x),from=0,to=0.8,add=i!=1,
        xlab="Distance", ylab="Detection probability",
        main="Range of detection probability disparity\nSouth function in blue")
}
curve(hn(sigma.south,x),from=0,to=0.8, lwd=2, col='blue', add=TRUE)


## ----loopsigma---------------------------------------------------------------------------
equalcover <- list()
whichmodel <- list()
num.sims <- 90
#for (i in seq_along(delta.multiplier)) {
i=1
  sigma.strata <- c(sigma.south, sigma.south*delta.multiplier[i])
  detect <- make.detectability(key.function = "hn",
                               scale.param = sigma.strata,
                               truncation = 0.8)
  #equalcover.sim <- make.simulation(reps = num.sims,
  #                                  design = equal.cover,
  #                                  population.description = minkepop,
  #                                  detectability = detect,
  #                                  ds.analysis = pooled.hn)
  whichmodel.sim <- make.simulation(reps = num.sims,
                                    design = equal.cover,
                                    population.description = minkepop,
                                    detectability = detect,
                                    ds.analysis = strat.specific.or.not)
  #equalcover[[i]] <- run.simulation(equalcover.sim, run.parallel = TRUE, max.cores=2)
  whichmodel[[i]] <- run.simulation(whichmodel.sim, run.parallel = TRUE)
#}


