# Creates reference-d668415.rds: results of a small line transect simulation
# run with dsims at commit d668415 (before the population generator and
# zero.n options were added). Run with that version of dsims installed:
#   Rscript make-reference-d668415.R
library(dsims)
region <- make.region()
dens <- make.density(region = region, x.space = 50, constant = 1)
pd <- make.population.description(region = region, density = dens, N = 300, fixed.N = TRUE)
det <- make.detectability(scale.param = 25, truncation = 50)
des <- make.design(region = region, transect.type = "line", design = "systematic",
                   samplers = 10, truncation = 50)
ana <- make.ds.analysis(truncation = 50)
sim <- make.simulation(reps = 5, design = des, population.description = pd,
                       detectability = det, ds.analysis = ana)
set.seed(123)
sim <- run.simulation(sim, run.parallel = FALSE, counter = FALSE)
ref <- list(version = as.character(packageVersion("dsims")),
            individuals = sim@results$individuals[c("summary", "N", "D")],
            Detection = sim@results$Detection)
saveRDS(ref, "reference-d668415.rds")
