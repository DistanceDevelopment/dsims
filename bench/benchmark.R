# Benchmark run.simulation(): dsims 1.0.6 (parallel package) vs mirai backend
#
# Setup (from the dsims package root):
#   bench/lib_baseline - dsims installed from commit eb2bef4 (before the switch)
#   bench/lib_new      - dsims installed from the working tree
#     R CMD INSTALL --library=bench/lib_new .
#
# Usage: Rscript bench/benchmark.R [serial.reps] [parallel.reps] [times]
# Each configuration runs in a fresh R process (callr) so the two dsims
# versions never share a session. Results are written to bench/results.csv.

args <- commandArgs(trailingOnly = TRUE)
serial.reps <- if (length(args) >= 1) as.integer(args[1]) else 20L
parallel.reps <- if (length(args) >= 2) as.integer(args[2]) else 64L
times <- if (length(args) >= 3) as.integer(args[3]) else 3L
workers <- c(2L, 4L, 8L, 16L)

libs <- c(baseline = normalizePath("bench/lib_baseline"),
          new = normalizePath("bench/lib_new"))

# Runs one timed simulation in a fresh process
# mode: "serial", "parallel" (workers started per call) or
#       "persistent" (mirai daemons set once by the user, warm-up call not timed)
run.one <- function(version, mode, reps, workers = 1L) {
  callr::r(function(version, mode, reps, workers) {
    suppressMessages(library(dsims))
    sim <- make.simulation(reps = reps)
    run <- function() {
      if (mode == "serial") {
        run.simulation(sim, run.parallel = FALSE, counter = FALSE)
      } else {
        run.simulation(sim, run.parallel = TRUE, max.cores = workers, counter = FALSE)
      }
    }
    if (mode == "persistent") {
      mirai::daemons(workers)
      # User daemons start with the default library paths, so point them at
      # the same library as this process (otherwise dsims stops on a version mismatch)
      mirai::everywhere(.libPaths(lib.paths), lib.paths = .libPaths(), .min = workers)
      warm <- sim
      warm@reps <- workers
      invisible(run.simulation(warm, counter = FALSE))
    }
    invisible(gc(reset = TRUE))
    elapsed <- suppressMessages(suppressWarnings(system.time(res <- run())))[["elapsed"]]
    # Reps without an abundance estimate (e.g. failed with an error)
    failed <- sum(is.na(res@results$individuals$N[1, "Estimate", seq_len(reps)]))
    max.mb <- sum(gc()[, ncol(gc())])
    data.frame(version = version,
               dsims = as.character(packageVersion("dsims")),
               mode = mode, reps = reps, workers = workers,
               elapsed = elapsed, max.mem.mb = max.mb, failed = failed)
  }, args = list(version = version, mode = mode, reps = reps, workers = workers),
  libpath = c(libs[[version]], .libPaths()))
}

scenarios <- rbind(
  data.frame(version = c("baseline", "new"), mode = "serial",
             reps = serial.reps, workers = 1L),
  expand.grid(version = "baseline", mode = "parallel", reps = parallel.reps,
              workers = workers, stringsAsFactors = FALSE),
  expand.grid(version = "new", mode = c("parallel", "persistent"),
              reps = parallel.reps, workers = workers, stringsAsFactors = FALSE)
)

results <- list()
for (k in seq_len(times)) {
  # Shuffle order each round to spread out any drift in machine load
  for (s in sample(seq_len(nrow(scenarios)))) {
    sc <- scenarios[s, ]
    message(sprintf("[round %d] %s %s reps=%d workers=%d", k, sc$version,
                    sc$mode, sc$reps, sc$workers))
    res <- run.one(sc$version, sc$mode, sc$reps, sc$workers)
    res$round <- k
    results[[length(results) + 1]] <- res
  }
}
results <- do.call(rbind, results)
write.csv(results, "bench/results.csv", row.names = FALSE)

# Median per configuration, with speed-up relative to the baseline
if (any(results$failed > 0)) {
  warning("Some reps failed, see the failed column of bench/results.csv", call. = FALSE)
}
summ <- aggregate(cbind(elapsed, max.mem.mb, failed) ~ version + mode + reps + workers,
                  data = results, FUN = median)
base <- summ[summ$version == "baseline", c("mode", "workers", "elapsed")]
summ$baseline.elapsed <- base$elapsed[match(summ$workers, base$workers)]
summ$ratio.vs.baseline <- round(summ$elapsed / summ$baseline.elapsed, 3)
summ$sec.per.rep <- round(summ$elapsed / summ$reps, 3)
summ <- summ[order(summ$workers, summ$version, summ$mode), ]
print(summ, row.names = FALSE)
