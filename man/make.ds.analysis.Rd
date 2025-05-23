% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClassConstructors.R
\name{make.ds.analysis}
\alias{make.ds.analysis}
\title{Creates an Analysis object}
\usage{
make.ds.analysis(
  dfmodel = list(~1),
  key = "hn",
  truncation = numeric(0),
  cutpoints = numeric(0),
  er.var = "R2",
  control.opts = list(),
  group.strata = data.frame(),
  criteria = "AIC"
)
}
\arguments{
\item{dfmodel}{list of distance sampling model formula specifying the detection function
(see \code{?Distance::ds} for further details)}

\item{key}{key function to use; "hn" gives half-normal (default) and "hr" gives
hazard-rate.}

\item{truncation}{absolute truncation distance in simulation units matching the
region units.}

\item{cutpoints}{supply a vector of cutpoints if you wish the simulation to perform
binned analyses.}

\item{er.var}{encounter rate variance estimator to use when abundance estimates are
required. Defaults to "R2" for line transects and "P3" for point transects. See
\code{mrds::varn} for more information / options.}

\item{control.opts}{A list of control options: method - optimisation method,}

\item{group.strata}{Dataframe with two columns ("design.id" and "analysis.id"). The
former gives the strata names as defined in the design (i.e. the region object) the
second specifies how they should be grouped (into less strata) for the analyses. See
details for more information.}

\item{criteria}{character model selection criteria (AIC, AICc, BIC)}
}
\value{
\code{\link{DS.Analysis-class}} object
}
\description{
This method creates an Analysis objects which describes a one or more
models to fit to the distance data. The simulation will fit each of these
models to the data generated in the simulation and select the model with
the minimum criteria value.
}
\details{
It is possible to group strata at the analysis stage using the group.strata
argument. For example, for design purposes it may have been sensible to
divide strata into substrata. This can help make more convex shapes and
therefore zigzag designs more efficient or perhaps it helped to keep
transects angled parallel to density gradients across the study area.
Despite these (purely design relevant) substrata we may still wish to
calculate estimates of density / abundance etc. for each stratum. The
table below gives an example of the data.frame which can be used to do
this. Imagine a study region with an onshore strata and an offshore
strata. The onshore strata has been divided in two at the design stage
to keep transects perpendicular to the coast. We now want to analyse
this as just two strata the onshore and offshore.

\tabular{ll}{ design.id         \tab analysis.id \cr
              ---------         \tab ----------- \cr
              onshoreN          \tab onshore     \cr
              onshoreS          \tab onshore     \cr
              offshore          \tab offshore    \cr}
}
\examples{

# Model selection considering both a half-normal and a hazard-rate model
# using AIC criteria and truncating 5\% of the data
ds.analyses <- make.ds.analysis(dfmodel = ~1,
                                key = c("hn", "hr"),
                                truncation = 500,
                                criteria = "AIC")

# Model selection considering both a half-normal with no covariates and with size
# as a covariate using AIC criteria and truncating at 500
ds.analyses <- make.ds.analysis(dfmodel = list(~1, ~size),
                                key = "hn",
                                truncation = 500,
                                criteria = "AIC")

# Model selection considering both a half-normal with no covariates and with size
# as a covariate and a hazard rate, using AIC criteria and truncating at 500
ds.analyses <- make.ds.analysis(dfmodel = list(~1, ~size, ~1),
                                key = c("hn", "hn", "hr"),
                                truncation = 500,
                                criteria = "AIC")

}
\seealso{
\code{\link[Distance]{ds}} \code{\link{make.simulation}}
}
\author{
Laura Marshall
}
