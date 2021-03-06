% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotBinMeas.R
\name{plotBinMeas}
\alias{plotBinMeas}
\alias{plotBinMeas.arcosTS}
\title{"Visualise measurement de-trending and binarisation"}
\usage{
plotBinMeas(
  obj,
  ntraj = 16L,
  xfac = 1,
  measfac = 1,
  plotResc = TRUE,
  inSeed = NULL
)

\method{plotBinMeas}{arcosTS}(
  obj,
  ntraj = 16L,
  xfac = 1,
  measfac = 1,
  plotResc = TRUE,
  inSeed = NULL
)
}
\arguments{
\item{obj}{an arcosTS object.}

\item{ntraj}{an integer with the number of random trajectories to plot. Set to 0 to plot all trajectories; default 16L.}

\item{xfac}{a numeric with a rescaling factor for the x-axis; default 1.}

\item{measfac}{a numeric with a rescaling factor for the measurement for visualisation purposes; default 1.}

\item{plotResc}{logical, plot rescaled trajectory; default TRUE.}

\item{inSeed}{an integer with the seed for the random number generator, default NULL.}
}
\value{
a ggplot2 object.
}
\description{
Visualise binarisation
}
\details{
Visualise de-trending and visualisation of the measurement column on a random selection of time series.
}
\examples{
cat("no examples")
}
