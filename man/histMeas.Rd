% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meas.R
\name{histMeas}
\alias{histMeas}
\alias{histMeas.arcosTS}
\title{"Plot a histogram of the measurement"}
\usage{
histMeas(obj, clip = NULL, quant = FALSE)

\method{histMeas}{arcosTS}(obj, clip = NULL, quant = FALSE)
}
\arguments{
\item{obj}{an arcosTS object.}

\item{clip}{a two-element vector with lower and upper measurement values (if quant is FALSE) or quantiles (if quant is TRUE) to indicate clipping, default NULL.}

\item{quant}{a logical to indicate whether the clip parameter is treated as lower and upper bounds of the measurement or as quantiles, default FALSE.}
}
\value{
a ggplot2 object.
}
\description{
Plot a histogram of the measurement
}
\details{
The histogram can optionally indicate regions defined by lower and upper clipping values or by quantiles.
}
\examples{
cat("no examples")
}
