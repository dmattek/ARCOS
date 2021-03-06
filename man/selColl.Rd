% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selCollEvents.R
\name{selColl}
\alias{selColl}
\alias{selColl.arcosTS}
\title{Select collective events}
\usage{
selColl(obj, colldur = c(1, Inf), colltotsz = c(1, Inf))

\method{selColl}{arcosTS}(obj, colldur = c(1, Inf), colltotsz = c(1, Inf))
}
\arguments{
\item{obj}{an arcosTS object.}

\item{colldur}{a two-element vector of integers with minimum and maximum duration of collective events; default c(1, Inf).}

\item{colltotsz}{a two-element vector of integers with minimum and maximum total size of collective events; default c(1, Inf).}
}
\value{
an arcosTS object
}
\description{
Wrapper for the \code{selCollEvents} function.
}
\examples{
library(ARCOS)
library(data.table)
 ts = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
                objid = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
                x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3))

arcosTS(ts,
        colPos = "x",
        col = list(Frame = "frame",
                   IDobj = "objid",
                   RT = NULL,
                   IDcoll = NULL),
        interVal = 1.,
        interType = "fixed")

tc = trackColl(ts)
tcsel = selColl(tc,
                colldur = c(3,Inf),
                colltotsz = c(3, Inf))
}
