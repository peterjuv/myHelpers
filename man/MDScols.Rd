% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myHelpers.R
\name{MDScols}
\alias{MDScols}
\alias{MASS_MDScols}
\alias{plotIsoMDS}
\title{MDS by columns and optional plot using Minkowski metrics}
\usage{
MDScols(
  data,
  scale = FALSE,
  center = FALSE,
  FUN = "isoMDS",
  p = 2,
  selection = "pairwise",
  top = 500,
  k = 2,
  maxit = 50,
  trace = TRUE,
  tol = 1e-04,
  plot = FALSE,
  labels = names(data),
  col = NULL,
  cex = 1,
  main = NULL,
  cex.main = 1,
  xlab = "Coordinate 1",
  ylab = "Coordinate 2",
  ...
)

MASS_MDScols(
  data,
  scale = FALSE,
  center = FALSE,
  method = "euclidean",
  FUN = "isoMDS",
  p = 2,
  k = 2,
  maxit = 50,
  trace = TRUE,
  tol = 0.001,
  plot = FALSE,
  labels = names(data),
  col = NULL,
  cex = 1,
  main = NULL,
  cex.main = 1,
  xlab = "Coordinate 1",
  ylab = "Coordinate 2",
  ...
)

plotIsoMDS(FUN = "isoMDS", plot = TRUE, selection = NULL, ...)
}
\arguments{
\item{data}{Matrix-like object to MDS (and plot) distances between columns}

\item{scale}{Logical scale data; standardize together with center}

\item{center}{Logical center data; standardize together with scale}

\item{FUN}{MDS function from MASS, default "isoMDS", alternative "sammon"}

\item{p}{Power of the Minkowski distance, passed to distance calculation \code{dist(...)} and \code{isoMDS(...)}}

\item{selection}{Character "pairwise" or "common" for selection of rows or NULL for using all rows; default "pairwise"}

\item{top}{Integer number of rows for distance calculation, default 500}

\item{k}{Desired dimension for the solution, passed to \code{cmdscale(...)} through \code{FUN}}

\item{maxit}{Max number of iterations, passed to \code{isoMDS(maxit)} or \code{sammon(niter = maxit)},}

\item{trace}{Print trace, passed to \code{FUN()},}

\item{tol}{Tolerance, passed to \code{FUN()},}

\item{plot}{Logical, plot using R, default FALSE}

\item{labels}{Character vector of alternative column names, default \code{names(data)}}

\item{col}{Colors of labels}

\item{cex}{Size of labels}

\item{main}{String or TRUE to generate title generated automatically; default NULL}

\item{cex.main}{Size of title}

\item{xlab}{a label for the x axis, defaults to a description of \code{x}.}

\item{ylab}{a label for the y axis, defaults to a description of \code{y}.}

\item{...}{other \link[graphics]{graphical parameters} (see \code{\link[graphics]{par}} and
    section \sQuote{Details} below).}

\item{method}{Distance metrics, passed to \code{dist(...)}, must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"}
}
\value{
A k-column vector of the fitted configuration from \code{FUN()}

A k-column vector of the fitted configuration from \code{FUN()}
}
\description{
Parametric (\code{stats::cmdscale()}) and non-parametric MDS (\code{MASS::isoMDS()} or \code{MASS::sammon()})
Data may be scales and/or centered before distance calculation.
Distances are calculated between columns on a (possibly subset) of rows.
Similar to limma::plotMDS in terms of subseting rows, but it allows for all rows for distance calculation, while limma uses only top=XX genes

Uses MASS::isoMDS or MASS::sammon
Similar to limma::plotMDS, except that is uses all parameters for distance calculation,
while limma uses only top=XX genes
}
\section{TODO}{
 add parameter dim.plot
}

