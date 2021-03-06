% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myHelpers.R
\name{plot_corr}
\alias{plot_corr}
\title{Plot correlations using package:corrplot}
\usage{
plot_corr(
  data,
  fNameMid,
  folder = ".",
  varsExclude = c(),
  varsInclude = NULL,
  orders = c("AOE", "original"),
  wh = 12,
  ...
)
}
\description{
2019-10-13 for OBV; 2020-05-05 for Kiraly
uses cor() and corrplot.mixed()
}
\section{Implementation_old}{

\dontrun{
plot_corr <- function(data, fNameMid, folder=".", varsExclude=c(), varsInclude=NULL, order="AOE", width=12, height=12) {
  ## 2019-10-13 for OBV
  ## uses cor() and corrplot.mixed
  require(corrplot)
  numVars <- unlist(lapply(data, is.numeric))
  numVars[names(numVars) %in% varsExclude] <- FALSE
  numNames <- names(numVars[numVars])
  if (!is.null(varsInclude)) numNames <- intersect(varsInclude, numNames)
  pdf(file.path(folder,paste0("corrplot_",fNameMid,"_",deparse(substitute(data)),".pdf")),width, height)
  cors <- cor(data[, numNames], use="na.or.complete")
  colnames(cors) <- rownames(cors) <- numNames
  corrplot.mixed(cors, order=order)
  dev.off()
}
}
}

