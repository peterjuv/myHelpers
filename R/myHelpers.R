#' myHelpers: Misc helper functions for R
#'
#' Misc helper functions for R
#' 
#' @author Peter Juvan, \email{peter.juvan@gmail.com}
#' @docType package
#' @section Base
#' @section STAT + PLOTS:
#' @section Write:
#' @section Colors:
#' @section Conversions:
#' @section Plots:
#' @section Calculations:
#' @name myHelpers
NULL

##############
#### Base ####
##############

#' Equivalent to setNames(object) for arrays
#' 
#' Sets the first component of dimnames(x)
#' @param x A matrix, array or data frame
#' @param rowNames New names for rows
#' @return Input x with new row names
#' @examples
#' setRowNames(matrix(1:4, nrow=2, ncol=2), c("a","b"))
#' setRowNames(matrix(1:4, nrow=2, ncol=2), NULL)
#' setRowNames(array(1:8, dim=c(2,2,2)), c("a","b"))
#' setRowNames(as.data.frame(matrix(1:4, nrow=2, ncol=2)), c("a","b"))
#' \dontrun{setRowNames(as.data.frame(matrix(1:4, nrow=2, ncol=2)), NULL) # Is this a BUG?!}
#' \dontrun{setRowNames(1:2, c("a","b")) # Error 'dimnames' applied to non-array}
#' @section Implementation:
#' Alternative way, does not work for data.frame:
#' \code{function(x, rowNames) return(structure(x, dimnames=c(list(rowNames), dimnames(x)[-1])))}
#' @export
setRowNames <- function(x, rowNames) {
    dimnames(x) <- c(list(rowNames), dimnames(x)[-1])
    return(x)
}


######################
#### STAT + PLOTS ####
######################

#' Descriptive statistics tables, boxplots
#' 
#' 2019-10-13 for OBV
#' @param varColor variable for colors in boxplots
#' @import dplyr
#' @importFrom ggplot2 ggplot aes_string geom_boxplot geom_jitter
#' @importFrom ggpubr theme_pubr stat_compare_means
#' @export
write_boxplot_descStat <- function(data, fNameMid, folder=".", varsExclude=c(), varsInclude=NULL, varColor=NA, statMethod=NULL) {
  # require(dplyr)
  # require(ggplot2)
  # require(ggpubr)
  numVars <- unlist(lapply(data, is.numeric))
  numVars[names(numVars) %in% varsExclude] <- FALSE
  numNames <- names(numVars[numVars])
  if (!is.null(varsInclude)) numNames <- intersect(varsInclude, numNames)
  disVars <- !unlist(lapply(data, is.numeric))
  disVars[names(disVars) %in% varsExclude] <- FALSE
  disNames <- names(disVars[disVars])
  if (!is.null(varsInclude)) disNames <- intersect(varsInclude, disNames)
  for (dVar in disNames) {
    varColorUse <- if (!is.na(varColor) & dVar == varColor) NA else varColor
    dataSum1 <- group_by_(data, dVar, varColorUse) %>% 
      summarise(count=n())
    dataSum2 <- group_by_(data, dVar, varColorUse) %>% 
      summarise_at(numNames, funs("5min"=min(.),"3med"=median(.), "4IQR"=IQR(.), "1mean"=mean(.), "2sd"=sd(.), "0max"=max(.)), na.rm=TRUE)
    dataSum <- inner_join(dataSum1, dataSum2)
    write.table(dataSum[,c(1,2,order(colnames(dataSum)[3:dim(dataSum)[2]], decreasing=TRUE)+2)], file.path(folder,paste0("descStat_",fNameMid,"_",deparse(substitute(data)),"_",dVar,"_",varColorUse,".tab")), sep="\t",row.names=FALSE)
#    write.table(dataSum, file.path(folder,paste0("descStat_",fNameMid,"_",deparse(substitute(data)),"_",dVar,"_",varColorUse,".tab")), sep="\t",row.names=FALSE)
    # boxplot
    varColorPlot <- if (is.na(varColorUse)) NULL else varColorUse
    pdf(file.path(folder,paste0("boxplot_",fNameMid,"_",deparse(substitute(data)),"_",dVar,"_",varColorUse,".pdf")),6,6)
    for (nVar in numNames) {
      gg<-ggplot(data, aes_string(dVar, nVar, color=varColorPlot)) +
        geom_boxplot(outlier.shape=NA, width=0.35) + 
        geom_jitter(height=0,width=0.35) + 
        theme_pubr() +
        stat_compare_means(method=statMethod, na.rm=TRUE, show.legend=FALSE)
      print(gg)
    }
    dev.off()
  }
}

#' Plot correlations using package:corrplot
#' 
#' 2019-10-13 for OBV; 2020-05-05 for Kiraly
#' uses cor() and corrplot.mixed()
#' 
#' @section Implementation_old:
#' \dontrun{
#' plot_corr <- function(data, fNameMid, folder=".", varsExclude=c(), varsInclude=NULL, order="AOE", width=12, height=12) {
#'   ## 2019-10-13 for OBV
#'   ## uses cor() and corrplot.mixed
#'   require(corrplot)
#'   numVars <- unlist(lapply(data, is.numeric))
#'   numVars[names(numVars) %in% varsExclude] <- FALSE
#'   numNames <- names(numVars[numVars])
#'   if (!is.null(varsInclude)) numNames <- intersect(varsInclude, numNames)
#'   pdf(file.path(folder,paste0("corrplot_",fNameMid,"_",deparse(substitute(data)),".pdf")),width, height)
#'   cors <- cor(data[, numNames], use="na.or.complete")
#'   colnames(cors) <- rownames(cors) <- numNames
#'   corrplot.mixed(cors, order=order)
#'   dev.off()
#' }
#' }
#' @export
plot_corr <- function(data, fNameMid, folder=".", varsExclude=c(), varsInclude=NULL, orders=c("AOE", "original"), wh=12, ...) {
  require(corrplot)
  require(dplyr)
  dataName <- deparse(substitute(data))
  #R-4: data <- data %>% mutate_if(~not(is.numeric(.)), ~as.numeric(as.factor(.))) %>% select_at(vars(-all_of(varsExclude)))
  data <- data %>% mutate_if(~!(is.numeric(.)), ~as.numeric(as.factor(.))) %>% select_at(vars(-all_of(varsExclude)))
  if (!is.null(varsInclude)) data <- data %>% select_at(vars(all_of(varsInclude)))
  cors <- cor(data, use="na.or.complete")
  colnames(cors) <- rownames(cors) <- colnames(data)
  pdf(file.path(folder,paste0("corrplot_",fNameMid,"_",dataName,".pdf")),width=wh,height=wh)
  for (order in orders) corrplot.mixed(cors, order=order, ...)
  dev.off()
}  

###############
#### WRITE ####
###############

#' Write a table with rownames using write.table
#' 
#' @export
write.table.rowNames <- function(x, col1name="rowNames", quote=F, sep="\t", row.names=F, col.names=T, ...) {
  xx <- cbind("rowNames"=rownames(x), x)
  colnames(xx) <- c(col1name, colnames(xx)[-1])
  write.table(xx, quote=quote, sep=sep, row.names=row.names, col.names=col.names, ...)
}

################
#### COLORS ####
################

#' Brew a color palette from a factor x with optional names.
#' 
#' Uses RColorBrewer palette names, default is OrRd with 9 colors
#' Use more/less colors (n) for a bigger/smaller range, i.e. more/less extremes at each end.
#' Colors are ordered according to the order of factor levels.
#' If namesFrom given, add names to colors; otherwise, add names(x) or x.
#' 
#' @param x Numeric vector/factor with optional names.
#' @param n Number of colors for RColorBrewer (depends on the name).
#' @param name Name of the RColorBrewer palette.
#' @param pull Indices to pull colors from RColorBrewer::brewer.pal
#' @param namesFrom Vector of names to be added to colors; default is names(x) or x.
#' @return Named factor of colors of the same length as x where the order of colors corresponds to the order of x.
#' @section Implementation:
#' Levels of x are ordered as they apper in data; this is achieved by as.factor(x, levels=unique(x))
#' In the 2nd to the last line, we create a factor from colsn with levels ordered as they appear;
#' In the last line, we reorder the levels of that factor according to the order of levels in x;
#' xtfrm(unique(x)) reports indices of (unique) values in levels.
#' @export
brewPalFac <- function(x, n=9, name="OrRd", pull=NULL, namesFrom=NULL) {
    if (!is.null(namesFrom)) x <- setNames(x, namesFrom)
    if (!is.factor(x)) x <- factor(x, levels=unique(x))
    if (is.null(pull)) pull <- 1:n
    assertthat::assert_that(all(pull > 0) & all(pull <= n) & all(pull == round(pull)))
    cols <- colorRampPalette(rev(RColorBrewer::brewer.pal(n, name)[pull]))(length(levels(x)))[x]
    if (!is.null(names(x))) colsn <- setNames(cols,names(x)) else colsn <- setNames(cols,x)
    colsf <- factor(colsn, levels = unique(colsn))
    factor(colsf, levels = levels(colsf)[xtfrm(unique(x))])
}

#' Brew a color palette from a numerical vector x with optional names.
#' 
#' Uses RColorBrewer palette names, default is OrRd with 9 colors
#' Use more/less colors (n) for a bigger/smaller range, i.e. more/less extremes at each end.
#' Use more/less digits for more/less categories of colors.
#' If namesFrom given, add names to colors; otherwise, add names(x) or x.
#' 
#' @param x Numeric vector with optional names.
#' @param n Number of colors for RColorBrewer (depends on the name).
#' @param name Name of the RColorBrewer palette.
#' @param digits Number of digits in x to form categories for colors. May be negative to round to tens, hundreds, etc.
#' @param namesFrom Vector of names to be added to colors; default is names(x) or x.
#' @param NAcolor Color for NA values, default black #000000
#' @param rev Revert colors, e.g. Rd to Or instead of Or to Rd; default TRUE
#' @return Named vector of colors of the same length as x.
#' @examples
#' brewPalCont(10:20)
#' brewPalCont(10:20, digits=-1)
#' brewPalCont(10:20, n=3, digits=-1)
#' brewPalCont(c(18,12,14,16,20,11,10,17,13,19,15), n=3, digits=-1)[as.character(10:20)]
#' brewPalCont(c(10:14, 0,  16:20), n=3, digits=-1)
#' brewPalCont(c(10:14, NA, 16:20), n=3, digits=-1)
#' brewPalCont(c(10:14, NA, 16:20), n=3, digits=-1, rev=FALSE)
#' @export
brewPalCont <- function(x, n=9, name="OrRd", digits=2, namesFrom=NULL, NAcolor="#000000", rev=TRUE) {
    if (!is.null(namesFrom)) x <- setNames(x, namesFrom)
    myBrew <- RColorBrewer::brewer.pal(n, name)
    if (rev) myBrew <- rev(myBrew)
    myPalette <- colorRampPalette(myBrew)
    xInd <- round(x*10**digits)
    cols <- myPalette(max(xInd,na.rm=TRUE)-min(xInd,na.rm=TRUE)+1)[xInd-min(xInd,na.rm=TRUE)+1]
    cols[is.na(cols)] <- NAcolor
    if (!is.null(names(x))) namesx <- names(x) else namesx <- x
    namesx[is.na(namesx)] <- "<NA>"
    setNames(cols,namesx)
}

#####################
#### CONVERSIONS ####
#####################

#' Convert a factor with names to a named character vector
#' 
#' @rdname defactor
#' @return Character vector with optional names; as is if not a factor
#' @examples
#' defactorChr(c(1,"a"))
#' defactorChr(factor(c(1,"a")))
#' defactorChr(setNames(c(1,2,3),c("a","b","c")))
#' defactorChr(factor(setNames(c(1,2,3),c("a","b","c"))))
#' defactorChr(factor(setNames(c("1","2","3"),c("a","b","c"))) )
#' defactorChr(factor(setNames(c("1a","2b","3c"),c("a","b","c"))))
#' defactorChr(factor(setNames(c(TRUE, FALSE), c("t","f"))))
#' @export
defactorChr <- function(fac) {
  if (is.factor(fac))
    return(setNames(levels(fac)[as.integer(fac)], names(fac)))
  else return(fac)
}

#' Convert a factor with names to a named vector and converts to logical or numeric if possible
#' 
#' @param fac Factor (or vector) with optional with names
#' @return Vector (character, numeric or logical) with optional names; as is if not a factor
#' @examples
#' defactor(c(1,"a"))
#' defactor(factor(c(1,"a")))
#' defactor(setNames(c(1,2,3),c("a","b","c")))
#' defactor(factor(setNames(c(1,2,3),c("a","b","c"))))
#' defactor(factor(setNames(c("1","2","3"),c("a","b","c"))) )
#' defactor(factor(setNames(c("1a","2b","3c"),c("a","b","c"))))
#' defactor(factor(setNames(c(TRUE, FALSE), c("t","f"))))
#' @export
defactor <- function(fac) {
  dfac <- defactorChr(fac)
  testn <- suppressWarnings(as.numeric(dfac))
  testl <- suppressWarnings(as.logical(dfac))
  if(all(!is.na(testn))) return(setNames(as.numeric(dfac), names(fac)))
  else if(all(!is.na(testl))) return(setNames(as.logical(dfac), names(fac)))
  else return(dfac)
}


#' Convert X to discretized values with elements in the form of intervals min-max.
#' 
#' See infotheo::discretize for parameters; defaults are: 
#' @param X Numeric vector or list
#' @param disc Default "equalfreq"
#' @param nbins Default NROW(X)^(1/3)
#' @section Old implementation:
#' \dontrun{
#' discretize_namedIntervals_vector <- function(X,...) {
#'     ## See infotheo::discretize for parameters; defaults are: disc="equalfreq", nbins=sqrt(NROW(x)).
#'     ## Returns discretized data in the form of intervals min-max.
#'     require(infotheo)
#'     d<- infotheo::discretize(X,...)$X
#'     m<-tapply(X,d,min)
#'     M<-tapply(X,d,max)
#'     d_int <- paste0(format(m), "-", format(M))
#'     names(d_int) <- names(m)
#'     d_int[m==M] <- format(m)[m==M]
#'     #d_int[d]
#'     browser()
#'     #as.character((sapply(as.character(d), function(cd) {d_int[cd]})))
#'     as.character(d_int[as.character(d)])
#' }
#' }
#' @importFrom infotheo discretize
#' @export
discretize_namedIntervals <- function(X,...) {
  d <- discretize(X,...)
  Xf <- as.data.frame(X)
  m <- list(); M <- list()
  for (nm in names(d)) {
    m[[nm]]<-tapply(Xf[[nm]],d[[nm]],min)
    M[[nm]]<-tapply(Xf[[nm]],d[[nm]],max)
  }
  d_int <- lapply(names(m), function(nm,m,M) {
    d_int1<-paste0(format(m[[nm]]), "-", format(M[[nm]]))
    names(d_int1)<-names(m[[nm]])
    d_int1[m[[nm]]==M[[nm]]] <- format(m[[nm]])[m[[nm]]==M[[nm]]]
    d_int1
    }, m, M)
  names(d_int) <- names(d)
  Xd <- lapply(names(d), function(nd) {as.character(d_int[[nd]][as.character(d[[nd]])])})
  names(Xd) <- names(d)
  if (is.atomic(X)) Xd[[1]] else Xd
}


#' Get discretized time intervals in form "H:MM-H:MM"
#' 
#' @param times Vector of times in POSIX* format
#' @param time0 ???
#' @param nbins Number of intervals
#' @return Character vector of discretized time intervals in form "H:MM-H:MM"
#' @importFrom infotheo discretize
#' @export
discretizeTime_equalInterval <- function(times, time0, nbins) {
  CT_M <- difftime(times, rep(time0, length(times)), units="mins")
  CT_EqT <- discretize(CT_M, nbins=nbins, disc="equalwidth")$X
  CTmin <- tapply(CT_M,CT_EqT,min)
  CTminH <- CTmin%/%60
  CTminM <- round(CTmin) - 60*CTminH
  CTmax <- tapply(CT_M,CT_EqT,max)
  CTmaxH <- CTmax%/%60
  CTmaxM <- round(CTmax) - 60*CTmaxH
  CT_intervals <- sprintf("%d:%02d-%d:%02d", CTminH, CTminM, CTmaxH, CTmaxM)
  names(CT_intervals) <- names(CTmin)
  CT_intervals[CT_EqT]
}

#' Capitalize words
#' 
#' @export
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
    {s <- substring(s,2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


###############
#### PLOTS ####
###############

#' MDS by columns and optional plot using Minkowski metrics
#' 
#' Parametric (\code{stats::cmdscale()}) and non-parametric MDS (\code{MASS::isoMDS()} or \code{MASS::sammon()})
#' Data may be scales and/or centered before distance calculation.
#' Distances are calculated between columns on a (possibly subset) of rows.
#' Similar to limma::plotMDS in terms of subseting rows, but it allows for all rows for distance calculation, while limma uses only top=XX genes
#' @param data Matrix-like object to MDS (and plot) distances between columns
#' @param scale Logical scale data; standardize together with center 
#' @param center Logical center data; standardize together with scale
#' @param FUN Character MDS function, can be either "cmdscale" from package:stats, or isoMDS or sammon form package:MASS
#' @param p Power of the Minkowski distance, passed to distance calculation \code{dist(...)} and \code{isoMDS(...)}
#' @param selection Character "pairwise" or "common" for selection of rows or NULL for using all rows; default "pairwise"
#' @param top Integer number of rows for distance calculation, default 500
#' @param k Desired dimension for the solution, passed to \code{FUN}
#' @param maxit Max number of iterations, passed to \code{isoMDS(maxit)} or \code{sammon(niter = maxit)}
#' @param trace Print trace, passed to \code{isoMDS(maxit)} or \code{sammon()}
#' @param tol Tolerance, passed to \code{isoMDS(maxit)} or \code{sammon()}
#' @param plot Logical, plot using R, default FALSE
#' @param labels Character vector of alternative column names, default \code{names(data)}
#' @param col Colors of labels, default NULL
#' @param cex Size of labels, default 1
#' @param main Character or NULL (default, no title) or TRUE to generate title automatically
#' @param cex.main Size of title, default 1
#' @inheritParams graphics::plot
#' @return A k-column vector of the fitted configuration from \code{FUN()}
#' @section TODO: add parameter dim.plot
#' @export
MDScols <- function(data, scale=FALSE, center=FALSE, FUN = "isoMDS", p = 2, selection = "pairwise", top = 500,
  k = 2, maxit = 50, trace = TRUE, tol = 1e-4, plot = FALSE, labels = names(data), 
  col=NULL, cex=1, main=NULL, cex.main=1, xlab="Coordinate 1", ylab="Coordinate 2", ...) {  
  assertthat::assert_that(is.numeric(p) & is.numeric(k) & is.numeric(maxit) & is.logical(trace) & is.numeric(tol))
  assertthat::assert_that(is.null(selection) || selection %in% c("pairwise", "common"))
  if(scale) { 
    if(center) mainStdUsed <- "standardized"
    else       mainStdUsed <- "scaled"
  } else {
    if(center) mainStdUsed <- "centered"
    else       mainStdUsed <- "non-standardized"
  }
  xt <- scale(t(data), scale=scale, center=center)
  # Distance measure is p-root of sum of top absolute deviations to the power of p
  if (is.null(selection))
    dd <- dist(xt, method = "minkowski", p = p)
  else {
    # Dist by selection of rows 
    # Note that the code is from limma::plotMDS.R and that it operates on columns; therefore we transpose data again
    x <- t(xt)
    nprobes <- nrow(x)
    top <- min(top,nprobes)
    nsamples <- ncol(x)
    cn <- colnames(x)
    dd <- matrix(0,nrow=nsamples,ncol=nsamples,dimnames=list(cn,cn))
    if(selection=="pairwise") {
      # Different genes (rows) are selected for each pair, based on tob absolute deviations for each pair of arrays
      topindex <- nprobes-top+1L
      for (i in 2L:(nsamples))
        for (j in 1L:(i-1L))
          dd[i,j]=(sum(sort.int(abs(x[,i]-x[,j])^p,partial=topindex)[topindex:nprobes]))^(1/p)
    } else {
      # Same genes (rows) used for all comparisons; rows with the largest deviation from the mean column (=sample) are selected
      if(nprobes > top) {
        s <- rowMeans(abs(x-rowMeans(x))^p)
        o <- order(s,decreasing=TRUE)
        x <- x[o[1L:top],,drop=FALSE]
      }
      for (i in 2L:(nsamples))
        dd[i,1L:(i-1L)]=(colSums(abs(x[,i]-x[,1:(i-1),drop=FALSE])^p))^(1/p)
   }
    dd <- as.dist(dd)
  }
  # Parametric (=initial) fit
  fitCmdscale <- suppressWarnings(stats::cmdscale(dd, k=k))
  if (FUN == "isoMDS")
    fitMDS <- eval(rlang::call2(FUN, y=fitCmdscale, dd, k=k, maxit=maxit, trace=trace, tol=tol, p=p, .ns="MASS"))$points
  else if (FUN == "sammon")
    fitMDS <- eval(rlang::call2(FUN, y=fitCmdscale, dd, k=k, niter=maxit, trace=trace, tol=tol,      .ns="MASS"))$points
  else if (FUN == "cmdscale")
    fitMDS <- fitCmdscale
  else 
    abort(paste(FUN, "not implemented or a valid MDS function"))
  if (plot) {
    if (!is.null(main) & main==TRUE) {
      if (is.null(selection))
        main <- paste(FUN, mainStdUsed, Minkowski, p, ",", dim(data)[[2]], "objects,", dim(data)[[1]], "parameters")
      else
        main <- paste(FUN, mainStdUsed, Minkowski, p, ",", dim(data)[[2]], "objects, top", top, selection, "parameters")
    }
    plot(fitMDS[,1], fitMDS[,2], type="n", xlab=xlab, ylab=ylab, ...)
    text(fitMDS[,1], fitMDS[,2], labels=labels, cex=cex, col=col)
    title(main=main, cex.main=cex.main)
  }
  return(fitMDS)
}


#' Depricated: Non-parametric MDS by columns and optional plot from package MASS 
#' 
#' Uses MASS::isoMDS or MASS::sammon
#' Similar to limma::plotMDS, except that is uses all parameters for distance calculation, 
#' while limma uses only top=XX genes
#' @rdname MDScols
#' @param data Matrix-like object to MDS (and plot) distances between columns
#' @param scale Logical scale data; standardize together with center 
#' @param center Logical center data; standardize together with scale
#' @param method Distance metrics, passed to \code{dist(...)}, must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
#' @param FUN MDS function from MASS, default "isoMDS", alternative "sammon"
#' @param p Power of the Minkowski distance, passed to distance calculation \code{dist(...)} and \code{isoMDS(...)}
#' @param k Desired dimension for the solution, passed to \code{cmdscale(...)} through \code{FUN}
#' @param maxit Max number of iterations, passed to \code{isoMDS(maxit)} or \code{sammon(niter = maxit)}, 
#' @param trace Print trace, passed to \code{FUN()}, 
#' @param tol Tolerance, passed to \code{FUN()}, 
#' @param plot Logical, plot using R, default FALSE
#' @param labels Character vector of alternative column names, default \code{names(data)}
#' @param col Colors of labels
#' @param cex Size of labels
#' @param main String or TRUE to generate title generated automatically; default NULL
#' @param cex.main Size of title 
#' @inheritParams graphics::plot
#' @return A k-column vector of the fitted configuration from \code{FUN()}
#' @export
MASS_MDScols <- function(data, scale=FALSE, center=FALSE, method = "euclidean", FUN = "isoMDS", p = 2, 
  k = 2, maxit = 50, trace = TRUE, tol = 1e-3, plot = FALSE, labels = names(data), 
  col=NULL, cex=1, main=NULL, cex.main=1, xlab="Coordinate 1", ylab="Coordinate 2", ...) {  
  assertthat::assert_that(is.numeric(p) & is.numeric(k) & is.numeric(maxit) & is.logical(trace) & is.numeric(tol))
  if(scale) { 
    if(center) mainStdUsed <- "standardized"
    else       mainStdUsed <- "scaled"
  } else {
    if(center) mainStdUsed <- "centered"
    else       mainStdUsed <- "non-standardized"
  }
  scTdata <- scale(t(data), scale=scale, center=center)
  dist9 <- dist(scTdata, method = method, p = p)
  if (FUN == "isoMDS")
    callMDS <- rlang::call2(FUN, dist9, k = k, maxit = maxit, trace = trace, tol = tol, p = p, .ns="MASS")
  else if (FUN == "sammon")
    callMDS <- rlang::call2(FUN, dist9, k = k, niter = maxit, trace = trace, tol = tol, .ns="MASS")
  else 
    abort(paste(FUN, "not part of namespace MASS"))
  fit9 <- eval(callMDS)
  if (plot) {
    if (!is.null(main) & main==TRUE)
        main <- paste(FUN, mainStdUsed, method, dim(data)[[2]], "objects,", dim(data)[[1]], "parameters")
    plot(fit9$points[,1], fit9$points[,2], type="n", xlab=xlab, ylab=ylab, ...)
    text(fit9$points[,1], fit9$points[,2], labels=labels, cex=cex, col=col)
    title(main=main, cex.main=cex.main)
  }
  return(fit9$points)
}

#' Plot MDS, alternative name and default parameters for backward compatibility
#' 
#' @rdname MDScols
#' @inheritParams MDScols
#' @export
plotIsoMDS <- function(FUN = "isoMDS", plot = TRUE, selection = NULL, ...) {
  invisible(MDScols(FUN=FUN, plot=plot, selectiuon=selection...))
} 


#' Logistic Regression plot of effects
#' 
#' Developed for OBV 2019-10-13
#' @export
multiEffectPlot <- function(model, effectNames=c("anova", "summary"), OR=FALSE, test="Chi") {
  require(ggplot2); require(ggthemes); require(dplyr)
  if ((effectNames == "anova")[[1]]) {
    beta.table <- data.frame(anova(model, test=test), summary(model)$coef, confint(model))
    row.names(beta.table) <- c("(Intercept)", row.names(beta.table)[-1])
  } else if ((effectNames == "summary")[[1]]) {
    beta.table <- data.frame(summary(model)$coef, anova(model, test=test), confint(model))
  } else stop('The argument "effectNames" must c("anova", "summary")')
  beta.table[1,"Pr..Chi."] <- beta.table[1,"Pr...z.."]
  beta.table$variable <- row.names(beta.table)
  beta.table <- beta.table  %>% arrange(Estimate)
  beta.table <- mutate(beta.table, OR=exp(Estimate), ORlow=exp(X2.5..), ORhigh=exp(X97.5..), p = Pr..Chi., variable = reorder(variable, order(Estimate))) 
  if (OR) {
    p <- ggplot(beta.table, aes(y=OR, x=variable, ymin=ORlow, ymax=ORhigh, color = (p < 0.1))) + ylab("Odds ratio, 95% confidence interval")
  } else {
    p <- ggplot(beta.table, aes(y=Estimate, x=variable, ymin=X2.5.., ymax=X97.5.., color = (p < 0.1))) + ylab("ln(odds_ratio), 95% confidence interval")
  }
  p + geom_pointrange() +  xlab("") + geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=1) + coord_flip() + theme_few()
}


#' Logistic Regression boxplot
#' 
#' Developed for OBV 2019-10-13
#' @section Old implementation:
#' \dontrun{
#' plotBinary <- function (X, Y, ...) {
#'   plot(X, jitter(Y, factor = 0.1), col = rgb(0, 0, 0, 0.5), pch = 19, ylim = c(-0.2, 1.2), ...)
#'   boxplot(X ~ Y, horizontal = TRUE, notch = TRUE, add = TRUE,
#'           at = c(-0.1, 1.1), width = c(0.1, 0.1), col = "grey",
#'           boxwex = 0.1, yaxt = "n")
#' }
#' }
#' @export
ggBinary <- function (data1, X, Y, color, xlab, ylab, notch=FALSE, ...) {
  ggplot(data1, aes_string(Y,X), ...) + xlab(xlab) + ylab(ylab) + 
    coord_flip() + #scale_x_discrete(breaks=f, drop=FALSE) +
    geom_boxplot(outlier.shape=NA, notch=notch, width=0.2) + 
    geom_jitter(aes_string(color=color),height=0,width=0.2)
}

######################
#### CALCULATIONS ####
######################

#' 1-Persons/2 that handles NA
#' @export
corDist <- function(x) as.dist((1-cor(t(x), use="pairwise.complete.obs"))/2)

#' Robust scale columns, does not handle NA
#' @export
scaleRobust <- function(x) sweep(sweep(x,2,apply(x,2,median)), 2, apply(x,2,mad), "/")
