#' myHelpers: Misc helper functions for R
#'
#' Misc helper functions for R
#' 
#' @author Peter Juvan, \email{peter.juvan@gmail.com}
#' @docType package
#' @section STAT + PLOTS:
#' @section Write:
#' @section Colors:
#' @section Conversions:
#' @section Plots:
#' @section Calculations:
#' @name myHelpers
NULL

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
#' @return Named vector of colors of the same length as x.
#' @export
brewPalCont <- function(x, n=9, name="OrRd", digits=2, namesFrom=NULL) {
    if (!is.null(namesFrom)) x <- setNames(x, namesFrom)
    myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(n, name)))
    xInd <- round(x*10**digits)
    cols <- myPalette(max(xInd)-min(xInd)+1)[xInd-min(xInd)+1]
    if (!is.null(names(x))) setNames(cols,names(x)) else setNames(cols,x)
}

#####################
#### CONVERSIONS ####
#####################

#' Convert a factor with names to a named character vector
#' 
#' @param fac Factor with optional with names
#' @return Character vector with optional names
#' @examples
#' defactorChr(factor(c(1,"a")))
#' defactorChr(factor(setNames(c(1,2,3),c("a","b","c"))))
#' defactorChr(factor(setNames(c("1","2","3"),c("a","b","c"))) )
#' defactorChr(factor(setNames(c("1a","2b","3c"),c("a","b","c"))))
#' defactorChr(factor(setNames(c(TRUE, FALSE), c("t","f"))))
#' @rdname defactor
#' @export
defactorChr <- function(fac) {
  setNames(levels(fac)[as.integer(fac)], names(fac))
}

#' Convert a factor with names to a named vector and converts to logical or numeric if possible
#' 
#' @return Vector with optional names 
#' @examples
#' defactor(factor(c(1,"a")))
#' defactor(factor(setNames(c(1,2,3),c("a","b","c"))))
#' defactor(factor(setNames(c("1","2","3"),c("a","b","c"))) )
#' defactor(factor(setNames(c("1a","2b","3c"),c("a","b","c"))))
#' defactor(factor(setNames(c(TRUE, FALSE), c("t","f"))))
#' @rdname defactor
#' @export
defactor <- function(fac) {
  dfac <- defactorChr(fac)
  testn <- suppressWarnings(as.numeric(dfac))
  testl <- suppressWarnings(as.logical(dfac))
  if(all(!is.na(testn))) setNames(as.numeric(dfac), names(fac))
  else if(all(!is.na(testl))) setNames(as.logical(dfac), names(fac))
  else dfac
}


#' Convert X to discretized values with elements in the form of intervals min-max.
#' 
#' See infotheo::discretize for parameters; defaults are: 
#' @param X Numeric vector or list
#' @param disc Default "equalfreq"
#' @param nbins Default NROW(X)^(1/3)
#' @section Old implementation:
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

#' MDS by columns and optional plot from package MASS 
#' 
#' Uses MASS::isoMDS or MASS::sammon
#' Similar to limma::plotMDS, except that is uses all parameters for distance calculation, 
#' while limma uses only top=XX genes
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
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param ... Passed to \code{plot(...)}
#' @return A k-column vector of the fitted configuration from \code{FUN()}
#' @rdname MASS_MDScols
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

#' Plot MDS, alternative name for backward compatibility
#' 
#' @param data Pased to plot Logical, plot using R, default TRUE
#' @param plot Logical, plot using R, default TRUE
#' @rdname MASS_MDScols
#' @export
plotIsoMDS <- function(data, plot = TRUE, ...) {
  invisible(MASS_MDScols(data, FUN = "isoMDS", plot = plot, ...))
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
#' @section Old implementation:
#' plotBinary <- function (X, Y, ...) {
#'   plot(X, jitter(Y, factor = 0.1), col = rgb(0, 0, 0, 0.5), pch = 19, ylim = c(-0.2, 1.2), ...)
#'   boxplot(X ~ Y, horizontal = TRUE, notch = TRUE, add = TRUE,
#'           at = c(-0.1, 1.1), width = c(0.1, 0.1), col = "grey",
#'           boxwex = 0.1, yaxt = "n")
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
