## OLD
# #' MDS by columns and optional plot using Minkowski metrics
# #' 
# #' Parametric (\code{stats::cmdscale()}) and non-parametric MDS (\code{MASS::isoMDS()} or \code{MASS::sammon()})
# #' Data may be scales and/or centered before distance calculation.
# #' Distances are calculated between columns on a (possibly subset) of rows.
# #' Similar to limma::plotMDS in terms of subseting rows, but it allows for all rows for distance calculation, while limma uses only top=XX genes
# #' @param data Matrix-like object to MDS (and plot) distances between columns
# #' @param scale Logical scale data; standardize together with center 
# #' @param center Logical center data; standardize together with scale
# #' @param FUN Character MDS function, can be either "cmdscale" from package:stats, or isoMDS or sammon form package:MASS
# #' @param p Power of the Minkowski distance, passed to distance calculation \code{dist(...)} and \code{isoMDS(...)}
# #' @param selection Character selection of rows (either "pairwise" or "common" or NULL for using all rows; default "pairwise"
# #' @param top Integer number of rows for distance calculation, default 500
# #' @param k Desired dimension for the solution, passed to \code{FUN}
# #' @param maxit Max number of iterations, passed to \code{isoMDS(maxit)} or \code{sammon(niter = maxit)}
# #' @param trace Print trace, passed to \code{isoMDS(maxit)} or \code{sammon()}
# #' @param tol Tolerance, passed to \code{isoMDS(maxit)} or \code{sammon()}
# #' @param plot Logical, plot using R, default FALSE
# #' @param labels Character vector of alternative column names, default \code{names(data)}
# #' @param col Colors of labels, default NULL
# #' @param cex Size of labels, default 1
# #' @param main Character or NULL (default, no title) or TRUE to generate title automatically
# #' @param cex.main Size of title, default 1
# #' @inheritParams graphics::plot
# #' @return A k-column vector of the fitted configuration from \code{FUN()}
# #' @rdname MDScols
# #' @export
# MDScols <- function(data, scale=FALSE, center=FALSE, FUN = "isoMDS", p = 2, selection = "pairwise", top = 500,
#   k = 2, maxit = 50, trace = TRUE, tol = 1e-4, plot = FALSE, labels = names(data), 
#   col=NULL, cex=1, main=NULL, cex.main=1, xlab="Coordinate 1", ylab="Coordinate 2", ...) {  
#   assertthat::assert_that(is.numeric(p) & is.numeric(k) & is.numeric(maxit) & is.logical(trace) & is.numeric(tol))
#   assertthat::assert_that(is.null(selection) || selection %in% c("pairwise", "common"))
#   if(scale) { 
#     if(center) mainStdUsed <- "standardized"
#     else       mainStdUsed <- "scaled"
#   } else {
#     if(center) mainStdUsed <- "centered"
#     else       mainStdUsed <- "non-standardized"
#   }
#   xt <- scale(t(data), scale=scale, center=center)
#   # Dist
#   if (is.null(selection))
#     dd2 <- dist(xt, method = "minkowski", p = p)
#   else {
#     # Dist by selection of rows 
#     # Note that the code is from limma::plotMDS.R and that it operates on columns; therefore we transpose data again
#     x <- t(xt)
#     nprobes <- nrow(x)
#     top <- min(top,nprobes)
#     nsamples <- ncol(x)
#     cn <- colnames(x)
#     dd2 <- matrix(0,nrow=nsamples,ncol=nsamples,dimnames=list(cn,cn))
#     if(selection=="pairwise") {
#       # Distance measure is mean of top squared deviations for each pair of arrays
#       topindex <- nprobes-top+1L
#       for (i in 2L:(nsamples))
#         for (j in 1L:(i-1L))
#           dd2[i,j]=sqrt(mean(sort.int((x[,i]-x[,j])^2,partial=topindex)[topindex:nprobes]))
#     } else {
#       # Same genes used for all comparisons
#       if(nprobes > top) {
#         s <- rowMeans((x-rowMeans(x))^2)
#         o <- order(s,decreasing=TRUE)
#         x <- x[o[1L:top],,drop=FALSE]
#       }
#       for (i in 2L:(nsamples))
#         dd2[i,1L:(i-1L)]=sqrt(colMeans((x[,i]-x[,1:(i-1),drop=FALSE])^2))
#     }
#     dd2 <- as.dist(dd2)
#   }
#   # Parametric (=initial) fit
#   fitCmdscale <- suppressWarnings(stats::cmdscale(dd2, k=k))
#   if (FUN == "isoMDS")
#     fitMDS <- eval(rlang::call2(FUN, y=fitCmdscale, dd2, k=k, maxit=maxit, trace=trace, tol=tol, p=p, .ns="MASS"))$points
#   else if (FUN == "sammon")
#     fitMDS <- eval(rlang::call2(FUN, y=fitCmdscale, dd2, k=k, niter=maxit, trace=trace, tol=tol,      .ns="MASS"))$points
#   else if (FUN == "cmdscale")
#     fitMDS <- fitCmdscale
#   else 
#     abort(paste(FUN, "not implemented or a valid MDS function"))
#   if (plot) {
#     if (!is.null(main) & main==TRUE) {
#       if (is.null(selection))
#         main <- paste(FUN, mainStdUsed, Minkowski, p, ",", dim(data)[[2]], "objects,", dim(data)[[1]], "parameters")
#       else
#         main <- paste(FUN, mainStdUsed, Minkowski, p, ",", dim(data)[[2]], "objects, top", top, selection, "parameters")
#     }
#     plot(fitMDS[,1], fitMDS[,2], type="n", xlab=xlab, ylab=ylab, ...)
#     text(fitMDS[,1], fitMDS[,2], labels=labels, cex=cex, col=col)
#     title(main=main, cex.main=cex.main)
#   }
#   return(fitMDS)
# }


## OLD
# #' Brew a color palette from a factor x with optional names.
# #' 
# #' Uses RColorBrewer palette names, default is OrRd with 9 colors
# #' Use more/less colors (n) for a bigger/smaller range, i.e. more/less extremes at each end.
# #' Colors are ordered according to the order of factor levels.
# #' If namesFrom given, add names to colors; otherwise, add names(x) or x.
# #' 
# #' @param x Numeric vector with optional names.
# #' @param n Number of colors for RColorBrewer (depends on the name).
# #' @param name Name of the RColorBrewer palette.
# #' @param pull Indices to pull colors from RColorBrewer::brewer.pal
# #' @param namesFrom Vector of names to be added to colors; default is names(x) or x.
# #' @return Named vector of colors of the same length as x.
# #' @export
# brewPalFac <- function(x, n=9, name="OrRd", pull=NULL, namesFrom=NULL) {
#     if (!is.null(namesFrom)) x <- setNames(x, namesFrom)
#     if (is.null(pull)) pull <- 1:n
#     assertthat::assert_that(all(pull > 0) & all(pull <= n) & all(pull == round(pull)))
#     x <- as.factor(x)
#     cols <- colorRampPalette(rev(RColorBrewer::brewer.pal(n, name)[pull]))(length(levels(x)))[x]
#     if (!is.null(names(x))) setNames(cols,names(x)) else setNames(cols,x)
# }

if (FALSE) {

    ## MDScols
    # MDScols <- function(data, scale=FALSE, center=FALSE, FUN = "isoMDS", p = 2, selection = "pairwise", top = 500,
    #   k = 2, maxit = 50, trace = TRUE, tol = 1e-3, plot = FALSE, labels = names(data), 
    #   col=NULL, cex=1, main=NULL, cex.main=1, xlab="Coordinate 1", ylab="Coordinate 2", ...)
    das <- dataRG$Mproc %>% as_tibble %>% slice(20000:50000)
    mmc1 <- MDScols(das, FUN = "cmdscale", selection=NULL)
    mmc2 <- MDScols(das, FUN = "cmdscale", selection="pairwise")
    mmc3 <- MDScols(das, FUN = "cmdscale", selection="common")
    mms1 <- MDScols(das, FUN = "sammon",   selection=NULL)
    mms2 <- MDScols(das, FUN = "sammon",   selection="pairwise")
    mms3 <- MDScols(das, FUN = "sammon",   selection="common")
    mmi1 <- MDScols(das, FUN = "isoMDS",   selection=NULL)
    mmi2 <- MDScols(das, FUN = "isoMDS",   selection="pairwise")
    mmi3 <- MDScols(das, FUN = "isoMDS",   selection="common")
    # Dist
    dist2 <- dist(t(das), method = "minkowski", p = 2)



    ## brewPalFac2
    aa <- c("b","a","c","b")
    unique(aa)
    order(unique(aa))
    (fa <- factor(aa, levels=c("a","c","b")))   # factor with a specified (non-alphabetical) order
    # how the levels appear in the vector
    xtfrm(unique(fa))
    # how xftrm works
    unique(fa)
    levels(fa)
    xtfrm(unique(fa))
    xtfrm(levels(fa))
    # make bb unordered factor
    bb <- c("cc","aa","bb","bb")
    (fb <- factor(bb, levels=unique(bb)))
    # reoder bb as aa
    (fba <- factor(fb, levels=unique(fb)[xtfrm(unique(fa))]))
    (fba2 <- factor(fb, levels=levels(fb)[xtfrm(unique(fa))]))

    #### 

    ## brewPalFac2 with colors
    (n3 <- targets$HybName)
    (fb3 <- targets$Birth3)
    factor(brewPalFac(fb3, 4, "RdBu", pull=c(1,2,4), namesFrom=n3))
    (cb3 <- brewPalFac2(fb3, 4, "RdBu", pull=c(1,2,4), namesFrom=n3))
    targets$color_Birth3

    (f3 <- factor(targets$Birth3, levels=c("term", "pLate", "pVery")))
    (c3 <- brewPalFac2(f3, 4, "RdBu", pull=c(1,2,4)))
    targets$color_Birth3

    (db3 <- factor(cb3))
    (d3 <- factor(c3))

    (ob3 <- order(levels(fb3)))
    (qb3 <- order(levels(db3)))
    (o3 <- order(levels(f3)))
    (q3 <- order(levels(d3)))
    order(levels(db3))
    order(levels(d3))

    (eb3 <- factor(cb3, levels=levels(db3)[qb3]))
    (e3 <-  factor(c3,  levels=levels(d3)[q3]))
    
}