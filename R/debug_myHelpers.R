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