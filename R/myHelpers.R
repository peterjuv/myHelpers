######################
#### STAT + PLOTS ####
######################

write_boxplot_descStat <- function(data, fNameMid, folder=".", varsExclude=c(), varsInclude=NULL, varColor=NA, statMethod=NULL) {
  ## 2019-10-13 for OBV
  ## Descriptive statistics tables, boxplots
  ## varColor: variable for colors in boxplots
  require(dplyr)
  require(ggplot2)
  require(ggpubr)
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

# plot_corr <- function(data, fNameMid, folder=".", varsExclude=c(), varsInclude=NULL, order="AOE", width=12, height=12) {
#   ## 2019-10-13 for OBV
#   ## uses cor() and corrplot.mixed
#   require(corrplot)
#   numVars <- unlist(lapply(data, is.numeric))
#   numVars[names(numVars) %in% varsExclude] <- FALSE
#   numNames <- names(numVars[numVars])
#   if (!is.null(varsInclude)) numNames <- intersect(varsInclude, numNames)
#   pdf(file.path(folder,paste0("corrplot_",fNameMid,"_",deparse(substitute(data)),".pdf")),width, height)
#   cors <- cor(data[, numNames], use="na.or.complete")
#   colnames(cors) <- rownames(cors) <- numNames
#   corrplot.mixed(cors, order=order)
#   dev.off()
# }
plot_corr <- function(data, fNameMid, folder=".", varsExclude=c(), varsInclude=NULL, orders=c("AOE", "original"), wh=12, ...) {
  ## 2019-10-13 for OBV; 2020-05-05 for Kiraly
  ## uses cor() and corrplot.mixed()
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

write.table.rowNames <- function(x, col1name="rowNames", quote=F, sep="\t", row.names=F, col.names=T, ...) {
  xx <- cbind("rowNames"=rownames(x), x)
  colnames(xx) <- c(col1name, colnames(xx)[-1])
  write.table(xx, quote=quote, sep=sep, row.names=row.names, col.names=col.names, ...)
}

#####################
#### CONVERSIONS ####
#####################

#' Brew a color palette from a numerical vector x. 
#' 
#' Uses RColorBrewer palette names, default is OrRd with 9 colors
#' Use more/less colors (n) for a bigger/smaller range, i.e. mess/less extremes at each end.
#' Use more/less digits for more/less categories of colors.
#' 
#' @param x Numeric vector.
#' @param n Number of colors for RColorBrewer (depends on the name).
#' @param name Name of the RColorBrewer palette.
#' @param digits Number of digits in x to form categories for colors. May be negative to round to tens, hundreds, etc.
#' @return Named vector of color names of the same length as x.
brewPalCont <- function(x, n=9, name="OrRd", digits=2) {
    require(RColorBrewer)
    myPalette <- colorRampPalette(rev(brewer.pal(n, name)))
    xInd <- round(x*10**digits)
    cols <- myPalette(max(xInd)-min(xInd)+1)[xInd-min(xInd)+1]
    if (!is.null(names(x))) setNames(cols,names(x)) else setNames(cols,x)
}

discretize_namedIntervals <- function(X,...) {
  ## See infotheo::discretize for parameters; defaults are: 
  ##    disc  = "equalfreq"
  ##    nbins = NROW(X)^(1/3)
  ## Returns discretized data (atomic or list) with elements in the form of intervals min-max.
  require(infotheo)
  d <- infotheo::discretize(X,...)
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

# discretize_namedIntervals_vector <- function(X,...) {
#     ## See infotheo::discretize for parameters; defaults are: disc="equalfreq", nbins=sqrt(NROW(x)).
#     ## Returns discretized data in the form of intervals min-max.
#     require(infotheo)
#     d<- infotheo::discretize(X,...)$X
#     m<-tapply(X,d,min)
#     M<-tapply(X,d,max)
#     d_int <- paste0(format(m), "-", format(M))
#     names(d_int) <- names(m)
#     d_int[m==M] <- format(m)[m==M]
#     #d_int[d]
#     browser()
#     #as.character((sapply(as.character(d), function(cd) {d_int[cd]})))
#     as.character(d_int[as.character(d)])
# }

discretizeTime_equalInterval <- function(times, time0, nbins) {
  ## Input times (vector) in POSIX* format and number of intervals (nbins)
  ## Outputs character vector of discretized time intervals in form "H:MM-H:MM"
  require(infotheo)
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


capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
    {s <- substring(s,2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


#### PLOTS ####

plotIsoMDS <- function(data9, scale=F, center=F, labels=names(data9), col=NULL, cex=1, main=NULL, cex.main=1, xlab="Coordinate 1", ylab="Coordinate 2", ...) {
  ## similar to limma::plotMDS, except that is uses all parameters for distance calculation, while limma uses only top=XX genes, but XX cannot be set?!
  ## main=T: title is generated automatically
  mainStdUsed <- "non-standardized"
  if(scale) { if(center) {mainStdUsed<-"standardized"} else {mainStdUsed<-"scaled"}} else {if(center) {mainStdUsed<-"centered"}}
  scTdata9 <- scale(t(data9), scale=scale, center=center)
  dist9 <- dist(scTdata9)
  require(MASS)
  fit9 <- isoMDS(dist9, k=2)
  if (!is.null(main)) {
    if (main==T) {
      myMain <- paste("isoMDS", dim(data9)[[2]], "objects,", dim(data9)[[1]], mainStdUsed, "parameters")
    } else {
      myMain <- main
    }
  }
  plot(fit9$points[,1], fit9$points[,2], type="n", xlab=xlab, ylab=ylab, ...)
  text(fit9$points[,1], fit9$points[,2], labels=labels, cex=cex, col=col)
  title(main=myMain, cex.main=cex.main)
} 

multiEffectPlot <- function(model, effectNames=c("anova", "summary"), OR=FALSE, test="Chi") {
  ## 2019-10-13 for OBV
  ## LOGISTIC REGRESSION PLOT
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

ggBinary <- function (data1, X, Y, color, xlab, ylab, notch=FALSE, ...) {
  ggplot(data1, aes_string(Y,X), ...) + xlab(xlab) + ylab(ylab) + 
    coord_flip() + #scale_x_discrete(breaks=f, drop=FALSE) +
    geom_boxplot(outlier.shape=NA, notch=notch, width=0.2) + 
    geom_jitter(aes_string(color=color),height=0,width=0.2)
}

# plotBinary <- function (X, Y, ...) {
#   plot(X, jitter(Y, factor = 0.1), col = rgb(0, 0, 0, 0.5), pch = 19, ylim = c(-0.2, 1.2), ...)
#   boxplot(X ~ Y, horizontal = TRUE, notch = TRUE, add = TRUE,
#           at = c(-0.1, 1.1), width = c(0.1, 0.1), col = "grey",
#           boxwex = 0.1, yaxt = "n")
# }

#### CALCULATIONS ####

## 1-Persons/2, handles NA
corDist <- function(x) as.dist((1-cor(t(x), use="pairwise.complete.obs"))/2)

## robust scale columns, does not handle NA
scaleRobust <- function(x) sweep(sweep(x,2,apply(x,2,median)), 2, apply(x,2,mad), "/")
