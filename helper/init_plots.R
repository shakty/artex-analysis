saveOrDisplayPlot <- function(p, varname, SAVE=NA, width=7, height=7, ex=FALSE,
                              imgdir=IMGDIR, P1=FALSE, type=NA, facet=NA,
                              ex.name="ex", format="svg", PREFIX = '') {

    if (!is.na(SAVE) & SAVE != FALSE) {
        if (gregexpr(pattern ='cf.changes.', varname)[[1]][1] != -1) {
            ## Remove "cf.changes." from the beginning of varname.
            filename <- substr(varname, (nchar("cf.changes.")+1),
                               nchar(varname))
        } else if (gregexpr(pattern ='gini.', varname)[[1]][1] != -1) {
            ## If pattern not found, try gini.
            filename <- substr(varname, (nchar("gini.")+1), nchar(varname))
        } else {
            ## If pattern not found, keep varname as it is.
            filename <- varname
        }
        ## Add type.
        if (!is.na(type)) filename <- paste0(filename, "_", type)
        ## Build prefix
        prefix <- ifelse(P1 == TRUE, paste0(IMGPREFIX, "p1_"), IMGPREFIX)
        if (format == "png") {
            imgdir <- paste0(imgdir, "png/")
        }
        filename <- paste0(imgdir, paste0(prefix, PREFIX, filename))
        if (!is.na(facet)) {
            filename <- paste0(filename, '_by_', facet)
        }
        if (ex == TRUE && (is.na(facet) || facet != "ex")) {
            filename <- paste0(filename, "-", ex.name)
        }
        filename <- paste0(filename, ".", format)
        ggsave(filename, p, width=width, height=height)
        return(filename)
    } else {
        return(p)
    }
}

capitalizeWord <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

decoratePlot <- function(p, varname, doLegend=FALSE, lab="y",
                         measure='') {

    str <- paste0('Avg. ', measure, ' ')

    if (varname == "published") {
        str <- paste0(str, 'Ratio of Published Images')
    } else if (varname == "score") {
        str <- paste0(str, 'AMT Score')
    } else if (varname == "r.mean.from") {
        str <- paste0(str, 'Image Review Score')
    } else if (gregexpr(pattern ='subcurr', varname)[[1]][1] != -1) {
        str <- paste0(str, 'Diversity')
    } else if (gregexpr(pattern ='ownprev', varname)[[1]][1] != -1) {
        str <- paste0(str, 'Personal Change')
    } else if (gregexpr(pattern ='pubprev', varname)[[1]][1] != -1) {
        str <- paste0(str, 'Innovation')
    } else if (gregexpr(pattern ='.tot', varname)[[1]][1] != -1) {
        str <- paste0(str, 'Slider Movement')
    } else if (gregexpr(pattern ='.nfeatures', varname)[[1]][1] != -1) {
        str <- paste0(str, 'Num. Slider Moved')
    } else {
        str <- paste0(str, capitalizeWord(varname))
    }

    if (gregexpr(pattern ='.norm', varname)[[1]][1] != -1) {
        str <- paste0(str, ' Norm.')
        if (gregexpr(pattern ='.fix', varname)[[1]][1] != -1) {
            str <- paste0(str, ' (fix)')
        } else if (gregexpr(pattern ='.avg', varname)[[1]][1] != -1) {
            str <- paste0(str, ' (avg)')
        }
    }
    if (lab == "y") {
        p <- p + ylab(str)
    } else {
        p <- p + xlab(str)
    }
    if (doLegend == FALSE) {
        p <- p + theme(legend.position="none")
    }
    return(p)
}

doPlotRound <- function(varname, SAVE=FALSE, P1=TRUE, P1.INCLUDED=FALSE,
                        doLegend=FALSE, data=pr, facet=NA, nocond=FALSE,
                        format="svg", width=7, height=7, na.rm=FALSE, ex=FALSE,
                        ex.name="ex",
                        customize=NA, PREFIX='', round="round") {

    colorVar <- "condition"
    myFacet <- ""
    if (P1.INCLUDED) {
        P1 = FALSE
    }
    mycolors <- gg_color_hue(2)
    if (P1 == TRUE || P1.INCLUDED == TRUE) {
       mycolors <- gamePlusPracticeColors
    }
    if (is.na(facet)) {
        if (nocond == FALSE) {
            vars <- c("condition", round)
        } else {
            vars <- c(round)
        }
    } else {
        if (nocond == FALSE) {
            vars <- c("condition", round, facet)
        } else {
            vars <- c(round, facet)
        }
        if (na.rm == TRUE) {
            data <- data[!is.na(data[,facet]),]
        }
    }
    if (ex != FALSE && (is.na(facet) || facet != ex.name)) {
        colorVar <- ex.name
        vars <- c(vars, ex.name)
        if (is.na(facet)) {
            myFacet <- as.formula(paste("~ condition"))
        } else {
            myFacet <- as.formula(paste("condition ~", facet))
        }
        if (na.rm == TRUE) {
            data <- data[!is.na(mydata[,ex.name]),]
        }
    } else if (!is.na(facet)) {
        myFacet <- as.formula(paste("~", facet))
    }
    mydata <- summarySE(data, varname, vars, na.rm=na.rm)

    if (myFacet != "" && na.rm == TRUE) {
        if (is.na(facet)) {
          if (ex != FALSE) {
            toCheck <- ex.name
          } else {
            stop("Code in progress...Check here what to check!!")
          }
        } else {
          toCheck <- facet
        }
        mydata <- mydata[!is.na(mydata[,toCheck]),]
    }

    if (nocond == FALSE || colorVar == ex.name) {
        p <- ggplot(mydata, aes_string(round, varname, color=colorVar,
                                       group = colorVar))
    } else {
        p <- ggplot(mydata, aes_string(round, varname))
    }

    p <- p + geom_line(size=2,alpha=0.5)
    p <- p + geom_point(size=3)
    p <- p + geom_errorbar(aes_string(ymin=paste0(varname,"-ci"),
                                      ymax=paste0(varname, "+ci"),
                                      width=0.5))
    if (P1 == TRUE) {
        p1Level <- as.numeric(mean(p1[,varname]))
        p <- p + geom_hline(yintercept=p1Level, linetype="dashed", alpha=0.5)
        p <- p + annotate("text", x=11.5, y=p1Level*1.01, label = "Practice")
    }
    p <- p + xlab(capitalizeWord(round))
    p <- decoratePlot(p, varname, doLegend)
    if (ex == FALSE) {
        p <- p + scale_fill_manual(values=mycolors)
        p <- p + scale_color_manual(values=mycolors)
    }
    if (myFacet != "") {
        p <- p + facet_grid(myFacet)
    }
    if (!any(is.na(customize))) {
        p <- p + customize
    }
    return(saveOrDisplayPlot(p, varname, SAVE=SAVE, P1=P1, type="round",
                             format=format, width=width, height=height,
                             PREFIX=PREFIX, facet=facet, ex=ex,
                             ex.name=ex.name))
}


doPlotMean <- function(varname, SAVE=FALSE, P1=TRUE,
                       P1.INCLUDED = FALSE, doLegend=FALSE,
                       data=pr, ex=FALSE, facet=NA,
                       PREFIX='', ex.name="ex",
                       customize=NA, group="condition", color=NA,
                       format="svg", width=7, height=7, na.rm=FALSE) {

    if (is.na(color)) {
        colorVar <- group
    } else {
        colorVar <- color
    }
    myFacet <- ""
    if (P1.INCLUDED) {
        P1 = FALSE
    }
    mycolors <- gg_color_hue(length(unique(data[,colorVar])))
    if (P1 == TRUE || P1.INCLUDED == TRUE) {
       mycolors <- gamePlusPracticeColors
    }
    if (is.na(facet)) {
        vars <- c(group)
    } else {
        vars <- c(group, facet)
        if (na.rm == TRUE) {
            data <- data[!is.na(data[,facet]),]
        }
    }
    if (ex != FALSE && (is.na(facet) || facet != ex.name)) {
        colorVar <- ex.name
        vars <- c(vars, ex.name)
        if (is.na(facet)) {
            myFacet <- as.formula(paste("~ ", group))
        } else {
            myFacet <- as.formula(paste(group, " ~", facet))
        }
        if (na.rm == TRUE) {
            data <- data[!is.na(mydata[,ex.name]),]
        }
    } else if (!is.na(facet)) {
        myFacet <- as.formula(paste("~", facet))
    }

    if (myFacet != "" && na.rm == TRUE) {
      if (is.na(facet)) {
        if (ex != FALSE) {
          toCheck <- ex.name
        } else {
          stop("Code in progress...Check here what to check!!")
        }
      } else {
        toCheck <- facet
      }
      data <- data[!is.na(data[,toCheck]),]
      if (toCheck != group ) {
        data <- data[!is.na(data[,group]),]
      }
    }

    mydata <- summarySE(data, varname, vars, na.rm=na.rm)

    if (P1 == TRUE) {
        mydata <- rbind(mydata, summarySE(p1, varname, vars))
    }
    p <- ggplot(mydata, aes_string(group, varname, fill=colorVar))
    p <- p + geom_bar(stat="identity", position="dodge")
    p <- p + geom_errorbar(aes_string(ymin=paste0(varname,"-ci"),
                                      ymax=paste0(varname, "+ci")), width=0.5)
    p <- p + xlab('')
    p <- decoratePlot(p, varname, doLegend)
    if (ex == FALSE) {
        p <- p + scale_fill_manual(values=mycolors)
    }
    if (myFacet != "") {
        p <- p + facet_grid(myFacet)
    }
    if (!any(is.na(customize))) {
        p <- p + customize
    }
    return(saveOrDisplayPlot(p, varname, SAVE=SAVE, P1=P1, type="mean",
                             format=format, width=width, height=height, ex=ex,
                             facet=facet, PREFIX=PREFIX, ex.name=ex.name))
}

doPlotDistr <- function(varname, SAVE=FALSE, P1=TRUE, P1.INCLUDED=FALSE,
                        doLegend=FALSE, data=pr, extraData=NA, facet=NA,
                        ex=FALSE, ex.name="ex", na.rm=FALSE, customize=NA,
                        format="svg", width=7, height=7,  group="condition",
                        PREFIX='', color=NA) {

    if (is.na(color)) {
        colorVar <- group
    } else {
        colorVar <- color
    }
    myFacet <- ""
    if (P1.INCLUDED) {
        P1 <- FALSE
    }
    mycolors <- gg_color_hue(2)
    if (P1 == TRUE || P1.INCLUDED == TRUE) {
       mycolors <- gamePlusPracticeColors
    }
    if (is.na(facet)) {
        vars <- c(group, varname)
    } else {
        vars <- c(group, varname, facet)
        if (na.rm == TRUE) {
            data <- data[!is.na(mydata[,facet]),]
        }
    }
    if (ex != FALSE && (is.na(facet) || facet != ex.name)) {
        colorVar <- ex.name
        vars <- c(vars, ex.name)
        if (is.na(facet)) {
            myFacet <- as.formula(paste("~ ", group))
        } else {
            myFacet <- as.formula(paste(group, " ~", facet))
        }
        if (na.rm == TRUE) {
            data <- data[!is.na(mydata[,ex.name]),]
        }
    } else if (!is.na(facet)) {
        myFacet <- as.formula(paste("~", facet))
    }
    mydata <- data[, vars]
    if (P1 == TRUE) {
        mydata <- rbind(mydata, p1[, vars])
    }
    if (!missing(extraData)) {
        mydata <- rbind(mydata, extraData[, vars])
        mycolors <- c(mycolors, "green")
    }

    if (na.rm == TRUE) {
      if (!is.na(facet)) {
        mydata <- mydata[!is.na(mydata[, facet]),]
      }
      mydata <- mydata[!is.na(mydata[, group]),]
    }

    p <- ggplot(mydata, aes_string(varname, fill=colorVar, color=colorVar))
    p <- p + geom_density(alpha = 0.5)
    p <- decoratePlot(p, varname, doLegend, lab="x")
    p <- p + ylab("Density")
    if (ex == FALSE) {
        p <- p + scale_fill_manual(values=mycolors)
        p <- p + scale_color_manual(values=mycolors)
    }
    if (myFacet != "") {
        p <- p + facet_grid(myFacet)
    }
    if (!any(is.na(customize))) {
        p <- p + customize
    }
    return(saveOrDisplayPlot(p, varname, SAVE=SAVE, P1=P1, type="distr",
                             format=format, width=width, height=height,
                             ex=ex,facet=facet, PREFIX=PREFIX,
                             ex.name=ex.name))
}

doPlotMeanSd <- function(varname, SAVE=FALSE, P1=TRUE, doLegend=FALSE,
                         format="svg", width=7, height=7, data=pr, PREFIX='') {

    mydata <- summarySE(data, varname, c("session", "round", "condition"))
    mydata <- summarySE(mydata, "sd", c("round", "condition"))
    colnames(mydata)[4] <- "mean.sd"
    p <- ggplot(mydata, aes(round, mean.sd, color=condition, group=condition))
    p <- p + geom_line(size=2,alpha=0.5)
    p <- p + geom_point(size=3)
    p <- p + geom_errorbar(aes(ymin=mean.sd-ci, ymax=mean.sd+ci), width=0.3)
    if (P1 == TRUE) {
        if (!(varname %in% names(p1))) {
            print(paste0('warn: variable not found in P1: ', varname))
        } else {
            p1Level <- as.numeric(sd(p1[,varname]))
            p <- p + geom_hline(yintercept=p1Level, linetype="dashed", alpha=.5)
            p <- p + annotate("text", x=11.5, y=p1Level*1.05, label="Practice")
        }
    }
    p <- p + xlab('Round')
    p <- decoratePlot(p, varname, doLegend, measure='St.Dev.')
    return(saveOrDisplayPlot(p, varname, SAVE=SAVE, P1=P1, type="meansd",
                             format=format, width=width, height=height,
                             PREFIX=PREFIX))
}

##
doPlotGini <- function(varname, SAVE=FALSE, P1=TRUE, doLegend=FALSE,
                       format="svg", width=7, height=7, PREFIX='') {
    mydata <- summarySE(ginis, varname, c("round", "condition"))
    p <- ggplot(mydata, aes_string("round", varname,
                                   color="condition", group="condition"))
    p <- p + geom_line(size=2,alpha=0.5)
    p <- p + geom_point(size=3)
    p <- p + geom_errorbar(aes_string(ymin=paste0(varname, "-ci"),
                                      ymax=paste0(varname, "+ci")), width=0.3)
    if (P1 == TRUE) {
        if (!(varname %in% names(ginis.p1))) {
            print(paste0('warn: variable not found in P1: ', varname))
        } else {
            p1Level <- as.numeric(ginis.p1[,varname])
            p <- p + geom_hline(yintercept=p1Level, linetype="dashed", alpha=.5)
            p <- p + annotate("text", x=11.5, y=p1Level*1.05, label="Practice")
        }
    }
    p <- p + xlab('Round')
    p <- decoratePlot(p, varname, doLegend, measure='Gini')
    return(saveOrDisplayPlot(p, varname, SAVE=SAVE, P1=P1, type="gini",
                             format=format, width=width, height=height,
                             PREFIX=PREFIX, ex.name=ex.name))
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




########### MAIN ENTRY POINT.

doPlot <- function(type, varname="cf.changes.tot", P1=TRUE, SAVE=FALSE, ...) {
    if (type == "round") {
        res <- doPlotRound(varname, P1=P1, SAVE=SAVE, ...)
    } else if (type == "mean") {
        res <- doPlotMean(varname, P1=P1, SAVE=SAVE, ...)
    } else if (type == "distr") {
        res <- doPlotDistr(varname, P1=P1, SAVE=SAVE, ...)
    } else if (type == "meansd") {
        res <- doPlotMeanSd(varname, P1=P1, SAVE=SAVE, ...)
    } else if (type == "gini") {
        res <- doPlotGini(varname, P1=P1, SAVE=SAVE, ...)
    } else {
        stop(paste0("WHAT? Are you kidding me? ", type))
    }
    return(res)
}
