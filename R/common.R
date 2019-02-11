.ARMformula <- function(options, jaspResults, position = 2){

    if(!is.null(jaspResults[["DR"]])) return()

    AR                      <- 1 - options[["confidence"]]
    IR                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    CR                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    DR                      <- AR / IR / CR

    if(options[["show"]] == "percentage"){
        text <- paste0("Audit risk (", round(AR * 100, 2),"%) = Inherent risk (", round(IR * 100, 2), "%) x Control risk (", round(CR * 100, 2), "%) x Detection risk (", round(DR * 100, 2), "%)")
    } else {
        text <- paste0("Audit risk (", round(AR, 2),") = Inherent risk (", round(IR, 2), ") x Control risk (", round(CR, 2), ") x Detection risk (", round(DR, 2), ")")
    }

    jaspResults[["ARMformula"]] <- createJaspHtml(text, "h3")
    jaspResults[["ARMformula"]]$position <- position
    jaspResults[["ARMformula"]]$dependOnOptions(c("IR", "CR", "confidence", "show"))

    jaspResults[["DR"]]     <- createJaspState(DR)
    jaspResults[["DR"]]     $dependOnOptions(c("IR", "CR", "confidence"))

}

.dataTable <- function(dataset, options, jaspResults, position){

  if(!is.null(jaspResults[["dataTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  dataTable                        <- createJaspTable("Population descriptives")
  jaspResults[["dataTable"]]       <- dataTable
  dataTable$position               <- position
  dataTable$dependOnOptions(c("monetaryVariable", "recordNumberVariable"))

  dataTable$addColumnInfo(name = 'popSize',     title = "Population size",        type = 'string')
  dataTable$addColumnInfo(name = 'value',       title = "Total value",            type = 'string')
  dataTable$addColumnInfo(name = 'mean',        title = "Mean",                   type = 'string')
  dataTable$addColumnInfo(name = 'sd',          title = "sd",                     type = 'string')
  dataTable$addColumnInfo(name = 'q1',          title = "25%",                    type = 'string', overtitle = "Quantiles")
  dataTable$addColumnInfo(name = 'q2',          title = "50%",                    type = 'string', overtitle = "Quantiles")
  dataTable$addColumnInfo(name = 'q3',          title = "75%",                    type = 'string', overtitle = "Quantiles")

  if(options[["recordNumberVariable"]] == "" || options[["monetaryVariable"]] == ""){
    row <- data.frame(popSize = ".", value = ".", mean = ".", sd = ".", q1 = ".", q2 = ".", q3 = ".")
    dataTable$addRows(row)
    return()
  }

  popSize                           <- options[["N"]]
  values                            <- dataset[, .v(options[["monetaryVariable"]])]
  total.value                       <- ceiling(sum(values))
  mean.value                        <- ceiling(mean(values))
  sd.value                          <- ceiling(sd(values))
  Q                                 <- ceiling(as.numeric(quantile(values, c(0.25, 0.50, 0.75))))

  row <- data.frame(popSize = popSize, value = total.value, mean = mean.value, sd = sd.value, q1 = Q[1], q2 = Q[2], q3 = Q[3])
  dataTable$addRows(row)
}

.plotCriticalErrorsPrior <- function(allowed.errors, reject.errors, jaspResults){

  errorrange <- 0:max(reject.errors)
  errors <- c(allowed.errors, reject.errors)
  fill <- c(rep(rgb(0,1,.5,.7), length(allowed.errors)),
            rep(rgb(1,0,0,.7), length(reject.errors)))

  pointdata <- data.frame(x = errorrange, y = 0.2)

  df <- data.frame()
  p <- ggplot2::ggplot(df) +
      ggplot2::geom_point() +
      ggplot2::ylim(0, 0.5) +
      ggplot2::ylab(NULL) +
      ggplot2::xlab("Sample errors")

  p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, fill = fill, shape = 21, stroke = 2, size = 15)
  pdata <- data.frame(x = c(0,0), y = c(0,0), l = c("1","2"))
  p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0), rgb(1,0,0,0)))
  p <- p + ggplot2::scale_shape_manual(name = "", values = c(21,21), labels = c("Accept population", "Reject population"))
  p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 9, shape = 21, fill = c(rgb(0,1,.5,.7),rgb(1,0,0,.7)), stroke = 2, color = "black")), order = 1)

  p <- p + ggplot2::scale_x_continuous(breaks = errorrange)

    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")
    p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_text(size = 17))

    return(createJaspPlot(plot = p, title = "Decision Plot", width = 600, height = 200))

}

.plotValueDistribution <- function(dataset, options, jaspResults){

    values <- dataset[, .v(options[["monetaryVariable"]])]

    meanx <- mean(values)
    sdx <- sd(values)
    q <- as.numeric(quantile(values, c(0.25, 0.5, 0.75)))
    minx <- min(q[1], meanx - sdx)
    maxx <- max(q[3], meanx + sdx)

    p <- .plotMarginalJfA(values, options[["monetaryVariable"]])

    p <- p + ggplot2::geom_point(ggplot2::aes(x = q[1], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = q[2], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = q[3], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx, y = 0), shape = 21, fill = "red", stroke = 2, size = 5)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx + sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx - sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)

    pdata <- data.frame(x = c(0,0,0), y = c(0,0,0), l = c("1","2","3"))
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0))) +
    ggplot2::scale_shape_manual(name = "", values = c(21,21,21), labels = c("Mean", "Mean \u00B1 sd", "Quantile")) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = c(5, 4, 3), shape = 21, fill = c("red","dodgerblue1", "orange"), stroke = 2, color = "black")), order = 1)

    p <- JASPgraphs::themeJasp(p, legend.position = "top")

    # # Code for points on distribution bars
    # h <- hist(values, plot = FALSE)
    # q1loc <- h$counts[which(q[1] < h$breaks)[1] - 1]
    # q2loc <- h$counts[which(q[2] < h$breaks)[1] - 1]
    # q3loc <- h$counts[which(q[3] < h$breaks)[1] - 1]
    # meanloc <- h$counts[which(meanx < h$breaks)[1] - 1]
    # sd1loc <- h$counts[which(meanx - sdx < h$breaks)[1] - 1]
    # sd2loc <- h$counts[which(meanx + sdx < h$breaks)[1] - 1]
    #
    # if(options[["interpretation"]]){
    #   p <- p + ggplot2::geom_point(ggplot2::aes(x = q[1], y = q1loc),         shape = 21, fill = "orange", stroke = 2, size = 3)
    #   p <- p + ggplot2::geom_point(ggplot2::aes(x = q[2], y = q2loc),         shape = 21, fill = "orange", stroke = 2, size = 3)
    #   p <- p + ggplot2::geom_point(ggplot2::aes(x = q[3], y = q3loc),         shape = 21, fill = "orange", stroke = 2, size = 3)
    #   p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx, y = meanloc),      shape = 21, fill = "red", stroke = 2, size = 5)
    #   p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx + sdx, y = sd2loc), shape = 21, fill = "blue", stroke = 2, size = 4)
    #   p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx - sdx, y = sd1loc), shape = 21, fill = "blue", stroke = 2, size = 4)
    #
    #   pdata <- data.frame(x = c(0,0,0), y = c(0,0,0), l = c("1","2","3"))
    #   p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0))) +
    #   ggplot2::scale_shape_manual(name = "", values = c(21,21,21), labels = c("Mean", "Mean \u00B1 sd", "Quantile")) +
    #   ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 5, shape = 21, fill = c("red","blue", "orange"), stroke = 2, color = "black")), order = 1)
    #   p <- JASPgraphs::themeJasp(p, legend.position = "top")
    # }

    return(createJaspPlot(plot = p, title = "Distribution Information", width = 600, height = 300))
}

.plotMarginalJfA <- function(column, variableName, rugs = FALSE, displayDensity = FALSE) {
  column <- as.numeric(column)
  variable <- na.omit(column)

  if(length(variable) == 0)
    return(NULL)

  h <- hist(variable, plot = FALSE)

  if (!displayDensity)
    yhigh <- max(h$counts)
  else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }

  ylow <- 0

  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)

  if (!displayDensity)
    p <-
      JASPgraphs::drawAxis(
        xName = variableName, yName = "Counts", xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  else
    p <-
      JASPgraphs::drawAxis(
        xName = variableName, yName = "Density", xBreaks = xticks,
        yBreaks = c(0,  1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )

  if (displayDensity)
    p <- p +
      ggplot2::geom_histogram(
        data = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill = "grey",
        col = "black",
        size = .7,
        center = ((h$breaks[2] - h$breaks[1])/2)
      ) +
      ggplot2::geom_line(
        data = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd = 1,
        col = "black"
      )
  else
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill     = "grey",
        col      = "black",
        size     = .7,
        center    = ((h$breaks[2] - h$breaks[1])/2)
      )

  # JASP theme
  p <- JASPgraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  # TODO: Fix jaspgraphs axis width X vs Y. See @vandenman.

  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.plotConfidenceBounds <- function(options, result, jaspResults){

  materiality     <- options[["materiality"]]
  bound           <- result[["bound"]]
  expected.errors <- base::switch(options[["expected.errors"]],
                                      "kPercentage" = options[["kPercentageNumber"]],
                                      "kNumber" = options[["kNumberNumber"]] / result[["n"]])
  found.errors    <- result[["k"]] / result[["n"]]

  name <- rev(c("Materiality", "Upper Bound", "MLE", "Expected Error"))
  column <- rev(c(materiality, bound, found.errors, expected.errors))

  boundColor      <- ifelse(bound < materiality, yes = rgb(0,1,.5,1), no = rgb(1,0,0,.7))

  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0,column), min.n = 4)

  tb <- data.frame(x = name, column = column)
  tb$x <- factor(tb$x, levels = tb$x)
  p  <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_bar(stat = "identity", col = "black", size = 1, fill = rev(c("darkgray", boundColor, "orange", "yellow"))) +
      ggplot2::coord_flip() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab("Error percentage") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = yBreaks, labels = paste0(round(yBreaks,2)*100, "%"))

  # JASP theme
  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

 #  xlim            <- round(max(c(materiality, bound, expected.errors, found.errors)) * 1.1, 2)
 #  xBreaks         <- pretty(c(0, xlim))
 #  xLabels         <- paste0(round(xBreaks * 100, 2), "%")
 #  boundColor      <- ifelse(bound < materiality, yes = rgb(0,1,.5,1), no = rgb(1,0,0,.7))
 #
 #  pdata <- data.frame(x = c(0,0,0,0), y = c(0,0,0,0), l = c("1","2","3","4"))
 #  boundData <- data.frame(xmin = c(0, bound), xmax = c(bound, xlim), ymin = 0, ymax = 0.5,
 #                         fill = c(boundColor, rgb(1,1,1)))
 #
 # df <- data.frame()
 # p <- ggplot2::ggplot(df) +
 #     ggplot2::geom_point() +
 #     ggplot2::ylim(0, 1) +
 #     ggplot2::ylab(NULL) +
 #     ggplot2::xlab("Error percentage") +
 #     ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
 #                        data = boundData, fill = boundData[["fill"]], color = "black", size = 2) +
 #     ggplot2::scale_x_continuous(breaks = xBreaks, labels = xLabels) +
 #     ggplot2::geom_segment(ggplot2::aes(x = materiality, xend = materiality, y = 0.025, yend = 0.475), size = 2, col = rgb(0,.5,1,1)) +
 #     ggplot2::geom_segment(ggplot2::aes(x = expected.errors, xend = expected.errors, y = 0.025, yend = 0.475), size = 2, col = "yellow") +
 #     ggplot2::geom_segment(ggplot2::aes(x = found.errors, xend = found.errors, y = 0.025, yend = 0.475), size = 2, col = "orange") +
 #     ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0))) +
 #     ggplot2::scale_shape_manual(name = "", values = c(21,21,21,21), labels = c("Expected errors", "Observed errors", "Maximum error", "Materiality")) +
 #     ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 9, shape = 21, fill = c("yellow","orange", "black", rgb(0,.5,1,1)), stroke = 2, color = "black")), order = 1)
 #
 #  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")
 #  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
 #                          axis.text.y = ggplot2::element_blank(),
 #                          axis.text.x = ggplot2::element_text(size = 17),
 #                          legend.text = ggplot2::element_text(size = 12))

  return(createJaspPlot(plot = p, title = "Evaluation information", width = 600, height = 300))

}

.plotScatterJFA <- function(dataset, options, jaspResults) {

    d <- data.frame(xx= dataset[,.v(options[["monetaryVariable"]])], yy= dataset[,.v(options[["correctMUS"]])])
    d <- na.omit(d)
    xVar <- d$xx
    yVar <- d$yy

    fit <- vector("list", 1)# vector("list", 4)
    fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), data = d)

    bestModel <- 1 # which.min(Bic)

    xlow <- min(pretty(xVar))
    xhigh <- max(pretty(xVar))
    xticks <- pretty(c(xlow, xhigh))
    ylow <- min(min(pretty(yVar)), min(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
    yhigh <- max(max(pretty(yVar)), max(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))

    yticks <- pretty(c(ylow, yhigh))

    # format x labels
    xLabs <- vector("character", length(xticks))
    xLabs <- format(xticks, digits= 3, scientific = FALSE)

    # Format y labels
    yLabs <- vector("character", length(yticks))
    yLabs <- format(yticks, digits= 3, scientific = FALSE)

    co <- round(cor(dataset[,.v(options[["monetaryVariable"]])], dataset[,.v(options[["correctMUS"]])]), 2)

    cols <- rep("gray", nrow(d))
    cols[which(d$xx != d$yy)] <- "red"

    p <- JASPgraphs::drawAxis(xName = "Book Values", yName = "True Values", xBreaks = xticks, yBreaks = yticks, yLabels = yLabs, xLabels = xLabs, force = TRUE)
    p <- JASPgraphs::drawPoints(p, dat = d, size = 3, fill = cols)
    p <- .poly.pred(fit[[bestModel]], plot = p, line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd = 1)
    p <- p + ggplot2::annotate("text", x = xticks[1], y = yticks[length(yticks)],
                                label = paste0("italic(r) == ", co), size = 8, parse = TRUE, hjust = -0.5, vjust = 0.5)

    # JASP theme
    p <- JASPgraphs::themeJasp(p)

    return(createJaspPlot(plot = p, title = "Correlation Plot", width = 500, height = 400))

}

.poly.pred <- function(fit, plot = NULL, line=FALSE, xMin, xMax, lwd) {
  # create function formula
  f <- vector("character", 0)

  for (i in seq_along(coef(fit))) {
    if (i == 1) {
      temp <- paste(coef(fit)[[i]])
      f <- paste(f, temp, sep="")
    }

    if (i > 1) {
      temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
      f <- paste(f, temp, sep="+")
    }
  }

  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text=f))

  if (line == FALSE) {
    return(predY)
  }

  if (line) {
    plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),mapping = ggplot2::aes(x = x, y = predY), size=lwd)
    return(plot)
  }
}
