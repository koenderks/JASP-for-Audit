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
  mean.value                        <- round(mean(values), 2)
  sd.value                          <- round(sd(values), 2)
  Q                                 <- round(as.numeric(quantile(values, c(0.25, 0.50, 0.75))), 2)

  row <- data.frame(popSize = popSize, value = total.value, mean = mean.value, sd = sd.value, q1 = Q[1], q2 = Q[2], q3 = Q[3])
  dataTable$addRows(row)
}

.plotCriticalErrorsPrior <- function(allowed.errors, reject.errors, jaspResults){

    errorrange <- 0:max(reject.errors)
    errors <- c(allowed.errors, reject.errors)
    fill <- c(rep(rgb(0,1,.5,.7), length(allowed.errors)),
              rep(rgb(1,0,0,.7), length(reject.errors)))

    rectdata <- data.frame(xmin = errors - 0.5, xmax = errors + 0.5, ymin = 0, ymax = 0.5,
                           fill = fill)

    df <- data.frame()
    p <- ggplot2::ggplot(df) +
        ggplot2::geom_point() +
        ggplot2::ylim(0, 1) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab("Observed sample errors")

    p <- p + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                data = rectdata, fill = rectdata$fill, color = "black")

    pdata <- data.frame(x = c(0,0), y = c(0,0), l = c("1","2"))
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0), rgb(1,0,0,0)))
    p <- p + ggplot2::scale_shape_manual(name = "", values = c(22,22), labels = c("Accept population", "Reject population"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 9, shape = 22, fill = c(rgb(0,1,.5,.7),rgb(1,0,0,.7)), stroke = 2, color = "black")), order = 1)

    p <- p + ggplot2::scale_x_continuous(breaks = errorrange)


    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")
    p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_text(size = 17))

    return(createJaspPlot(plot = p, title = "Decision Plot", width = 500, height = 300))

}

.plotValueDistribution <- function(dataset, options, jaspResults){

    values <- dataset[, .v(options[["monetaryVariable"]])]

    p <- .plotMarginalJfA(values, options[["monetaryVariable"]])
    return(createJaspPlot(plot = p, title = "Distribution Information", width = 700, height = 300))

    # filename <- tempfile()
    # png(filename = filename)
    # p <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
    # dev.off()
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
  xlim            <- round(max(c(materiality, bound, expected.errors, found.errors)) * 1.1, 2)
  xBreaks         <- pretty(c(0, xlim))
  xLabels         <- paste0(round(xBreaks * 100, 2), "%")

  boundData <- data.frame(xmin = c(0, bound), xmax = c(bound, xlim), ymin = 0, ymax = 0.5,
                         fill = c(rgb(1,0,0,.7), rgb(1,1,1)))

  df <- data.frame()
  p <- ggplot2::ggplot(df) +
      ggplot2::geom_point() +
      ggplot2::ylim(0, 1) +
      ggplot2::ylab(NULL) +
      ggplot2::xlab("Error percentage") +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         data = boundData, fill = boundData[["fill"]], color = "black", size = 2) +
      ggplot2::scale_x_continuous(breaks = xBreaks, labels = xLabels) +
      ggplot2::geom_segment(ggplot2::aes(x = materiality, xend = materiality, y = 0.025, yend = 0.475, col = "Materiality"), size = 2) +
      ggplot2::geom_segment(ggplot2::aes(x = bound, xend = bound, y = 0.025, yend = 0.475, col = "Bound"), size = 2) +
      ggplot2::geom_segment(ggplot2::aes(x = expected.errors, xend = expected.errors, y = 0.025, yend = 0.475, col = "Expected errors"), size = 2) +
      ggplot2::geom_segment(ggplot2::aes(x = found.errors, xend = found.errors, y = 0.025, yend = 0.475, col = "Found errors"), size = 2) +
      ggplot2::scale_color_manual(name = "", values=c("Bound" = "black", "Materiality" = rgb(0,1,.5,1), "Expected errors" = "yellow", "Found errors" = "orange")) +
      ggplot2::guides(col = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_text(size = 17))

  return(createJaspPlot(plot = p, title = "Evaluation information", width = 650, height = 200))

}
