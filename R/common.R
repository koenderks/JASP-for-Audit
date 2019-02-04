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
  dataTable$dependOnOptions(c("nothing"))

  dataTable$addColumnInfo(name = 'popSize',      title = "Population size",        type = 'string')
  dataTable$addColumnInfo(name = 'value',        title = "Total value",            type = 'string')

  popSize <- options[["N"]]
  total.value <- round(sum(dataset[, .v(options[["monetaryVariable"]])]), 2)

  row <- data.frame(popSize = popSize, value = total.value)
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
        ggplot2::xlab("Sample errors")

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
    index <- 1:length(values)
    xBreaks <- pretty(index)

    tb <- data.frame(index, values)
    p1  <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
        ggplot2::xlab("Record number") +
        ggplot2::ylab("Value") +
        ggplot2::scale_x_continuous(breaks = xBreaks)
    p1 <- JASPgraphs::themeJasp(p1)

    p2 <- .plotMarginalJfA(values, options[["monetaryVariable"]])

    if(!options[["showCumulative"]]){
      filename <- tempfile()
      png(filename = filename)
      p <- gridExtra::grid.arrange(p1, p2, ncol = 2)
      dev.off()
      return(createJaspPlot(plot = p, title = "Distribution information", width = 700, height = 300))
    }

    cum3 <- cumsum(values) / sum(values)
    tb3 <- data.frame(index, cum3)
    p3  <- ggplot2::ggplot(data = data.frame(x = tb3[, 1], y = tb3[, 2]), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = max(index), y = 0, yend = 1, linetype = "segment"), size = 1, color = "darkred") +
        ggplot2::geom_point(col = "black", size = 1, pch = 21, stroke = 1, fill = "gray") +
        ggplot2::xlab("Record number") +
        ggplot2::ylab("Cumulative density")
    p3 <- p3 + ggplot2::scale_linetype_manual(name = "", values = c("segment" = 1), labels = "Uniform")
    p3 <- JASPgraphs::themeJasp(p3, legend.position = "top")

    p4 <- ggplot2::ggplot(data.frame(values), ggplot2::aes(sample = values)) +
          ggplot2::geom_segment(ggplot2::aes(x = -4, xend = 4, y = 0, yend = max(values), linetype = "segment"), size = 1, color = "darkred") +
          ggplot2::stat_qq(col = "black", size = 1, pch = 21, stroke = 1, fill = "gray") +
          ggplot2::ylab("Value") +
          ggplot2::xlab("Theoretical quantiles")
    p4 <- p4 + ggplot2::scale_linetype_manual(name = "", values = c("segment" = 1), labels = "Normal")
    p4 <- JASPgraphs::themeJasp(p4, legend.position = "top")

    filename <- tempfile()
    png(filename = filename)
    p <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
    dev.off()

    return(createJaspPlot(plot = p, title = "Distribution information", width = 700, height = 600))
}

.plotMarginalJfA <- function(column, variableName,
                          rugs = FALSE, displayDensity = FALSE) {
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
