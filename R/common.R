.stratumInfoTable <- function(options, result, jaspResults, type){

  if(!is.null(jaspResults[["stratuminfotable"]])) return()

  stratuminfotable                       <- createJaspTable("Stratum Information Table")
  jaspResults[["stratuminfotable"]]      <- stratuminfotable
  stratuminfotable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show",
                                      "correctID", "stratum", "stratuminfo", "method"))
  stratuminfotable$position <- 2

  stratuminfotable$addColumnInfo(name = 'stratum',     title = "Stratum",  type = 'string')
  stratuminfotable$addColumnInfo(name = 'n',     title = "Size",  type = 'string')
  stratuminfotable$addColumnInfo(name = 'k',     title = "Errors",  type = 'string')
  stratuminfotable$addColumnInfo(name = 'bound',     title = paste0(result[["confidence"]]*100,"% Confidence bound"),  type = 'string')

  if(options[["stratum"]] == ""){
    message <- "Please define a stratum variable."
    stratuminfotable$errorMessage <- message
    stratuminfotable$error <- "badData"
    return()
  }

  if(options[["show"]] == "percentage"){
    boundTable <- paste0(round(result[["stratumbounds"]],3) * 100, "%")
    meanBoundTable <- paste0(round(result[["nsbound"]],3) * 100, "%")
  } else if(options[["show"]] == "proportion"){
    boundTable <- round(result[["stratumbounds"]], 3)
    meanBoundTable <- round(result[["nsbound"]], 3)
  }

  for(i in 1:length(result[["stratumbounds"]])){
    row <- list(stratum = result[["stratumnames"]][i], n = result[["stratumsize"]][i], k = result[["stratumerrors"]][i], bound = boundTable[i])
    stratuminfotable$addRows(row)
  }
  row <- list(stratum = "Final:", n = sum(result[["stratumsize"]]), k = sum(result[["stratumerrors"]]), bound = meanBoundTable)
  stratuminfotable$addRows(row)

  if(type == "frequentist"){
    if(options[["method"]] == "mean"){
      message <- "The final confidence bound is the mean of the sub-bounds."
    } else if(options[["method"]] == "normal"){
      message <- "The final confidence bound is aggregated from the sub-bounds with the Normal method."
    }
    stratuminfotable$addFootnote(message = message, symbol="<i>Note.</i>")
  }

}

.plotConfidenceBounds <- function(options, result, jaspResults){

  plotStat <- data.frame(materiality = options[["materiality"]],
                          bound = result[["bound"]],
                          xlab = "Population")

  materialityStat <- data.frame(materiality = options[["materiality"]])

  base_breaks_y <- function(x, options) {

      values <- c(options$materiality, 0, x[, "bound"])
      ci.pos <- c(min(values), max(values))
      b <- pretty(ci.pos)
      d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
      yBreaks <- c(min(b),  options$materiality, max(b))

      if(options[["show"]] == "percentage"){
          yLabels <- paste(yBreaks * 100, "%")
      } else if(options[["show"]] == "proportion"){
          yLabels <- yBreaks
      }

      list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                        yend = yend), inherit.aes = FALSE, size = 1),
           ggplot2::scale_y_continuous(breaks = yBreaks, labels = yLabels))
  }

  pd <- ggplot2::position_dodge(0.2)

  p <- ggplot2::ggplot(plotStat, ggplot2::aes(x = stratum, y = bound, group = stratum)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = 0, ymax = bound), colour = "black", width = 0.2, position = pd) +
      ggplot2::geom_hline(data = materialityStat, ggplot2::aes(yintercept = materiality), linetype = "dashed") +
      ggplot2::scale_x_discrete(labels = plotStat[["xlab"]]) +
      base_breaks_y(plotStat, options)

  if(options[["show"]] == "percentage"){
    p <- p + ggplot2::ylab("Error percentage")
  } else if(options[["show"]] == "proportion"){
    p <- p + ggplot2::ylab("Error proportion")
  }

  p <- p + ggplot2::xlab(NULL)

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE)

  return(createJaspPlot(plot = p, title = "Confidence Bounds Plot", width = 600, height = 450))

}

.samplingDescriptivesTable <- function(dataset, options, jaspResults, sample)
{

    if(!is.null(jaspResults[["descriptives"]])) return()

    all.variables                   <- unlist(options$variables)
    descriptives                    <- createJaspTable("Sample Descriptives")
    jaspResults[["descriptives"]]   <- descriptives
    descriptives$transpose          <- TRUE
    descriptives$position           <- 3

    descriptives$dependOnOptions(c("variables", "allowDuplicates", "seed", "sampleSize", "seedNumber",
                                    "showDescriptives", "mean", "sd", "var", "range", "min", "max", "median", "recordVariable"))

                                    descriptives$addColumnInfo(name="name",                        type="string", format="sf:4", title = "")
                                    descriptives$addColumnInfo(name="Valid cases",                 type="integer")
    if (options$mean)               descriptives$addColumnInfo(name="Mean",                        type="number", format="sf:4")
    if (options$median)             descriptives$addColumnInfo(name="Median",                      type="number", format="sf:4")
    if (options$sd)                 descriptives$addColumnInfo(name="Std. Deviation",              type="number", format="sf:4")
    if (options$var)                descriptives$addColumnInfo(name="Variance",                    type="number", format="sf:4")
    if (options$range)              descriptives$addColumnInfo(name="Range",                       type="number", format="sf:4")
    if (options$min)                descriptives$addColumnInfo(name="Minimum",                     type="number", format="sf:4")
    if (options$max)                descriptives$addColumnInfo(name="Maximum",                     type="number", format="sf:4")

    if(is.null(sample))
        return()

    for (variable in all.variables)
    {
      column    <- sample[[ .v(variable) ]]
      row <- list()

                                    row[["name"]]                   <- variable
                                    row[["Valid cases"]]            <- base::length(column)
      if(!is.factor(column))
      {
      if(options$mean)              row[["Mean"]]                   <- base::mean(column, na.rm = TRUE)
      if(options$sd)                row[["Std. Deviation"]]         <- stats::sd(column, na.rm = TRUE)
      if(options$var)               row[["Variance"]]               <- stats::var(column, na.rm = TRUE)
      if(options$median)            row[["Median"]]                 <- stats::median(column, na.rm = TRUE)
      if(options$range)             row[["Range"]]                  <- base::abs(base::range(column, na.rm = TRUE)[1] - base::range(column, na.rm = TRUE)[2])
      if(options$min)               row[["Minimum"]]                <- base::min(column, na.rm = TRUE)
      if(options$max)               row[["Maximum"]]                <- base::max(column, na.rm = TRUE)
      }

      descriptives$addRows(row)
    }

     descriptives$addFootnote(message="Not all statistics are available for <i>Nominal Text</i> variables", symbol="<i>Note.</i>")

}

.intervalTable <- function(dataset, options, jaspResults, interval)
{

    if(!is.null(jaspResults[["intervalTable"]])) return()

    intervalTable                           <- createJaspTable("Interval information")
    jaspResults[["intervalTable"]]          <- intervalTable
    intervalTable$position                  <- 1

    intervalTable$addColumnInfo(name="N", title ="Population size", type = "integer")
    intervalTable$addColumnInfo(name="n", title ="Sample size", type = "integer")
    intervalTable$addColumnInfo(name="I", title ="Interval width", type = "number", format="sf:4")

    sampleSize                              <- options$sampleSize

    row <- list("N" = nrow(dataset), "n" = sampleSize, "I" = interval)
    intervalTable$addRows(row)

}

.plotSampleLocations <- function(options, rows, samples, xlab, jaspResults){

  df <- data.frame()
  p <- ggplot2::ggplot(df) +
      ggplot2::geom_point() +
      ggplot2::xlim(min(rows), max(rows)) +
      ggplot2::ylim(-0.5, 0.5) +
      ggplot2::ylab(NULL) +
      ggplot2::xlab(xlab) +
      ggplot2::geom_segment(data = data.frame(x = min(rows)-1, xend = max(rows) + 1, y = 0, yend = 0),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = min(rows) -1, xend = min(rows) -1, y = 0.3, yend = -0.3),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = max(rows)+1, xend = max(rows)+1, y = 0.3, yend = -0.3),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = samples, xend = samples, y = rep(0.1, length(samples)), yend = rep(-0.1, length(samples))),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1, color = "red")

      if(options[["markSamples"]]){
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = samples, y = rep(0, length(samples)), label=samples), vjust = -1, hjust = -0.5)
      }

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_blank())

  return(createJaspPlot(plot = p, title = "Sampling locations", width = 1000, height = 200))

}
