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

  if(options[["stratum"]] == ""){
    plotStat <- data.frame(materiality = options[["materiality"]],
                            bound = result[["bound"]],
                            stratum = "Population")
  } else {
    plotStat <- data.frame(materiality = options[["materiality"]],
                            bound = result[["stratumbounds"]],
                            stratum = result[["stratumnames"]])
  }

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
      ggplot2::scale_x_discrete(labels = plotStat[["stratum"]]) +
      base_breaks_y(plotStat, options)

  if(options[["show"]] == "percentage"){
    p <- p + ggplot2::ylab("Error percentage")
  } else if(options[["show"]] == "proportion"){
    p <- p + ggplot2::ylab("Error proportion")
  }

  if(options[["stratum"]] == ""){
    p <- p + ggplot2::xlab(NULL)
  } else {
    p <- p + ggplot2::xlab("Stratum")
  }

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE)

  return(createJaspPlot(plot = p, title = "Confidence Bounds Plot", width = 600, height = 450))

}
