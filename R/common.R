.ARMformula <- function(options, jaspResults, position = 2){

    if(!is.null(jaspResults[["ARMformula"]])) return()

    AR <- 1 - options[["confidence"]]

    if(options[["IR"]] == "Low" && options[["CR"]] == "Low"){
        IR <- 0.30
        CR <- 0.30
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "Medium"){
        IR <- 0.30
        CR <- 0.60
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "High"){
        IR <- 0.30
        CR <- 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "High"){
        IR <- 0.60
        CR <- 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Medium"){
        IR <- 0.60
        CR <- 0.60
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Low"){
        IR <- 0.60
        CR <- 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Low"){
        IR <- 1
        CR <- 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Medium"){
        IR <- 1
        CR <- 0.60
    } else if (options[["IR"]] == "High" && options[["CR"]] == "High"){
        IR <- 1
        CR <- 1
    }
    # Audit Risk Model
    DR               <- AR / IR / CR

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

.samplingDescriptivesTable <- function(dataset, options, jaspResults, sample, position = 3)
{

    if(!is.null(jaspResults[["descriptives"]])) return()

    all.variables                   <- unlist(options$variables)
    descriptives                    <- createJaspTable("Sample Descriptives")
    jaspResults[["descriptives"]]   <- descriptives
    descriptives$transpose          <- TRUE
    descriptives$position           <- position

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

.intervalTable <- function(dataset, options, jaspResults, interval, position = 1)
{

    if(!is.null(jaspResults[["intervalTable"]])) return()

    intervalTable                           <- createJaspTable("Interval information")
    jaspResults[["intervalTable"]]          <- intervalTable
    intervalTable$position                  <- position

    intervalTable$addColumnInfo(name="N", title ="Population size", type = "integer")
    intervalTable$addColumnInfo(name="n", title ="Sample size", type = "integer")
    intervalTable$addColumnInfo(name="I", title ="Interval width", type = "number", format="sf:4")

    sampleSize                              <- options$sampleSize

    row <- list("N" = nrow(dataset), "n" = sampleSize, "I" = round(interval, 0))
    intervalTable$addRows(row)

}

.plotSampleLocations <- function(options, rows, samples, xlab, jaspResults){

  df <- data.frame()
  p <- ggplot2::ggplot(df) +
      ggplot2::geom_point() +
      ggplot2::xlim(min(rows) - 1, max(rows) + 1) +
      ggplot2::ylim(-0.5, 0.5) +
      ggplot2::ylab(NULL) +
      ggplot2::xlab(xlab) +
      ggplot2::geom_segment(data = data.frame(x = min(rows) - 1, xend = max(rows) + 1, y = 0, yend = 0),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = min(rows) - 1, xend = min(rows) - 1, y = 0.3, yend = -0.3),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = max(rows) + 1, xend = max(rows) + 1, y = 0.3, yend = -0.3),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = samples, xend = samples, y = rep(0.1, length(samples)), yend = rep(-0.1, length(samples))),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1, color = "red")

      if(options[["markSamples"]]){
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = samples, y = rep(0, length(samples)), label = samples), vjust = -1, hjust = -0.5)
      }

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_blank())

  return(createJaspPlot(plot = p, title = "Sampling locations", width = 1000, height = 200))

}
