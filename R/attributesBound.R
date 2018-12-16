attributesBound <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							                     <- list()
  dataset                                      <- .readDataBayesianAttributesBound(dataset, options)
  # Set the title
  jaspResults$title 					                 <- "Audit Attributes Bound"
  # Perform the analysis
  .attributesBound(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  .attributesBoundTable(options, result, jaspResults)
  if(options[["stratuminfo"]])
  {
    if(is.null(jaspResults[["stratuminfotable"]]))
      .stratumInfoTable(options, result, jaspResults)
  }
  if(options[['plotBounds']] && options[["correctID"]] != "")
  {
     if(is.null(jaspResults[["confidenceBoundPlot"]]))
     {
     jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
     jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID", "stratum",
                                                                "show", "plotBounds", "materiality", "method"))
     jaspResults[["confidenceBoundPlot"]] 		$position <- 3
     }
  }
  # Save the state
  state[["options"]] 					                  <- options
  return(state)

}

.attributesBound <- function(dataset, options, jaspResults){

  confidence              <- options[["confidence"]]
  correctID               <- options[["correctID"]]

  if(options[["IR"]] == "Low" && options[["CR"]] == "Low"){
      alpha               <- (1-confidence) / 0.30 / 0.30
  } else if (options[["IR"]] == "Low" && options[["CR"]] == "Medium"){
      alpha               <- (1-confidence) / 0.30 / 0.60
  } else if (options[["IR"]] == "Low" && options[["CR"]] == "High"){
      alpha               <- (1-confidence) / 0.30 / 1
  } else if (options[["IR"]] == "Medium" && options[["CR"]] == "High"){
      alpha               <- (1-confidence) / 0.60 / 1
  } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Medium"){
      alpha               <- (1-confidence) / 0.60 / 0.60
  } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Low"){
      alpha               <- (1-confidence) / 0.60 / 0.30
  } else if (options[["IR"]] == "High" && options[["CR"]] == "Low"){
      alpha               <- (1-confidence) / 1 / 0.30
  } else if (options[["IR"]] == "High" && options[["CR"]] == "Medium"){
      alpha               <- (1-confidence) / 0.60 / 1
  } else if (options[["IR"]] == "High" && options[["CR"]] == "High"){
      alpha               <- (1-confidence) / 1 / 1
  }

  if(is.null(dataset)){
    n                     <- 0
    k                     <- 0
  } else {
    n                     <- nrow(dataset)
    if(options[["correctID"]] != ""){
      k                     <- length(which(dataset[,.v(correctID)] == 1))
    } else {
      k                     <- "."
    }
  }

  if(options[["stratum"]] == "" && options[["correctID"]] != ""){

    if(n == 0){
      bound                 <- "."
      approve               <- "."
    } else {
      binomResult <- binom.test(x = k,
                                n = n,
                                p = options[["materiality"]],
                                alternative = "less",
                                conf.level = 1 - alpha)
      bound                 <- binomResult$conf.int[2]
      if(bound <= alpha){
        approve             <- "Yes"
      } else {
        approve             <- "No"
      }
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- k
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- confidence
    resultList[["bound"]]       <- bound
    resultList[["approve"]]     <- approve
    resultList[["alpha"]]       <- alpha

  } else if(options[["stratum"]] != "" && options[["correctID"]] != ""){

  stratum <- options[["stratum"]]

  if(n == 0){
    bound                 <- "."
    approve               <- "."
  } else {

    strata <- dataset[, .v(stratum)]
    stratumbounds <- numeric()
    levels <- levels(strata)
    stratumN <- numeric()
    stratumK <- numeric()

    for(i in 1:length(levels)){
      level <- dataset[which(dataset[, .v(stratum)] == levels[i]), .v(correctID)]
      curN <- length(level)
      curK <- length(which(level == 1))
      binomResult <- binom.test(x = curK,
                           n = curN,
                           p = options[["materiality"]],
                           alternative = "less",
                           conf.level = 1 - alpha)
      levelbound           <- binomResult$conf.int[2]

      stratumbounds <- c(stratumbounds, levelbound)
      stratumN <- c(stratumN, curN)
      stratumK <- c(stratumK, curK)
    }

    if(options[["method"]] == "normal"){
      midpoints <- stratumbounds - (stratumbounds/2)
      boundVar <- sqrt(sum((stratumbounds-midpoints)^2))
      boundMean <- mean(midpoints)
      nsbound <- boundMean + boundVar
    } else if(options[["method"]] == "mean"){
      nsbound <- mean(stratumbounds)
    }

    binomResult <- binom.test(x = k,
                              n = n,
                              p = options[["materiality"]],
                              alternative = "less",
                              conf.level = 1 - alpha)
    bound                 <- binomResult$conf.int[2]

    if(nsbound < alpha){
      approve             <- "Yes"
    } else {
      approve             <- "No"
    }

  }

  resultList <- list()
  resultList[["n"]]           <- n
  resultList[["k"]]           <- k
  resultList[["IR"]]          <- options[["IR"]]
  resultList[["CR"]]          <- options[["CR"]]
  resultList[["confidence"]]  <- confidence
  resultList[["nsbound"]]     <- nsbound
  resultList[["bound"]]       <- bound
  resultList[["approve"]]     <- approve
  resultList[["alpha"]]       <- alpha
  resultList[["stratumnames"]] <- levels
  resultList[["stratumbounds"]] <- stratumbounds
  resultList[["stratumsize"]] <- stratumN
  resultList[["stratumerrors"]] <- stratumK

} else if((options[["stratum"]] != "" && options[["correctID"]] == "") || is.null(dataset)){

  resultList <- list()
  resultList[["n"]]           <- n
  resultList[["k"]]           <- k
  resultList[["IR"]]          <- options[["IR"]]
  resultList[["CR"]]          <- options[["CR"]]
  resultList[["confidence"]]  <- confidence
  resultList[["bound"]]       <- "."
  resultList[["approve"]]     <- "."
  resultList[["alpha"]]       <- alpha

}

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "stratum", "method"))

}

.attributesBoundTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Classical Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID", "stratum", "method"))
  summaryTable$position <- 1

  summaryTable$addColumnInfo(name = 'IR',     title = "Inherent risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'CR',     title = "Control risk",   type = 'string')
  summaryTable$addColumnInfo(name = 'SR',     title = "Sampling risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'n',      title = "Sample size",    type = 'string')
  summaryTable$addColumnInfo(name = 'k',      title = "Errors",         type = 'string')

  if(options[["stratum"]] == ""){

    summaryTable$addColumnInfo(name = 'bound',  title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')

    if(options[["show"]] == "percentage"){
      SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
      if(result[["bound"]] == "."){
        boundTable          <- "."
      } else {
        boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
      }
    } else if(options[["show"]] == "proportion"){
      SRtable <- round(result[["alpha"]], 3)
      if(result[["bound"]] == "."){
        boundTable          <- "."
      } else {
        boundTable <- round(result[["bound"]],3)
      }
    }

    row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable)
    summaryTable$addRows(row)

  } else {

    summaryTable$addColumnInfo(name = 'boundns',  title = "Non-stratified", type = 'string', overtitle = paste0(result[["confidence"]]*100,"% Confidence bounds"))
    summaryTable$addColumnInfo(name = 'bounds',  title = "Stratified", type = 'string', overtitle = paste0(result[["confidence"]]*100,"% Confidence bounds"))

    if(options[["show"]] == "percentage"){
      SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
      if(result[["bound"]] == "."){
        boundTableNs          <- "."
        boundTableS          <- "."
      } else {
        boundTableNs <- paste0(round(result[["bound"]],3) * 100, "%")
        boundTableS <- paste0(round(result[["nsbound"]],3) * 100, "%")
      }
    } else if(options[["show"]] == "proportion"){
      SRtable <- round(result[["alpha"]], 3)
      if(result[["bound"]] == "."){
        boundTableNs          <- "."
        boundTableS          <- "."
      } else {
        boundTableNs <- round(result[["bound"]],3)
        boundTableS <- round(result[["nsbound"]],3)
      }
    }

    row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], boundns = boundTableNs, bounds = boundTableS)
    summaryTable$addRows(row)

  }

}

.stratumInfoTable <- function(options, result, jaspResults){

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

  if(options[["method"]] == "mean"){
    message <- "The final confidence bound is the mean of the sub-bounds."
  } else if(options[["method"]] == "normal"){
    message <- "The final confidence bound is aggregated from the sub-bounds with the Normal method."
  }
  stratuminfotable$addFootnote(message = message, symbol="<i>Note.</i>")
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
