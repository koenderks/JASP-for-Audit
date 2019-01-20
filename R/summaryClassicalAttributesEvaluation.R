summaryClassicalAttributesEvaluation <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							                     <- list()
  # Set the title
  jaspResults$title 					<- "Summary Statistics Attributes Bound"

  .ARMformula(options, jaspResults, position = 1)   # Show the Audit Risk Model formula and quantify detection risk
  DR              <- jaspResults[["DR"]]$object
  # Perform the analysis
  .summaryAttributesBound(options, jaspResults)
  result                      <- jaspResults[["result"]]$object
  .summaryAttributesBoundTable(options, result, jaspResults, position = 3)

  if(options[["interpretation"]]){

      jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>",options[["n"]], "</b> observations, <b>", result[["k"]] , "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                            bined with the prior knowledge results in an <b>",round((1 - result[["alpha"]]) * 100, 2), "%</b> upper confidence bound of <b>",round(result[["bound"]]*100, 2),"%</b>. The cumulative knowledge states that there
                                                            is a <b>",options$confidence*100, "%</b> probability that, when one would repeaditly sample from this population, the maximum error is calculated to be lower
                                                            than <b>",round(result[["bound"]]*100, 2),"%</b>."), "p")
      jaspResults[["resultParagraph"]]$position <- 2

  }

  if(options[['plotBounds']])
  {
     if(is.null(jaspResults[["confidenceBoundPlot"]]))
     {
     jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBoundsSummary(options, result, jaspResults)
     jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "n", "k", "plotBounds", "materiality",
                                                                "show", "N", "K"))
     jaspResults[["confidenceBoundPlot"]] 		$position <- 4
     }
  }

  if(options[["interpretation"]]){

      jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
      jaspResults[["conclusionTitle"]]$position <- 5

      if(result[["bound"]] < options[["materiality"]]){
          above_below <- "lower"
          approve <- "<b>no material misstatement</b>"
      } else if(result[["bound"]] >= options[["materiality"]]){
          above_below <- "higher"
          approve <- "<b>material misstatement, or more information has to be seen.</b>"
      }

      jaspResults[["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", options$confidence*100 ,"%</b> upper confidence bound on the population proportion of full errors should be determined to be
                                                                  lower than materiality, in this case <b>", options$materiality*100 ,"%</b>. For the current data, the confidence bound is <b>", above_below ,"</b> than materiality. The conclusion for
                                                                  these data is that the data contain ", approve ,"."), "p")
      jaspResults[["conclusionParagraph"]]$position <- 6

  }
  # Save the state
  state[["options"]] 					                  <- options
  return(state)

}

.summaryAttributesBound <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence <- options[["confidence"]]
    n <- options[["n"]]
    k <- options[["k"]]

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

    if(options[["n"]] == 0){
      bound         <- "."
    } else {

        binomResult <- binom.test(x = options[["k"]],
                                  n = n,
                                  p = options[["materiality"]],
                                  alternative = "less",
                                  conf.level = 1 - alpha)
        bound       <- binomResult$conf.int[2]

    }

    resultList <- list()
    resultList[["n"]]               <- n
    resultList[["k"]]               <- k
    resultList[["IR"]]              <- options[["IR"]]
    resultList[["CR"]]              <- options[["CR"]]
    resultList[["confidence"]]      <- confidence
    resultList[["bound"]]           <- bound
    resultList[["alpha"]]           <- alpha

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "n", "k", "N", "expected.errors",
                                              "kPercentageNumber", "kNumberNumber"))

}

.summaryAttributesBoundTable <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Classical Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "n", "k", "show", "N", "K"))

  summaryTable$addColumnInfo(name = 'IR', title = "Inherent risk", type = 'string')
  summaryTable$addColumnInfo(name = 'CR', title = "Control risk", type = 'string')
  summaryTable$addColumnInfo(name = 'SR', title = "Sampling risk", type = 'string')
  summaryTable$addColumnInfo(name = 'n', title = "Sample size", type = 'string')
  summaryTable$addColumnInfo(name = 'k', title = "Errors", type = 'string')
  summaryTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')

  summaryTable$position <- position

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
    if(result[["bound"]] == "."){
        boundTable <- "."
    } else {
      boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(result[["bound"]] == "."){
      boundTable <- "."
    } else {
      boundTable <- round(result[["bound"]],3)
    }
  }

  row <- list(IR = result[["IR"]],
              CR = result[["CR"]],
              SR = SRtable,
              n = result[["n"]],
              k = result[["k"]],
              bound = boundTable)

  summaryTable$addRows(row)

}

.plotConfidenceBoundsSummary <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  if(options[["n"]] == 0)
    return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))

  plotStat <- data.frame(materiality = options[["materiality"]],
                          bound = result[["bound"]],
                          stratum = "Population")


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
      ggplot2::xlab(NULL) +
      ggplot2::scale_x_discrete(labels = plotStat[["stratum"]]) +
      base_breaks_y(plotStat, options)

  if(options[["show"]] == "percentage"){
    p <- p + ggplot2::ylab("Error percentage")
  } else if(options[["show"]] == "proportion"){
    p <- p + ggplot2::ylab("Error proportion")
  }

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE)

  return(createJaspPlot(plot = p, title = "Confidence Bound Plot", width = plotWidth, height = plotHeight))

}
