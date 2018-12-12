bayesianAttributesPlanning <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							    <- list()

  jaspResults$title 					<- "Bayesian Audit Planning"
  .bayesianAttributesPlanning(options, jaspResults)
  result                      <- jaspResults[["result"]]$object
  .bayesianAttributesPlanningTable(options, result, jaspResults)
  if (options$implicitsample)
  {
    if(is.null(jaspResults[["sampletable"]]))
      .priorSampleTable(options, result, jaspResults)
  }

  if(options[['plotPriorAndPosterior']])
  {
     if(is.null(jaspResults[["priorPlot"]]))
     {
     jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanning(options, result, jaspResults)
     jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence",
                                                      "materiality", "expected.k", "limx", "plotPriorAndPosterior",
                                                      "plotPriorAndPosteriorAdditionalInfo", "show", "statistic"))
     jaspResults[["priorPlot"]] 		$position <- 3
     }
  }

  # Save the state
  state[["options"]] 					<- options
  return(state)

}

.bayesianAttributesPlanning <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence                <- options[["confidence"]]
    p                         <- options[["materiality"]]
    k                         <- options[["expected.k"]]

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

    n_noprior         <- .calculateBayesianSampleSize(k, p, 1 - confidence)
    n_withprior       <- .calculateBayesianSampleSize(k, p, alpha)

    pn                <- n_noprior - n_withprior
    pk                <- pn * k
    priorA           <- 1 + pk
    priorB           <- 1 + (pn - pk)

    resultList <- list()
    resultList[["n"]]           <- n_withprior
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["k"]]           <- k
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- confidence

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.k", "materiality"))

}

.bayesianAttributesPlanningTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Planning")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.k", "materiality", "show"))

  summaryTable$addColumnInfo(name = 'IR', title = "Inherent risk", type = 'string')
  summaryTable$addColumnInfo(name = 'CR', title = "Control risk", type = 'string')
  summaryTable$addColumnInfo(name = 'SR', title = "Sampling risk", type = 'string')
  summaryTable$addColumnInfo(name = 'k', title = "Expected errors", type = 'string')
  summaryTable$addColumnInfo(name = 'n', title = "Required sample size", type = 'string')

  summaryTable$position <- 1

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")
    ktable <- paste0(result[["k"]] * 100, "%")
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    ktable <- result[["k"]]
  }

  row <- list(IR = result[["IR"]],
              CR = result[["CR"]],
              SR = SRtable,
              k = ktable,
              n = result[["n"]])

  summaryTable$addRows(row)

}

.plotPriorBayesianAttributesPlanning <- function(options, result, jaspResults){

  xseq <- seq(0, options[["limx"]], 0.001)
  d <- data.frame(
      x = xseq,
      y = dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]])
  )

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y), lwd = 1, lty = 2)

  if(options[["show"]] == "percentage"){
    p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))
  } else if(options[["show"]] == "proportion"){
    p <- p + ggplot2::scale_x_continuous(name = "Error proportion", breaks = xBreaks, limits = xLim)
  }

  if(options[["plotPriorAndPosteriorAdditionalInfo"]]){
    pdata <- data.frame(x = 0, y = 0, l = "1")
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
    p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 20, shape = 21, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))
    if(options[["statistic"]] == "bound"){
      p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
                                      xlim = c(0, qbeta(options[["confidence"]], result[["priorA"]], result[["priorB"]])),
                                      geom = "area", fill = rgb(0, 1, 0.5, .7))
    } else if(options[["statistic"]] == "interval"){
      p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
                                      xlim = c(qbeta((1 - (1-(1-options[["confidence"]])/2)), result[["priorA"]], result[["priorB"]]), qbeta((1 - ((1-options[["confidence"]])/2)), result[["priorA"]], result[["priorB"]])),
                                      geom = "area", fill = rgb(0, 1, 0.5, .7))
    }
  }

  thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p +
  	ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  return(createJaspPlot(plot = p, title = "Prior Plot", width = 600, height = 450))

}
