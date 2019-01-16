.calculateBayesianSampleSize <- function(options, alpha){
    for(n in 1:5000){
        if(options[["expected.errors"]] == "kPercentage"){
            impk <- n * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            impk <- options[["kNumberNumber"]]
        }
        if(impk >= n){ next }
        x                     <- qbeta(p = 1 - alpha, shape1 = 1 + impk, shape2 = 1 + (n - impk))
        if(x < options[["materiality"]]){
            return(n)
        }
    }
}

.priorSampleTable <- function(options, result, jaspResults, position = 3){

  if(!is.null(jaspResults[["sampletable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  sampletable                       <- createJaspTable("Prior Sample Table")
  jaspResults[["sampletable"]]      <- sampletable
  sampletable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "implicitsample", "statistic",
                                "show", "kPercentageNumber", "kNumberNumber"))

  sampletable$addColumnInfo(name = 'implicitn', title = "Prior sample size", type = 'string')
  sampletable$addColumnInfo(name = 'implicitk', title = "Prior errors", type = 'string')
  sampletable$position <- position

  if(options[["statistic"]] == "bound"){
    priorbound <- round(qbeta(p = options[["confidence"]], shape1 = result[["priorA"]], shape2 = result[["priorB"]]), 3)
    sampletable$addColumnInfo(name = 'priorbound', title = paste0(result[["confidence"]]*100,"% Prior confidence bound"), type = 'string')
    if(options[["show"]] == "percentage"){
      row <- list(implicitn = result[["implicitn"]], implicitk = result[["implicitk"]], priorbound = paste0(priorbound * 100, "%"))
    } else if(options[["show"]] == "proportion"){
      row <- list(implicitn = result[["implicitn"]], implicitk = result[["implicitk"]], priorbound = priorbound)
    }
    sampletable$addRows(row)
  } else if(options[["statistic"]] == "interval"){
    priorbound <- round(qbeta(p = c(  (1 - (1-(1-options[["confidence"]])/2)) , (1 - ((1-options[["confidence"]])/2)) ), shape1 = result[["priorA"]], shape2 = result[["priorB"]]), 3)
    sampletable$addColumnInfo(name = 'ciLow', title = "Lower", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Prior confidence interval"))
    sampletable$addColumnInfo(name = 'ciHigh', title = "Upper", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Prior confidence interval"))
    if(options[["show"]] == "percentage"){
      row <- list(implicitn = result[["implicitn"]], implicitk = result[["implicitk"]], ciLow = paste0(priorbound[1] * 100, "%"), ciHigh = paste0(priorbound[2] * 100, "%"))
    } else if(options[["show"]] == "proportion"){
      row <- list(implicitn = result[["implicitn"]], implicitk = result[["implicitk"]], ciLow = priorbound[1], ciHigh = priorbound[2])
    }
    sampletable$addRows(row)
  }

  message <- paste0("Sample sizes shown are implicit sample sizes derived from the ARM risk assessments: IR = <b>", options[["IR"]], "</b> and CR = <b>", options[["CR"]], "</b>.")
  sampletable$addFootnote(message = message, symbol="<i>Note.</i>")

}

.plotPriorBayesianAttributesPlanning <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

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

  return(createJaspPlot(plot = p, title = "Prior Plot", width = plotWidth, height = plotHeight))

}

.plotPriorAndPosteriorBayesianAttributesBound <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  xseq <- seq(0, options[["limx"]], 0.001)
  d <- data.frame(
      x = rep(xseq, 2),
      y = c(dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]]), dbeta(x = xseq, shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]])),
      type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
  )
  # Reorder factor levels to display in legend
  d$type = factor(d$type,levels(d$type)[c(2,1)])

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      ggplot2::scale_linetype_manual(values=c("dashed", "solid"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))

  if(options[["show"]] == "percentage"){
    p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))
  } else if(options[["show"]] == "proportion"){
    p <- p + ggplot2::scale_x_continuous(name = "Error proportion", breaks = xBreaks, limits = xLim)
  }

  if(options[["plotPriorAndPosteriorAdditionalInfo"]]){
    pdata <- data.frame(x = 0, y = 0, l = "1")
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
    p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \nconfidence region"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 20, shape = 21, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)

    if(options[["statistic"]] == "bound"){
      p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]]), xlim = c(0, result[["bound"]]),
                                      geom = "area", fill = rgb(0, 0.25, 1, .5))
    } else if(options[["statistic"]] == "interval") {
      p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]]), xlim = c(result[["bound"]][1], result[["bound"]][2]),
                                      geom = "area", fill = rgb(0, 0.25, 1, .5))
    }
  }

  thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	       ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  return(createJaspPlot(plot = p, title = "Prior and Posterior Plot", width = plotWidth, height = plotHeight))

}
