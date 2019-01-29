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

.dBetaBinom <- function (x, N, u, v, log = FALSE)
{
    logval <- lbeta(x + u, N - x + v) - lbeta(u, v) + lchoose(N, x)
    if (log) {
        ret <- logval
    }
    else {
        ret <- exp(logval)
    }
    return(ret)
}

.qBetaBinom <- function (p, N, u, v)
{
    pp <- cumsum(.dBetaBinom(0:N, N, u, v))
    return(sapply(p, function(x) sum(pp < x)))
}

.calculateBayesianSampleSizeBetaBinom <- function(options, alpha, N){
    for(n in 1:5000){
        if(options[["expected.errors"]] == "kPercentage"){
            impk <- n * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            impk <- options[["kNumberNumber"]]
        }
        if(impk >= n){ next }
        x                     <- .qBetaBinom(p = 1 - alpha, N = N, u = 1 + impk, v = 1 + (n - impk))
        if((x / N) < options[["materiality"]]){
            return(n)
        }
    }
}

.bayesianAttributesPlanningFullAudit <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence              <- options[["confidence"]]
    p                       <- options[["materiality"]]

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

    if(options[["distribution"]] == "binomial"){
      n_noprior               <- .calculateBayesianSampleSize(options, 1 - confidence)
      n_withprior             <- .calculateBayesianSampleSize(options, alpha)
    } else if(options[["distribution"]] == "hypergeometric"){
      n_noprior               <- .calculateBayesianSampleSizeBetaBinom(options, 1 - confidence, options[["N"]])
      n_withprior             <- .calculateBayesianSampleSizeBetaBinom(options, alpha, options[["N"]])
    }
    pn                      <- n_noprior - n_withprior
    if(pn == 0){
        pk                  <- 0
        if(options[["expected.errors"]] == "kPercentage"){
            k               <- options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            k               <- options[["kNumberNumber"]]
        }
    } else {
        if(options[["expected.errors"]] == "kPercentage"){
            k               <- options[["kPercentageNumber"]]
            pk              <- pn * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            k               <- options[["kNumberNumber"]]
            pk              <- k
        }
    }
    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    if(options[["prior"]] == "5050"){

      priorA                <- 1
      priorB                <- 1/((3/2) * options[["materiality"]]) - (1/3)

    }

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
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "kPercentageNumber",
                                              "kNumberNumber", "inference", "prior", "distribution", "N"))

}

.bayesianAttributesPlanningTableFullAudit <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Attributes Planning Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "show", "N",
                                  "kPercentageNumber", "kNumberNumber", "expectedBF", "prior", "distribution"))

  summaryTable$addColumnInfo(name = 'IR', title = "Inherent risk", type = 'string')
  summaryTable$addColumnInfo(name = 'CR', title = "Control risk", type = 'string')
  summaryTable$addColumnInfo(name = 'SR', title = "Detection risk", type = 'string')
  summaryTable$addColumnInfo(name = 'k', title = "Allowed errors", type = 'string')
  summaryTable$addColumnInfo(name = 'n', title = "Required sample size", type = 'string')
  if(options[["expectedBF"]])
    summaryTable$addColumnInfo(name = 'expBF', title = "Expected Bayes factor", type = 'string')

  summaryTable$position <- position

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- floor(result[["k"]] * result[["n"]])
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- floor(result[["k"]] * result[["n"]])
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  }

  if(options[["expectedBF"]]){
    row <- list(IR = result[["IR"]],
                CR = result[["CR"]],
                SR = SRtable,
                k = ktable,
                n = result[["n"]],
                expBF = .expectedBF(options, result, ktable))
  } else {
    row <- list(IR = result[["IR"]],
                CR = result[["CR"]],
                SR = SRtable,
                k = ktable,
                n = result[["n"]])
  }

  summaryTable$addRows(row)

  if(options[["distribution"]] == "binomial"){
          message <- "The sample size is calculated using the <b>beta</b> distribution."
          summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  } else if(options[["distribution"]] == "hypergeometric"){
          message <- paste0("The sample size is calculated using the <b>beta-binomial</b> distribution (N = ", options[["N"]] ,").")
          summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  }

}

.priorSampleTable <- function(options, result, jaspResults, position = 3){

  if(!is.null(jaspResults[["sampletable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  sampletable                       <- createJaspTable("Prior Sample Table")
  jaspResults[["sampletable"]]      <- sampletable
  sampletable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "implicitsample", "statistic",
                                "show", "kPercentageNumber", "kNumberNumber", "distribution", "prior", "N"))

  sampletable$addColumnInfo(name = 'implicitn', title = "Prior sample size", type = 'string')
  sampletable$addColumnInfo(name = 'implicitk', title = "Prior errors", type = 'string')
  sampletable$position <- position

  result[["implicitn"]] <- round(result[["implicitn"]], 2)
  result[["implicitk"]] <- round(result[["implicitk"]], 2)

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

.plotPriorBayesianAttributesPlanningFullAudit <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  mle <- floor(result[["k"]] * result[["n"]])

  xseq <- seq(0, options[["limx"]], 0.001)
  d <- data.frame(
      x = rep(xseq, 2),
      y = c(dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]]), dbeta(x = xseq, shape1 = result[["priorA"]] + mle, shape2 = result[["priorB"]] + (result[["n"]] - mle))),
      type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
  )

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  pointdata <- data.frame(x = options[["materiality"]], y = dbeta(options[["materiality"]], result[["priorA"]], result[["priorB"]]))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      #ggplot2::scale_linetype_manual(values=c("dotted", "dashed"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))
      ggplot2::scale_linetype_manual(values=c("dotted", "dashed"), guide = FALSE)

  if(options[["show"]] == "percentage"){
      p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))
  } else if(options[["show"]] == "proportion"){
      p <- p + ggplot2::scale_x_continuous(name = "Error proportion", breaks = xBreaks, limits = xLim)
  }

  if(options[["plotPriorAdditionalInfo"]]){
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

  p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

  thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p +
  	ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  return(createJaspPlot(plot = p, title = "Implied Prior and Posterior", width = plotWidth, height = plotHeight))

}

.bayesianAttributesBoundFullAudit <- function(dataset, options, jaspResults){

    confidence              <- options[["confidence"]]
    correctID               <- options[["correctID"]]
    materiality             <- options[["materiality"]]

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

    if(is.null(dataset) || options[["correctID"]] == ""){
      n                     <- 0
      k                     <- 0
    } else {
      n                     <- nrow(dataset)
      k                     <- length(which(dataset[,.v(correctID)] == 1))
    }

    n_noprior               <- .calculateBayesianSampleSize(options, 1 - confidence)
    n_withprior             <- .calculateBayesianSampleSize(options, alpha)

    pn                      <- n_noprior - n_withprior

    if(pn == 0){
        pk                  <- 0
        if(options[["expected.errors"]] == "kPercentage"){
            exp.k                   <- options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            exp.k                   <- options[["kNumberNumber"]]
        }
    } else {
        if(options[["expected.errors"]] == "kPercentage"){
            exp.k               <- options[["kPercentageNumber"]]
            pk                  <- pn * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            exp.k               <- options[["kNumberNumber"]]
            pk                  <- exp.k
        }
    }

    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    if(options[["prior"]] == "5050"){

      priorA <- 1
      priorB <- 1/((3/2) * options[["materiality"]]) - (1/3)

    }

    if(n == 0 || k > n){
      bound                 <- "."
      approve               <- "."
    } else {
        if(options[["statistic"]] == "bound"){
          bound             <- qbeta(p = confidence,
                                    shape1 = priorA + k,
                                    shape2 = priorB + (n - k),
                                    lower.tail = TRUE)
         if(bound < materiality){
           approve          <- "Yes"
         } else {
           approve          <- "No"
         }
       } else if(options[["statistic"]] =="interval"){
          bound             <- qbeta(p = c(  (1 - (1-(1-confidence)/2)) , (1 - ((1-confidence)/2)) ),
                                    shape1 = priorA + k,
                                    shape2 = priorB + (n - k),
                                    lower.tail = TRUE)
         if(bound[2] < materiality){
           approve          <- "Yes"
         } else {
           approve          <- "No"
         }
      }
  }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- k
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- confidence
    resultList[["bound"]]       <- bound
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["posteriorA"]]  <- priorA + k
    resultList[["posteriorB"]]  <- priorB + (n - k)
    resultList[["approve"]]     <- approve

    jaspResults[["result"]]     <- createJaspState(resultList)
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality",
                                                    "correctID", "expected.errors", "kPercentageNumber",
                                                    "kNumberNumber", "sampleFilter", "prior", "k", "n",
                                                    "distribution", "N"))

}

.bayesianAttributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Bayesian Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "expected.errors", "kPercentageNumber", "kNumberNumber", "sampleFilter",
                                      "mostLikelyError", "bayesFactor", "N", "n", "k", "prior", "distribution", "prior"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',    title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',    title = "Full errors",         type = 'string')
    evaluationTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',    title = "Most Likely Error",         type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',     title = "Bayes factor",         type = 'string')

    mle <- ceiling( (result[["posteriorA"]] - 1) / (result[["posteriorA"]] + result[["posteriorB"]] - 2) * options[["N"]] )

    if(options[["correctID"]] != ""){
      if(options[["mostLikelyError"]]){
        row <- list(materiality = ".", n = ".", k = ".", bound = ".", mle = ".")
      } else {
        row <- list(materiality = ".", n = ".", k = ".", bound = ".")
      }
    }

    if(options[["show"]] == "percentage"){
        materialityTable <- paste0(round(options[["materiality"]],2) * 100, "%")
        if(result[["bound"]] == "."){
            if(options[["statistic"]] == "bound"){
                boundTable          <- "."
            } else if(options[["statistic"]] == "interval"){
                boundTable          <- c(".", ".")
            }
        } else {
            boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
        }
    } else if(options[["show"]] == "proportion"){
        materialityTable <- round(options[["materiality"]], 2)
        if(result[["bound"]] == "."){
            if(options[["statistic"]] == "bound"){
                boundTable          <- "."
            } else if(options[["statistic"]] == "interval"){
                boundTable          <- c(".", ".")
            }
        } else {
            boundTable <- round(result[["bound"]],3)
        }
    }
    if(options[["bayesFactor"]]){
      if(options[["mostLikelyError"]]){
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable, mle = mle, bf = .BF(options, result))
      } else {
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable, bf = .BF(options, result))
      }
    } else {
      if(options[["mostLikelyError"]]){
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable, mle = mle)
      } else {
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable)
      }
    }

    evaluationTable$addRows(row)

}

.plotPriorAndPosteriorBayesianAttributesBoundFullAudit <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  xseq <- seq(0, options[["limx_backup"]], 0.001)
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

  pointdata <- data.frame(x = options[["materiality"]], y = dbeta(options[["materiality"]], result[["posteriorA"]], result[["posteriorB"]]))

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

  p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

  thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	       ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  return(createJaspPlot(plot = p, title = "Prior and Posterior", width = plotWidth, height = plotHeight))

}

.expectedBF <- function(options, result, ktable){
    priorOdds     <- diff(pbeta(c(0, options[["materiality"]]), result[["priorA"]], result[["priorB"]])) / diff(pbeta(c(options[["materiality"]], 1), result[["priorA"]], result[["priorB"]]))
    posteriorOdds <- diff(pbeta(c(0, options[["materiality"]]), result[["priorA"]] + ktable, result[["priorB"]] + (result[["n"]] + ktable))) / diff(pbeta(c(options[["materiality"]], 1), result[["priorA"]] + ktable, result[["priorB"]] + (result[["n"]] + ktable)))
    BF            <- round(posteriorOdds / priorOdds, 2)
    return(BF)
}

.BF <- function(options, result){
  priorOdds     <- diff(pbeta(c(0, options[["materiality"]]), result[["priorA"]], result[["priorB"]])) / diff(pbeta(c(options[["materiality"]], 1), result[["priorA"]], result[["priorB"]]))
  posteriorOdds <- diff(pbeta(c(0, options[["materiality"]]), result[["posteriorA"]], result[["posteriorB"]])) / diff(pbeta(c(options[["materiality"]], 1), result[["posteriorA"]], result[["posteriorB"]]))
  BF            <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

.coxAndSnellBound <- function(dataset, options, jaspResults, priorPi = 0.10, priorMu = 0.40, priorA = 1, priorB = 6){

    # Based on the paper:
    # Cox, D. R., & Snell, E. J. (1979). On sampling and the estimation of rare errors. Biometrika, 66(1), 125-132.
    # Default options are recommendations from the paper

    n                       <- nrow(dataset)
    alpha                   <- 1 - options[["confidence"]]
    sample                  <- dataset[, c(.v(options[["monetaryVariableMUS"]]), .v(options[["correctMUS"]]))]

    t                       <- sample[, 1] - sample[, 2]
    z                       <- t / sample[, 1]
    z                       <- subset(z, z > 0)
    M                       <- length(z)

    z_bar                   <- mean(z)
    if(M == 0)
        z_bar               <- 0

    prior_part_1            <- (0 + priorA) / (0 + priorB)
    prior_part_2            <- ((priorMu * (priorB - 1)) + (0 * 0)) / (0 + (priorA / priorPi))
    prior                   <- prior_part_1 * prior_part_2 * stats::rf(n = 10000, df1 = (2 * (0 + priorA)), df2 = ( 2 *(0 + priorB)))

    posterior_part_1        <- (M + priorA) / (M + priorB)
    posterior_part_2        <- ((priorMu * (priorB - 1)) + (M * z_bar)) / (n + (priorA / priorPi))
    posterior               <- posterior_part_1 * posterior_part_2 * stats::rf(n = 10000, df1 = (2 * (M + priorA)), df2 = ( 2 *(M + priorB)))

    bound                   <- as.numeric(quantile(posterior, probs = (1 - alpha), na.rm = TRUE))

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["priorPi"]]     <- priorPi
    resultList[["priorMu"]]     <- priorMu
    resultList[["posteriorA"]]  <- priorA + sum(z)
    resultList[["posteriorB"]]  <- priorB + (n - (sum(z)))
    resultList[["prior"]]       <- prior
    resultList[["posterior"]]   <- posterior

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctMUS", "sampleFilterMUS", "auditType", "boundMethodMUS"))

}

.bayesianMusBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Bayesian Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "sampleFilter", "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS",
                                      "boundMethodMUS"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Sum of fractional errors",         type = 'string')

    evaluationTable$addColumnInfo(name = 'bound',  title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',  title = "Most Likely Error", type = 'string')

    if(options[["correctMUS"]] == "" || options[["sampleFilterMUS"]] == ""){
      if(options[["mostLikelyError"]]){
        row <- list(materiality = ".", n = ".", k = ".", bound = ".", mle = ".")
      } else {
        row <- list(materiality = ".", n = ".", k = ".", bound = ".")
      }
      evaluationTable$addRows(row)
      return()
    }

    if(options[["N"]] == 0){
        mle <- 0
    } else {
        mle <- floor( sum(result[["z"]]) / result[["n"]] * options[["N"]] )
    }

      if(options[["show"]] == "percentage"){
          materialityTable <- paste0(round(options[["materiality"]],2) * 100, "%")
          if(result[["bound"]] == "."){
              boundTable          <- "."
          } else {
              boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
          }
      } else if(options[["show"]] == "proportion"){
          materialityTable <- round(options[["materiality"]], 2)
          if(result[["bound"]] == "."){
              boundTable          <- "."
          } else {
              boundTable <- round(result[["bound"]],3)
          }
      }

    errors <- round(sum(result[["z"]]), 2)

    if(options[["mostLikelyError"]]){
      row <- list(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, bound = boundTable, mle = mle)
    } else {
      row <- list(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, bound = boundTable)
    }
    evaluationTable$addRows(row)

    if(options[["boundMethodMUS"]] == "coxAndSnellBound"){
      message <- "The confidence bound is calculated according to the <b>Cox and Snell</b> method."
      evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")
    }
}

.plotPriorAndPosteriorBayesianMUSBoundFullAudit <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  prior <- density(result[["prior"]], from = 0, to = options[["limx_backup"]])
  posterior <- density(result[["posterior"]], from = 0, to = options[["limx_backup"]])

  d <- data.frame(
      x = c(prior$x, posterior$x),
      y = c(prior$y, posterior$y),
      type = c(rep("Prior", length(prior$x)), rep("Posterior", length(posterior$x)))
  )
  # Reorder factor levels to display in legend
  d$type = factor(d$type,levels(d$type)[c(2,1)])

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(posterior$x, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  posteriorPointData <- subset(d, d$type == "Posterior")
  posteriorPointDataY <- posteriorPointData$y[which.min(abs(posteriorPointData$x - options[["materiality"]]))]

  pointdata <- data.frame(x = options[["materiality"]], y = posteriorPointDataY)

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
      p <- p + ggplot2::geom_area(mapping = ggplot2::aes(x = x, y = y), data = subset(subset(d, d$type == "Posterior"), subset(d, d$type == "Posterior")$x <= result[["bound"]]), fill = rgb(0, 0.25, 1, .5))
    } else if(options[["statistic"]] == "interval") {
      # p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]]), xlim = c(result[["bound"]][1], result[["bound"]][2]),
      #                                 geom = "area", fill = rgb(0, 0.25, 1, .5))
    }
  }

  p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

  thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	       ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  return(createJaspPlot(plot = p, title = "Prior and Posterior", width = plotWidth, height = plotHeight))

}
