.calc.n.binomial <- function(options, alpha){
    for(n in 1:5000){
      if(options[["expected.errors"]] == "kPercentage"){
        impk <- ceiling(n * options[["kPercentageNumber"]])
      } else if(options[["expected.errors"]] == "kNumber"){
        impk <- options[["kNumberNumber"]]
      }
      if(impk >= n){ next }
      if(impk%%1 == 0){
          x <- choose(n, 0:impk) * options[["materiality"]]^(0:impk) * (1-options[["materiality"]])^(n - (0:impk))
          if(sum(x) < alpha)
              return(n)
      }
    }
}

.calc.n.hypergeometric <- function(options, alpha){
    for(n in 1:5000){
        if(options[["expected.errors"]] == "kPercentage"){
            k <- ceiling(n * options[["kPercentageNumber"]])
        } else if(options[["expected.errors"]] == "kNumber"){
            k <- options[["kNumberNumber"]]
        }
        K <- floor(options[["N"]] * options[["materiality"]])
        if(n <= k) { next }
        x <- choose(K, 0:k) * choose(options[["N"]]-K, n-(0:k)) / choose(options[["N"]], n)
        if(sum(x) < alpha)
            return(n)
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

      list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1),
           ggplot2::scale_y_continuous(breaks = yBreaks, labels = yLabels))
  }

  pd <- ggplot2::position_dodge(0.2)

  p <- ggplot2::ggplot(plotStat, ggplot2::aes(x = xlab, y = bound)) +
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

  return(createJaspPlot(plot = p, title = "Confidence Bound Plot", width = 600, height = 450))

}

.attributesPlanningFullAudit <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence <- options[["confidence"]]
    p <- options[["materiality"]]

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
      n <- .calc.n.binomial(options, alpha)
    } else if(options[["distribution"]] == "hypergeometric"){
      if(options[["N"]] != 0){
        n <- .calc.n.hypergeometric(options, alpha)
      } else {
        n <- 1
      }
    }

    if(options[["expected.errors"]] == "kPercentage"){
      k <- options[["kPercentageNumber"]]
    } else if(options[["expected.errors"]] == "kNumber"){
      k <- options[["kNumberNumber"]] / n
    }

    resultList <- list()
    resultList[["n"]]             <- n
    resultList[["k"]]             <- k
    resultList[["IR"]]            <- options[["IR"]]
    resultList[["CR"]]            <- options[["CR"]]
    resultList[["alpha"]]         <- alpha
    resultList[["confidence"]]    <- confidence

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "distribution",
                                                "N", "kPercentageNumber", "kNumberNumber", "inference"))

}

.attributesPlanningTableFullAudit <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                              <- createJaspTable("Classical Attributes Planning Table")
  jaspResults[["summaryTable"]]             <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "show", "distribution", "N",
                                  "expected.errors" , "kPercentageNumber", "kNumberNumber", "inference"))

  summaryTable$addColumnInfo(name = 'IR',   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'SR',   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',    title = "Allowed errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',    title = "Required sample size", type = 'string')

  summaryTable$position                     <- position

  if(options[["N"]] == 0 && options[["distribution"]] == "hypergeometric"){
    message <- "The population size is specified to be 0. Please enter your population size."
    summaryTable$errorMessage <- message
    summaryTable$error <- "badData"
    return()
  }

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- ceiling(result[["k"]] * result[["n"]])
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- ceiling(result[["k"]] * result[["n"]])
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  }

  row <- list(IR = result[["IR"]],
              CR = result[["CR"]],
              SR = SRtable,
              k = ktable,
              n = result[["n"]])

  summaryTable$addRows(row)

  if(options[["distribution"]] == "binomial"){
          message <- "The sample size is calculated using the <b>binomial</b> distribution."
          summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  } else if(options[["distribution"]] == "hypergeometric"){
          message <- paste0("The sample size is calculated using the <b>hypergeometric</b> distribution (N = ", options[["N"]] ,").")
          summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  }

}

.attributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Classical Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID", "sampleFilter", "inference"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'IR',     title = "Inherent risk",  type = 'string')
    evaluationTable$addColumnInfo(name = 'CR',     title = "Control risk",   type = 'string')
    evaluationTable$addColumnInfo(name = 'SR',     title = "Detection risk",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',      title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',      title = "Errors",         type = 'string')

    evaluationTable$addColumnInfo(name = 'bound',  title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')

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
    evaluationTable$addRows(row)

}
