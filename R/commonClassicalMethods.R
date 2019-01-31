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

.plotConfidenceBounds <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  plotStat <- data.frame(materiality = options[["materiality"]],
                          bound = result[["bound"]],
                          xlab = "")

  materialityStat <- data.frame(materiality = options[["materiality"]])

  base_breaks_y <- function(x, options) {

      values <- c(options$materiality, 0, x[, "bound"])
      ci.pos <- c(min(values), max(values))
      b <- pretty(ci.pos)
      d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
      yBreaks <- c(min(b),  options$materiality, max(b))

      if(options[["show"]] == "percentage"){
          yLabels <- paste0(yBreaks * 100, "%")
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
      base_breaks_y(plotStat, options) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank())

  if(options[["show"]] == "percentage"){
    p <- p + ggplot2::ylab("Error percentage")
  } else if(options[["show"]] == "proportion"){
    p <- p + ggplot2::ylab("Error proportion")
  }

  p <- p + ggplot2::xlab(NULL)

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE)

  return(createJaspPlot(plot = p, title = "Confidence Bound Plot", width = plotWidth, height = plotHeight))

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
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "sampleFilter", "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',      title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',      title = "Full errors",         type = 'string')

    evaluationTable$addColumnInfo(name = 'bound',  title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',  title = "Most Likely Error", type = 'string')

    if(options[["correctID"]] != ""){
      if(options[["mostLikelyError"]]){
        row <- list(materiality = ".", n = ".", k = ".", bound = ".", mle = ".")
      } else {
        row <- list(materiality = ".", n = ".", k = ".", bound = ".")
      }
    }

    if(options[["N"]] == 0){
        mle <- 0
    } else {
        mle <- floor(result[["k"]] / result[["n"]] * options[["N"]])
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

    if(options[["mostLikelyError"]]){
      row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable, mle = mle)
    } else {
      row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable)
    }
    evaluationTable$addRows(row)

}

.attributesBoundFullAudit <- function(dataset, options, jaspResults){

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
    n                       <- nrow(dataset)
    if(options[["correctID"]] != ""){
      k                     <- length(which(dataset[,.v(correctID)] == 1))
    } else {
      k                     <- "."
    }
  }


  if(n == 0){
    bound                 <- "."
    approve               <- "."
  } else {
    binomResult <- binom.test(x = k,
                              n = n,
                              p = options[["materiality"]],
                              alternative = "less",
                              conf.level = options[["confidence"]])
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

  jaspResults[["result"]] <- createJaspState(resultList)
  jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "sampleFilter", "inference"))

}

.stringerBound <- function(dataset, options, jaspResults){

    # Based on the paper:
    # Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. In Proceedings of the Business and Economic Statistics Section (pp. 405-411). American Statistical Association.

    n                       <- nrow(dataset)
    alpha                   <- 1 - options[["confidence"]]

    if(options[["correctMUS"]] == "" || options[["sampleFilterMUS"]] == "" || options[["monetaryVariableMUS"]] == ""){
      bound <- "."
      M <- 0
      z <- 0
    } else {
      sample                  <- dataset[, c(.v(options[["monetaryVariableMUS"]]), .v(options[["correctMUS"]]))]

      t                       <- sample[, 1] - sample[, 2]
      z                       <- t / sample[, 1]
      z                       <- sort(subset(z, z > 0), decreasing = TRUE)
      M                       <- length(z)
      # Use the attributes method for zero errors p(0; 1-alpha) = 1 - alpha^(1/n)
      bound                   <- 1 - alpha^(1/n)
      # Calculate the proportional sum
      if(M > 0){
          prop.sum            <- 0
          for(i in 1:M){
              # p(i, 1 - alpha) - p(i-1, 1-alpha) * z_i
              prop.sum        <- prop.sum + ((qbeta(1 - alpha, i + 1, n - i) - qbeta(1 - alpha, (i-1) + 1, n - (i-1) ))  * z[i])
          }
          # Add the sum to p(0; 1-alpha)
          bound               <- bound + prop.sum
      }
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctMUS", "sampleFilterMUS",
                                              "auditType", "boundMethodMUS", "monetaryVariableMUS"))
}

.musBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Classical Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "sampleFilter", "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS",
                                      "boundMethodMUS","monetaryVariableMUS"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Sum of fractional errors",         type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',  title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',  title = "Most Likely Error", type = 'string')

    if(options[["boundMethodMUS"]] == "stringerBound"){
      message <- "The confidence bound is calculated according to the <b>Stringer</b> method."
      evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")
    }

    if(options[["correctMUS"]] == "" || options[["sampleFilterMUS"]] == "" || options[["monetaryVariableMUS"]] == ""){
      if(options[["mostLikelyError"]]){
        row <- list(materiality = ".", n = ".", fk = ".", k = ".", bound = ".", mle = ".")
      } else {
        row <- list(materiality = ".", n = ".", fk = ".", k = ".", bound = ".")
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
}
