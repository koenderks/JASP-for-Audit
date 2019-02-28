.summaryBayesianAttributesBound <- function(options, jaspResults){

    confidence              <- options[["confidence"]]
    n                       <- options[["n"]]
    materiality             <- options[["materiality"]]
    k                       <- options[["k"]]

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

    if(options[["n"]] == 0 || options[["k"]] > options[["n"]]){
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
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "n", "k", "statistic", "materiality",
                                                    "expected.errors", "kPercentageNumber", "kNumberNumber", "prior"))

}

.summaryBayesianAttributesBoundTable <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "n", "k", "statistic", "materiality", "show",
                                  "expected.errors", "kPercentageNumber", "kNumberNumber", "prior",
                                  "mostLikelyError", "bayesFactor"))
  summaryTable$position <- position

  summaryTable$addColumnInfo(name = 'IR',   title = "Inherent risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'CR',   title = "Control risk",   type = 'string')
  summaryTable$addColumnInfo(name = 'SR',   title = "Sampling risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'n',    title = "Sample size",    type = 'string')
  summaryTable$addColumnInfo(name = 'k',    title = "Errors",         type = 'string')
  if(options[["statistic"]] == "bound"){
    summaryTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
  } else {
    summaryTable$addColumnInfo(name = 'ciLow', title = "Lower", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
    summaryTable$addColumnInfo(name = 'ciHigh', title = "Upper", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
  }
  if(options[["mostLikelyError"]])
    summaryTable$addColumnInfo(name = 'mle',  title = "Most Likely Error", type = 'string')
  if(options[["bayesFactor"]])
    summaryTable$addColumnInfo(name = 'bf',     title = "Bayes factor",         type = 'string')

  mle <- floor(qbeta(p = 0.5, result[["posteriorA"]], result[["posteriorB"]]) * options[["N"]])

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
    if(result[["bound"]] == "."){
      if(options[["statistic"]] == "bound"){
        boundTable <- "."
      } else if(options[["statistic"]] == "interval"){
        boundTable <- c(".", ".")
      }
    } else {
      boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(result[["bound"]] == "."){
      if(options[["statistic"]] == "bound"){
        boundTable <- "."
      } else if(options[["statistic"]] == "interval"){
        boundTable <- c(".", ".")
      }
    } else {
      boundTable <- round(result[["bound"]],3)
    }
  }

  if(options[["statistic"]] == "bound"){
    if(options[["mostLikelyError"]]){
      if(options[["bayesFactor"]]){
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable, mle = mle, bf = .BF(options, result))
      } else {
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable, mle = mle)
      }
    } else {
        if(options[["bayesFactor"]]){
          row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable, bf = .BF(options, result))
        } else {
          row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable)
        }
    }
    summaryTable$addRows(row)
  } else {
    if(options[["mostLikelyError"]]){
      if(options[["bayesFactor"]]){
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], ciLow = boundTable[1], ciHigh = boundTable[2], mle = mle, bf = .BF(options, result))
      } else {
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], ciLow = boundTable[1], ciHigh = boundTable[2], mle = mle)
      }
    } else {
        if(options[["bayesFactor"]]){
          row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], ciLow = boundTable[1], ciHigh = boundTable[2], bf = .BF(options, result))
        } else {
          row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], ciLow = boundTable[1], ciHigh = boundTable[2])
        }
    }
    summaryTable$addRows(row)
  }
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
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "n", "k", "show", "N", "K", "mostLikelyError"))

  summaryTable$addColumnInfo(name = 'IR', title = "Inherent risk", type = 'string')
  summaryTable$addColumnInfo(name = 'CR', title = "Control risk", type = 'string')
  summaryTable$addColumnInfo(name = 'SR', title = "Sampling risk", type = 'string')
  summaryTable$addColumnInfo(name = 'n', title = "Sample size", type = 'string')
  summaryTable$addColumnInfo(name = 'k', title = "Errors", type = 'string')
  summaryTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
  if(options[["mostLikelyError"]])
    summaryTable$addColumnInfo(name = 'mle',  title = "Most Likely Error", type = 'string')

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

  if(options[["N"]] == 0){
      mle <- 0
  } else {
      mle <- floor(options[["k"]] / options[["n"]] * options[["N"]])
  }

  if(options[["mostLikelyError"]]){
    row <- list(IR = result[["IR"]],
                CR = result[["CR"]],
                SR = SRtable,
                n = result[["n"]],
                k = result[["k"]],
                bound = boundTable, mle = mle)
  } else {
    row <- list(IR = result[["IR"]],
                CR = result[["CR"]],
                SR = SRtable,
                n = result[["n"]],
                k = result[["k"]],
                bound = boundTable)
  }

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
