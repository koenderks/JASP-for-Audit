.calc.n.binomial <- function(options, alpha){
    for(n in 1:5000){
      impk <- base::switch(options[["expected.errors"]],
                            "kPercentage" = ceiling(n * options[["kPercentageNumber"]]),
                            "kNumber" = options[["kNumberNumber"]])
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
      k <- base::switch(options[["expected.errors"]],
                            "kPercentage" = ceiling(n * options[["kPercentageNumber"]]),
                            "kNumber" = options[["kNumberNumber"]])
      K <- floor(options[["N"]] * options[["materiality"]])
      if(n <= k) { next }
      x <- choose(K, 0:k) * choose(options[["N"]]-K, n-(0:k)) / choose(options[["N"]], n)
      if(sum(x) < alpha)
          return(n)
  }
}

.plotConfidenceBounds <- function(options, result, jaspResults){

  materiality     <- options[["materiality"]]
  bound           <- result[["bound"]]
  expected.errors <- base::switch(options[["expected.errors"]],
                                      "kPercentage" = options[["kPercentageNumber"]],
                                      "kNumber" = options[["kNumberNumber"]] / result[["n"]])
  found.errors    <- result[["k"]] / result[["n"]]
  xlim            <- round(max(c(materiality, bound, expected.errors, found.errors)) * 1.2, 2)
  xBreaks         <- pretty(c(0, xlim))
  xLabels         <- paste0(round(xBreaks * 100, 2), "%")

  boundData <- data.frame(xmin = c(0, bound), xmax = c(bound, xlim), ymin = 0, ymax = 0.5,
                         fill = c(rgb(0,1,.5,.7), rgb(1,1,1)))

  df <- data.frame()
  p <- ggplot2::ggplot(df) +
      ggplot2::geom_point() +
      ggplot2::ylim(0, 1) +
      ggplot2::ylab(NULL) +
      ggplot2::xlab("Error percentage") +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                              data = boundData, fill = boundData$fill, color = "black", size = 2) +
      ggplot2::scale_x_continuous(breaks = xBreaks, labels = xLabels) +
      #ggplot2::geom_segment(ggplot2::aes(x = bound, xend = bound, y = 0, yend = 0.5), lty = 1, size = 2) +
      ggplot2::geom_segment(ggplot2::aes(x = materiality, xend = materiality, y = 0, yend = 0.5), lty = 1, size = 2, color = rgb(1,0,0,.7)) +
      ggplot2::geom_segment(ggplot2::aes(x = expected.errors, xend = expected.errors, y = 0, yend = 0.5), lty = 2, size = 2, color = "darkgray") +
      ggplot2::geom_segment(ggplot2::aes(x = found.errors, xend = found.errors, y = 0, yend = 0.5), lty = 2, size = 2)


  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_text(size = 17))

  return(createJaspPlot(plot = p, title = "Outcome information", width = 600, height = 200))

}

.attributesPlanningFullAudit <- function(options, jaspResults){

    if(!is.null(jaspResults[["planningResult"]])) return()

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    if(options[["distribution"]] == "binomial"){
      n                     <- .calc.n.binomial(options, alpha)
    } else if(options[["distribution"]] == "hypergeometric"){
      if(options[["N"]] != 0){
        n                   <- .calc.n.hypergeometric(options, alpha)
      } else {
        n                   <- 1
      }
    }

    k <- base::switch(options[["expected.errors"]],
                      "kPercentage" = options[["kPercentageNumber"]],
                      "kNumber" = options[["kNumberNumber"]] / n)

    resultList <- list()
    resultList[["n"]]             <- n
    resultList[["k"]]             <- k
    resultList[["IR"]]            <- options[["IR"]]
    resultList[["CR"]]            <- options[["CR"]]
    resultList[["alpha"]]         <- alpha
    resultList[["confidence"]]    <- options[["confidence"]]

    jaspResults[["planningResult"]] <- createJaspState(resultList)
    jaspResults[["planningResult"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "distribution",
                                                "N", "kPercentageNumber", "kNumberNumber", "inference"))

}

.attributesPlanningTableFullAudit <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                              <- createJaspTable("Classical Attributes Planning Table")
  jaspResults[["summaryTable"]]             <- summaryTable
  summaryTable$position                     <- position
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "show", "distribution", "N",
                                  "expected.errors" , "kPercentageNumber", "kNumberNumber", "inference"))

  summaryTable$addColumnInfo(name = 'IR',   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'SR',   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',    title = "Allowed errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',    title = "Required sample size", type = 'string')

  message <- base::switch(options[["distribution"]],
                          "binomial" =  "The sample size is calculated using the <b>binomial</b> distribution.",
                          "hypergeometric" = paste0("The sample size is calculated using the <b>hypergeometric</b> distribution (N = ", options[["N"]] ,")."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  if(options[["N"]] == 0 && options[["distribution"]] == "hypergeometric"){
    message <- "The population size is specified to be 0. Please enter your population size."
    summaryTable$errorMessage <- message
    summaryTable$error <- "badData"
    return()
  }

  ktable <- base::switch(options[["expected.errors"]],
                          "kPercentage" = ceiling(result[["k"]] * result[["n"]]),
                          "kNumber" = options[["kNumberNumber"]])
  SRtable <- base::switch(options[["show"]],
                            "percentage" = paste0(round(result[["alpha"]], 3) * 100, "%"),
                            "proportion" = round(result[["alpha"]], 3))

  row <- data.frame(IR = result[["IR"]],
                    CR = result[["CR"]],
                    SR = SRtable,
                    k = ktable,
                    n = result[["n"]])
  summaryTable$addRows(row)
}

.attributesBoundFullAudit <- function(dataset, options, jaspResults){

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr

  n                       <- 0
  k                       <- 0
  bound                   <- "."

  if(options[["correctID"]] != "" && options[["sampleFilter"]] != ""){
    n                     <- nrow(dataset)
    k                     <- length(which(dataset[,.v(options[["correctID"]])] == 1))
    binomResult <- binom.test(x = k,
                              n = n,
                              p = options[["materiality"]],
                              alternative = "less",
                              conf.level = (1 - alpha))
    bound                 <- binomResult$conf.int[2]
  }

  resultList <- list()
  resultList[["n"]]           <- n
  resultList[["k"]]           <- k
  resultList[["IR"]]          <- options[["IR"]]
  resultList[["CR"]]          <- options[["CR"]]
  resultList[["confidence"]]  <- options[["confidence"]]
  resultList[["bound"]]       <- bound
  resultList[["alpha"]]       <- alpha

  jaspResults[["result"]] <- createJaspState(resultList)
  jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "sampleFilter"))

}

.attributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Classical Attributes Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "sampleFilter", "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",          type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",          type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Full errors",          type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                  type = 'string')

    # Return empty table
    if(options[["correctID"]] == "" || options[["sampleFilter"]] == ""){
      row <- data.frame(materiality = ".", n = ".", k = ".", bound = ".")
      if(options[["mostLikelyError"]])
        row <- cbind(row, mle = ".")
      evaluationTable$addRows(row)
      return()
    }

    mle <- 0
    if(options[["N"]] != 0)
      mle <- floor(result[["k"]] / result[["n"]] * options[["N"]])

    materialityTable <- round(options[["materiality"]], 2)
    if(options[["show"]] == "percentage")
      materialityTable <- paste0(materialityTable * 100, "%")

    boundTable          <- "."
    if(result[["bound"]] != "."){
        boundTable <- round(result[["bound"]],3)
        if(options[["show"]] == "percentage")
          boundTable <- paste0(boundTable * 100, "%")
    }

    row <- data.frame(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable)
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)

    evaluationTable$addRows(row)
}

.stringerBound <- function(dataset, options, jaspResults){
    # Based on the paper:
    # Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. In Proceedings of the Business and Economic Statistics Section (pp. 405-411). American Statistical Association.
    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."

    if(options[["correctMUS"]] != "" && options[["sampleFilter"]] != "" && options[["monetaryVariable"]] != ""){
      sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["correctMUS"]]))]
      n                       <- nrow(sample)
      t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["correctMUS"]])]
      z                       <- t / sample[, .v(options[["monetaryVariable"]])]
      z                       <- sort(subset(z, z > 0), decreasing = TRUE)
      M                       <- length(z)
      bound                   <- 1 - alpha^(1/n)
      if(M > 0){
          prop.sum            <- 0
          for(i in 1:M){
              prop.sum        <- prop.sum + ((qbeta(1 - alpha, i + 1, n - i) - qbeta(1 - alpha, (i-1) + 1, n - (i-1) ))  * z[i])
          }
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
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show",
                                      "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS",
                                      "boundMethodMUS", "monetaryVariableMUS"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",                           type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Sum of fractional errors",         type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                              type = 'string')

    message <- base::switch(options[["boundMethodMUS"]],
                              "stringerBound" = "The confidence bound is calculated according to the <b>Stringer</b> method.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    # Return empty table
    if(options[["correctMUS"]] == "" || options[["sampleFilter"]] == "" || options[["monetaryVariable"]] == ""){
      row <- data.frame(materiality = ".", n = ".", fk = ".", k = ".", bound = ".")
      if(options[["mostLikelyError"]])
        row <- cbind(row, mle = ".")
      evaluationTable$addRows(row)
      return()
    }

    mle <- 0
    if(options[["N"]] != 0)
      mle <- floor( sum(result[["z"]]) / result[["n"]] * options[["N"]] )

    errors <- round(sum(result[["z"]]), 2)
    materialityTable <- round(options[["materiality"]], 2)
    if(options[["show"]] == "percentage")
      materialityTable <- paste0(materialityTable * 100, "%")

    boundTable          <- "."
    if(result[["bound"]] != "."){
        boundTable <- round(result[["bound"]],3)
        if(options[["show"]] == "percentage")
          boundTable <- paste0(boundTable * 100, "%")
    }

    row <- data.frame(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, bound = boundTable)
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    evaluationTable$addRows(row)
}
