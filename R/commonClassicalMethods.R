.calc.n.binomial <- function(options, alpha){
    for(n in 1:10000){
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
    for(n in 1:10000){
      k <- base::switch(options[["expected.errors"]],
                            "kPercentage" = ceiling(n * options[["kPercentageNumber"]]),
                            "kNumber" = options[["kNumberNumber"]])
      K <- floor(options[["N"]] * options[["materiality"]])
      if(k > n) { next }
      x <- choose(K, 0:k) * choose(options[["N"]]-K, n-(0:k)) / choose(options[["N"]], n)
      if(sum(x) < alpha)
          return(n)
  }
}

.attributesPlanningFullAudit <- function(options, jaspResults){

    if(!is.null(jaspResults[["planningResult"]])) return()

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- "..."
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
                                                "N", "kPercentageNumber", "kNumberNumber", "materialityValue", "auditType"))

}

.attributesPlanningTableFullAudit <- function(dataset, options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                              <- createJaspTable("Planning Table")
  jaspResults[["summaryTable"]]             <- summaryTable
  summaryTable$position                     <- position
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "show", "distribution", "N",
                                  "expected.errors" , "kPercentageNumber", "kNumberNumber", "materialityValue", "auditType"))

  summaryTable$addColumnInfo(name = 'materialityPercent',   title = "Percentage",           type = 'string', overtitle = "Materiality")
  summaryTable$addColumnInfo(name = 'materialityValue',     title = "Value",                type = 'string', overtitle = "Materiality")
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Allowed errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')

  message <- base::switch(options[["distribution"]],
                          "binomial" =  "The sample size is calculated using the <b>binomial</b> distribution.",
                          "hypergeometric" = paste0("The sample size is calculated using the <b>hypergeometric</b> distribution (N = ", options[["N"]] ,")."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  if(options[["materiality"]] == 0){
    row <- data.frame(materialityPercent = ".", materialityValue = ".", IR = ".", CR = ".", DR = ".", k = ".", n = ".")
    summaryTable$addRows(row)
    return()
  }

  ktable <- base::switch(options[["expected.errors"]],
                          "kPercentage" = ceiling(result[["k"]] * result[["n"]]),
                          "kNumber" = options[["kNumberNumber"]])
  DRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")

  materialityTitle <- paste0(round(options[["materiality"]] * 100, 2), "%")
  materialityValue <- base::switch(options[["auditType"]],
                                    "attributes" = ceiling(options[["materiality"]] * sum(dataset[, .v(options[["monetaryVariable"]])])),
                                    "mus" = options[["materialityValue"]])

  row <- data.frame(materialityPercent    = materialityTitle,
                    materialityValue      = materialityValue,
                    IR                    = result[["IR"]],
                    CR                    = result[["CR"]],
                    DR                    = DRtable,
                    k                     = ktable,
                    n                     = result[["n"]])
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
  jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "sampleFilter", "materiality", "materialityValue"))

}

.attributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Attributes Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "sampleFilter", "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS",
                                      "materialityValue"))

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
        boundTable <- round(result[["bound"]], 4)
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
    NoOfErrors              <- 0
    z                       <- 0
    bound                   <- "."

    if(options[["correctMUS"]] != "" && options[["sampleFilter"]] != "" && options[["monetaryVariable"]] != ""){
      sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["correctMUS"]]))]
      n                       <- nrow(sample)
      t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["correctMUS"]])]
      z                       <- t / sample[, .v(options[["monetaryVariable"]])]
      zplus                   <- sort(subset(z, z > 0), decreasing = TRUE)
      M                       <- length(zplus)
      NoOfErrors              <- length(which(t != 0))
      bound                   <- 1 - alpha^(1/n)
      if(M > 0){
          prop.sum            <- 0
          for(i in 1:M){
              prop.sum        <- prop.sum + ((qbeta(1 - alpha, i + 1, n - i) - qbeta(1 - alpha, (i-1) + 1, n - (i-1) ))  * zplus[i])
          }
          bound               <- bound + prop.sum
      }
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- NoOfErrors
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctMUS", "sampleFilterMUS",
                                              "auditType", "boundMethodMUS", "monetaryVariableMUS", "materialityValue"))
}

.musBoundTableFullAudit <- function(total_data_value, options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("MUS Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show",
                                      "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS",
                                      "boundMethodMUS", "monetaryVariableMUS", "materialityValue"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",                           type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Total tainting",                   type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                              type = 'string')

    message <- base::switch(options[["boundMethodMUS"]],
                              "stringerBound" = "The confidence bound is calculated according to the <b>Stringer</b> method.",
                              "regressionBound" = "The confidence bound is calculated according to the <b>Regression</b> method.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    # Return empty table
    if(options[["correctMUS"]] == "" || options[["sampleFilter"]] == ""){
      row <- data.frame(materiality = ".", n = ".", fk = ".", k = ".", bound = ".", projm = ".")
      if(options[["mostLikelyError"]])
        row <- cbind(row, mle = ".")
      evaluationTable$addRows(row)
      return()
    }

    mle <- 0
    if(options[["N"]] != 0)
      mle <- floor( sum(result[["z"]]) / result[["n"]] * options[["N"]] )

    errors <- round(sum(result[["z"]]), 2)
    materialityTable <- ceiling(options[["materialityValue"]])

    boundTable          <- "."
    projectedMisstatement <- "."
    if(result[["bound"]] != "."){
        boundTable <- round(result[["bound"]],3)
        projectedMisstatement <- ceiling(result[["bound"]] * total_data_value)
        if(options[["show"]] == "percentage")
          boundTable <- paste0(boundTable * 100, "%")
    }

    row <- data.frame(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, bound = boundTable, projm = projectedMisstatement)
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    evaluationTable$addRows(row)
}

.regressionEstimator <- function(dataset, options, total_data_value, jaspResults){

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
        zplus                   <- sort(subset(z, z > 0), decreasing = TRUE)
        M                       <- length(which(t != 0))

        B                       <- total_data_value
        N                       <- options[["N"]]
        b                       <- sample[, .v(options[["monetaryVariable"]])]
        w                       <- sample[, .v(options[["correctMUS"]])]
        #b1                      <- as.numeric(coef(lm(w ~ b))[2])
        b1                      <- (sum(b * w) - (sum(b) * sum(w) / n)) / (sum(b^2) - (sum(b)^2 / n))

        meanb                   <- mean(b)
        meanw                   <- mean(w)

        mleregression           <- (N * meanw + b1 * (B - N * meanb))
        s                       <- sd(w) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) ) * sqrt(1 - cor(b, w)^2)
        upperValue              <- mleregression + qnorm(options[["confidence"]]) * s
        bound                   <- (upperValue - B) / B
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
                                              "auditType", "boundMethodMUS", "monetaryVariableMUS", "materialityValue"))
}
