.calc.n.binomial <- function(options, alpha, jaspResults){
    for(n in 1:5000){
      impk <- base::switch(options[["expected.errors"]],
                            "kPercentage" = ceiling(n * options[["kPercentageNumber"]]),
                            "kNumber" = options[["kNumberNumber"]])
      if(impk >= n){ next }
      if(impk%%1 == 0){
          x <- choose(n, 0:impk) * jaspResults[["materiality"]]$object^(0:impk) * (1-jaspResults[["materiality"]]$object)^(n - (0:impk))
          if(sum(x) < alpha)
              return(n)
      }
    }
}

.calc.n.hypergeometric <- function(options, alpha, jaspResults){
    for(n in 1:5000){
      k <- base::switch(options[["expected.errors"]],
                            "kPercentage" = ceiling(n * options[["kPercentageNumber"]]),
                            "kNumber" = options[["kNumberNumber"]])
      K <- floor(jaspResults[["N"]]$object * jaspResults[["materiality"]]$object)
      if(k > n) { next }
      x <- choose(K, 0:k) * choose(jaspResults[["N"]]$object-K, n-(0:k)) / choose(jaspResults[["N"]]$object, n)
      if(sum(x) < alpha)
          return(n)
  }
}

.calc.n.gamma <- function(options, alpha, jaspResults){
  for(n in 1:5000){
    k <- base::switch(options[["expected.errors"]],
                          "kPercentage" = (n * options[["kPercentageNumber"]]),
                          "kNumber" = (options[["kNumberNumber"]]))
    x <- pgamma(jaspResults[["materiality"]]$object, shape = 1 + k, scale = 1 / n)
    if(x >= (1 - alpha)){
     return(n)
   }
 }
}

.classicalPlanningHelper <- function(options, jaspResults){

    if(!is.null(jaspResults[["planningResult"]]$object)) return()

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    k                       <- 0
    
    if(jaspResults[["ready"]]$object){
      if(options[["distribution"]] == "gamma"){
        n                     <- .calc.n.gamma(options, alpha, jaspResults)
      } else if(options[["distribution"]] == "binomial"){
        n                     <- .calc.n.binomial(options, alpha, jaspResults)
      } else if(options[["distribution"]] == "hypergeometric"){
        n                   <- .calc.n.hypergeometric(options, alpha, jaspResults)
      }  
      k <- base::switch(options[["expected.errors"]], "kPercentage" = options[["kPercentageNumber"]], "kNumber" = options[["kNumberNumber"]] / n)                      
    }

    resultList <- list()
    resultList[["n"]]             <- n
    resultList[["k"]]             <- k
    resultList[["IR"]]            <- options[["IR"]]
    resultList[["CR"]]            <- options[["CR"]]
    resultList[["alpha"]]         <- alpha
    resultList[["confidence"]]    <- options[["confidence"]]

    jaspResults[["planningResult"]] <- createJaspState(resultList)
    jaspResults[["planningResult"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "distribution", "kPercentageNumber", "kNumberNumber", "materialityValue", "auditType",
                                                      "recordNumberVariable", "monetaryVariable"))

}

.classicalPlanningTable <- function(dataset, options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["planningContainer"]][["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                              <- createJaspTable("Planning Summary")
  jaspResults[["planningContainer"]][["summaryTable"]]             <- summaryTable
  summaryTable$position                     <- position
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "distribution", "recordNumberVariable", "monetaryVariable",
                                  "expected.errors" , "kPercentageNumber", "kNumberNumber", "materialityValue", "auditType"))

  summaryTable$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')

  message <- base::switch(options[["distribution"]],
                          "gamma" = "The sample size is based on the <b>poisson</b> distribution.",
                          "binomial" =  "The sample size is based on the <b>binomial</b> distribution.",
                          "hypergeometric" = paste0("The sample size is based on the <b>hypergeometric</b> distribution (N = ", jaspResults[["N"]]$object ,")."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  
  if(options[["distribution"]] != "gamma" && options[["expected.errors"]] == "kNumber" && options[["kNumberNumber"]]%%1 != 0){
    summaryTable$setError("Expected errors should be a whole number in this sampling model.")
    return()
  }

  if(options[["distribution"]] != "gamma"){
    ktable <- base::switch(options[["expected.errors"]],
                            "kPercentage" = floor(result[["k"]] * result[["n"]]),
                            "kNumber" = options[["kNumberNumber"]])
  } else {
    ktable <- base::switch(options[["expected.errors"]],
                            "kPercentage" = round(result[["k"]] * result[["n"]], 2),
                            "kNumber" = options[["kNumberNumber"]])
  }
  DRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")

  if(jaspResults[["materiality"]]$object == 0){
    row <- data.frame(materiality = ".", IR = result[["IR"]], CR = result[["CR"]], DR = DRtable, k = 0, n = ".")
    summaryTable$addRows(row)
    return()
  }

  materialityTitle <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue <- base::switch(options[["auditType"]],
                                    "attributes" = ceiling(jaspResults[["materiality"]]$object * sum(dataset[, .v(options[["monetaryVariable"]])])),
                                    "mus" = options[["materialityValue"]])

  materiality <- base::switch(options[["auditType"]],
                                "attributes" = materialityTitle,
                                "mus" = materialityValue)

  if(!jaspResults[["ready"]]$object){
    row <- data.frame(materiality = materiality, IR = result[["IR"]], CR = result[["CR"]], DR = DRtable, k = ktable, n = ".")
    summaryTable$addRows(row)
    return()
  }

  row <- data.frame(materiality = materiality, IR = result[["IR"]], CR = result[["CR"]], DR = DRtable, k = ktable, n = result[["n"]])                  
  summaryTable$addRows(row)
}

.attributesBound <- function(dataset, options, jaspResults){

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr

  n                       <- 0
  k                       <- 0
  bound                   <- "."

  if(jaspResults[["runEvaluation"]]$object){
    if(options[["boundMethod"]] == "binomialBound"){
      n                     <- nrow(dataset)
      k                     <- length(which(dataset[,.v(options[["correctID"]])] == 1))
      binomResult <- binom.test(x = k,
                                n = n,
                                p = jaspResults[["materiality"]]$object,
                                alternative = "less",
                                conf.level = (1 - alpha))
      bound                 <- binomResult$conf.int[2]
    } else if(options[["boundMethod"]] == "hyperBound"){
      
      N <- jaspResults[["N"]]$object
      n <- nrow(dataset)
      k <- length(which(dataset[,.v(options[["correctID"]])] == 1))
      for(K in 1000:1){
       x <- phyper(k, K, N - K, n)
       if(x >= 0.05)
           break
      }
      bound <- K / N
    } else if(options[["boundMethod"]] == "gammaBound"){
      n <- nrow(dataset)
      k <- length(which(dataset[,.v(options[["correctID"]])] == 1))
      bound <- qgamma(p = (1 - alpha), shape = 1 + k, scale = 1 / n)
    }
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

.attributesBoundTable <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "sampleFilter", "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS",
                                      "materialityValue"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",          type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",          type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Errors",          type = 'string')
    if(options[["variableType"]] == "variableTypeCorrect"){
        evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
        if(options[["auditType"]] == "mus" && options[["monetaryVariable"]] != "")
        evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    } else {
      evaluationTable$addColumnInfo(name = 'projm',         title = "Maximum Misstatement",           type = 'string')
    }
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                  type = 'string')
      
    message <- base::switch(options[["boundMethod"]],
                              "gammaBound" = "The confidence bound is calculated according to the <b>gamma</b> distributon.",
                              "binomialBound" = "The confidence bound is calculated according to the <b>binomial</b> distributon.",
                              "hyperBound" = "The confidence bound is calculated according to the <b>hypergeometric</b> distribution.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")
    
    if(options[["auditType"]] == "attributes"){
        materialityTable <- paste0(round(jaspResults[["materiality"]]$object, 2) * 100, "%")
    } else {
      materialityTable <- options[["materialityValue"]]
    }

    # Return empty table
    if(!jaspResults[["runEvaluation"]]$object){
      row <- data.frame(materiality = materialityTable, n = ".", k = ".")
      if(options[["variableType"]] == "variableTypeCorrect")
        row <- cbind(row, bound = ".")
      if(options[["auditType"]] == "mus" && options[["monetaryVariable"]] != "")
        row <- cbind(row, projm = ".")
      evaluationTable$addRows(row)
      return()
    }

    mle <- 0
    if(jaspResults[["N"]]$object != 0)
      mle <- floor(result[["k"]] / result[["n"]] * jaspResults[["N"]]$object)

    boundTable          <- "."
    if(result[["bound"]] != "."){
        boundTable <- round(result[["bound"]], 4)
        boundTable <- paste0(boundTable * 100, "%")
    }

    row <- data.frame(materiality = materialityTable, n = result[["n"]], k = result[["k"]])
    if(options[["variableType"]] == "variableTypeCorrect")
      row <- cbind(row, bound = boundTable)
    if(options[["auditType"]] == "mus")
      row <- cbind(row, projm = round(result[["bound"]] * jaspResults[["total_data_value"]]$object, 2))
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

    if(jaspResults[["runEvaluation"]]$object){
      sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["correctID"]]))]
      n                       <- nrow(sample)
      t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["correctID"]])]
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
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "variableType", "correctID",
                                              "auditType", "boundMethod", "materialityValue", "materiality"))
}

.musBoundTable <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "correctID", "distribution", "mostLikelyError", "sampleFilter", "variableType",
                                      "boundMethod", "monetaryVariable", "materialityValue"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",                           type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Total tainting",                   type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    if(options[["monetaryVariable"]] != "")
        evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                              type = 'string')

    message <- base::switch(options[["boundMethod"]],
                              "stringerBound" = "The confidence bound is calculated according to the <b>Stringer</b> method.",
                              "regressionBound" = "The confidence bound is calculated according to the <b>Regression</b> method.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")
    
    if(options[["auditType"]] == "mus"){
        materialityTable <- ceiling(options[["materialityValue"]])
    } else {
      materialityTable <- paste0(round(options[["materiality"]] * 100, 2) , "%")
    }

    # Return empty table with materiality
    if(!jaspResults[["runEvaluation"]]$object){
      row <- data.frame(materiality = materialityTable, n = ".", fk = ".", k = ".", bound = ".")
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projm = ".")
      evaluationTable$addRows(row)
      return()
    }
    
    total_data_value <- jaspResults[["total_data_value"]]$object

    mle <- 0
    if(options[["boundMethod"]] == "stringerBound"){
        mle <- ceiling( sum(result[["z"]]) / result[["n"]] * total_data_value )
    } else if(options[["boundMethod"]] == "regressionBound"){
      mle <- round(result[["mle"]], 2)
    }

    errors <- round(sum(result[["z"]]), 2)

    boundTable          <- "."
    projectedMisstatement <- "."
    if(result[["bound"]] != "."){
        boundTable <- round(result[["bound"]],3)
        projectedMisstatement <- ceiling(result[["bound"]] * total_data_value)
        boundTable <- paste0(boundTable * 100, "%")
    }

    row <- data.frame(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, bound = boundTable)
    if(options[["monetaryVariable"]] != "")
      row <- cbind(row, projm = projectedMisstatement)
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    evaluationTable$addRows(row)
}

.regressionBound <- function(dataset, options, jaspResults){

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."
    mleregression           <- 0

    if(jaspResults[["runEvaluation"]]$object){
      
        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["correctID"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["correctID"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        M                       <- length(which(t != 0))

        B                       <- jaspResults[["total_data_value"]]$object
        N                       <- jaspResults[["N"]]$object
        b                       <- sample[, .v(options[["monetaryVariable"]])]
        w                       <- sample[, .v(options[["correctID"]])]
        b1                      <- as.numeric(coef(lm(b ~ w))[2])
        #b1                      <- (sum(b * w) - (sum(b) * sum(w) / n)) / (sum(b^2) - (sum(b)^2 / n))

        meanb                   <- mean(b)
        meanw                   <- mean(w)

        mleregression           <- (N * meanw) + (b1 * (B - N * meanb)) - B
        stand.dev               <- sd(w) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) ) * sqrt(1 - cor(b, w)^2)
        upperValue              <- mleregression + qt(p = options[["confidence"]], df = n - 1) * stand.dev
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
    resultList[["mle"]]         <- mleregression

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "populationValue", "sampleFilter",
                                              "auditType", "boundMethod", "monetaryVariable", "materialityValue", "variableType"))
}
