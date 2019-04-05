classicalPlanning <- function(jaspResults, dataset, options, ...){
  ### TITLE ###
  jaspResults$title <- "Planning"
  
  jaspResults[["procedureContainer"]] <- createJaspContainer(title= "<u>Procedure</u>")
  jaspResults[["procedureContainer"]]$position <- 1
  
  # Interpretation for the Global Options phase
  if(options[["interpretation"]] && is.null(jaspResults[["procedureContainer"]][["procedureParagraph"]])){
    if(is.null(jaspResults[["confidenceLevelLabel"]]$object)){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOnOptions(c("confidence"))
    }
    criterion <- base::switch(options[["auditType"]], "attributes" = "<b>proportion</b>", "mus" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["auditType"]], "attributes" = paste0(round(options[["materiality"]] * 100, 2), "%"), "mus" = paste0(format(options[["materialityValue"]], scientific = FALSE), " monetary units"))
    jaspResults[["procedureContainer"]][["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of 
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$position <- 1
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$dependOnOptions(c("interpretation", "confidence"))
  }
  
  .auditRiskModel(options, jaspResults)
  
  if(options[["auditType"]] == "mus"){
    jaspResults[["ready"]] <- createJaspState(options[["materialityValue"]] != 0 && options[["populationSize"]] != 0 && options[["populationValue"]] != 0)
  } else {
    jaspResults[["ready"]] <- createJaspState(options[["materiality"]] != 0 && options[["populationSize"]] != 0)
  }
  jaspResults[["ready"]]$dependOnOptions(c("materialityValue", "materiality", "populationSize", "populationValue", "auditType"))
  
  if(is.null(jaspResults[["materiality"]]$object) && jaspResults[["ready"]]$object){
    if(options[["populationValue"]] == 0) { populationValue <- 0.01 } else { populationValue <- options[["populationValue"]] }
    materiality <- ifelse(options[["auditType"]] == "mus", yes = options[["materialityValue"]] / populationValue, no = options[["materiality"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOnOptions(c("materialityValue", "materiality", "populationSize", "populationValue", "auditType"))
  }
  
  planningResult <- .classicalPlanningManual(options, jaspResults)
  
  expected.errors   <- ifelse(options[["expected.errors"]] == "kPercentage", yes = paste0(round(options[["kPercentageNumber"]] * 100, 2), "%"), no = options[["kNumberNumber"]])
  max.errors        <- ifelse(options[["expected.errors"]] == "kPercentage", yes = floor(options[["kPercentageNumber"]] * planningResult[["n"]]) + 1, no = options[["kNumberNumber"]] + 1)

  # Decision plot
  if(options[['plotCriticalErrors']] && jaspResults[["ready"]]$object)
  {
      if(is.null(jaspResults[["planningContainer"]][["criticalErrorPlot"]]))
      {
          allowed.errors <- 0:(max.errors - 1)
          reject.errors <- max.errors : (max.errors + 2)
          jaspResults[["planningContainer"]][["criticalErrorPlot"]] 		<- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
          jaspResults[["planningContainer"]][["criticalErrorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors",
                                                                      "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors",
                                                                      "distribution", "N", "materialityValue"))
          jaspResults[["planningContainer"]][["criticalErrorPlot"]] 		$position <- 4
      }
      jaspResults[["planningContainer"]][["figure2"]] <- createJaspHtml(paste0("<b>Figure 1.</b> The number of full errors that are allowed in the sample before rejecting the population are displayed in green.
                                                        Whenever more than this number of full errors is found, displayed in red, the population should be rejected."), "p")
      jaspResults[["planningContainer"]][["figure2"]]$position <- 5
    } else if(options[["plotCriticalErrors"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Decision plot")
        errorPlot$setError("Plotting not possible: Please specify all  your variables.")
        jaspResults[["planningContainer"]][["criticalErrorPlot"]] <- errorPlot
    }
}

.classicalPlanningManual <- function(options, jaspResults){
  
  jaspResults[["planningContainer"]] <- createJaspContainer(title= "<u>Planning</u>")
  jaspResults[["planningContainer"]]$position <- 3
  
  summaryTable                                                <- createJaspTable("Planning Summary")
  jaspResults[["planningContainer"]][["summaryTable"]]        <- summaryTable
  summaryTable$position                                       <- 1
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "show", "populationSize", "kPercentageNumber", "kNumberNumber", "expectedBF", 
                                  "distribution", "materialityValue", "populationValue", "auditType"))

  summaryTable$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')
  
  message <- base::switch(options[["distribution"]],
                            "gamma" = "The sample size is based on the <b>Poisson</b> distribution.",
                            "binomial" = "The sample size is based on the <b>binomial</b> distribution.",
                            "hypergeometric" = paste0("The sample size is based on the <b>hypergeometric</b> distribution (N = ", options[["populationSize"]] ,")."))
                              
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  
  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr
  DRtable                 <- paste0(round(alpha, 3) * 100, "%")
  
  if(!jaspResults[["ready"]]$object){
    row <- data.frame(materiality = ".", IR = options[["IR"]], CR = options[["CR"]], DR = DRtable, k = ".", n = ".")
    summaryTable$addRows(row)
    return()
  }
  
  jaspResults[["N"]] <- createJaspState(options[["populationSize"]])
  jaspResults[["N"]]$dependOnOptions(c("populationSize"))

  if(is.null(jaspResults[["planningResult"]]$object)){
    if(options[["distribution"]] == "gamma"){
      n                     <- .calc.n.gamma(options, alpha, jaspResults)
    } else if(options[["distribution"]] == "binomial"){
      n                     <- .calc.n.binomial(options, alpha, jaspResults)
    } else if(options[["distribution"]] == "hypergeometric"){
      n                     <- .calc.n.hypergeometric(options, alpha, jaspResults)
    }  
    k <- base::switch(options[["expected.errors"]], "kPercentage" = options[["kPercentageNumber"]], "kNumber" = options[["kNumberNumber"]] / n)                   

  resultList <- list()
  resultList[["n"]]             <- n
  resultList[["k"]]             <- k
  resultList[["IR"]]            <- options[["IR"]]
  resultList[["CR"]]            <- options[["CR"]]
  resultList[["alpha"]]         <- alpha
  resultList[["confidence"]]    <- options[["confidence"]]
  jaspResults[["planningResult"]] <- createJaspState(resultList)
  jaspResults[["planningResult"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "populationSize", "kPercentageNumber", "kNumberNumber", 
                                  "distribution", "materialityValue", "populationValue", "auditType"))
  } 

  resultList <- jaspResults[["planningResult"]]$object
  
  if(options[["distribution"]] != "gamma" && options[["expected.errors"]] == "kNumber" && options[["kNumberNumber"]]%%1 != 0){
    summaryTable$setError("Expected errors should be a whole number in this sampling model.")
    return()
  }

  if(options[["distribution"]] != "gamma"){
    ktable <- base::switch(options[["expected.errors"]],
                            "kPercentage" = floor(resultList[["k"]] * resultList[["n"]]),
                            "kNumber" = options[["kNumberNumber"]])
  } else {
    ktable <- base::switch(options[["expected.errors"]],
                            "kPercentage" = round(resultList[["k"]] * resultList[["n"]], 2),
                            "kNumber" = options[["kNumberNumber"]])
  }

  materialityTitle <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue <- base::switch(options[["auditType"]],
                                    "attributes" = ceiling(jaspResults[["materiality"]]$object * options[["populationValue"]]),
                                    "mus" = options[["materialityValue"]])

  materiality <- base::switch(options[["auditType"]],
                                "attributes" = materialityTitle,
                                "mus" = materialityValue)

  row <- data.frame(materiality = materiality, IR = resultList[["IR"]], CR = resultList[["CR"]], DR = DRtable, k = ktable, n = resultList[["n"]])                  
  summaryTable$addRows(row)
  
  return(resultList)
}
