bayesianPlanning <- function(jaspResults, dataset, options, ...){

  jaspResults$title <- "Bayesian Planning"
  
  jaspResults[["figNumber"]]          <- createJaspState(1)
  jaspResults[["figNumber"]]$dependOnOptions(c("plotPrior", "plotCriticalErrors"))

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
  
  planningResult <- .bayesianPlanningManual(options, jaspResults)
  .priorSampleTable(options, planningResult, jaspResults, position = 3)
  
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
      jaspResults[["planningContainer"]][["figure2"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The number of full errors that are allowed in the sample before rejecting the population are displayed in green.
                                                        Whenever more than this number of full errors is found, displayed in red, the population should be rejected."), "p")
      jaspResults[["planningContainer"]][["figure2"]]$position <- 5
      jaspResults[["planningContainer"]][["figure2"]]$copyDependenciesFromJaspObject(jaspResults[["planningContainer"]][["criticalErrorPlot"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
    } else if(options[["plotCriticalErrors"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Decision plot")
        errorPlot$setError("Plotting not possible: Please specify all  your variables.")
        jaspResults[["planningContainer"]][["criticalErrorPlot"]] <- errorPlot
    }
  
    # Prior plot
    if(options[['plotPrior']] && jaspResults[["ready"]]$object)
    {
        if(is.null(jaspResults[["planningContainer"]][["priorPlot"]]))
        {
            jaspResults[["planningContainer"]][["priorPlot"]] 		<- .plotPrior(options, planningResult, jaspResults)
            jaspResults[["planningContainer"]][["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx", "plotPrior", "plotPriorAdditionalInfo", 
                                                                                      "distribution", "kPercentageNumber", "kNumberNumber", "materialityValue"))
            jaspResults[["planningContainer"]][["priorPlot"]] 		$position <- 6
        }
        jaspResults[["planningContainer"]][["figure3"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The prior probability distribution <b>(", options[["distribution"]] ,")</b> on the misstatement in the population. The prior parameters are
                                                              derived from the assessments of the inherent and control risk, along with the expected errors."), "p")
        jaspResults[["planningContainer"]][["figure3"]]$position <- 7
        jaspResults[["planningContainer"]][["figure3"]]$copyDependenciesFromJaspObject(jaspResults[["planningContainer"]][["priorPlot"]])
        jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
    } else if(options[["plotPrior"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Implied Prior from Risk Assessments")
        errorPlot$setError("Plotting not possible: Please specify all  your variables.")
        jaspResults[["planningContainer"]][["priorPlot"]] <- errorPlot
    }
}

.bayesianPlanningManual <- function(options, jaspResults){
  
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
  if(options[["expectedBF"]])
    summaryTable$addColumnInfo(name = 'expBF',              title = "Expected BF\u208B\u208A", type = 'string')

  message <- base::switch(options[["distribution"]],
                            "beta" = "The sample size is based on the <b>beta</b> distribution.",
                            "beta-binomial" = paste0("The sample size is based on the <b>beta-binomial</b> distribution (N = ", options[["populationSize"]] ,")."))
                              
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  
  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr
  DRtable <- paste0(round(alpha, 3) * 100, "%")
  
  if(!jaspResults[["ready"]]$object){
    row <- data.frame(materiality = ".", IR = options[["IR"]], CR = options[["CR"]], DR = DRtable, k = ".", n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    summaryTable$addRows(row)
    return()
  }
  
  jaspResults[["N"]] <- createJaspState(options[["populationSize"]])
  jaspResults[["N"]]$dependOnOptions(c("populationSize"))
    if(options[["distribution"]] == "beta"){
      n_noprior               <- .calc.n.beta(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.beta(options, alpha, jaspResults)
    } else if(options[["distribution"]] == "beta-binomial"){
      n_noprior               <- .calc.n.betabinom(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.betabinom(options, alpha, jaspResults)
    }

    pk                      <- 0
    pn                      <- n_noprior - n_withprior
    k                       <- base::switch(options[["expected.errors"]],
                                            "kPercentage" = options[["kPercentageNumber"]],
                                            "kNumber" = options[["kNumberNumber"]])
    if(pn != 0){
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
  resultList[["confidence"]]  <- options[["confidence"]]
  
  ktable <- base::switch(options[["expected.errors"]],
                          "kPercentage" = round(resultList[["k"]] * resultList[["n"]], 2),
                          "kNumber" = options[["kNumberNumber"]])

  materialityTitle <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue <- base::switch(options[["auditType"]],
                                    "attributes" = ceiling(jaspResults[["materiality"]]$object * options[["populationValue"]]),
                                    "mus" = options[["materialityValue"]])

  materiality <- base::switch(options[["auditType"]], "attributes" = materialityTitle, "mus" = materialityValue)

  row <- data.frame(materiality = materiality, IR  = resultList[["IR"]], CR = resultList[["CR"]], DR = DRtable, k = ktable, n = resultList[["n"]])
  if(options[["expectedBF"]])
    row <- cbind(row, expBF = .expectedBF(options, resultList, ktable, jaspResults))
  summaryTable$addRows(row)
  
  return(resultList)
}
