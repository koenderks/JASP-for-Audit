classicalPlanning <- function(jaspResults, dataset, options, ...){

  jaspResults[["procedureContainer"]] <- createJaspContainer(title= "<u>Procedure</u>")
  jaspResults[["procedureContainer"]]$position <- 1

  # Interpretation for the Global Options phase
  if(options[["explanatoryText"]] && is.null(jaspResults[["procedureContainer"]][["procedureParagraph"]])){
    if(is.null(jaspResults[["confidenceLevelLabel"]]$object)){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOn(options = c("confidence"))
    }
    criterion <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste0(format(options[["materialityValue"]], scientific = FALSE), " monetary units"))
    jaspResults[["procedureContainer"]][["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$position <- 1
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$dependOn(options = c("explanatoryText", "confidence"))
  }

  .auditRiskModel(options, jaspResults)

  if(options[["materiality"]] == "materialityAbsolute"){
    jaspResults[["ready"]] <- createJaspState(options[["materialityValue"]] != 0 && options[["populationSize"]] != 0 && options[["populationValue"]] != 0)
  } else {
    jaspResults[["ready"]] <- createJaspState(options[["materialityPercentage"]] != 0 && options[["populationSize"]] != 0)
  }
  jaspResults[["ready"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

  jaspResults[["planningContainer"]] <- createJaspContainer(title= "<u>Planning</u>")
  jaspResults[["planningContainer"]]$position <- 3

  if(is.null(jaspResults[["materiality"]]$object) && jaspResults[["ready"]]$object){
    if(options[["populationValue"]] == 0) { populationValue <- 0.01 } else { populationValue <- options[["populationValue"]] }
    materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / populationValue, no = options[["materialityPercentage"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

    expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", yes = options[["expectedPercentage"]], no = options[["expectedAbsolute"]] / populationValue)
    if(expTMP > materiality){
      jaspResults[["planningContainer"]][["summaryTable"]] <- createJaspTable("Planning summary")
      jaspResults[["planningContainer"]][["summaryTable"]]$setError("Analysis not possible: Your expected errors are higher than materiality.")
      return()
    }
  }

  planningResult <- .classicalPlanningManual(options, jaspResults)

  expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = options[["expectedNumber"]])
  max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = floor(options[["expectedPercentage"]] * planningResult[["n"]]) + 1, no = options[["expectedNumber"]] + 1)

  # Decision plot
  if(options[['decisionPlot']] && jaspResults[["ready"]]$object)
  {
      if(is.null(jaspResults[["planningContainer"]][["decisionPlot"]]))
      {
          allowed.errors <- 0:(max.errors - 1)
          reject.errors <- max.errors : (max.errors + 2)
          jaspResults[["planningContainer"]][["decisionPlot"]] 		<- .decisionPlot(allowed.errors, reject.errors, options, jaspResults)
          jaspResults[["planningContainer"]][["decisionPlot"]]		  $dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", "decisionPlot",
                                                                                        "planningModel", "materialityValue", "materiality"))
          jaspResults[["planningContainer"]][["decisionPlot"]] 		$position <- 4
      }
      jaspResults[["planningContainer"]][["figure2"]] <- createJaspHtml(paste0("<b>Figure 1.</b> The number of full errors that are allowed in the sample before rejecting the population are displayed in green.
                                                        Whenever more than this number of full errors is found, displayed in red, the population should be rejected."), "p")
      jaspResults[["planningContainer"]][["figure2"]]$position <- 5
    } else if(options[["decisionPlot"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Decision plot")
        errorPlot$setError("Plotting not possible: Please specify all required variables.")
        jaspResults[["planningContainer"]][["decisionPlot"]] <- errorPlot
    }
}

.classicalPlanningManual <- function(options, jaspResults){

  summaryTable                                                <- createJaspTable("Planning summary")
  jaspResults[["planningContainer"]][["summaryTable"]]        <- summaryTable
  summaryTable$position                                       <- 1
  summaryTable$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedPercentage", "expectedNumber", "expectedBF",
                                  "planningModel", "materialityValue", "populationValue", "materiality"))

  summaryTable$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')

  message <- base::switch(options[["planningModel"]],
                            "Poisson" = "The sample size is based on the <b>Poisson</b> distribution.",
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
  jaspResults[["N"]]$dependOn(options = c("populationSize"))

  if(is.null(jaspResults[["planningResult"]]$object)){
    n <- base::switch(options[["planningModel"]],
                        "Poisson"         = .calc.n.poisson(options, alpha, jaspResults),
                        "binomial"        = .calc.n.binomial(options, alpha, jaspResults),
                        "hypergeometric"  = .calc.n.hypergeometric(options, alpha, jaspResults))
    k <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]] / n)

  resultList <- list()
  resultList[["n"]]             <- n
  resultList[["k"]]             <- k
  resultList[["IR"]]            <- options[["IR"]]
  resultList[["CR"]]            <- options[["CR"]]
  resultList[["alpha"]]         <- alpha
  resultList[["confidence"]]    <- options[["confidence"]]
  jaspResults[["planningResult"]] <- createJaspState(resultList)
  jaspResults[["planningResult"]]$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedPercentage", "expectedNumber",
                                                      "planningModel", "materialityValue", "populationValue", "materiality"))
  }

  resultList <- jaspResults[["planningResult"]]$object

  if(options[["planningModel"]] != "Poisson" && options[["expectedErrors"]] == "expectedAbsolute" && options[["expectedNumber"]]%%1 != 0){
    summaryTable$setError("Expected errors should be a whole number in this sampling model.")
    return()
  }

  if(options[["planningModel"]] != "Poisson"){
    ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = floor(resultList[["k"]] * resultList[["n"]]), "expectedAbsolute" = options[["expectedNumber"]])
  } else {
    ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(resultList[["k"]] * resultList[["n"]], 2), "expectedAbsolute" = options[["expectedNumber"]])
  }

  materialityTitle <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * options[["populationValue"]]), "materialityAbsolute" = options[["materialityValue"]])
  materiality <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = materialityValue)

  row <- data.frame(materiality = materiality, IR = resultList[["IR"]], CR = resultList[["CR"]], DR = DRtable, k = ktable, n = resultList[["n"]])
  summaryTable$addRows(row)

  return(resultList)
}
