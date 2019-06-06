bayesianPlanning <- function(jaspResults, dataset, options, ...){

  jaspResults[["figNumber"]]          <- createJaspState(1)
  jaspResults[["figNumber"]]$dependOn(options = c("priorPlot", "decisionPlot"))

  # Interpretation for the Global Options phase
  if(options[["explanatoryText"]] && is.null(jaspResults[["procedureContainer"]])){
    jaspResults[["procedureContainer"]] <- createJaspContainer(title= "<u>Procedure</u>")
    jaspResults[["procedureContainer"]]$position <- 1
    if(is.null(jaspResults[["confidenceLevelLabel"]]$object)){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOn(options = c("confidence"))
    }
    criterion <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste0(format(options[["materialityValue"]], scientific = FALSE), " monetary units"))
    jaspResults[["procedureContainer"]][["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$position <- 1
    jaspResults[["procedureContainer"]]$dependOn(options = c("explanatoryText", "confidence", "materiality", "materialityValue", "materialityPercentage"))
  }

  .auditRiskModel(options, jaspResults)

  if(options[["materiality"]] == "materialityAbsolute"){
    jaspResults[["ready"]] <- createJaspState(options[["materialityValue"]] != 0 && options[["populationSize"]] != 0 && options[["populationValue"]] != 0)
  } else {
    jaspResults[["ready"]] <- createJaspState(options[["materiality"]] != 0 && options[["populationSize"]] != 0)
  }
  jaspResults[["ready"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

  jaspResults[["total_data_value"]] <- createJaspState(options[["populationValue"]])

  jaspResults[["planningContainer"]] <- createJaspContainer(title= "<u>Planning</u>")
  jaspResults[["planningContainer"]]$position <- 3

  if(is.null(jaspResults[["materiality"]]$object) && jaspResults[["ready"]]$object){
    if(options[["populationValue"]] == 0) { populationValue <- 0.01 } else { populationValue <- options[["populationValue"]] }
    materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / populationValue, no = options[["materialityPercentage"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

    expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", yes = options[["expectedPercentage"]], no = options[["expectedNumber"]] / populationValue)
    if(expTMP > materiality){
      jaspResults[["planningContainer"]][["summaryTable"]] <- createJaspTable("Planning summary")
      jaspResults[["planningContainer"]][["summaryTable"]]$setError("Analysis not possible: Your expected errors are higher than materiality.")
      return()
    }
  }

  planningResult <- .bayesianPlanningManual(options, jaspResults)
  .implicitSampleTable(options, planningResult, jaspResults, position = 3)

  expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = options[["expectedNumber"]])
  max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = floor(options[["expectedPercentage"]] * planningResult[["n"]]) + 1, no = options[["expectedNumber"]] + 1)

  # Decision plot
  if(options[['decisionPlot']] && jaspResults[["ready"]]$object)
  {
      if(is.null(jaspResults[["planningContainer"]][["decisionPlot"]]))
      {
          allowed.errors <- 0:(max.errors - 1)
          reject.errors <- max.errors : (max.errors + 2)
          jaspResults[["planningContainer"]][["decisionPlot"]] 		<- .decisionAnalysisBayesian(options, jaspResults)
          jaspResults[["planningContainer"]][["decisionPlot"]]		  $dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", "decisionPlot",
                                                                                        "materialityValue", "materiality"))
          jaspResults[["planningContainer"]][["decisionPlot"]] 		$position <- 4
      }
      jaspResults[["planningContainer"]][["figure2"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Decision analysis for the current options. The bars represent the sample size that is required under different planning distributions.
                                                                                  The the number of expected errors in the selection is colored in red. The number of expected correct observations is colored in green. 
                                                                                  The most efficient distribution for these data is the <b>", jaspResults[["mostEfficientPlanningDistribution"]]$object ,"</b> distribution."), "p")
      jaspResults[["planningContainer"]][["figure2"]]$position <- 5
      jaspResults[["planningContainer"]][["figure2"]]$dependOn(optionsFromObject= jaspResults[["planningContainer"]][["decisionPlot"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
    } else if(options[["decisionPlot"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Decision analysis")
        errorPlot$status <- "complete"
        jaspResults[["planningContainer"]][["decisionPlot"]] <- errorPlot
    }
    # Prior plot
    if(options[['priorPlot']] && jaspResults[["ready"]]$object)
    {
        if(is.null(jaspResults[["planningContainer"]][["priorPlot"]]))
        {
            jaspResults[["planningContainer"]][["priorPlot"]] 		<- .plotPrior(options, planningResult, jaspResults)
            jaspResults[["planningContainer"]][["priorPlot"]]		  $dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "priorPlotLimit", "priorPlot", "priorPlotAdditionalInfo",
                                                                                      "planningModel", "expectedPercentage", "expectedNumber", "materialityValue", "priorPlotExpectedPosterior", "materiality"))
            jaspResults[["planningContainer"]][["priorPlot"]] 		$position <- 6
        }
        jaspResults[["planningContainer"]][["figure3"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The prior probability distribution <b>(", options[["planningModel"]] ,")</b> on the misstatement in the population. The prior parameters are
                                                              derived from the assessments of the inherent and control risk, along with the expected errors."), "p")
        jaspResults[["planningContainer"]][["figure3"]]$position <- 7
        jaspResults[["planningContainer"]][["figure3"]]$dependOn(optionsFromObject= jaspResults[["planningContainer"]][["priorPlot"]])
        jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
    } else if(options[["priorPlot"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Implied prior from risk assessments")
        errorPlot$status <- "complete"
        jaspResults[["planningContainer"]][["priorPlot"]] <- errorPlot
    }
}

.bayesianPlanningManual <- function(options, jaspResults){

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
  if(options[["expectedBF"]])
    summaryTable$addColumnInfo(name = 'expBF',              title = "Expected BF\u208B\u208A", type = 'string')

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr
  DRtable <- paste0(round(alpha, 3) * 100, "%")

  if(!jaspResults[["ready"]]$object){

    message <- base::switch(options[["planningModel"]],
                          "beta" = "The required sample size is based on the <b>beta</b> distribution.",
                          "beta-binomial" = paste0("The required sample size is based on the <b>beta-binomial</b> distribution (N = ", options[["populationSize"]] ,")."))
    summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

    row <- data.frame(materiality = ".", IR = options[["IR"]], CR = options[["CR"]], DR = DRtable, k = ".", n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    summaryTable$addRows(row)
    return()
  }

  jaspResults[["N"]] <- createJaspState(options[["populationSize"]])
  jaspResults[["N"]]$dependOn(options = c("populationSize"))

  if(is.null(jaspResults[["planningResult"]]$object)){
    if(options[["planningModel"]] == "beta"){
      n_noprior               <- .calc.n.beta(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.beta(options, alpha, jaspResults)
    } else if(options[["planningModel"]] == "beta-binomial"){
      n_noprior               <- .calc.n.betabinom(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.betabinom(options, alpha, jaspResults)
    }

    pk                      <- 0
    pn                      <- n_noprior - n_withprior
    k                       <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]] / options[["populationValue"]])
    if(pn != 0){
        if(options[["expectedErrors"]] == "expectedRelative"){
            k               <- options[["expectedPercentage"]]
            pk              <- pn * k
        } else {
            k               <- options[["expectedNumber"]] / options[["populationValue"]]
            pk              <- pn * k
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
  jaspResults[["planningResult"]] <- createJaspState(resultList)
  jaspResults[["planningResult"]]$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedNumber", "expectedPercentage",
                                                      "planningModel", "materialityValue", "populationValue", "materiality"))
  }

  resultList <- jaspResults[["planningResult"]]$object

  message <- base::switch(options[["planningModel"]],
                        "beta" = paste0("The required sample size is based on the <b>beta</b> distribution <i>(\u03B1 = ", round(resultList[["priorA"]], 2) ,", \u03B2 = ", round(resultList[["priorB"]], 2), ")</i>."),
                        "beta-binomial" = paste0("The required sample size is based on the <b>beta-binomial</b> distribution <i>(N = ", options[["populationSize"]] ,", \u03B1 = ", round(resultList[["priorA"]], 2) , ", \u03B2 = ", round(resultList[["priorB"]], 2), ")</i>."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(resultList[["k"]] * resultList[["n"]], 2), "expectedAbsolute" = options[["expectedNumber"]])

  materialityTitle  <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue  <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * options[["populationValue"]]), "materialityAbsolute" = options[["materialityValue"]])
  materiality       <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = materialityValue)

  row <- data.frame(materiality = materiality, IR  = resultList[["IR"]], CR = resultList[["CR"]], DR = DRtable, k = ktable, n = resultList[["n"]])
  if(options[["expectedBF"]])
    row <- cbind(row, expBF = .expectedBF(options, resultList, ktable, jaspResults))
  summaryTable$addRows(row)

  return(resultList)
}
