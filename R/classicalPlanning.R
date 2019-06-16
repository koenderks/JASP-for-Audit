classicalPlanning <- function(jaspResults, dataset, options, ...){

  # Valuta title
  jaspResults[["valutaTitle"]] <- createJaspState(base::switch(options[["valuta"]], "euroValuta" = "\u20AC", "dollarValuta" = "\u0024", "otherValuta" = options[["otherValutaName"]]))

  # Interpretation for the Global Options phase
  if(options[["explanatoryText"]] && is.null(jaspResults[["procedureContainer"]])){
    jaspResults[["procedureContainer"]] <- createJaspContainer(title= "<u>Procedure</u>")
    jaspResults[["procedureContainer"]]$position <- 1
    if(is.null(jaspResults[["confidenceLevelLabel"]]$object)){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOn(options = c("confidence"))
    }
    criterion <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
    jaspResults[["procedureContainer"]][["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$position <- 1
    jaspResults[["procedureContainer"]]$dependOn(options = c("explanatoryText", "confidence", "materiality", "materialityValue", "materialityPercentage", "valuta"))
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
    jaspResults[["total_data_value"]] <- createJaspState(populationValue)
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

  planningResult <- .classicalPlanningManual(options, jaspResults)

  expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))
  max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = ceiling(options[["expectedPercentage"]] * planningResult[["n"]]), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]] + 1))

  # Decision plot
  if(options[['decisionPlot']] && jaspResults[["ready"]]$object)
  {
      if(is.null(jaspResults[["planningContainer"]][["decisionPlot"]]))
      {
          jaspResults[["planningContainer"]][["decisionPlot"]] 		<- .decisionAnalysisFrequentist(options, jaspResults)
          jaspResults[["planningContainer"]][["decisionPlot"]]		  $dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", "decisionPlot",
                                                                                        "planningModel", "materialityValue", "materiality"))
          jaspResults[["planningContainer"]][["decisionPlot"]] 		$position <- 4
      }
      jaspResults[["planningContainer"]][["figure2"]] <- createJaspHtml(paste0("<b>Figure 1.</b> Decision analysis for the current options. The bars represent the sample size that is required under different planning distributions.
                                                                                  The the number of expected errors in the selection is colored in red. The number of expected correct observations is colored in green. 
                                                                                  The most efficient distribution for these data is the <b>", jaspResults[["mostEfficientPlanningDistribution"]]$object ,"</b> distribution."), "p")
      jaspResults[["planningContainer"]][["figure2"]]$position <- 5
    } else if(options[["decisionPlot"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Decision analysis")
        errorPlot$status <- "complete"
        jaspResults[["planningContainer"]][["decisionPlot"]] <- errorPlot
    }
}

.classicalPlanningManual <- function(options, jaspResults){

  summaryTable                                                <- createJaspTable("Planning summary")
  jaspResults[["planningContainer"]][["summaryTable"]]        <- summaryTable
  summaryTable$position                                       <- 1
  summaryTable$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedPercentage", "expectedNumber", "expectedBF",
                                  "planningModel", "materialityValue", "populationValue", "materiality", "valuta"))

  summaryTable$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr
  DRtable                 <- paste0(round(alpha, 3) * 100, "%")

  if(!jaspResults[["ready"]]$object){

    message <- base::switch(options[["planningModel"]],
                        "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution."),
                        "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution."),
                        "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution."))
    summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

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
    k <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]] / options[["populationValue"]])

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

  message <- base::switch(options[["planningModel"]],
                        "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution <i>(\u03BB = ", round(jaspResults[["materiality"]]$object * resultList[["n"]], 4) , ")</i>."),
                        "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution <i>(p = ", round(jaspResults[["materiality"]]$object, 2) ,")</i>."),
                        "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution <i>(N = ", options[["populationSize"]] ,", K = ", ceiling(options[["populationSize"]] * jaspResults[["materiality"]]$object) ,")</i>."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  if(!is.null(jaspResults[["errorInSampler"]])){
    summaryTable$setError("There is no sample size (< 5000) large enough to prove the current materiality. Please try other values.")
    return()
  }

  ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(resultList[["k"]] * resultList[["n"]]), "expectedAbsolute" = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))

  materialityTitle <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * options[["populationValue"]]), "materialityAbsolute" = options[["materialityValue"]])
  materiality <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, materialityValue))

  row <- data.frame(materiality = materiality, IR = resultList[["IR"]], CR = resultList[["CR"]], DR = DRtable, k = ktable, n = resultList[["n"]])
  summaryTable$addRows(row)

  return(resultList)
}
