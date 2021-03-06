bayesianPlanning <- function(jaspResults, dataset, options, ...){

  jaspResults[["figNumber"]]          <- createJaspState(1)
  jaspResults[["figNumber"]]$dependOn(options = c("priorPlot", "decisionPlot"))

  # Valuta title
  jaspResults[["valutaTitle"]] <- createJaspState(base::switch(options[["valuta"]], "euroValuta" = "\u20AC", "dollarValuta" = "\u0024", "otherValuta" = options[["otherValutaName"]]))

  if(options[["explanatoryText"]] && is.null(jaspResults[["procedureContainer"]])){
    procedureContainer <- createJaspContainer(title= "<u>Procedure</u>")
    procedureContainer$position <- 1
    if(is.null(jaspResults[["confidenceLevelLabel"]]$object)){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOn(options = c("confidence"))
    }
    criterion <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
    procedureContainer[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    procedureContainer[["procedureParagraph"]]$position <- 1
    procedureContainer$dependOn(options = c("explanatoryText", "confidence", "materiality", "materialityValue", "materialityPercentage", "valuta"))
    jaspResults[["procedureContainer"]] <- procedureContainer
  }

  .auditRiskModel(options, jaspResults)

  if(options[["materiality"]] == "materialityAbsolute"){
    jaspResults[["ready"]] <- createJaspState(options[["materialityValue"]] != 0 && options[["populationSize"]] != 0 && options[["populationValue"]] != 0)
  } else {
    jaspResults[["ready"]] <- createJaspState(options[["materiality"]] != 0 && options[["populationSize"]] != 0)
  }
  jaspResults[["ready"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

  planningContainer <- createJaspContainer(title= "<u>Planning</u>")
  planningContainer$position <- 3

  if(jaspResults[["ready"]]$object){
    if(options[["populationValue"]] == 0) { populationValue <- 0.01 } else { populationValue <- options[["populationValue"]] }
    jaspResults[["total_data_value"]] <- createJaspState(populationValue)
    materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / populationValue, no = options[["materialityPercentage"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

    if(options[["materiality"]] == "materialityAbsolute" && options[["materialityValue"]] >= jaspResults[["total_data_value"]]$object)
     planningContainer$setError("Analysis not possible: Your materiality is higher than the total value of the observations.") 
    expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", yes = options[["expectedPercentage"]], no = options[["expectedNumber"]] / populationValue)
    if(expTMP > materiality){
      planningContainer$setError("Analysis not possible: Your expected errors are higher than materiality.")
    }
  }

  planningResult <- .bayesianPlanningManual(options, jaspResults, planningContainer)

  if(options[["implicitSampleTable"]])
    .implicitSampleTable(options, planningResult, jaspResults, position = 3, planningContainer)

  if(options[["explanatoryText"]] && is.null(planningContainer[["planningParagraph"]])){
    materialityLevelLabel         <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]], 4) * 100, "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
    expected.errors <- max.errors <- requiredSampleSize <- materiality <- 0
    priorA <- priorB <- 1
    if(!is.null(jaspResults[["planningResult"]]$object)){
      requiredSampleSize <- planningResult[["n"]]
      expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))
      max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = ceiling(options[["expectedPercentage"]] * planningResult[["n"]]), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]] + 1))
      priorA <- round(planningResult[["priorA"]], 2)
      priorB <- round(planningResult[["priorB"]], 2)
      materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / jaspResults[["total_data_value"]]$object, no = options[["materialityPercentage"]])
    }
    planningContainer[["planningParagraph"]] <- createJaspHtml(paste0("The most likely error in the data was expected to be <b>", expected.errors ,"</b>.  The sample size that is required to prove a materiality of <b>", materialityLevelLabel ,"</b>, assuming
                                                                                              the sample contains <b>", expected.errors ,"</b> full errors, is <b>", requiredSampleSize ,"</b>. This sample size is calculated according to the <b>", options[["planningModel"]] , "</b> distribution, the inherent risk <b>(", options[["IR"]] , ")</b>,
                                                                                              the control risk <b>(", options[["CR"]] , ")</b> and the expected errors. The specific distribution that corresponds with this prior knowledge is the
                                                                                              <b>Beta(", priorA , ",", priorB ,")</b> distribution. The information in this prior distribution states that there is a <b>",
                                                                                              round(pbeta(materiality, priorA, priorB) * 100, 2) ,"%</b> prior probability that the population misstatement
                                                                                              is lower than materiality. Consequently, if the sum of errors from the audited observations exceeds <b>", max.errors ,"</b> the maximum misstatement
                                                                                              exceeds materiality and the population cannot be approved."), "p")
    planningContainer[["planningParagraph"]]$position <- 1
    planningContainer[["planningParagraph"]]$dependOn(options = c("expectedPercentage", "expectedErrors", "expectedNumber", "planningModel", "IR", "CR", "materialityPercentage", "confidence", "materialityValue"))
  }

  expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))
  max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = floor(options[["expectedPercentage"]] * planningResult[["n"]]) + 1, no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]] + 1))

  # Create a decision plot (if the user wants it)
  if(options[["decisionPlot"]])
    .decisionAnalysis(options, jaspResults, position = 4, planningContainer, type = "bayesian")
  # Plot the prior (and optional expected posterior) distribution (if the user wants it)
  if(options[["priorPlot"]])
    .plotPrior(options, planningResult, jaspResults, position = 5, planningContainer)
  # Finish planning
  jaspResults[["planningContainer"]] <- planningContainer
}

.bayesianPlanningManual <- function(options, jaspResults, planningContainer){

  summaryTable <- createJaspTable("Planning Summary")
  summaryTable$position <- 2
  summaryTable$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedPercentage", "expectedNumber", "expectedBF",
                                  "planningModel", "materialityValue", "populationValue", "materiality", "valuta"))

  summaryTable$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')
  if(options[["expectedBF"]])
    summaryTable$addColumnInfo(name = 'expBF',              title = "Expected BF\u208B\u208A", type = 'string')

  planningContainer[["summaryTable"]]        <- summaryTable

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr
  DRtable                 <- paste0(round(alpha, 3) * 100, "%")

  if(!jaspResults[["ready"]]$object || planningContainer$getError()){

    message <- base::switch(options[["planningModel"]],
                          "beta" = "The required sample size is based on the <b>beta</b> distribution.",
                          "beta-binomial" = paste0("The required sample size is based on the <b>beta-binomial</b> distribution (N = ", options[["populationSize"]] ,")."))
    summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

    row <- data.frame(materiality = ".", IR = options[["IR"]], CR = options[["CR"]], DR = DRtable, k = ".", n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    summaryTable$addRows(row)
    summaryTable$addFootnote(message = "The materiality is defined as 0.", symbol="<b>ANALYSIS NOT READY.</b>")
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

  if(!is.null(jaspResults[["errorInSampler"]])){
    planningContainer$setError("There is no sample size (< 5000) large enough to prove the current materiality. Please try other values.")
    return()
  }

  if(resultList[["n"]] > jaspResults[["N"]]$object && jaspResults[["ready"]]$object){
    planningContainer$setError("The required sample size is larger than the population size. You cannot audit this population with this materiality and this amount of confidence.")
    return()
  }

  ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(resultList[["k"]] * resultList[["n"]], 2), "expectedAbsolute" = options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object)

  materialityTitle  <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue  <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * options[["populationValue"]]), "materialityAbsolute" = options[["materialityValue"]])
  materiality       <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, materialityValue))
  kTitle            <- base::switch(options[["expectedErrors"]], "expectedRelative" = ktable, "expectedAbsolute" = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))

  row <- data.frame(materiality = materiality, IR  = resultList[["IR"]], CR = resultList[["CR"]], DR = DRtable, k = kTitle, n = resultList[["n"]])
  if(options[["expectedBF"]])
    row <- cbind(row, expBF = .expectedBF(options, resultList, ktable, jaspResults))
  summaryTable$addRows(row)

  return(resultList)
}
