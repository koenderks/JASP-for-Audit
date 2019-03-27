bayesianAudit <- function(jaspResults, dataset, options){
    ### TITLE ###
    jaspResults$title <- "Bayesian Sampling Workflow"
    ### PROCEDURE ###
    .bayesianProcedure(options, jaspResults)
    ### AUDIT RISK MODEL ###
    .auditRiskModel(options, jaspResults)
    ### PLANNING ###
    .bayesianPlanning(dataset, options, jaspResults)
    ### SELECTION ###
    if(!options[["samplingChecked"]])         return() # Stop if button is not pressed
    .bayesianSelection(options, jaspResults)
    ### EXECUTION ###
    .execution(options, jaspResults)
    ### EVALUATION ###
    if(!options[["evaluationChecked"]])       return() # Stop if button is not pressed
    .bayesianEvaluation(options, jaspResults)
    ### CONCLUSION ###
    .bayesianConclusion(options, jaspResults)
}

.bayesianProcedure <- function(options, jaspResults){

  dataset <- .readDataProcedure(options, jaspResults)

  jaspResults[["figNumber"]]          <- createJaspState(1)
  jaspResults[["figNumber"]]$dependOnOptions(c("distributionPlot", "plotCriticalErrors"))

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

  if(options[["descriptivesTable"]])
    .dataTable(dataset, options, jaspResults, position = 2)

  # Population distribution plot
  if(options[['distributionPlot']] && jaspResults[["ready"]]$object)
  {
      if(is.null(jaspResults[["procedureContainer"]][["valueDistributionPlot"]]))
      {
          jaspResults[["procedureContainer"]][["valueDistributionPlot"]] 		<- .plotValueDistribution(dataset, options, jaspResults)
          jaspResults[["procedureContainer"]][["valueDistributionPlot"]]		$dependOnOptions(c("distributionPlot", "monetaryVariable"))
          jaspResults[["procedureContainer"]][["valueDistributionPlot"]] 		$position <- 3
      }
      jaspResults[["procedureContainer"]][["figure1"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The distribution of book values in the audit population. The red and blue dots respectively represent the mean
                                                                                        and the values exactly one standard deviation from the mean. The orange dots represent the 25th, 50th (median) and
                                                                                        75th percentile of the book values."), "p")
      jaspResults[["procedureContainer"]][["figure1"]]$position <- 4
      jaspResults[["procedureContainer"]][["figure1"]]$copyDependenciesFromJaspObject(jaspResults[["procedureContainer"]][["valueDistributionPlot"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
      jaspResults[["figNumber"]]$dependOnOptions(c("distributionPlot", "plotCriticalErrors"))
    } else if(options[["distributionPlot"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Population Distribution")
        errorPlot$setError("Plotting not possible: Please specify all  your variables.")
        jaspResults[["procedureContainer"]][["valueDistributionPlot"]] <- errorPlot
    }
}

.bayesianPlanning <- function(dataset, options, jaspResults){

  if(is.null(jaspResults[["materiality"]]$object)){
    materiality <- ifelse(options[["auditType"]] == "mus", yes = options[["materialityValue"]] / jaspResults[["total_data_value"]]$object, no = options[["materiality"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOnOptions(c("materialityValue", "materiality", "monetaryVariable", "recordNumberVariable"))
  }

  jaspResults[["planningContainer"]] <- createJaspContainer(title= "<u>Planning</u>")
  jaspResults[["planningContainer"]]$position <- 3

  # Perform the planning
  .bayesianPlanningHelper(options, jaspResults)
  planningResult              <- jaspResults[["planningResult"]]$object
  .bayesianPlanningTable(dataset, options, planningResult, jaspResults, position = 2)

  expected.errors   <- ifelse(options[["expected.errors"]] == "kPercentage", yes = paste0(round(options[["kPercentageNumber"]] * 100, 2), "%"), no = options[["kNumberNumber"]])
  max.errors        <- ifelse(options[["expected.errors"]] == "kPercentage", yes = floor(options[["kPercentageNumber"]] * planningResult[["n"]]) + 1, no = options[["kNumberNumber"]] + 1)

  # Interpretation after the planning table
  if(options[["interpretation"]] && is.null(jaspResults[["planningContainer"]][["priorKnowledgeParagraph"]])){
    materialityLevelLabel           <- base::switch(options[["auditType"]],
                                                    "attributes" = paste0(round(jaspResults[["materiality"]]$object, 4) * 100, "%"),
                                                    "mus" = format(options[["materialityValue"]], scientific = FALSE))
    jaspResults[["materialityLevelLabel"]] <- createJaspState(materialityLevelLabel)
    jaspResults[["planningContainer"]][["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The probability distribution that corresponds with
                                                                  this prior knowledge is the <b>Beta(",round(planningResult[["priorA"]],2), ",", round(planningResult[["priorB"]],2),")</b> distribution. This probability distribution states that there is a <b>",
                                                                      round(pbeta(jaspResults[["materiality"]]$object, planningResult[["priorA"]], planningResult[["priorB"]]) * 100, 2) ,"%</b> prior probability that the
                                                                  population error is lower than materiality. The sample size that is required to prove an upper confidence bound of <b>", materialityLevelLabel ,"</b>,
                                                                  assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. This sample size is calculated according to the <b>", options[["distribution"]] , "</b> distribution.
                                                                  Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the projected misstatement exceeds the upper confidence bound
                                                                  and the population cannot be approved."), "p")
    jaspResults[["planningContainer"]][["priorKnowledgeParagraph"]]$position <- 1
    jaspResults[["planningContainer"]][["priorKnowledgeParagraph"]]$dependOnOptions(c("kPercentageNumber", "expected.errors", "kNumberNumber", "distribution", "IR", "CR", "materiality", "N",
                                                            "confidence", "materialityValue"))
  }

  # Implicit sample table
  .priorSampleTable(options, planningResult, jaspResults, position = 3)

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

.bayesianSelection <- function(options, jaspResults){

  total_data_value              <- jaspResults[["total_data_value"]]$object
  planningResult                <- jaspResults[["planningResult"]]$object
  jaspResults[["sampleSize"]]   <- createJaspState(planningResult[["n"]])
  monetaryVariable              <- unlist(options[["monetaryVariable"]])

  dataset <- .readDataSelection(options)

  jaspResults[["selectionContainer"]] <- createJaspContainer(title= "<u>Selection</u>")
  jaspResults[["selectionContainer"]]$position <- 4

  # Interpretation for the sampling phase
  if(options[["interpretation"]]){
    technique <- base::switch(options[["samplingType"]],
                                "simplerandomsampling" = "random",
                                "systematicsampling" = "systematic",
                                "cellsampling" = "cell")
    technique <- base::switch(options[["samplingMethod"]],
                                "recordsampling" = paste(technique, "record sampling"),
                                "mussampling" = paste(technique, "monetary unit sampling"))
      jaspResults[["selectionContainer"]][["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", jaspResults[["N"]]$object, "</b> observations, <b>", planningResult[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
      jaspResults[["selectionContainer"]][["samplingParagraph"]]$position <- 1
      jaspResults[["selectionContainer"]][["samplingParagraph"]]$dependOnOptions(c("sampleSize", "N", "samplingType", "samplingMethod"))
  }
  
  # Perform the sampling and draw the outcome tables
  if(options[["samplingType"]] == "simplerandomsampling"){
      .randomSampling(dataset, options, jaspResults, position = 4)
  } else if(options[["samplingType"]] == "systematicsampling"){
      .systematicSampling(dataset, options, jaspResults, position = 4)
  } else if(options[["samplingType"]] == "cellsampling"){
      .cellSampling(dataset, options, jaspResults, position = 4)
  }
  .samplingInfo(dataset, options, jaspResults, position = 2)
  # Descriptives table
  .sampleDescriptives(dataset, options, jaspResults, position = 3)
}

.readDataBayesianEvaluation <- function(options, jaspResults){

  recordVariable                  <- unlist(options$recordNumberVariable)
  if(recordVariable == "")        recordVariable <- NULL
  rankingVariable                 <- unlist(options$rankingVariable)
  if(rankingVariable == "")       rankingVariable <- NULL
  monetaryVariable                <- unlist(options$monetaryVariable)
  if(monetaryVariable == "")      monetaryVariable <- NULL
  variables                       <- unlist(options$variables)
  sampleFilter                    <- unlist(options$sampleFilter)
  if(sampleFilter == "")          sampleFilter <- NULL
  correctID                       <- base::switch(options[["variableType"]], "variableTypeCorrect" = unlist(options$correctID), "variableTypeTrueValues" = unlist(options$correctMUS))
  if(correctID == "")             correctID <- NULL
  variables.to.read               <- c(recordVariable, variables, rankingVariable, correctID, sampleFilter, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  jaspResults[["runEvaluation"]] <- createJaspState( (!is.null(correctID) && !is.null(sampleFilter)) )
  jaspResults[["runEvaluation"]]$dependOnOptions(c("correctID", "sampleFilter", "correctMUS"))

  return(dataset)
}

.bayesianEvaluation <- function(options, jaspResults){

  dataset <- .readDataEvaluation(options, jaspResults)

  total_data_value              <- jaspResults[["total_data_value"]]$object
  planningResult                <- jaspResults[["planningResult"]]$object
  runEvaluation                 <- jaspResults[["runEvaluation"]]$object

  jaspResults[["evaluationContainer"]] <- createJaspContainer(title = "<u>Evaluation</u>")
  jaspResults[["evaluationContainer"]]$position <- 5

  # Apply the sample filter
  if(jaspResults[["runEvaluation"]]$object){
      dataset <- subset(dataset, dataset[, .v(options[["sampleFilter"]])] == 1)
  }

  if(options[["variableType"]] == "variableTypeCorrect"){
    # Perform the attributes evaluation
    .bayesianAttributesBound(dataset, options, jaspResults)
    result                                       <- jaspResults[["result"]]$object
    .bayesianAttributesBoundTable(options, result, jaspResults, position = 2)
  } else {
    # Perform the mus evaluation
    if(options[["boundMethod"]] == "coxAndSnellBound"){
      # Prior parameters for pi and mu are recommendations from the paper
      .coxAndSnellBound(dataset, options, jaspResults, priorPi = 0.1, priorMu = 0.4, priorA = planningResult[["priorA"]], priorB = planningResult[["priorB"]])
    } else if(options[["boundMethod"]] == "regressionBound"){
      
    }
    result                                       <- jaspResults[["result"]]$object
    .bayesianMusBoundTable(options, result, jaspResults, position = 2)
  }

  # Interpretation before the evalution table
  if(options[["interpretation"]]){
    boundLabel <- base::switch(runEvaluation,
                                "TRUE" = paste0(round(result[["bound"]] * 100, 2), "%"),
                                "FALSE" = "...%")
    jaspResults[["evaluationContainer"]][["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>",result[["k"]], "</b> of which were found to contain an error. The knowledge from these data, com-
                                                          bined with the prior knowledge results in an <b>", jaspResults[["confidenceLevelLabel"]]$object , "</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                          is a true probability of <b>", jaspResults[["confidenceLevelLabel"]]$object , "</b> that the error proportion in the population is lower than <b>", boundLabel ,"</b>."), "p")
    jaspResults[["evaluationContainer"]][["resultParagraph"]]$position <- 1
    jaspResults[["evaluationContainer"]][["resultParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                             "method", "materialityValue"))
  }

    # Confidence bound plot
    if(options[['plotBound']] && runEvaluation)
    {
        if(is.null(jaspResults[["confidenceBoundPlot"]]))
        {
            jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
            jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID",
                                                                     "show", "plotBound", "materiality", "method",
                                                                     "materialityValue", "result", "boundMethod"))
            jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]] 		$position <- 3
        }
        jaspResults[["evaluationContainer"]][["figure4"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Results of the sample evaluation compared with materiality and expected errors. The most likely error (MLE)
                                                              is an estimate of the true misstatement in the population. The maximum error is the upper confidence bound on this MLE."), "p")
        jaspResults[["evaluationContainer"]][["figure4"]]$position <- 4
        jaspResults[["evaluationContainer"]][["figure4"]]$copyDependenciesFromJaspObject(jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]])
        jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
    } else if(options[["plotBound"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Evaluation Information")
        errorPlot$setError("Plotting not possible: Please specify your variables.")
        jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]] <- errorPlot
    }

    # Prior and Posterior plot
    if(options[['plotPriorAndPosterior']] && runEvaluation)
    {
        if(is.null(jaspResults[["evaluationContainer"]][["priorAndPosteriorPlot"]]))
        {
          if(options[["variableType"]] == "variableTypeCorrect"){
            jaspResults[["evaluationContainer"]][["priorAndPosteriorPlot"]] 		<- .priorAndPosteriorBayesianAttributes(options, result, jaspResults)
          } else {
            jaspResults[["evaluationContainer"]][["priorAndPosteriorPlot"]] 		<- .priorAndPosteriorBayesianMUS(options, result, jaspResults)
          }
          jaspResults[["evaluationContainer"]][["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "plotPriorAndPosterior",
                                                                     "plotPriorAndPosteriorAdditionalInfo", "materiality", "correctID",
                                                                     "expected.errors", "kPercentageNumber", "kNumberNumber", "prior", "sampleFilter",
                                                                     "distribution", "N", "correctMUS", "sampleFilterMUS", "materialityValue"))
          jaspResults[["evaluationContainer"]][["priorAndPosteriorPlot"]] 		$position <- 5
        }
        jaspResults[["evaluationContainer"]][["figure5"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The prior and posterior probability distrubution on the percentage of misstatement in the population. The red dot
                                                              represents the set materiality. If the area under the distribution exceeds this limit, the population should be rejected."), "p")
        jaspResults[["evaluationContainer"]][["figure5"]]$position <- 6
        jaspResults[["evaluationContainer"]][["figure5"]]$copyDependenciesFromJaspObject(jaspResults[["evaluationContainer"]][["priorAndPosteriorPlot"]])
        jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
    } else if(options[["plotPriorAndPosterior"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Prior and Posterior Plot")
        errorPlot$setError("Plotting not possible: Please specify your variables.")
        jaspResults[["evaluationContainer"]][["priorAndPosteriorPlot"]] <- errorPlot
    }

    # Correlation plot
    if(options[['plotCorrelation']] && runEvaluation)
    {
        if(is.null(jaspResults[["evaluationContainer"]][["correlationPlot"]]))
        {
            jaspResults[["evaluationContainer"]][["correlationPlot"]] 		<- .plotScatterJFA(dataset, options, jaspResults)
            jaspResults[["evaluationContainer"]][["correlationPlot"]]		$dependOnOptions(c("correctMUS", "plotCorrelation", "monetaryVariable"))
            jaspResults[["evaluationContainer"]][["correlationPlot"]] 		$position <- 7
        }
        jaspResults[["evaluationContainer"]][["figure6"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Scatterplot of the sample book values versus their audit values. Red dots indicate observations that did not match
                                                                their original book value. If these red dots lie in the bottom part of the graph, the observations are overstated. If these red dots
                                                                lie in the upper part of the graph, they are understated."), "p")
        jaspResults[["evaluationContainer"]][["figure6"]]$position <- 8
        jaspResults[["evaluationContainer"]][["figure6"]]$copyDependenciesFromJaspObject(jaspResults[["evaluationContainer"]][["correlationPlot"]])
        jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
    } else if(options[["plotCorrelation"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Correlation Plot")
        errorPlot$setError("Plotting not possible: Please specify your variables.")
        jaspResults[["evaluationContainer"]][["correlationPlot"]] <- errorPlot
    }
}

.bayesianConclusion <- function(options, jaspResults){

  result <- jaspResults[["result"]]$object

  # Interpretation after the evaluation table
  if(options[["interpretation"]] && jaspResults[["runEvaluation"]]$object){

    jaspResults[["conclusionContainer"]] <- createJaspContainer(title = "<u>Conclusion</u>")
    jaspResults[["conclusionContainer"]]$position <- 5

    above_below   <- ifelse(result[["bound"]] < jaspResults[["materiality"]]$object, yes = "lower", no = "higher")
    approve       <- ifelse(result[["bound"]] < jaspResults[["materiality"]]$object, yes = "<b>no material misstatement</b>", no = "<b>material misstatement</b>")

    jaspResults[["conclusionContainer"]][["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", jaspResults[["confidenceLevelLabel"]]$object ,"</b> upper confidence bound on the population proportion of full errors should be determined to be
                                                                lower than materiality, in this case <b>", jaspResults[["materialityLevelLabel"]]$object ,"</b>. For the current data, the confidence bound is <b>", above_below ,"</b> than materiality.
                                                                The conclusion for these data is that the data contain ", approve ,"."), "p")
    jaspResults[["conclusionContainer"]][["conclusionParagraph"]]$position <- 1
    jaspResults[["conclusionContainer"]][["conclusionParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "materiality", "boundMethod", "materialityValue", "sampleFilter"))
  }
}
