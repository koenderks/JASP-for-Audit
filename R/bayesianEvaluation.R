bayesianEvaluation <- function(jaspResults, dataset, options, state=NULL){

  monetaryVariable                  <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")        monetaryVariable <- NULL
  correctID                         <- base::switch(options[["variableType"]], "variableTypeCorrect" = unlist(options$correctID), "variableTypeTrueValues" = unlist(options$correctMUS))
  if(correctID == "")               correctID <- NULL
  variables.to.read                 <- c(monetaryVariable, correctID)
  dataset                           <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  total_data_value                  <- options[["populationValue"]]

  options[["sampleSize"]]           <- nrow(dataset)
  options[["sampleFilter"]]         <- "Not applicable"
  options[["show"]]                 <- "percentage"
  options[["distribution"]]         <- "binomial"
  options[["expected.errors"]]      <- "kNumber"
  options[["kNumberNumber"]]        <- 0
  options[["statistic"]]            <- "bound"

  .ARMformula(options, jaspResults, position = 1)   # Show the Audit Risk Model formula and quantify detection risk
  DR              <- jaspResults[["DR"]]$object

  # Rewrite materiality based on value
  if(options[["auditType"]] == "mus")
      options[["materiality"]] <- options[["materialityValue"]] / total_data_value

  # Perform the analysis
  if(options[["variableType"]] == "variableTypeCorrect"){
    # Perform the attributes evaluation
    .bayesianAttributesBoundFullAudit(dataset, options, jaspResults)
    result                                       <- jaspResults[["result"]]$object
    .bayesianAttributesBoundTableFullAudit(options, result, jaspResults, position = 3)
  } else {
    # Perform planning to get prior parameters
    # .bayesianAttributesPlanningFullAudit(options, jaspResults)
    # planningResult              <- jaspResults[["planningResult"]]$object
    planningResult <- list("priorA" = 1, "priorB" = 1)
    # Perform the mus evaluation
    if(options[["boundMethod"]] == "coxAndSnellBound"){
      # Prior parameters for pi and mu are recommendations from the paper
      .coxAndSnellBound(dataset, options, jaspResults, priorPi = 0.10, priorMu = 0.40, priorA = planningResult[["priorA"]], priorB = planningResult[["priorB"]])
    } else if(options[["boundMethod"]] == "regressionBound"){
      .regressionEstimatorBayesian(dataset, options, total_data_value, jaspResults)
    }
    result                                       <- jaspResults[["result"]]$object
    .bayesianMusBoundTableFullAudit(total_data_value, options, result, jaspResults, position = 3)
  }
  # Interpretation before the evalution table
  if(options[["interpretation"]]){
    if(options[["show"]] == "percentage"){
      confidenceLevelLabel            <- paste0(round(options[["confidence"]] * 100, 2), "%")
      materialityLevelLabel           <- paste0(round(options[["materiality"]] * 100, 2), "%")
      boundLabel                      <- paste0(round(result[["bound"]] * 100, 2), "%")
    } else {
      confidenceLevelLabel            <- round(options[["confidence"]], 2)
      materialityLevelLabel           <- round(options[["materiality"]], 2)
      boundLabel                      <- round(result[["bound"]], 2)
    }
    jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>",result[["k"]], "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                          bined with the prior knowledge results in an <b>", confidenceLevelLabel , "</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                          is a true probability of <b>", confidenceLevelLabel , "</b> that the error proportion in the population is lower than <b>", boundLabel ,"</b>."), "p")
    jaspResults[["resultParagraph"]]$position <- 2
  }

  # Prior and Posterior plot
  if(options[['plotPriorAndPosterior']])
  {
      if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
      {
        if(options[["variableType"]] == "variableTypeCorrect"){
          jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBoundFullAudit(options, result, jaspResults)
        } else {
          jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianMUSBoundFullAudit(options, result, jaspResults)
        }
        jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "statistic", "plotPriorAndPosterior",
                                                                   "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                   "expected.errors", "kPercentageNumber", "kNumberNumber", "prior", "sampleFilter",
                                                                   "distribution", "N", "correctMUS", "sampleFilterMUS"))
        jaspResults[["priorAndPosteriorPlot"]] 		$position <- 4
      }
  }

  # Interpretation after the evaluation table
  if(options[["interpretation"]]){
      jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
      jaspResults[["conclusionTitle"]]$position <- 5
      if(result[["bound"]] < options[["materiality"]]){
          above_below <- "lower"
          approve <- "<b>no material misstatement</b>"
      } else if(result[["bound"]] >= options[["materiality"]]){
          above_below <- "higher"
          approve <- "<b>material misstatement, or more information has to be seen.</b>"
      }
      jaspResults[["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", confidenceLevelLabel ,"</b> upper confidence bound on the population proportion of full errors should be determined to be
                                                                  lower than materiality, in this case <b>", materialityLevelLabel ,"</b>. For the current data, the confidence bound is <b>", above_below ,"</b> than materiality. The conclusion for
                                                                  these data is that the data contain ", approve ,"."), "p")
      jaspResults[["conclusionParagraph"]]$position <- 6
  }

  # Confidence bound plot
  if(options[['plotBound']])
  {
      if(is.null(jaspResults[["confidenceBoundPlot"]]))
      {
          jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
          jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID",
                                                                   "show", "plotBound", "materiality", "method",
                                                                   "materialityValue", "result", "boundMethod"))
          jaspResults[["confidenceBoundPlot"]] 		$position <- 23
      }
  }

  # Correlation plot
  if(options[['plotCorrelation']])
  {
      if(is.null(jaspResults[["correlationPlot"]]))
      {
          jaspResults[["correlationPlot"]] 		<- .plotScatterJFA(dataset, options, jaspResults)
          jaspResults[["correlationPlot"]]		$dependOnOptions(c("correctMUS", "plotCorrelation", "monetaryVariable"))
          jaspResults[["correlationPlot"]] 		$position <- 25
      }
  }
}
