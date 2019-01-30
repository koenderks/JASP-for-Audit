classicalPlanning <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							    <- list()

  # Set the title
  jaspResults$title 					<- "Planning"

  if(options[["interpretation"]]){
      jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Attributes Procedure</u>", "h2")
      jaspResults[["procedureHeader"]]$position <- 1
      jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of an attributes sampling procedure is to determine with a specified <b>", options[["confidence"]]*100,"%</b> confidence whether the percentage
                                                                of errors in the target population is lower than the specified materiality of <b>",options[["materiality"]]*100,"%</b>. An attributes bound procedure considers
                                                                the observations in the population to be of one of two categories: 1) the observation is fully correct or 2) the observation is
                                                                fully incorrect."), "p")
      jaspResults[["procedureParagraph"]]$position <- 2
  }

  .ARMformula(options, jaspResults, position = 5)   # Show the Audit Risk Model formula and quantify detection risk
  DR              <- jaspResults[["DR"]]$object

  if(options[["interpretation"]]){
      jaspResults[["AuditRiskModelHeader"]] <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
      jaspResults[["AuditRiskModelHeader"]]$position <- 3
      jaspResults[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined
                                                                      to be <b>", options$CR,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be <b>",round(DR*100, 2), "%</b>."), "p")
      jaspResults[["AuditRiskModelParagraph"]]$position <- 4

      jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]$position <- 6
  }
  # Perform the analysis
  .attributesPlanningFullAudit(options, jaspResults)
  result              <- jaspResults[["result"]]$object
  .attributesPlanningTableFullAudit(options, result, jaspResults, position = 8)

  if(options[["expected.errors"]] == "kPercentage"){
      expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
      max.errors <- ceiling(options[["kPercentageNumber"]] * result[["n"]]) + 1
  } else {
      expected.errors <- options[["kNumberNumber"]]
      max.errors <- options[["kNumberNumber"]] + 1
  }

  if(options[["interpretation"]]){

      jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The sample size that is required to prove an <b>",options$materiality*100,"%</b>
                                                                      upper confidence bound, assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. The sample size is calculated with the <b>", options[["distribution"]] , "</b>
                                                                      distribution. Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the population cannot be approved."), "p")
      jaspResults[["priorKnowledgeParagraph"]]$position <- 7

  }

  # Decision plot
  if(options[['plotCriticalErrors']])
  {
      if(is.null(jaspResults[["criticalErrorPlot"]]))
      {
          allowed.errors <- 0:(max.errors-1)
          reject.errors <- max.errors : (max.errors + 2)
          jaspResults[["criticalErrorPlot"]] 		<- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
          jaspResults[["criticalErrorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors",
                                                                      "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors"))
          jaspResults[["criticalErrorPlot"]] 		$position <- 9
      }
  }

  # Save the state
  state[["options"]] 					<- options
  return(state)

}
