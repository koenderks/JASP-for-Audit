bayesianAttributesPlanning <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							    <- list()

  jaspResults$title 					<- "Bayesian Attributes Planning"

  .ARMformula(options, jaspResults, position = 3)   # Show the Audit Risk Model formula and quantify detection risk
  DR              <- jaspResults[["DR"]]$object

  # Interpretation for the Planning phase
  if(options[["interpretation"]]){
      jaspResults[["AuditRiskModelHeader"]] <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
      jaspResults[["AuditRiskModelHeader"]]$position <- 1
      jaspResults[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined
                                                                      to be <b>", options$CR,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be <b>",round(DR*100, 2), "%</b>."), "p")
      jaspResults[["AuditRiskModelParagraph"]]$position <- 2

      jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]$position <- 4
  }

  .bayesianAttributesPlanningFullAudit(options, jaspResults)
  result              <- jaspResults[["result"]]$object
  .bayesianAttributesPlanningTableFullAudit(options, result, jaspResults, position = 5)

  if(options[["expected.errors"]] == "kPercentage"){
      expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
      max.errors <- floor(options[["kPercentageNumber"]] * result[["n"]]) + 1
  } else {
      expected.errors <- options[["kNumberNumber"]]
      max.errors <- options[["kNumberNumber"]] + 1
  }

  # Interpretation for the Planning phase
  if(options[["interpretation"]]){

      jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The probability distribution that corresponds with
                                                                    this prior knowledge is the <b>Beta(",round(result[["priorA"]],2), ",", round(result[["priorB"]],2),")</b> distribution. This probability distribution states that there is a <b>",
                                                                        round(pbeta(options[["materiality"]], result[["priorA"]],result[["priorB"]])*100, 2) ,"%</b> prior probability that the
                                                                    population error is lower than materiality. The sample size that is required to prove an <b>",options$materiality*100,"%</b> upper confidence bound, assuming
                                                                    the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. Consequently, if <b>", max.errors ,"</b> or more full errors are found, the population cannot be approved."), "p")
      jaspResults[["priorKnowledgeParagraph"]]$position <- 6

  }

  # Implicit sample table
  if (options[["implicitsample"]])
  {
      if(is.null(jaspResults[["sampletable"]]))
          .priorSampleTable(options, result, jaspResults, position = 7)
  }

  # Decision plot
  if(options[['plotCriticalErrors']])
  {
      if(is.null(jaspResults[["criticalErrorPlot"]]))
      {
          allowed.errors <- 0:(max.errors-1)
          reject.errors <- max.errors : (max.errors + 2)
          jaspResults[["criticalErrorPlot"]] 		<- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
          jaspResults[["criticalErrorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "distribution", "N",
                                                                      "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors"))
          jaspResults[["criticalErrorPlot"]] 		$position <- 8
      }
  }

  # Prior plot
  if(options[['plotPrior']])
  {
      if(is.null(jaspResults[["priorPlot"]]))
      {
          jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanningFullAudit(options, result, jaspResults)
          jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                           "plotPrior", "plotPriorAdditionalInfo", "show", "prior", "distribution", "N",
                                                           "statistic", "kPercentageNumber", "kNumberNumber"))
          jaspResults[["priorPlot"]] 		$position <- 9
      }
  }

  # Save the state
  state[["options"]] 					<- options
  return(state)

}
