bayesianPlanning <- function(jaspResults, dataset, options, state=NULL){

  options[["statistic"]]                            <- "bound"

  jaspResults$title 					                      <- "Bayesian Planning"

  # Interpretation for the Global Options phase
  if(options[["interpretation"]]){
    # Headers for the sub-analyses
    jaspResults[["procedureHeader"]]                <- createJaspHtml("<u>Procedure</u>", "h2")
    jaspResults[["procedureHeader"]]                $position <- 1
    jaspResults[["procedureHeader"]]                $dependOnOptions(c("interpretation"))
    confidenceLevelLabel                            <- paste0(round(options[["confidence"]] * 100, 2), "%")
    jaspResults[["procedureParagraph"]]             <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", confidenceLevelLabel, ")</b>
                                                                  whether the amount of misstatement in the target population is lower than the specified materiality."), "p")
    jaspResults[["procedureParagraph"]]             $position <- 2
    jaspResults[["procedureParagraph"]]             $dependOnOptions(c("interpretation", "confidence"))
  }

  .ARMformula(options, jaspResults, position = 5)   # Show the Audit Risk Model formula and quantify detection risk
  DR                                                <- jaspResults[["DR"]]$object

  populationValue                                   <- options[["populationValue"]]
  if(options[["populationValue"]] == 0)
    populationValue                                 <- 0.01

  # Rewrite materiality based on value
  if(options[["auditType"]] == "mus")
      options[["materiality"]]                      <- options[["materialityValue"]] / populationValue

  options[["run"]]                                  <- base::switch(options[["auditType"]],
                                                                      "mus" = (options[["materiality"]] != 0 && (populationValue != 0 && populationValue != 0.01) && options[["N"]] != 0),
                                                                      "attributes" = options[["materiality"]] != 0)

  # Interpretation for the Planning phase
  if(options[["interpretation"]]){
      jaspResults[["AuditRiskModelHeader"]]         <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
      jaspResults[["AuditRiskModelHeader"]]         $position <- 3
      jaspResults[["AuditRiskModelParagraph"]]      <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined
                                                                      to be <b>", options$CR,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be <b>",round(DR*100, 2), "%</b>."), "p")
      jaspResults[["AuditRiskModelParagraph"]]      $position <- 4

      jaspResults[["priorKnowledgeHeader"]]         <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]         $position <- 6
  }

  # Perform the planning
  .bayesianAttributesPlanningFullAudit(options, jaspResults)
  planningResult                                    <- jaspResults[["planningResult"]]$object
  .bayesianAttributesPlanningTableFullAudit(dataset, options, planningResult, jaspResults, position = 7)

  if(options[["expected.errors"]] == "kPercentage"){
      expected.errors                               <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
      max.errors                                    <- floor(options[["kPercentageNumber"]] * planningResult[["n"]]) + 1
  } else {
      expected.errors                               <- options[["kNumberNumber"]]
      max.errors                                    <- options[["kNumberNumber"]] + 1
  }

  # Interpretation for the Planning phase
  if(options[["interpretation"]]){

      jaspResults[["priorKnowledgeParagraph"]]      <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The probability distribution that corresponds with
                                                                    this prior knowledge is the <b>Beta(",round(planningResult[["priorA"]],2), ",", round(planningResult[["priorB"]],2),")</b> distribution. This probability distribution states that there is a <b>",
                                                                        round(pbeta(options[["materiality"]], planningResult[["priorA"]],planningResult[["priorB"]])*100, 2) ,"%</b> prior probability that the
                                                                    population error is lower than materiality. The sample size that is required to prove an <b>",options$materiality*100,"%</b> upper confidence bound, assuming
                                                                    the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. Consequently, if <b>", max.errors ,"</b> or more full errors are found, the population cannot be approved."), "p")
      jaspResults[["priorKnowledgeParagraph"]]      $position <- 6

  }

  # Implicit sample table
  if (options[["implicitsample"]] && options[["run"]])
  {
      if(is.null(jaspResults[["sampletable"]]))
          .priorSampleTable(options, planningResult, jaspResults, position = 8)
  }

  # Decision plot
  if(options[['plotCriticalErrors']] && options[["run"]])
  {
      if(is.null(jaspResults[["criticalErrorPlot"]]))
      {
          allowed.errors <- 0:(max.errors-1)
          reject.errors <- max.errors : (max.errors + 2)
          jaspResults[["criticalErrorPlot"]] 		   <- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
          jaspResults[["criticalErrorPlot"]]		   $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "distribution", "N",
                                                                      "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors"))
          jaspResults[["criticalErrorPlot"]] 		   $position <- 9
      }
  }

  # Prior plot
  if(options[['plotPrior']] && options[["run"]])
  {
      if(is.null(jaspResults[["priorPlot"]]))
      {
          jaspResults[["priorPlot"]] 		           <- .plotPriorBayesianAttributesPlanningFullAudit(options, planningResult, jaspResults)
          jaspResults[["priorPlot"]]		          $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                           "plotPrior", "plotPriorAdditionalInfo", "show", "prior", "distribution", "N",
                                                           "statistic", "kPercentageNumber", "kNumberNumber"))
          jaspResults[["priorPlot"]] 		          $position <- 10
      }
  }
}
