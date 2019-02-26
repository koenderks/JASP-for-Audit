classicalPlanning <- function(jaspResults, dataset, options, state=NULL){

  # Set the title
  jaspResults$title 					                              <- "Planning"

  # Interpretation for the Global Options phase
  if(options[["interpretation"]]){
    # Headers for the sub-analyses
    jaspResults[["procedureHeader"]]                        <- createJaspHtml("<u>Procedure</u>", "h2")
    jaspResults[["procedureHeader"]]                        $position <- 1
    jaspResults[["procedureHeader"]]                        $dependOnOptions(c("interpretation"))
    confidenceLevelLabel                                    <- paste0(round(options[["confidence"]] * 100, 2), "%")
    jaspResults[["procedureParagraph"]]                     <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", confidenceLevelLabel, ")</b>
                                                                  whether the amount of misstatement in the target population is lower than the specified materiality."), "p")
    jaspResults[["procedureParagraph"]]                     $position <- 2
    jaspResults[["procedureParagraph"]]                     $dependOnOptions(c("interpretation", "confidence"))
  }

  .ARMformula(options, jaspResults, position = 5)   # Show the Audit Risk Model formula and quantify detection risk
  DR                                                        <- jaspResults[["DR"]]$object

  populationValue                                           <- options[["populationValue"]]
  if(options[["populationValue"]] == 0)
    populationValue                                         <- 0.01

  # Rewrite materiality based on value
  if(options[["auditType"]] == "mus")
      options[["materiality"]]                              <- options[["materialityValue"]] / populationValue

  # Create labels for the materiality
  materialityLevelLabel                                     <- base::switch(options[["auditType"]],
                                                                              "attributes" = paste0(round(options[["materiality"]], 4) * 100, "%"),
                                                                              "mus" = options[["materialityValue"]])

  options[["run"]]                                          <- (options[["materiality"]] != 0 && (populationValue != 0 && populationValue != 0.01) && options[["N"]] != 0)

  if(options[["interpretation"]]){
      jaspResults[["AuditRiskModelHeader"]]                 <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
      jaspResults[["AuditRiskModelHeader"]]                 $position <- 3
      jaspResults[["procedureParagraph"]]                   $dependOnOptions(c("interpretation"))
      jaspResults[["AuditRiskModelParagraph"]]              <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined
                                                                                    to be <b>", options$CR,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be <b>",round(DR*100, 2), "%</b>."), "p")
      jaspResults[["AuditRiskModelParagraph"]]              $position <- 4

      jaspResults[["priorKnowledgeHeader"]]                 <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]                 $position <- 6
  }
  # Perform the analysis
  .attributesPlanningFullAudit(options, jaspResults)
  planningResult                                            <- jaspResults[["planningResult"]]$object
  .attributesPlanningTableFullAudit(dataset, options, planningResult, jaspResults, position = 8)

  if(options[["expected.errors"]] == "kPercentage"){
      expected.errors                                       <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
      max.errors                                            <- ceiling(options[["kPercentageNumber"]] * planningResult[["n"]]) + 1
  } else {
      expected.errors                                       <- options[["kNumberNumber"]]
      max.errors                                            <- options[["kNumberNumber"]] + 1
  }

  if(options[["interpretation"]]){

      jaspResults[["priorKnowledgeParagraph"]]              <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The sample size that is required to prove an <b>",options$materiality*100,"%</b>
                                                                      upper confidence bound, assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. The sample size is calculated with the <b>", options[["distribution"]] , "</b>
                                                                      distribution. Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the population cannot be approved."), "p")
      jaspResults[["priorKnowledgeParagraph"]]              $position <- 7

  }

  # Decision plot
  if(options[['plotCriticalErrors']] && options[["run"]])
  {
      if(is.null(jaspResults[["criticalErrorPlot"]]))
      {
          allowed.errors <- 0:(max.errors-1)
          reject.errors <- max.errors : (max.errors + 2)
          jaspResults[["criticalErrorPlot"]] 		           <- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
          jaspResults[["criticalErrorPlot"]]		          $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors",
                                                                      "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors"))
          jaspResults[["criticalErrorPlot"]] 		          $position <- 9
      }
  }
}
