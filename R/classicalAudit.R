classicalAudit <- function(jaspResults, dataset, options, state=NULL){

    options[["show"]]                   <- "percentage"

    if(is.null(state))
        state 							    <- list()

    # Specify the title of the analysis
    jaspResults$title   <- "Audit"

    # Headers for the sub-analyses
    if(options[["auditType"]] == "attributes"){
      jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Attributes Procedure</u>", "h2")
      jaspResults[["procedureHeader"]]$position <- 1
    } else {
      jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Monetary Unit Procedure</u>", "h2")
      jaspResults[["procedureHeader"]]$position <- 1
    }
    jaspResults[["procedureHeader"]]$dependOnOptions(c("none"))

    # Interpretation for the Global Options phase
    if(options[["interpretation"]]){
      if(options[["auditType"]] == "attributes"){
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of an attributes procedure is to determine with a specified confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality. An attributes procedure considers the
                                                                  observations in the population to be of one of two categories: 1) the observation is fully correct or 2) the observation is
                                                                  fully incorrect."), "p")
      } else if(options[["auditType"]] == "mus"){
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a monetary unit (MUS) procedure is to determine with a specified confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality. A monetary unit procedure considers the
                                                                  errors <i>(taintings)</i> in the population to be proportional to the size of the observation, so that the taintings lie between 0 and 1."), "p")
      }
      jaspResults[["procedureParagraph"]]$position <- 2
      jaspResults[["procedureParagraph"]]$dependOnOptions(c("auditType", "interpretation"))
    }

    if(options[["recordNumberVariable"]] == "" || options[["monetaryVariable"]] == ""){
      .dataTable(dataset, options, jaspResults, position = 3)
    } else {

      dataset             <- .readDataSetToEnd(columns.as.numeric = c(options[["recordNumberVariable"]], options[["monetaryVariable"]]))
      options[["N"]]      <- nrow(dataset)
      total_data_value    <- ceiling(sum(dataset[, .v(options[["monetaryVariable"]])]))

      .dataTable(dataset, options, jaspResults, position = 3)

      # Distribution plot
      if(options[['distributionPlot']])
      {
          if(is.null(jaspResults[["valueDistributionPlot"]]))
          {
              jaspResults[["valueDistributionPlot"]] 		<- .plotValueDistribution(dataset, options, jaspResults)
              jaspResults[["valueDistributionPlot"]]		  $dependOnOptions(c("distributionPlot", "monetaryVariable", "recordNumberVariable", "interpretation"))
              jaspResults[["valueDistributionPlot"]] 		$position <- 4
          }
        }

      # Planning phase
      if(!options[["planningChecked"]]) # Only runs when "To planning" is clicked
        return()

      # Show the Audit Risk Model formula and quantify detection risk
      .ARMformula(options, jaspResults, position = 6)
      DR                          <- jaspResults[["DR"]]$object

      # Read in alternative materiality (based on value)
      if(options[["auditType"]] == "mus")
          options[["materiality"]] <- options[["materialityValue"]] / total_data_value

      # Create labels for the confidence and materiality
      confidenceLevelLabel              <- round(options[["confidence"]], 2)
      materialityLevelLabel             <- round(options[["materiality"]], 2)
      if(options[["show"]] == "percentage"){
        confidenceLevelLabel            <- paste0(confidenceLevelLabel * 100, "%")
        materialityLevelLabelBackup     <- paste0(materialityLevelLabel * 100, "%")
        materialityLevelLabel           <- base::switch(options[["auditType"]],
                                                        "attributes" = paste0(materialityLevelLabel * 100, "%"),
                                                        "mus" = paste0(options[["materialityValue"]], "</b> monetary units <b>(", materialityLevelLabelBackup,")</b><b>"))
      }

      # Interpretation before the planning table
      if(options[["interpretation"]]){
        if(options[["show"]] == "percentage"){
          auditRiskLabel          <- paste0(round((1 - options[["confidence"]]) * 100, 2), "%")
          dectectionRiskLabel     <- paste0(round(DR * 100, 2), "%")
        } else {
          auditRiskLabel          <- round((1 - options[["confidence"]]), 2)
          dectectionRiskLabel     <- round(DR, 2)
        }
        jaspResults[["AuditRiskModelHeader"]] <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
        jaspResults[["AuditRiskModelHeader"]]$position <- 5
        jaspResults[["AuditRiskModelHeader"]]$dependOnOptions(c("none"))
        jaspResults[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>", options[["IR"]] ,"</b>. The internal control risk was determined
                                                                        to be <b>", options[["CR"]] ,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", auditRiskLabel, "</b> for a materiality
                                                                        of <b>", materialityLevelLabel ,"</b> should be <b>", dectectionRiskLabel , "</b>."), "p")
        jaspResults[["AuditRiskModelParagraph"]]$position <- 7
        jaspResults[["AuditRiskModelParagraph"]]$dependOnOptions(c("confidence", "IR", "CR", "materiality", "materialityValue"))
      }

      jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]$position <- 8
      jaspResults[["priorKnowledgeHeader"]]$dependOnOptions(c("none"))

      # Perform the planning
      .attributesPlanningFullAudit(options, jaspResults)
      planningResult              <- jaspResults[["planningResult"]]$object
      .attributesPlanningTableFullAudit(dataset, options, planningResult, jaspResults, position = 10)

      if(options[["expected.errors"]] == "kPercentage"){
          expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
          max.errors <- ceiling(options[["kPercentageNumber"]] * planningResult[["n"]]) + 1
      } else {
          expected.errors <- options[["kNumberNumber"]]
          max.errors <- options[["kNumberNumber"]] + 1
      }

      # Interpretation after the planning table
      if(options[["interpretation"]]){

          jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The sample size that is required to prove an upper
                                                                            confidence bound of <b>", materialityLevelLabel ,"</b>, assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. This sample size is
                                                                            calculated according to the <b>", options[["distribution"]] , "</b> distribution. Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the projected
                                                                            misstatement exceeds the upper confidence bound and the population cannot be approved."), "p")
          jaspResults[["priorKnowledgeParagraph"]]$position <- 9
          jaspResults[["priorKnowledgeParagraph"]]$dependOnOptions(c("kPercentageNumber", "expected.errors", "kNumberNumber", "distribution", "IR", "CR", "materiality", "N",
                                                                  "confidence", "materialityValue"))
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
                                                                          "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors",
                                                                          "distribution", "N", "materialityValue"))
              jaspResults[["criticalErrorPlot"]] 		$position <- 11
          }
        }
    }

    # Sampling phase
    if(!options[["samplingChecked"]])
      return()
    # Read in variables for sampling TODO: Make this a function
    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")       rankingVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    variables                       <- unlist(options$variables)
    sampleFilter                    <- unlist(options$sampleFilter)
    if(sampleFilter == "")          sampleFilter <- NULL
    correctID                       <- base::switch(options[["auditType"]], "attributes" = unlist(options$correctID), "mus" = unlist(options$correctMUS))
    if(correctID == "")             correctID <- NULL
    variables.to.read               <- c(recordVariable, variables, rankingVariable, correctID, sampleFilter, monetaryVariable)
    dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

    # Only runs when a record variable has been specified
      if(!is.null(recordVariable)){

        # Keep the resulting sample size as an option
        options[["sampleSize"]] <- planningResult[["n"]]

        jaspResults[["samplingHeader"]] <- createJaspHtml("<u>Sampling</u>", "h2")
        jaspResults[["samplingHeader"]]$position <- 13
        jaspResults[["samplingHeader"]]$dependOnOptions(c("none"))

        # Interpretation for the sampling phase
        if(options[["interpretation"]]){
            technique <- base::switch(options[["samplingType"]],
                                        "simplerandomsampling" = "simple random",
                                        "systematicsampling" = "systematic",
                                        "cellsampling" = "cell")
            technique <- base::switch(options[["auditType"]],
                                        "attributes" = paste(technique, "attributes sampling"),
                                        "mus" = paste(technique, "MUS sampling"))
            jaspResults[["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", options[["N"]], "</b> observations, <b>", planningResult[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
            jaspResults[["samplingParagraph"]]$position <- 14
            jaspResults[["samplingParagraph"]]$dependOnOptions(c("sampleSize", "N", "samplingType"))
        }

        type <- options[["auditType"]]

        # Perform the sampling and draw the outcome tables
        if(options[["samplingType"]] == "simplerandomsampling"){
          if(type == "attributes"){
            .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "attributes", sample = jaspResults[["sample"]]$object, position = 16)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 15)
          } else {
            .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "mus", sample = jaspResults[["sample"]]$object, position = 16)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 15)
          }
        } else if(options[["samplingType"]] == "systematicsampling"){
          if(type == "attributes"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 16)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 15, interval = interval)
          } else {
            interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
            .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 16)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 15, interval = interval)
          }
        } else if(options[["samplingType"]] == "cellsampling"){
          if(type == "attributes"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .cellSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 16)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 15, interval = interval)
          } else {
            interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
            .cellSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 16)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 15, interval = interval)
          }
        }
        # Store the sample
        sample                          <- jaspResults[["sample"]]$object

        # Descriptives table
        if(options[["showDescriptives"]]){
          .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 17)
        }
    }

    # Evaluation phase
    if(!options[["evaluationChecked"]])
      return()

    jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
    jaspResults[["evaluationHeader"]]$position <- 18
    jaspResults[["evaluationHeader"]]$dependOnOptions(c("none"))

    runEvaluation <- (!is.null(correctID) && !is.null(sampleFilter))
    # Apply the sample filter
    if(runEvaluation){
        dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
    }

    if(type == "attributes"){
      # Perform the attributes evaluation
      .attributesBoundFullAudit(dataset, options, jaspResults)
      result                                       <- jaspResults[["result"]]$object
      .attributesBoundTableFullAudit(options, result, jaspResults, position = 20)
    } else {
      # Perform the MUS evaluaton
      if(options[["boundMethodMUS"]] == "stringerBound"){
        .stringerBound(dataset, options, jaspResults)
      }
      result                                       <- jaspResults[["result"]]$object
      .musBoundTableFullAudit(total_data_value, options, result, jaspResults, position = 20)
    }

    # Interpretation before the evalution table
    if(options[["interpretation"]]){
      boundLabel <- base::switch(runEvaluation,
                                  "TRUE" = paste0(round(result[["bound"]] * 100, 2), "%"),
                                  "FALSE" = "...%")
      jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>", result[["k"]] , "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                            bined with the prior knowledge results in an <b>", round((1 - result[["alpha"]]) * 100, 2) , "%</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                            is a <b>", confidenceLevelLabel , "</b> probability that, when one would repeaditly sample from this population, the maximum error is calculated to be lower
                                                            than <b>", boundLabel ,"</b>."), "p")
      jaspResults[["resultParagraph"]]$position <- 19
      jaspResults[["resultParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                               "method", "materialityValue"))
    }

    # Confidence bound plot
    if(options[['plotBound']] && runEvaluation)
    {
        if(is.null(jaspResults[["confidenceBoundPlot"]]))
        {
            jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
            jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                                     "method", "materialityValue", "correctMUS"))
            jaspResults[["confidenceBoundPlot"]] 		$position <- 21
        }
    }

    # Interpretation after the evaluation table
    if(options[["interpretation"]] && runEvaluation){
        jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
        jaspResults[["conclusionTitle"]]$position <- 22
        jaspResults[["conclusionTitle"]]$dependOnOptions(c("none"))

        if(result[["bound"]] < options[["materiality"]]){
            above_below <- "lower"
            approve <- "<b>no material misstatement</b>"
        } else if(result[["bound"]] >= options[["materiality"]]){
            above_below <- "higher"
            approve <- "<b>material misstatement, or more information has to be seen.</b>"
        }
        jaspResults[["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", confidenceLevelLabel ,"</b> upper confidence bound on the population proportion of full errors should be determined to be
                                                                    lower than materiality, in this case <b>", materialityLevelLabel ,"</b>. For the current data, the confidence bound is <b>", above_below ,"</b> than materiality.
                                                                    The conclusion for these data is that the data contain ", approve ,"."), "p")
        jaspResults[["conclusionParagraph"]]$position <- 23
        jaspResults[["conclusionParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                                 "method", "materialityValue", "correctMUS"))
    }

    # Save the state
    state[["options"]] 					<- options
    return(state)
}
