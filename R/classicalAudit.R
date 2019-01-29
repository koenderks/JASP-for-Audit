classicalAudit <- function(jaspResults, dataset, options, state=NULL){

    if(is.null(state))
        state 							    <- list()

    # Specify the title of the analysis
    jaspResults$title   <- "Full Audit"

    # Headers for the sub-analyses
    if(options[["auditType"]] == "attributes"){
      jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Attributes Procedure</u>", "h2")
      jaspResults[["procedureHeader"]]$position <- 1
    } else {
      jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Monetary Unit Procedure</u>", "h2")
      jaspResults[["procedureHeader"]]$position <- 1
    }

    # Interpretation for the Global Options phase
    if(options[["interpretation"]]){
      if(options[["show"]] == "percentage"){
        confidenceLevelLabel            <- paste0(round(options[["confidence"]] * 100, 2), "%")
        materialityLevelLabel           <- paste0(round(options[["materiality"]] * 100, 2), "%")
      } else {
        confidenceLevelLabel            <- round(options[["confidence"]], 2)
        materialityLevelLabel           <- round(options[["materiality"]], 2)
      }
      if(options[["auditType"]] == "attributes"){
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of an attributes sampling procedure is to determine with a specified <b>", confidenceLevelLabel ,"</b> confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality of <b>", materialityLevelLabel ,"</b>. An attributes bound procedure considers
                                                                  the observations in the population to be of one of two categories: 1) the observation is fully correct or 2) the observation is
                                                                  fully incorrect."), "p")
        jaspResults[["procedureParagraph"]]$position <- 2
      } else if(options[["auditType"]] == "mus"){
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a monetary unit sampling (MUS) procedure is to determine with a specified <b>", confidenceLevelLabel ,"</b> confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality of <b>", materialityLevelLabel ,"</b>. A monetary unit sampling procedure considers the
                                                                  errors <i>(taintings)</i> in the population to be proportional to the size of the observation, so that the taintings lie between 0 and 1."), "p")
        jaspResults[["procedureParagraph"]]$position <- 2
      }
    }

    # Planning phase
    # Only runs when population size is specified
    if(options[["N"]] != 0){

      # Show the Audit Risk Model formula and quantify detection risk
      .ARMformula(options, jaspResults, position = 5)
      DR                          <- jaspResults[["DR"]]$object

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
        jaspResults[["AuditRiskModelHeader"]]$position <- 3
        jaspResults[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options[["IR"]],"</b>. The internal control risk was determined
                                                                        to be <b>", options[["CR"]],"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", auditRiskLabel, "</b> should be <b>", dectectionRiskLabel , "</b>."), "p")
        jaspResults[["AuditRiskModelParagraph"]]$position <- 4
      }

      jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]$position <- 6

      # Perform the planning
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

      # Interpretation after the planning table
      if(options[["interpretation"]]){

          jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The sample size that is required to prove an <b>", materialityLevelLabel ,"</b>
                                                                          upper confidence bound, assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. This sample size is calculated with the <b>", options[["distribution"]] , "</b>
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
                                                                          "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors",
                                                                          "distribution", "N"))
              jaspResults[["criticalErrorPlot"]] 		$position <- 9
          }
        }
    }

    # Sampling phase

    # Read in variables for sampling TODO: Make this a function
    if(options[["auditType"]] == "attributes"){
      recordVariable                  <- unlist(options$recordNumberVariable)
      if(recordVariable == "")        recordVariable <- NULL
      rankingVariable                 <- unlist(options$rankingVariable)
      if(rankingVariable == "")       rankingVariable <- NULL
      monetaryVariable                <- NULL
      variables                       <- unlist(options$variables)
      correctID                       <- unlist(options$correctID)
      if(correctID == "")             correctID <- NULL
      sampleFilter                    <- unlist(options$sampleFilter)
      if(sampleFilter == "")          sampleFilter <- NULL
    } else {
      recordVariable                  <- unlist(options$recordNumberVariableMUS)
      if(recordVariable == "")        recordVariable <- NULL
      monetaryVariable                <- unlist(options$monetaryVariableMUS)
      if(monetaryVariable == "")      monetaryVariable <- NULL
      rankingVariable                 <- unlist(options$rankingVariableMUS)
      if(rankingVariable == "")       rankingVariable <- NULL
      variables                       <- unlist(options$variablesMUS)
      correctID                       <- unlist(options$correctMUS)
      if(correctID == "")             correctID <- NULL
      sampleFilter                    <- unlist(options$sampleFilterMUS)
      if(sampleFilter == "")          sampleFilter <- NULL
    }
    variables.to.read               <- c(recordVariable, variables, rankingVariable, correctID, sampleFilter, monetaryVariable)

    if (is.null(dataset))
        dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

    # Only runs when a record variable has been specified
      if(!is.null(recordVariable)){

        # Keep the resulting sample size as an option
        options[["sampleSize"]] <- result[["n"]]

        jaspResults[["samplingHeader"]] <- createJaspHtml("<u>Sampling</u>", "h2")
        jaspResults[["samplingHeader"]]$position <- 11

        # Interpretation for the sampling phase
        if(options[["interpretation"]]){
            # Adjust the technique TODO: Use switch function
            if(options[["samplingType"]] == "simplerandomsampling"){
                technique <- "simple random"
            } else if(options[["samplingType"]] == "systematicsampling"){
                technique <- "systematic"
            } else if(options[["samplingType"]] == "cellsampling"){
                technique <- "cell"
            }
            if(options[["auditType"]] == "attributes"){
              technique <- paste(technique, "attributes sampling")
            } else {
              technique <- paste(technique, "MUS sampling")
            }
            jaspResults[["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", options[["N"]], "</b> observations, <b>", result[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
            jaspResults[["samplingParagraph"]]$position <- 12
        }

        if(options[["auditType"]] == "attributes"){
          type <- "attributes"
        } else {
          type <- "mus"
        }

        # Perform the sampling and draw the outcome tables
        if(options[["samplingType"]] == "simplerandomsampling"){
          if(type == "attributes"){
            .simpleRandomSamplingInfoTable(dataset, options, jaspResults, position = 13)
            .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "attributes", sample = jaspResults[["sample"]]$object, position = 14)
          } else {
            if(!is.null(monetaryVariable)){
              .simpleRandomSamplingInfoTable(dataset, options, jaspResults, position = 13)
              .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "mus", sample = jaspResults[["sample"]]$object, position = 14)
            }
          }
        } else if(options[["samplingType"]] == "systematicsampling"){
          if(type == "attributes"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 13)
            .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 14)
          } else {
            if(!is.null(monetaryVariable)){
              interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
              .intervalTable(dataset, options, jaspResults, interval, position = 13)
              .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 14)
            }
          }
        } else if(options[["samplingType"]] == "cellsampling"){
          if(type == "attributes"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 13)
            .cellSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 14)
          } else {
            if(!is.null(monetaryVariable)){
              interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
              .intervalTable(dataset, options, jaspResults, interval, position = 13)
              .cellSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 14)
            }
          }
        }
        # Store the sample
        sample                          <- jaspResults[["sample"]]$object

        # Descriptives table
        if(options[["showDescriptives"]]){
          .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 15)
        }
    }

    # Evaluation phase
    # only runs when an error variable has been selected
    if(!is.null(correctID)){

      jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
      jaspResults[["evaluationHeader"]]$position <- 16

      # Apply the sample filter
      if(options[["sampleFilter"]] != ""){
          dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
      }

      if(type == "attributes"){
        # Perform the attributes evaluation
        .attributesBoundFullAudit(dataset, options, jaspResults)
        result                                       <- jaspResults[["result"]]$object
        .attributesBoundTableFullAudit(options, result, jaspResults, position = 18)
      } else {
        # Perform the MUS evaluaton
        if(options[["boundMethodMUS"]] == "stringerBound"){
          .stringerBound(dataset, options, jaspResults)
        }
        result                                       <- jaspResults[["result"]]$object
        .musBoundTableFullAudit(options, result, jaspResults, position = 18)
      }

      # Interpretation before the evalution table
      if(options[["interpretation"]]){
        if(options[["show"]] == "percentage"){
          boundLabel <- paste0(round(result[["bound"]] * 100, 2), "%")
        } else {
          boundLabel <- round(result[["bound"]], 2)
        }
        jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>",options[["sampleSize"]], "</b> observations, <b>", result[["k"]] , "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                              bined with the prior knowledge results in an <b>",round((1 - result[["alpha"]]) * 100, 2), "%</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                              is a <b>", confidenceLevelLabel , "</b> probability that, when one would repeaditly sample from this population, the maximum error is calculated to be lower
                                                              than <b>", boundLabel ,"</b>."), "p")
        jaspResults[["resultParagraph"]]$position <- 17
      }

      # Confidence bound plot TODO: Adjust width and height of plot
      if(options[['plotBound']] && !is.null(correctID))
      {
          if(is.null(jaspResults[["confidenceBoundPlot"]]))
          {
              jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults, plotWidth = 400, plotHeight = 400)
              jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID",
                                                                       "show", "plotBound", "materiality", "method", "inference"))
              jaspResults[["confidenceBoundPlot"]] 		$position <- 19
          }
      }

      # Interpretation after the evaluation table
      if(options[["interpretation"]]){
          jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
          jaspResults[["conclusionTitle"]]$position <- 20
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
          jaspResults[["conclusionParagraph"]]$position <- 21
      }
    }

    # Save the state
    state[["options"]] 					<- options
    return(state)
}
