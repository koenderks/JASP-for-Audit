bayesianAudit <- function(jaspResults, dataset, options, state=NULL){

    # Set the default internal option for "statistic" to bound
    options[["statistic"]]              <- "bound"

    if(is.null(state))
        state 							            <- list()

    # Specify the title of the analysis
    jaspResults$title                   <- "Full Bayesian Audit"

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
      .bayesianAttributesPlanningFullAudit(options, jaspResults)
      result              <- jaspResults[["result"]]$object
      .bayesianAttributesPlanningTableFullAudit(options, result, jaspResults, position = 8)

      if(options[["expected.errors"]] == "kPercentage"){
          expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
          max.errors <- floor(options[["kPercentageNumber"]] * result[["n"]]) + 1
      } else {
          expected.errors <- options[["kNumberNumber"]]
          max.errors <- options[["kNumberNumber"]] + 1
      }

      # Interpretation after the planning table
      if(options[["interpretation"]]){

          jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The probability distribution that corresponds with
                                                                        this prior knowledge is the <b>Beta(",round(result[["priorA"]],2), ",", round(result[["priorB"]],2),")</b> distribution. This probability distribution states that there is a <b>",
                                                                            round(pbeta(options[["materiality"]], result[["priorA"]],result[["priorB"]])*100, 2) ,"%</b> prior probability that the
                                                                        population error is lower than materiality. The sample size that is required to prove an <b>", materialityLevelLabel ,"</b> upper confidence bound, assuming
                                                                        the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. Consequently, if <b>", max.errors ,"</b> or more full errors are found, the population cannot be approved."), "p")
          jaspResults[["priorKnowledgeParagraph"]]$position <- 7
      }

      # Implicit sample table
      if (options[["implicitsample"]])
      {
          if(is.null(jaspResults[["sampletable"]]))
              .priorSampleTable(options, result, jaspResults, position = 9)
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
                                                                          "show", "statistic", "kPercentageNumber", "kNumberNumber",
                                                                          "plotCriticalErrors", "prior", "distribution", "N"))
              jaspResults[["criticalErrorPlot"]] 		$position <- 10
          }
      }

      # Prior plot
      if(options[['plotPrior']])
      {
          if(is.null(jaspResults[["priorPlot"]]))
          {
              jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanningFullAudit(options, result, jaspResults)
              jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                               "plotPrior", "plotPrior", "show", "prior", "distribution",
                                                               "statistic", "kPercentageNumber", "kNumberNumber", "N"))
              jaspResults[["priorPlot"]] 		$position <- 11
          }
      }
  }

    # Sampling phase
    # Only runs when a record variable has been specified
    if(options[["recordNumberVariable"]] != ""){

        # Keep the resulting sample size as an option
        options[["sampleSize"]] <- result[["n"]]

        # Read in variables for sampling TODO: Make this a function
        variables                       <- unlist(options$variables)
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        rankingVariable                 <- unlist(options$rankingVariable)
        if(rankingVariable == "")       rankingVariable <- NULL
        correctID                       <- unlist(options$correctID)
        if(correctID == "")             correctID <- NULL
        sampleFilter                    <- unlist(options$sampleFilter)
        if(sampleFilter == "")          sampleFilter <- NULL
        variables.to.read               <- c(recordVariable, variables, rankingVariable, correctID, sampleFilter)

        if (is.null(dataset))
            dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

        jaspResults[["samplingHeader"]] <- createJaspHtml("<u>Sampling</u>", "h2")
        jaspResults[["samplingHeader"]]$position <- 12

        # Interpretation for the sampling phase
        if(options[["interpretation"]]){
            # Adjust the technique TODO: Use switch function
            if(options[["samplingType"]] == "simplerandomsampling"){
                technique <- "simple random sampling"
            } else if(options[["samplingType"]] == "systematicsampling"){
                technique <- "systematic sampling"
            } else if(options[["samplingType"]] == "cellsampling"){
                technique <- "cell sampling"
            }
            jaspResults[["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", options[["N"]], "</b> observations, <b>", result[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
            jaspResults[["samplingParagraph"]]$position <- 13
        }

        # Perform the sampling and draw the outcome tables
        if(options[["samplingType"]] == "simplerandomsampling"){
            .SimpleRandomSamplingTable(dataset, options, jaspResults, position = 14)
        } else if(options[["samplingType"]] == "systematicsampling"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 14)
            .SystematicSamplingTable(dataset, options, jaspResults, interval, position = 15)
        } else if(options[["samplingType"]] == "cellsampling"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 14)
            .cellSamplingTable(dataset, options, jaspResults, interval, position = 15)
        }
        # Store the sample
        sample                          <- jaspResults[["sample"]]$object

        # Descriptives table
        if(options[["showDescriptives"]]){
          .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 16)
        }
    }

    # Evaluation phase
    # only runs when an error variable has been selected
    if(options[["correctID"]] != ""){

        # Apply the sample filter
        if(options[["sampleFilter"]] != ""){
            dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
        }

        jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
        jaspResults[["evaluationHeader"]]$position <- 17

        # Perform the evaluation
        .bayesianAttributesBoundFullAudit(dataset, options, jaspResults)
        result                                       <- jaspResults[["result"]]$object
        .bayesianAttributesBoundTableFullAudit(options, result, jaspResults, position = 19)

        # Interpretation before the evalution table
        if(options[["interpretation"]]){
          if(options[["show"]] == "percentage"){
            boundLabel <- paste0(round(result[["bound"]] * 100, 2), "%")
          } else {
            boundLabel <- round(result[["bound"]], 2)
          }
          jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>",result[["k"]], "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                                bined with the prior knowledge results in an <b>", confidenceLevelLabel , "</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                                is a true probability of <b>", confidenceLevelLabel , "</b> that the error proportion in the population is lower than <b>", boundLabel ,"</b>."), "p")
          jaspResults[["resultParagraph"]]$position <- 18
        }

        # Prior and Posterior plot
        if(options[['plotPriorAndPosterior']] && options[["correctID"]] != "")
        {
            if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
            {
                jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBoundFullAudit(options, result, jaspResults)
                jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "statistic", "plotPriorAndPosterior",
                                                                           "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                           "expected.errors", "kPercentageNumber", "kNumberNumber", "prior", "sampleFilter",
                                                                           "distribution", "N"))
                jaspResults[["priorAndPosteriorPlot"]] 		$position <- 20
            }
        }

        # Interpretation after the evaluation table
        if(options[["interpretation"]]){
            jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
            jaspResults[["conclusionTitle"]]$position <- 21
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
            jaspResults[["conclusionParagraph"]]$position <- 22
        }
    }

    # Save the state
    state[["options"]] 					<- options
    return(state)
}
