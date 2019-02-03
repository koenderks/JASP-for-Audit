bayesianAudit <- function(jaspResults, dataset, options, state=NULL){

    # Set the default internal option for "statistic" to bound
    options[["statistic"]]              <- "bound"
    options[["show"]]                   <- "percentage"

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
      confidenceLevelLabel            <- round(options[["confidence"]], 2)
      materialityLevelLabel           <- round(options[["materiality"]], 2)
      if(options[["show"]] == "percentage"){
        confidenceLevelLabel            <- paste0(confidenceLevelLabel * 100, "%")
        materialityLevelLabel           <- paste0(materialityLevelLabel * 100, "%")
      }
      if(options[["auditType"]] == "attributes"){
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of an attributes sampling procedure is to determine with a specified <b>", confidenceLevelLabel ,"</b> confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality of <b>", materialityLevelLabel ,"</b>. An attributes bound procedure considers
                                                                  the observations in the population to be of one of two categories: 1) the observation is fully correct or 2) the observation is
                                                                  fully incorrect."), "p")
      } else if(options[["auditType"]] == "mus"){
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a monetary unit sampling (MUS) procedure is to determine with a specified <b>", confidenceLevelLabel ,"</b> confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality of <b>", materialityLevelLabel ,"</b>. A monetary unit sampling procedure considers the
                                                                  errors <i>(taintings)</i> in the population to be proportional to the size of the observation, so that the taintings lie between 0 and 1."), "p")
      }
      jaspResults[["procedureParagraph"]]$position <- 2
    }

    if(options[["recordNumberVariable"]] != "" && options[["monetaryVariable"]] != ""){
      dataset <- .readDataSetToEnd(columns.as.numeric = c(options[["recordNumberVariable"]], options[["monetaryVariable"]]))
      options[["N"]] <- nrow(dataset)

      .dataTable(dataset, options, jaspResults, position = 3)

      # Planning phase
      if(!options[["planningChecked"]]) # Only runs when "To planning" is clicked
        return()

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
      planningResult              <- jaspResults[["planningResult"]]$object
      .bayesianAttributesPlanningTableFullAudit(options, planningResult, jaspResults, position = 8)

      if(options[["expected.errors"]] == "kPercentage"){
          expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
          max.errors <- floor(options[["kPercentageNumber"]] * planningResult[["n"]]) + 1
      } else {
          expected.errors <- options[["kNumberNumber"]]
          max.errors <- options[["kNumberNumber"]] + 1
      }

      # Interpretation after the planning table
      if(options[["interpretation"]]){

          jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The probability distribution that corresponds with
                                                                        this prior knowledge is the <b>Beta(",round(planningResult[["priorA"]],2), ",", round(planningResult[["priorB"]],2),")</b> distribution. This probability distribution states that there is a <b>",
                                                                            round(pbeta(options[["materiality"]], planningResult[["priorA"]],planningResult[["priorB"]])*100, 2) ,"%</b> prior probability that the
                                                                        population error is lower than materiality. The sample size that is required to prove an <b>", materialityLevelLabel ,"</b> upper confidence bound, assuming
                                                                        the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. Consequently, if <b>", max.errors ,"</b> or more full errors are found, the population cannot be approved."), "p")
          jaspResults[["priorKnowledgeParagraph"]]$position <- 7
      }

      # Implicit sample table
      if (options[["implicitsample"]])
      {
          if(is.null(jaspResults[["sampletable"]]))
              .priorSampleTable(options, planningResult, jaspResults, position = 9)
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

      # Distribution plot
      if(options[['distributionPlot']])
      {
          if(is.null(jaspResults[["valueDistributionPlot"]]))
          {
              jaspResults[["valueDistributionPlot"]] 		<- .plotValueDistribution(dataset, options, jaspResults)
              jaspResults[["valueDistributionPlot"]]		  $dependOnOptions(c("distributionPlot", "showCumulative", "monetaryVariable", "recordNumberVariable", "showHistogram"))
              jaspResults[["valueDistributionPlot"]] 		$position <- 9
          }
        }

      # Prior plot
      if(options[['plotPrior']])
      {
          if(is.null(jaspResults[["priorPlot"]]))
          {
              jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanningFullAudit(options, planningResult, jaspResults)
              jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                               "plotPrior", "plotPriorAdditionalInfo", "show", "prior", "distribution",
                                                               "statistic", "kPercentageNumber", "kNumberNumber", "N"))
              jaspResults[["priorPlot"]] 		$position <- 11
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
        jaspResults[["samplingHeader"]]$position <- 12

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
            jaspResults[["samplingParagraph"]]$position <- 13
        }

        type <- options[["auditType"]]

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
          .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 16)
        }
    }

    # Evaluation phase
    if(!options[["evaluationChecked"]])
      return()
    # only runs when an error variable has been selected
    if(!is.null(correctID)){

      jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
      jaspResults[["evaluationHeader"]]$position <- 17

      # Apply the sample filter
      if(!is.null(sampleFilter)){
          dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
      }

      if(type == "attributes"){
        # Perform the attributes evaluation
        .bayesianAttributesBoundFullAudit(dataset, options, jaspResults)
        result                                       <- jaspResults[["result"]]$object
        .bayesianAttributesBoundTableFullAudit(options, result, jaspResults, position = 19)
      } else {
        # Perform the mus evaluation
        if(options[["boundMethodMUS"]] == "coxAndSnellBound"){
          # Prior parameters for pi and mu are recommendations from the paper
          .coxAndSnellBound(dataset, options, jaspResults, priorPi = 0.1, priorMu = 0.4, priorA = planningResult[["priorA"]], priorB = planningResult[["priorB"]])
        }
        result                                       <- jaspResults[["result"]]$object
        .bayesianMusBoundTableFullAudit(options, result, jaspResults, position = 19)
      }

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
      if(options[['plotPriorAndPosterior']] && !is.null(correctID))
      {
          if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
          {
            if(type == "attributes"){
              jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBoundFullAudit(options, result, jaspResults)
            } else {
              jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianMUSBoundFullAudit(options, result, jaspResults)
            }
            jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "statistic", "plotPriorAndPosterior",
                                                                       "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                       "expected.errors", "kPercentageNumber", "kNumberNumber", "prior", "sampleFilter",
                                                                       "distribution", "N", "correctMUS", "sampleFilterMUS"))
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
