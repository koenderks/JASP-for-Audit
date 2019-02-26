bayesianAudit <- function(jaspResults, dataset, options, state=NULL){

    # Set the default internal option for "statistic" to bound
    options[["statistic"]]              <- "bound"
    options[["show"]]                   <- "percentage"

    # Specify the title of the analysis
    jaspResults$title                   <- "Bayesian Sampling Process"

    # Headers for the sub-analyses
    jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Procedure</u>", "h2")
    jaspResults[["procedureHeader"]]$position <- 1
    jaspResults[["procedureHeader"]]$dependOnOptions(c("none"))

    # Interpretation for the Global Options phase
    if(options[["interpretation"]]){
      confidenceLevelLabel              <- paste0(round(options[["confidence"]] * 100, 2), "%")
      jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", confidenceLevelLabel, ")</b>
                                                                    whether the amount of misstatement in the target population is lower than the specified materiality."), "p")
      jaspResults[["procedureParagraph"]]$position <- 2
      jaspResults[["procedureParagraph"]]$dependOnOptions(c("interpretation", "confidence"))
    }

    if(options[["recordNumberVariable"]] != "" && options[["monetaryVariable"]] != ""){
      dataset             <- .readDataSetToEnd(columns.as.numeric = c(options[["recordNumberVariable"]], options[["monetaryVariable"]]))
      options[["N"]]      <- nrow(dataset)
      total_data_value    <- ceiling(sum(dataset[, .v(options[["monetaryVariable"]])]))
      options[["run"]]    <- TRUE
    } else {
      dataset             <- NULL
      options[["N"]]      <- 0
      total_data_value    <- 0.01
      options[["run"]]    <- FALSE
    }

    # Population descriptives table
    .dataTable(dataset, options, jaspResults, position = 3)

    # Population distribution plot
    if(options[['distributionPlot']] && options[["run"]])
    {
        if(is.null(jaspResults[["valueDistributionPlot"]]))
        {
            jaspResults[["valueDistributionPlot"]] 		<- .plotValueDistribution(dataset, options, jaspResults)
            jaspResults[["valueDistributionPlot"]]		$dependOnOptions(c("distributionPlot", "monetaryVariable", "recordNumberVariable"))
            jaspResults[["valueDistributionPlot"]] 		$position <- 4
        }
      }

    #  Audit Risk Model formula
    .ARMformula(options, jaspResults, position = 6)
    DR                          <- jaspResults[["DR"]]$object

    # Rewrite materiality based on value
    if(options[["auditType"]] == "mus")
        options[["materiality"]] <- options[["materialityValue"]] / total_data_value

    # Create labels for the materiality
    materialityLevelLabel           <- base::switch(options[["auditType"]],
                                                    "attributes" = paste0(round(options[["materiality"]], 4) * 100, "%"),
                                                    "mus" = options[["materialityValue"]])

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
    .bayesianAttributesPlanningFullAudit(options, jaspResults)
    planningResult              <- jaspResults[["planningResult"]]$object
    .bayesianAttributesPlanningTableFullAudit(dataset, options, planningResult, jaspResults, position = 10)

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
                                                                          round(pbeta(options[["materiality"]], planningResult[["priorA"]], planningResult[["priorB"]]) * 100, 2) ,"%</b> prior probability that the
                                                                      population error is lower than materiality. The sample size that is required to prove an upper confidence bound of <b>", materialityLevelLabel ,"</b>,
                                                                      assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. This sample size is calculated according to the <b>", options[["distribution"]] , "</b> distribution.
                                                                      Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the projected misstatement exceeds the upper confidence bound
                                                                      and the population cannot be approved."), "p")
        jaspResults[["priorKnowledgeParagraph"]]$position <- 9
        jaspResults[["priorKnowledgeParagraph"]]$dependOnOptions(c("kPercentageNumber", "expected.errors", "kNumberNumber", "distribution", "IR", "CR", "materiality", "N",
                                                                "confidence", "materialityValue"))
    }

    # Implicit sample table
    if (options[["implicitsample"]] && options[["run"]])
    {
        if(is.null(jaspResults[["sampletable"]]))
            .priorSampleTable(options, planningResult, jaspResults, position = 11)
    }

    # Decision plot
    if(options[['plotCriticalErrors']] && options[["run"]])
    {
        if(is.null(jaspResults[["criticalErrorPlot"]]))
        {
            allowed.errors <- 0:(max.errors-1)
            reject.errors <- max.errors : (max.errors + 2)
            jaspResults[["criticalErrorPlot"]] 		<- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
            jaspResults[["criticalErrorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors",
                                                                        "show", "statistic", "kPercentageNumber", "kNumberNumber",
                                                                        "plotCriticalErrors", "prior", "distribution", "N", "materialityValue"))
            jaspResults[["criticalErrorPlot"]] 		$position <- 12
        }
    }

    # Prior plot
    if(options[['plotPrior']] && options[["run"]])
    {
        if(is.null(jaspResults[["priorPlot"]]))
        {
            jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanningFullAudit(options, planningResult, jaspResults)
            jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                             "plotPrior", "plotPriorAdditionalInfo", "show", "prior", "distribution",
                                                             "statistic", "kPercentageNumber", "kNumberNumber", "N", "materialityValue"))
            jaspResults[["priorPlot"]] 		$position <- 13
        }
    }

    # Selection phase (analysis starts when button is clicked)
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
    variables.to.read               <- c(recordVariable, variables, rankingVariable, monetaryVariable)
    dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

    # Only runs when a record variable has been specified
    if(!is.null(recordVariable)){

        # Keep the resulting sample size as an option
        options[["sampleSize"]] <- planningResult[["n"]]

        jaspResults[["samplingHeader"]] <- createJaspHtml("<u>Sampling</u>", "h2")
        jaspResults[["samplingHeader"]]$position <- 14
        jaspResults[["samplingHeader"]]$dependOnOptions(c("none"))

        # Interpretation for the sampling phase
        if(options[["interpretation"]]){
          technique <- base::switch(options[["samplingType"]],
                                      "simplerandomsampling" = "simple random",
                                      "systematicsampling" = "systematic",
                                      "cellsampling" = "cell")
          technique <- base::switch(options[["samplingMethod"]],
                                      "recordsampling" = paste(technique, "record sampling"),
                                      "mussampling" = paste(technique, "MUS sampling"))
            jaspResults[["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", options[["N"]], "</b> observations, <b>", planningResult[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
            jaspResults[["samplingParagraph"]]$position <- 15
            jaspResults[["samplingParagraph"]]$dependOnOptions(c("sampleSize", "N", "samplingType", "samplingMethod"))
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
          .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 18)
        }
    }

    # TODO: Add columns to data instead of replace existing
    if(options[["pasteVariables"]]){
      sampleFilter <- rep(0, options[["N"]])
      sampleFilter[sample[,.v(options[["recordNumberVariable"]])]] <- 1
      sampleFilter <- as.integer(sampleFilter)
      emptyVariable <- rep(NA, options[["N"]])
      .setColumnDataAsNominal("sampleFilter", sampleFilter)
      base::switch(options[["auditType"]],
                    "attributes" = .setColumnDataAsNominal("errorVariable", base::sample(0:1, size = options[["N"]], replace = TRUE, prob = c(0.97, 0.03))),
                    "mus" = .setColumnDataAsScale("TrueValues", dataset[,.v(options[["monetaryVariable"]])] - (base::sample(0:1, size = options[["N"]],replace = TRUE, prob = c(0.80, 0.20)) * rnorm(options[["N"]], mean = 300, sd = 150))))
    }

    # Evaluation phase
    if(!options[["evaluationChecked"]])
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
    correctID                       <- base::switch(options[["variableType"]], "variableTypeCorrect" = unlist(options$correctID), "variableTypeTrueValues" = unlist(options$correctMUS))
    if(correctID == "")             correctID <- NULL
    variables.to.read               <- c(recordVariable, variables, rankingVariable, correctID, sampleFilter, monetaryVariable)
    dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

    jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
    jaspResults[["evaluationHeader"]]$position <- 20
    jaspResults[["evaluationHeader"]]$dependOnOptions(c("none"))

    runEvaluation <- (!is.null(correctID) && !is.null(sampleFilter))
    # Apply the sample filter
    if(!is.null(sampleFilter)){
        dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
    }

    if(options[["variableType"]] == "variableTypeCorrect"){
      # Perform the attributes evaluation
      .bayesianAttributesBoundFullAudit(dataset, options, jaspResults)
      result                                       <- jaspResults[["result"]]$object
      .bayesianAttributesBoundTableFullAudit(options, result, jaspResults, position = 22)
    } else {
      # Perform the mus evaluation
      if(options[["boundMethod"]] == "coxAndSnellBound"){
        # Prior parameters for pi and mu are recommendations from the paper
        .coxAndSnellBound(dataset, options, jaspResults, priorPi = 0.1, priorMu = 0.4, priorA = planningResult[["priorA"]], priorB = planningResult[["priorB"]])
      }
      result                                       <- jaspResults[["result"]]$object
      .bayesianMusBoundTableFullAudit(total_data_value, options, result, jaspResults, position = 22)
    }

    # Interpretation before the evalution table
    if(options[["interpretation"]]){
      boundLabel <- base::switch(runEvaluation,
                                  "TRUE" = paste0(round(result[["bound"]] * 100, 2), "%"),
                                  "FALSE" = "...%")
      jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>",result[["k"]], "</b> of which were found to contain an error. The knowledge from these data, com-
                                                            bined with the prior knowledge results in an <b>", confidenceLevelLabel , "</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                            is a true probability of <b>", confidenceLevelLabel , "</b> that the error proportion in the population is lower than <b>", boundLabel ,"</b>."), "p")
      jaspResults[["resultParagraph"]]$position <- 21
      jaspResults[["resultParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                               "method", "materialityValue"))
    }

      # Confidence bound plot
      if(options[['plotBound']] && runEvaluation)
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

      # Prior and Posterior plot
      if(options[['plotPriorAndPosterior']] && runEvaluation)
      {
          if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
          {
            if(options[["variableType"]] == "variableTypeCorrect"){
              jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBoundFullAudit(options, result, jaspResults)
            } else {
              jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianMUSBoundFullAudit(options, result, jaspResults)
            }
            jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "plotPriorAndPosterior",
                                                                       "plotPriorAndPosteriorAdditionalInfo", "materiality", "correctID",
                                                                       "expected.errors", "kPercentageNumber", "kNumberNumber", "prior", "sampleFilter",
                                                                       "distribution", "N", "correctMUS", "sampleFilterMUS", "materialityValue"))
            jaspResults[["priorAndPosteriorPlot"]] 		$position <- 24
          }
      }

      # Correlation plot
      if(options[['plotCorrelation']] && runEvaluation)
      {
          if(is.null(jaspResults[["correlationPlot"]]))
          {
              jaspResults[["correlationPlot"]] 		<- .plotScatterJFA(dataset, options, jaspResults)
              jaspResults[["correlationPlot"]]		$dependOnOptions(c("correctMUS", "plotRegression", "monetaryVariable"))
              jaspResults[["correlationPlot"]] 		$position <- 25
          }
      }

      # Interpretation after the evaluation table
      if(options[["interpretation"]] && runEvaluation){
          jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
          jaspResults[["conclusionTitle"]]$position <- 26
          jaspResults[["conclusionTitle"]]$dependOnOptions(c("interpretation"))

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
          jaspResults[["conclusionParagraph"]]$position <- 27
          jaspResults[["conclusionParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                                   "method", "materialityValue", "correctMUS"))
      }
}
