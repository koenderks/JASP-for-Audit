classicalAudit <- function(jaspResults, dataset, options, state=NULL){

    options[["show"]]                   <- "percentage"

    # Specify the title of the analysis
    jaspResults$title   <- "Sampling Process"
    figNumber           <- 1

    jaspResults[["procedureContainer"]] <- createJaspContainer(title= "<u>Procedure</u>")
    jaspResults[["procedureContainer"]]$position <- 1

    # Interpretation for the Global Options phase
    if(options[["interpretation"]]){
      confidenceLevelLabel              <- paste0(round(options[["confidence"]] * 100, 2), "%")
      jaspResults[["procedureContainer"]][["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", confidenceLevelLabel, ")</b>
                                                                    whether the amount of misstatement in the target population is lower than the specified materiality."), "p")
      jaspResults[["procedureContainer"]][["procedureParagraph"]]$position <- 1
      jaspResults[["procedureContainer"]][["procedureParagraph"]]$dependOnOptions(c("interpretation", "confidence"))
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
    .dataTable(dataset, options, jaspResults, position = 2)

    # Population distribution plot
    if(options[['distributionPlot']] && options[["run"]])
    {
        if(is.null(jaspResults[["procedureContainer"]][["valueDistributionPlot"]]))
        {
            jaspResults[["procedureContainer"]][["valueDistributionPlot"]] 		<- .plotValueDistribution(dataset, options, jaspResults)
            jaspResults[["procedureContainer"]][["valueDistributionPlot"]]		$dependOnOptions(c("distributionPlot", "monetaryVariable", "recordNumberVariable"))
            jaspResults[["procedureContainer"]][["valueDistributionPlot"]] 		$position <- 3
        }
        jaspResults[["procedureContainer"]][["figure1"]] <- createJaspHtml(paste0("<b>Figure ", figNumber ,".</b>The distribution of book values in the audit population. The red and blue dots respectively represent the mean
                                                                                          and the values exactly one standard deviation from the mean. The orange dots represent the 25th, 50th (median) and
                                                                                          75th percentile of the book values."), "p")
        jaspResults[["procedureContainer"]][["figure1"]]$position <- 4
        jaspResults[["procedureContainer"]][["figure1"]]$copyDependenciesFromJaspObject(jaspResults[["procedureContainer"]][["valueDistributionPlot"]])
        figNumber <- figNumber + 1
      }

    jaspResults[["ARMcontainer"]] <- createJaspContainer(title= "<u>Audit Risk Model</u>")
    jaspResults[["ARMcontainer"]]$position <- 2

    # Audit Risk Model formula
    .ARMformula(options, jaspResults, position = 2)
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

      auditRiskLabel          <- paste0(round((1 - options[["confidence"]]) * 100, 2), "%")
      dectectionRiskLabel     <- paste0(round(DR * 100, 2), "%")

      jaspResults[["ARMcontainer"]][["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>", options[["IR"]] ,"</b>. The internal control risk was determined
                                                                      to be <b>", options[["CR"]] ,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", auditRiskLabel, "</b> for a materiality
                                                                      of <b>", materialityLevelLabel ,"</b> should be <b>", dectectionRiskLabel , "</b>."), "p")
      jaspResults[["ARMcontainer"]][["AuditRiskModelParagraph"]]$position <- 1
      jaspResults[["ARMcontainer"]][["AuditRiskModelParagraph"]]$dependOnOptions(c("confidence", "IR", "CR", "materiality", "materialityValue", "interpretation"))
    }

    jaspResults[["planningContainer"]] <- createJaspContainer(title= "<u>Planning</u>")
    jaspResults[["planningContainer"]]$position <- 3

    # Planning
    .attributesPlanningFullAudit(options, jaspResults)
    planningResult              <- jaspResults[["planningResult"]]$object
    .attributesPlanningTableFullAudit(dataset, options, planningResult, jaspResults, position = 2)

    expected.errors   <- ifelse(options[["expected.errors"]] == "kPercentage", yes = paste0(round(options[["kPercentageNumber"]] * 100, 2), "%"), no = options[["kNumberNumber"]])
    max.errors        <- ifelse(options[["expected.errors"]] == "kPercentage", yes = floor(options[["kPercentageNumber"]] * planningResult[["n"]]) + 1, no = options[["kNumberNumber"]] + 1)

    # Interpretation after the planning table
    if(options[["interpretation"]]){

        jaspResults[["planningContainer"]][["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The sample size that is required to prove an upper
                                                                          confidence bound of <b>", materialityLevelLabel ,"</b>, assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. This sample size is calculated according to the
                                                                          <b>", options[["distribution"]] , "</b> distribution. Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the projected misstatement exceeds
                                                                          the materiality and the population cannot be approved."), "p")
        jaspResults[["planningContainer"]][["priorKnowledgeParagraph"]]$position <- 1
        jaspResults[["planningContainer"]][["priorKnowledgeParagraph"]]$dependOnOptions(c("kPercentageNumber", "expected.errors", "kNumberNumber", "distribution", "IR", "CR", "materiality", "N",
                                                                "confidence", "materialityValue", "interpretation"))
    }

    # Decision plot
    if(options[['plotCriticalErrors']] && options[["run"]])
    {
        if(is.null(jaspResults[["planningContainer"]][["criticalErrorPlot"]]))
        {
            allowed.errors <- 0:(max.errors - 1)
            reject.errors <- max.errors : (max.errors + 2)
            jaspResults[["planningContainer"]][["criticalErrorPlot"]] 		<- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
            jaspResults[["planningContainer"]][["criticalErrorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors",
                                                                        "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors",
                                                                        "distribution", "N", "materialityValue"))
            jaspResults[["planningContainer"]][["criticalErrorPlot"]] 		$position <- 3
        }
        jaspResults[["planningContainer"]][["figure2"]] <- createJaspHtml(paste0("<b>Figure ", figNumber ,".</b>The number of full errors that are allowed in the sample before rejecting the population are displayed in green.
                                                          Whenever more than this number of full errors is found, displayed in red, the population should be rejected."), "p")
        jaspResults[["planningContainer"]][["figure2"]]$position <- 4
        jaspResults[["planningContainer"]][["figure2"]]$copyDependenciesFromJaspObject(jaspResults[["planningContainer"]][["criticalErrorPlot"]])
        figNumber <- figNumber + 1
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

        jaspResults[["selectionContainer"]] <- createJaspContainer(title= "<u>Selection</u>")
        jaspResults[["selectionContainer"]]$position <- 4
        # Keep the resulting sample size as an option
        options[["sampleSize"]] <- planningResult[["n"]]

        # Interpretation for the sampling phase
        if(options[["interpretation"]]){
            technique <- base::switch(options[["samplingType"]],
                                        "simplerandomsampling" = "random",
                                        "systematicsampling" = "systematic",
                                        "cellsampling" = "cell")
            technique <- base::switch(options[["samplingMethod"]],
                                        "recordsampling" = paste(technique, "record sampling"),
                                        "mussampling" = paste(technique, "MUS sampling"))
            jaspResults[["selectionContainer"]][["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", options[["N"]], "</b> observations, <b>", planningResult[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
            jaspResults[["selectionContainer"]][["samplingParagraph"]]$position <- 1
            jaspResults[["selectionContainer"]][["samplingParagraph"]]$dependOnOptions(c("sampleSize", "N", "samplingType", "samplingMethod"))
        }

        samplingMethod <- options[["samplingMethod"]]

        # Perform the sampling and draw the outcome tables
        if(options[["samplingType"]] == "simplerandomsampling"){
          if(samplingMethod == "recordsampling"){
            .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "attributes", sample = jaspResults[["sample"]]$object, position = 4)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 2)
          } else {
            .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "mus", sample = jaspResults[["sample"]]$object, position = 4)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 2)
          }
        } else if(options[["samplingType"]] == "systematicsampling"){
          if(samplingMethod == "recordsampling"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 4)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 2, interval = interval)
          } else {
            interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
            .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 4)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 2, interval = interval)
          }
        } else if(options[["samplingType"]] == "cellsampling"){
          if(samplingMethod == "recordsampling"){
            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .cellSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 4)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 2, interval = interval)
          } else {
            interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
            .cellSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 4)
            .samplingInfoTable(jaspResults[["sample"]]$object, total_data_value, options, jaspResults, position = 2, interval = interval)
          }
        }
        # Store the sample
        sample                          <- jaspResults[["sample"]]$object

        # Descriptives table
        if(options[["showDescriptives"]]){
          .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 3)
        }
    }

    # TODO: Add columns to data instead of replace existing
    if(options[["pasteVariables"]]){
      sampleFilter <- rep(0, options[["N"]])
      sampleFilter[sample[,.v(options[["recordNumberVariable"]])]] <- 1
      sampleFilter <- as.integer(sampleFilter)
      emptyVariable <- rep(NA, options[["N"]])
      .setColumnDataAsNominal("sampleFilter", sampleFilter)
      base::switch(options[["variableType"]],
                    "variableTypeCorrect" = .setColumnDataAsNominal("errorVariable", base::sample(0:1, size = options[["N"]], replace = TRUE, prob = c(0.97, 0.03))),
                    "variableTypeTrueValues" = .setColumnDataAsScale("TrueValues", dataset[,.v(options[["monetaryVariable"]])] - (base::sample(0:1, size = options[["N"]],replace = TRUE, prob = c(0.80, 0.20)) * rnorm(options[["N"]], mean = 300, sd = 150))))
    }

    # Evaluation phase (analysis starts when button is clicked)
    if(!options[["evaluationChecked"]])
      return()

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

    jaspResults[["evaluationContainer"]] <- createJaspContainer(title= "<u>Evaluation</u>")
    jaspResults[["evaluationContainer"]]$position <- 5

    runEvaluation <- (!is.null(correctID) && !is.null(sampleFilter))
    # Apply the sample filter
    if(runEvaluation){
        dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
    }

    if(options[["variableType"]] == "variableTypeCorrect"){
      # Perform the attributes evaluation
      .attributesBoundFullAudit(dataset, options, jaspResults)
      result                                       <- jaspResults[["result"]]$object
      .attributesBoundTableFullAudit(options, result, jaspResults, position = 21)
    } else {
      # Perform the MUS evaluaton
      if(options[["boundMethod"]] == "stringerBound"){
        .stringerBound(dataset, options, jaspResults)
      } else if(options[["boundMethod"]] == "regressionBound"){
        .regressionEstimator(dataset, options, total_data_value, jaspResults)
      }
      result                                       <- jaspResults[["result"]]$object
      .musBoundTableFullAudit(total_data_value, options, result, jaspResults, position = 21)
    }

    # Interpretation before the evalution table
    if(options[["interpretation"]]){
      boundLabel <- base::switch(runEvaluation,
                                  "TRUE" = paste0(round(result[["bound"]] * 100, 2), "%"),
                                  "FALSE" = "...%")
      jaspResults[["evaluationContainer"]][["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>", result[["k"]] , "</b> of which were found to contain an error. The knowledge from these data, com-
                                                            bined with the prior knowledge results in an <b>", confidenceLevelLabel , "%</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                            is a <b>", confidenceLevelLabel , "</b> probability that, when one would repeaditly sample from this population, the maximum error is calculated to be lower
                                                            than <b>", boundLabel ,"</b>."), "p")
      jaspResults[["evaluationContainer"]][["resultParagraph"]]$position <- 20
      jaspResults[["evaluationContainer"]][["resultParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                               "method", "materialityValue"))
    }

    # Confidence bound plot
    if(options[['plotBound']] && runEvaluation)
    {
        if(is.null(jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]]))
        {
            jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
            jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                                     "method", "materialityValue", "correctMUS", "boundMethod", "result"))
            jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]] 		$position <- 22
        }
        jaspResults[["evaluationContainer"]][["figure3"]] <- createJaspHtml(paste0("<b>Figure ", figNumber ,".</b> Results of the sample evaluation compared with materiality and expected errors. The most likely error (MLE)
                                                              is an estimate of the true misstatement in the population. The maximum error is the upper confidence bound on this MLE."), "p")
        jaspResults[["evaluationContainer"]][["figure3"]]$position <- 23
        jaspResults[["evaluationContainer"]][["figure3"]]$copyDependenciesFromJaspObject(jaspResults[["evaluationContainer"]][["confidenceBoundPlot"]])
        figNumber <- figNumber + 1
    }

    # Correlation plot
    if(options[['plotCorrelation']] && runEvaluation)
    {
        if(is.null(jaspResults[["evaluationContainer"]][["correlationPlot"]]))
        {
            jaspResults[["evaluationContainer"]][["correlationPlot"]] 		<- .plotScatterJFA(dataset, options, jaspResults)
            jaspResults[["evaluationContainer"]][["correlationPlot"]]		$dependOnOptions(c("correctMUS", "plotRegression", "monetaryVariable"))
            jaspResults[["evaluationContainer"]][["correlationPlot"]] 		$position <- 24
        }
        jaspResults[["evaluationContainer"]][["figure4"]] <- createJaspHtml(paste0("<b>Figure ", figNumber ,".</b> Scatterplot of the sample book values versus their audit values. Red dots indicate observations that did not match
                                                                their original book value. If these red dots lie in the bottom part of the graph, the observations are overstated. If these red dots
                                                                lie in the upper part of the graph, they are understated."), "p")
        jaspResults[["evaluationContainer"]][["figure4"]]$position <- 25
        jaspResults[["evaluationContainer"]][["figure4"]]$copyDependenciesFromJaspObject(jaspResults[["evaluationContainer"]][["correlationPlot"]])
        figNumber <- figNumber + 1
    }

    # Interpretation after the evaluation table
    if(options[["interpretation"]] && runEvaluation){

      jaspResults[["conclusionContainer"]] <- createJaspContainer(title= "<u>Conclusion</u>")
      jaspResults[["conclusionContainer"]]$position <- 6

      above_below   <- ifelse(result[["bound"]] < options[["materiality"]], yes = "lower", no = "higher")
      approve       <- ifelse(result[["bound"]] < options[["materiality"]], yes = "<b>no material misstatement</b>", no = "<b>material misstatement</b>")

      jaspResults[["conclusionContainer"]][["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", confidenceLevelLabel ,"</b> upper confidence bound on the population proportion of full errors should be determined to be
                                                                  lower than materiality, in this case <b>", materialityLevelLabel ,"</b>. For the current data, the confidence bound is <b>", above_below ,"</b> than materiality.
                                                                  The conclusion for these data is that the data contain ", approve ,"."), "p")
      jaspResults[["conclusionContainer"]][["conclusionParagraph"]]$position <- 27
      jaspResults[["conclusionContainer"]][["conclusionParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "plotBound", "materiality",
                                                               "method", "materialityValue", "correctMUS"))
    }
}
