classicalAudit <- function(jaspResults, dataset, options, ...){
    ### TITLE ###
    jaspResults$title <- "Audit Workflow"
    ### PROCEDURE ###
    .classicalProcedure(dataset, options, jaspResults)
    ### AUDIT RISK MODEL ###
    .auditRiskModel(options, jaspResults)
    ### PLANNING ###
    .classicalPlanning(dataset, options, jaspResults)
    ### SELECTION ###
    if(!options[["samplingChecked"]]) return()    # Stop if "To Selection" is not pressed
    .classicalSelection(options, jaspResults)
    ### EXECUTION ###
    .execution(options, jaspResults)
    ### EVALUATION ###
    if(!options[["evaluationChecked"]]) return()  # Stop if "To Evaluation" is not pressed
    .classicalEvaluation(options, jaspResults)
    ### CONCLUSION ###
    .classicalConclusion(options, jaspResults)
}

.classicalProcedure <- function(dataset, options, jaspResults){
  # Read in data
  dataset <- .readDataProcedure(options, jaspResults)
  # Create state for the figure number
  jaspResults[["figNumber"]] <- createJaspState(1)
  jaspResults[["figNumber"]]$dependOnOptions(c("bookValueDistribution", "decisionPlot"))
  # Create container for the procedure
  jaspResults[["procedureContainer"]] <- createJaspContainer(title= "<u>Procedure</u>")
  jaspResults[["procedureContainer"]]$position <- 1
  # Explanatory text for the procedure
  if(options[["explanatoryText"]] && is.null(jaspResults[["procedureContainer"]][["procedureParagraph"]])){
    if(is.null(jaspResults[["confidenceLevelLabel"]]$object)){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOnOptions(c("confidence"))
    }
    criterion <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste0(format(options[["materialityValue"]], scientific = FALSE), " monetary units"))
    jaspResults[["procedureContainer"]][["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of 
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$position <- 1
    jaspResults[["procedureContainer"]][["procedureParagraph"]]$dependOnOptions(c("explanatoryText", "confidence", "materiality", "materialityPercentage", "materialityValue"))
  }
  # Create a table of the population descriptives (if the user wants it)
  if(options[["bookValueDescriptives"]])
    .bookValueDescriptives(dataset, options, jaspResults, position = 2)
  # Create a plot of the population book values (if the user wants it)
  if(options[['bookValueDistribution']] && jaspResults[["ready"]]$object)
  {
      if(is.null(jaspResults[["procedureContainer"]][["bookValueDistribution"]]))
      {
          jaspResults[["procedureContainer"]][["bookValueDistribution"]] 		<- .bookValueDistribution(dataset, options, jaspResults)
          jaspResults[["procedureContainer"]][["bookValueDistribution"]]		$dependOnOptions(c("bookValueDistribution", "monetaryVariable"))
          jaspResults[["procedureContainer"]][["bookValueDistribution"]] 		$position <- 3
      }
      jaspResults[["procedureContainer"]][["figure1"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The distribution of book values in the audit population. The red and blue dots respectively represent the mean
                                                                                        and the values exactly one standard deviation from the mean. The orange dots represent the 25th, 50th (median) and
                                                                                        75th percentile of the book values."), "p")
      jaspResults[["procedureContainer"]][["figure1"]]$position <- 4
      jaspResults[["procedureContainer"]][["figure1"]]$copyDependenciesFromJaspObject(jaspResults[["procedureContainer"]][["bookValueDistribution"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
      jaspResults[["figNumber"]]$dependOnOptions(c("bookValueDistribution", "decisionPlot"))
    } else if(options[["bookValueDistribution"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Population Distribution")
        errorPlot$setError("Plotting not possible: No analysis has been run.")
        jaspResults[["procedureContainer"]][["bookValueDistribution"]] <- errorPlot
    }
  # Finish procedure
}

.classicalPlanning <- function(dataset, options, jaspResults){
  # Create a container for the planning
  jaspResults[["planningContainer"]] <- createJaspContainer(title= "<u>Planning</u>")
  jaspResults[["planningContainer"]]$position <- 3
  # Rewrite the materiality to a proportion of the total value
  if(is.null(jaspResults[["materiality"]]$object)){
    materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / jaspResults[["total_data_value"]]$object, no = options[["materialityPercentage"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOnOptions(c("materialityValue", "materialityPercentage", "monetaryVariable", "recordNumberVariable", "materiality"))
  }
  # Calculate the sample size and return the calculation as an object
  planningResult <- .classicalPlanningHelper(options, jaspResults)
  # Summarize the planning result in a summary table
  .classicalPlanningTable(dataset, options, planningResult, jaspResults, position = 2)
  # Rewrite the required sample size when the planning has not been run yet
  if(is.null(jaspResults[["planningResult"]]$object)){
    requiredSampleSize <- 0
  } else {
    requiredSampleSize <- planningResult[["n"]]
  }
  # Calculate the number of expected errors and the maximum number of allowed errors
  expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = options[["expectedNumber"]])
  if(options[["planningModel"]] == "Poisson"){
    max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = round(options[["expectedPercentage"]] * requiredSampleSize, 2), no = options[["expectedNumber"]])
  } else {
    max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = floor(options[["expectedPercentage"]] * requiredSampleSize) + 1, no = options[["expectedNumber"]] + 1)
  }
  # Explanatory text for the planning
  if(options[["explanatoryText"]] && is.null(jaspResults[["planningContainer"]][["planningParagraph"]])){
    materialityLevelLabel           <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(jaspResults[["materiality"]]$object, 4) * 100, "%"), "materialityAbsolute" = format(options[["materialityValue"]], scientific = FALSE))  
    jaspResults[["materialityLevelLabel"]] <- createJaspState(materialityLevelLabel)
    jaspResults[["planningContainer"]][["planningParagraph"]] <- createJaspHtml(paste0("The most likely error in the data was expected to be <b>", expected.errors ,"</b>. The sample size that is required to prove a materiality of <b>", materialityLevelLabel ,"</b>, assuming 
                                                                                              the sample contains <b>", expected.errors ,"</b> full errors, is <b>", requiredSampleSize ,"</b>. This sample size is based on the <b>", options[["planningModel"]] , "</b> distribution, the inherent risk <b>(", options[["IR"]] , ")</b>, the 
                                                                                              control risk <b>(", options[["CR"]] , ")</b> and the expected errors. Consequently, if the sum of errors from the audited observations exceeds <b>", max.errors ,"</b>, the 
                                                                                              projected misstatement exceeds materiality and the population cannot be approved."), "p")
      jaspResults[["planningContainer"]][["planningParagraph"]]$position <- 1
      jaspResults[["planningContainer"]][["planningParagraph"]]$dependOnOptions(c("expectedPercentage", "expectedErrors", "expectedNumber", "planningModel", "IR", "CR", "materialityPercentage", "confidence", "materialityValue"))
  }
  # Create a decision plot (if the user wants it)
  if(options[['decisionPlot']] && jaspResults[["ready"]]$object)
  {
      if(is.null(jaspResults[["planningContainer"]][["decisionPlot"]]))
      {
          if(options[["planningModel"]] == "Poisson")
            max.errors <- ceiling(max.errors)
          allowed.errors <- 0:(max.errors - 1)
          if(max.errors == 0)
            allowed.errors <- 0
          reject.errors <- allowed.errors[length(allowed.errors)] + 1 : (allowed.errors[length(allowed.errors)] + 3)
          jaspResults[["planningContainer"]][["decisionPlot"]] 		<- .decisionPlot(allowed.errors, reject.errors, options, jaspResults)
          jaspResults[["planningContainer"]][["decisionPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", "decisionPlot", "planningModel", "materialityValue"))
          jaspResults[["planningContainer"]][["decisionPlot"]] 		$position <- 4
      }
      jaspResults[["planningContainer"]][["figure2"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The number of full errors that are allowed in the sample before rejecting the population are displayed in green.
                                                        Whenever more than this number of full errors is found, displayed in red, the population should be rejected."), "p")
      jaspResults[["planningContainer"]][["figure2"]]$position <- 5
      jaspResults[["planningContainer"]][["figure2"]]$copyDependenciesFromJaspObject(jaspResults[["planningContainer"]][["decisionPlot"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)    
    } else if(options[["decisionPlot"]]){
        errorPlot <- createJaspPlot(plot = NULL, title = "Decision Plot")
        errorPlot$setError("Plotting not possible: Please specify your variables.")
        jaspResults[["planningContainer"]][["decisionPlot"]] <- errorPlot
    }
  # Finish planning
}

.classicalSelection <- function(options, jaspResults){
  # Create a container for the selection
  jaspResults[["selectionContainer"]] <- createJaspContainer(title= "<u>Selection</u>")
  jaspResults[["selectionContainer"]]$position <- 4
  # Read in data for selection
  dataset <- .readDataSelection(options)
  # Import stored objects from jaspResults
  total_data_value              <- jaspResults[["total_data_value"]]$object
  planningResult                <- jaspResults[["planningResult"]]$object
  jaspResults[["sampleSize"]]   <- createJaspState(planningResult[["n"]])
  monetaryVariable              <- unlist(options[["monetaryVariable"]])
  # Explanatory text for selection
  if(options[["explanatoryText"]]){
    technique <- base::switch(options[["selectionMethod"]], "randomSampling" = "random", "systematicSampling" = "systematic", "cellSampling" = "cell")
    technique <- base::switch(options[["selectionType"]], "recordSampling" = paste(technique, "record sampling"), "musSampling" = paste(technique, "monetary unit sampling"))
    jaspResults[["selectionContainer"]][["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", jaspResults[["N"]]$object, "</b> observations, <b>", planningResult[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
    jaspResults[["selectionContainer"]][["samplingParagraph"]]$position <- 1
    jaspResults[["selectionContainer"]][["samplingParagraph"]]$dependOnOptions(c("samplingType", "samplingMethod"))
  }
  # Perform the sampling and create the tables for displaying the selection
  base::switch(options[["selectionMethod"]],
                  "randomSampling"      = .randomSampling(dataset, options, jaspResults, position = 4),
                  "systematicSampling"  = .systematicSampling(dataset, options, jaspResults, position = 4),
                  "cellSampling"        = .cellSampling(dataset, options, jaspResults, position = 4))
  # Create a table at the top of the selection with information about the selection process
  .selectionInformationTable(dataset, options, jaspResults, position = 2)
  # Create a table with descriptive statistics for the selection (if the user wants it)
  .sampleDescriptivesTable(dataset, options, jaspResults, position = 3)
  # Finish selection
}

.classicalEvaluation <- function(options, jaspResults){
  # Create a container for the evaluation 
  jaspResults[["evaluationContainer"]] <- createJaspContainer(title = "<u>Evaluation</u>")
  jaspResults[["evaluationContainer"]]$position <- 5
  # Read data for the evaluation
  dataset <- .readDataEvaluation(options, jaspResults)
  # Import stored objects from jaspResults
  total_data_value              <- jaspResults[["total_data_value"]]$object
  planningResult                <- jaspResults[["planningResult"]]$object
  runEvaluation                 <- jaspResults[["runEvaluation"]]$object
  # Apply the selection filter to the dataset
  if(jaspResults[["runEvaluation"]]$object)
      dataset <- subset(dataset, dataset[, .v(options[["sampleFilter"]])] == 1)
  # Perform the evaluation conditional on the type of variable
  if(options[["variableType"]] == "variableTypeCorrect"){
    # Attributes evaluation
    evaluationResult <- .attributesBound(dataset, options, jaspResults)
    # Create the summary table for the evaluation
    .attributesBoundTable(options, evaluationResult, jaspResults, position = 2)
  } else if(options[["variableType"]] == "variableTypeAuditValues"){
    # Audit value evaluation
    evaluationResult <- base::switch(options[["estimator"]],
                                      "stringerBound"     = .stringerBound(dataset, options, jaspResults),
                                      "regressionBound"   = .regressionBound(dataset, options, jaspResults),
                                      "directBound"       = .directBound(dataset, options, jaspResults),
                                      "differenceBound"   = .differenceBound(dataset, options, jaspResults),
                                      "ratioBound"        = .ratioBound(dataset, options, jaspResults))
    # Create the summary table for the evaluation
    .auditValueBoundTable(options, evaluationResult, jaspResults, position = 2)
  }
  # Explanatory text for the evaluation
  if(options[["explanatoryText"]]){
    boundLabel      <- ifelse(jaspResults[["runEvaluation"]]$object, yes = paste0(round(evaluationResult[["bound"]] * 100, 2), "%"), no = ".....")  
    sampleSizeLabel <- ifelse(options[["auditResult"]] == "", yes = ".....", no = nrow(dataset))       
    jaspResults[["evaluationContainer"]][["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", sampleSizeLabel , "</b> observations, <b>", evaluationResult[["k"]] , "</b> of which were found to contain an error. The knowledge from these data, com-
                                                                                        bined with the prior knowledge results in an <b>", jaspResults[["confidenceLevelLabel"]]$object , "%</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                                                        is a <b>", jaspResults[["confidenceLevelLabel"]]$object , "</b> probability that, when one would repeaditly sample from this population, the maximum misstatement is calculated to be lower
                                                                                        than <b>", boundLabel ,"</b>."), "p")
    jaspResults[["evaluationContainer"]][["resultParagraph"]]$position <- 1
    jaspResults[["evaluationContainer"]][["resultParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "auditResult", "materialityPercentage", "estimator", "materialityValue", "sampleFilter"))
  }
  # Create a plot containing evaluation information (if the user wants it)
  if(options[['evaluationInformation']] && runEvaluation)
  {
      if(is.null(jaspResults[["evaluationContainer"]][["evaluationInformation"]]))
      {
          jaspResults[["evaluationContainer"]][["evaluationInformation"]] 		<- .evaluationInformation(options, evaluationResult, jaspResults)
          jaspResults[["evaluationContainer"]][["evaluationInformation"]]		$dependOnOptions(c("IR", "CR", "confidence", "auditResult", "evaluationInformation", "materialityPercentage", "estimator", "materialityValue"))
          jaspResults[["evaluationContainer"]][["evaluationInformation"]] 		$position <- 3
      }
      jaspResults[["evaluationContainer"]][["figure4"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Results of the sample evaluation compared with materiality and expected errors. The most likely error (MLE)
                                                            is an estimate of the true misstatement in the population. The maximum error is the upper confidence bound on this MLE."), "p")
      jaspResults[["evaluationContainer"]][["figure4"]]$position <- 4
      jaspResults[["evaluationContainer"]][["figure4"]]$copyDependenciesFromJaspObject(jaspResults[["evaluationContainer"]][["evaluationInformation"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  } else if(options[["evaluationInformation"]]){
      errorPlot <- createJaspPlot(plot = NULL, title = "Evaluation Information")
      errorPlot$setError("Plotting not possible: Please specify your variables.")
      jaspResults[["evaluationContainer"]][["evaluationInformation"]] <- errorPlot
  }
  # Create a plot containing the correlation between the book values and audit values (if the user wants it)
  if(options[['correlationPlot']] && runEvaluation)
  {
      if(is.null(jaspResults[["evaluationContainer"]][["correlationPlot"]]))
      {
          jaspResults[["evaluationContainer"]][["correlationPlot"]] 		<- .correlationPlot(dataset, options, jaspResults)
          jaspResults[["evaluationContainer"]][["correlationPlot"]]		  $dependOnOptions(c("auditResult", "correlationPlot", "monetaryVariable"))
          jaspResults[["evaluationContainer"]][["correlationPlot"]] 		$position <- 7
      }
      jaspResults[["evaluationContainer"]][["figure6"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Scatterplot of the sample book values versus their audit values. Red dots indicate observations that did not match
                                                              their original book value. If these red dots lie in the bottom part of the graph, the observations are overstated. If these red dots
                                                              lie in the upper part of the graph, they are understated."), "p")
      jaspResults[["evaluationContainer"]][["figure6"]]$position <- 8
      jaspResults[["evaluationContainer"]][["figure6"]]$copyDependenciesFromJaspObject(jaspResults[["evaluationContainer"]][["correlationPlot"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  } else if(options[["correlationPlot"]]){
      errorPlot <- createJaspPlot(plot = NULL, title = "Correlation Plot")
      errorPlot$setError("Plotting not possible: Please specify your variables.")
      jaspResults[["evaluationContainer"]][["correlationPlot"]] <- errorPlot
  }
  # Finish evaluation
}

.classicalConclusion <- function(options, jaspResults){
  # Import result of analysis from jaspResults
  evaluationResult <- jaspResults[["evaluationResult"]]$object
  # Explanatory text for conclusion
  if(options[["explanatoryText"]] && jaspResults[["runEvaluation"]]$object){
    # Create a container for the conclusion
    jaspResults[["conclusionContainer"]] <- createJaspContainer(title = "<u>Conclusion</u>")
    jaspResults[["conclusionContainer"]]$position <- 5
    # Produce relevant terms conditional on the analysis result
    above_below   <- ifelse(evaluationResult[["bound"]] < jaspResults[["materiality"]]$object, yes = "lower", no = "higher")
    approve       <- ifelse(evaluationResult[["bound"]] < jaspResults[["materiality"]]$object, yes = "<b>no material misstatement</b>", no = "<b>material misstatement</b>")
    jaspResults[["conclusionContainer"]][["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", jaspResults[["confidenceLevelLabel"]]$object ,"</b> upper confidence bound on the population proportion of full errors should be determined to be
                                                                lower than materiality, in this case <b>", jaspResults[["materialityLevelLabel"]]$object ,"</b>. For the current data, the confidence bound is <b>", above_below ,"</b> than materiality.
                                                                The conclusion for these data is that the data contain ", approve ,"."), "p")
    jaspResults[["conclusionContainer"]][["conclusionParagraph"]]$position <- 1
    jaspResults[["conclusionContainer"]][["conclusionParagraph"]]$dependOnOptions(c("IR", "CR", "confidence", "auditResult", "materialityPercentage", "estimator", "materialityValue", "sampleFilter"))
  }
  # Finsh conclusion
}
