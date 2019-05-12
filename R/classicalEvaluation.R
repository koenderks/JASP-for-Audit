classicalEvaluation <- function(jaspResults, dataset, options, ...){
  ### AUDIT RISK MODEL ###
  .auditRiskModel(options, jaspResults)
  ### READ DATA ###
  dataset <- .readDataEvaluationAnalysis(options, jaspResults)
  # Rewrite materiality based on value
  if(options[["populationValue"]] == 0) { populationValue <- 0.01 } else { populationValue <- options[["populationValue"]] }
  if(options[["materiality"]] == "materialityAbsolute"){
      jaspResults[["materiality"]] <- createJaspState(options[["materialityValue"]] / populationValue)
  } else {
      jaspResults[["materiality"]] <- createJaspState(options[["materialityPercentage"]])
  }

  jaspResults[["N"]] <- createJaspState(options[["populationSize"]])
  jaspResults[["sampleSize"]] <- createJaspState(nrow(dataset))

  jaspResults[["evaluationContainer"]] <- createJaspContainer(title = "<u>Evaluation</u>")
  jaspResults[["evaluationContainer"]]$position <- 5

  if(options[["estimator"]] == "gammaBound" || options[["estimator"]] == "binomialBound" || options[["estimator"]] == "hyperBound"){
    # Perform the attributes evaluation
    evaluationResult <- .attributesBound(dataset, options, jaspResults)
    .attributesBoundTable(options, evaluationResult, jaspResults, position = 21)
  } else {
    # Perform monetary evaluation
    if(options[["estimator"]] == "stringerBound"){
      evaluationResult <- .stringerBound(dataset, options, jaspResults)
    } else if(options[["estimator"]] == "regressionBound"){
      evaluationResult <- .regressionBound(dataset, options, jaspResults)
    } else if(options[["estimator"]] == "directBound"){
      evaluationResult <- .directBound(dataset, options, jaspResults)
    } else if(options[["estimator"]] == "differenceBound"){
      evaluationResult <- .differenceBound(dataset, options, jaspResults)
    } else if(options[["estimator"]] == "ratioBound"){
      evaluationResult <- .ratioBound(dataset, options, jaspResults)
    }
    .auditValueBoundTable(options, evaluationResult, jaspResults, position = 21)
  }
  # Explanatory text for the evaluation
  if(options[["explanatoryText"]]){
    boundLabel      <- ifelse(jaspResults[["runEvaluation"]]$object, yes = paste0(round(evaluationResult[["bound"]] * 100, 2), "%"), no = ".....")
    extraObsLabel   <- ifelse(jaspResults[["containsDoubleObservations"]]$object, no = nrow(dataset), yes = paste0(nrow(dataset), " + ", planningResult[["n"]] - nrow(jaspResults[["sample"]]$object)))
    sampleSizeLabel <- ifelse(options[["auditResult"]] == "", yes = ".....", no = extraObsLabel)
    jaspResults[["evaluationContainer"]][["resultParagraph"]] <- createJaspHtml(paste0("The selection consisted of <b>", sampleSizeLabel , "</b> observations, <b>", evaluationResult[["k"]] , "</b> of which were found to contain an error. The knowledge from these data, com-
                                                                                        bined with the prior knowledge results in an <b>", jaspResults[["confidenceLevelLabel"]]$object , "%</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                                                        is a <b>", jaspResults[["confidenceLevelLabel"]]$object , "</b> probability that, when one would repeaditly sample from this population, the maximum misstatement is calculated to be lower
                                                                                        than <b>", boundLabel ,"</b>."), "p")
    jaspResults[["evaluationContainer"]][["resultParagraph"]]$position <- 1
    jaspResults[["evaluationContainer"]][["resultParagraph"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "materialityPercentage", "estimator", "materialityValue", "sampleFilter"))
  }
  # Create a plot containing evaluation information (if the user wants it)
  if(options[['evaluationInformation']] && jaspResults[["runEvaluation"]]$object)
  {
      if(is.null(jaspResults[["evaluationContainer"]][["evaluationInformation"]]))
      {
          jaspResults[["evaluationContainer"]][["evaluationInformation"]] 		<- .evaluationInformationEvaluationAnalysis(options, evaluationResult, jaspResults)
          jaspResults[["evaluationContainer"]][["evaluationInformation"]]		$dependOn(options = c("IR", "CR", "confidence", "auditResult", "evaluationInformation", "materialityPercentage", "estimator", "materialityValue"))
          jaspResults[["evaluationContainer"]][["evaluationInformation"]] 		$position <- 3
      }
      jaspResults[["evaluationContainer"]][["figure4"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Results of the sample evaluation compared with materiality and expected errors. The most likely error (MLE)
                                                            is an estimate of the true misstatement in the population. The maximum error is the upper confidence bound on this MLE."), "p")
      jaspResults[["evaluationContainer"]][["figure4"]]$position <- 4
      jaspResults[["evaluationContainer"]][["figure4"]]$dependOn(optionsFromObject= jaspResults[["evaluationContainer"]][["evaluationInformation"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  } else if(options[["evaluationInformation"]]){
      errorPlot <- createJaspPlot(plot = NULL, title = "Evaluation Information")
      errorPlot$status <- "complete"
      jaspResults[["evaluationContainer"]][["evaluationInformation"]] <- errorPlot
  }
  # Create a plot containing the correlation between the book values and audit values (if the user wants it)
  if(options[['correlationPlot']] && jaspResults[["runEvaluation"]]$object)
  {
      if(is.null(jaspResults[["evaluationContainer"]][["correlationPlot"]]))
      {
          jaspResults[["evaluationContainer"]][["correlationPlot"]] 		<- .correlationPlot(dataset, options, jaspResults)
          jaspResults[["evaluationContainer"]][["correlationPlot"]]		  $dependOn(options = c("auditResult", "correlationPlot", "monetaryVariable"))
          jaspResults[["evaluationContainer"]][["correlationPlot"]] 		$position <- 7
      }
      jaspResults[["evaluationContainer"]][["figure6"]] <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Scatterplot of the sample book values versus their audit values. Red dots indicate observations that did not match
                                                              their original book value. If these red dots lie in the bottom part of the graph, the observations are overstated. If these red dots
                                                              lie in the upper part of the graph, they are understated."), "p")
      jaspResults[["evaluationContainer"]][["figure6"]]$position <- 8
      jaspResults[["evaluationContainer"]][["figure6"]]$dependOn(optionsFromObject= jaspResults[["evaluationContainer"]][["correlationPlot"]])
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  } else if(options[["correlationPlot"]]){
      errorPlot <- createJaspPlot(plot = NULL, title = "Correlation Plot")
      errorPlot$status <- "complete"
      jaspResults[["evaluationContainer"]][["correlationPlot"]] <- errorPlot
  }
  .classicalConclusion(options, jaspResults)
}

.readDataEvaluationAnalysis <- function(options, jaspResults){
  auditResult                     <- unlist(options[["auditResult"]])
  if(auditResult == "")           auditResult <- NULL
  monetaryVariable                <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")      monetaryVariable <- NULL
  sampleFilter                    <- unlist(options[["sampleFilter"]])
  if(sampleFilter == "")          sampleFilter <- NULL
  variables.to.read               <- c(auditResult, sampleFilter, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  if(options[["materiality"]] == "materialityRelative"){
      jaspResults[["runEvaluation"]] <- createJaspState(options[["auditResult"]] != "" && options[["materialityPercentage"]] != 0 && options[["populationSize"]] != 0)
  } else {
      jaspResults[["runEvaluation"]] <- createJaspState(options[["auditResult"]] != "" && options[["materialityValue"]] != 0 && options[["populationSize"]] != 0 && options[["populationValue"]] != 0)
  }
  jaspResults[["runEvaluation"]]$dependOn(options = c("auditResult"))
  return(dataset)
}

.evaluationInformationEvaluationAnalysis <- function(options, evaluationResult, jaspResults){

  materiality       <- jaspResults[["materiality"]]$object
  bound             <- evaluationResult[["bound"]]
  #proj.misstatement <- bound * jaspResults[["total_data_value"]]$object
  mle               <- ifelse(options[["variableType"]] == "variableTypeCorrect", yes = evaluationResult[["k"]] / evaluationResult[["n"]], no = sum(evaluationResult[["z"]]) / evaluationResult[["n"]])
  label             <- rev(c("Materiality", "Maximum error", "Most likely error"))
  values            <- rev(c(materiality, bound, mle))
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute")
    values          <- values * jaspResults[["total_data_value"]]$object
  boundColor        <- ifelse(bound < materiality, yes = rgb(0,1,.7,1), no = rgb(1,0,0,1))
  fillUp            <- rev(c("#1380A1", boundColor, "#1380A1"))
  yBreaks           <- as.numeric(JASPgraphs::getPrettyAxisBreaks(c(0, values), min.n = 4))
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute"){
    x.labels        <- format(yBreaks, scientific = TRUE)
    x.title         <- "Error amount"
  } else {
    x.labels        <- paste0(round(yBreaks * 100, 4), "%")
    x.title         <- "Error percentage"
  }
  tb                <- data.frame(x = label, values = values)
  tb$x              <- factor(tb$x, levels = tb$x)
  p                 <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
                        ggplot2::geom_bar(stat = "identity", col = "black", size = 1, fill = fillUp) +
                        ggplot2::coord_flip() +
                        ggplot2::xlab(NULL) +
                        ggplot2::ylab(x.title) +
                        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()) +
                        ggplot2::scale_y_continuous(breaks = yBreaks, labels = x.labels)
  p                 <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)
  return(createJaspPlot(plot = p, title = "Evaluation information", width = 600, height = 300))
}