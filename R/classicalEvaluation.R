classicalEvaluation <- function(jaspResults, dataset, options, state=NULL){

  monetaryVariable                  <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")        monetaryVariable <- NULL
  correctID                         <- base::switch(options[["variableType"]], "variableTypeCorrect" = unlist(options$correctID), "variableTypeTrueValues" = unlist(options$correctMUS))
  if(correctID == "")               correctID <- NULL
  variables.to.read                 <- c(monetaryVariable, correctID)
  dataset                           <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  options[["sampleSize"]]           <- nrow(dataset)
  options[["sampleFilter"]]         <- "Not applicable"
  options[["show"]]                 <- "percentage"

  # Set the title
  jaspResults$title 					                 <- "Evaluation"

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
    if(options[["show"]] == "percentage"){
      confidenceLevelLabel            <- paste0(round(options[["confidence"]] * 100, 2), "%")
      materialityLevelLabel           <- paste0(round(options[["materiality"]] * 100, 2), "%")
      boundLabel <- paste0(round(result[["bound"]] * 100, 2), "%")
    } else {
      confidenceLevelLabel            <- round(options[["confidence"]], 2)
      materialityLevelLabel           <- round(options[["materiality"]], 2)
      boundLabel <- round(result[["bound"]], 2)
    }
    jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>",options[["sampleSize"]], "</b> observations, <b>", result[["k"]] , "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                          bined with the prior knowledge results in an <b>",round((1 - result[["alpha"]]) * 100, 2), "%</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                          is a <b>", confidenceLevelLabel , "</b> probability that, when one would repeaditly sample from this population, the maximum error is calculated to be lower
                                                          than <b>", boundLabel ,"</b>."), "p")
    jaspResults[["resultParagraph"]]$position <- 1
  }

  # Create the confidence bounds plot
  if(options[['plotBound']] && !is.null(correctID))
  {
      if(is.null(jaspResults[["confidenceBoundPlot"]]))
      {
          jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
          jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID",
                                                                   "show", "plotBound", "materiality", "method", "inference"))
          jaspResults[["confidenceBoundPlot"]] 		$position <- 3
      }
  }

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
      jaspResults[["conclusionParagraph"]]$position <- 4
  }

  # Save the state
  state[["options"]] 					                  <- options
  return(state)

}
