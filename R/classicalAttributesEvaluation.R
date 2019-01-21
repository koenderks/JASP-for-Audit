classicalAttributesEvaluation <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							                     <- list()

    correctID                       <- unlist(options$correctID)
    if(correctID == "")             correctID <- NULL
    sampleFilter                    <- unlist(options$sampleFilter)
    if(sampleFilter == "")          sampleFilter <- NULL
    variables.to.read               <- c(correctID, sampleFilter)

    if (is.null(dataset))
        dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  if(options[["sampleFilter"]] != ""){
      dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
  }

  # Set the title
  jaspResults$title 					                 <- "Attributes Bound"
  # Perform the analysis
  .attributesBoundFullAudit(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  .attributesBoundTableFullAudit(options, result, jaspResults)
  # Create the confidence bounds plot
  if(options[['plotBounds']] && options[["correctID"]] != "")
  {
     if(is.null(jaspResults[["confidenceBoundPlot"]]))
     {
     jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
     jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID",
                                                                "show", "plotBounds", "materiality", "method"))
     jaspResults[["confidenceBoundPlot"]] 		$position <- 3
     }
  }
  # Save the state
  state[["options"]] 					                  <- options
  return(state)

}
