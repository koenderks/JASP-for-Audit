bayesianAttributesEvaluation <- function(jaspResults, dataset, options, state=NULL){
  # Set the state
  if(is.null(state))
      state 							                     <- list()
  # Read the data
  dataset                                      <- .readDataBayesianAttributesBound(dataset, options)
  # Set the title
  jaspResults$title 					                 <- "Bayesian Attributes Bound"

  .ARMformula(options, jaspResults, position = 1)   # Show the Audit Risk Model formula and quantify detection risk
  DR              <- jaspResults[["DR"]]$object
  # Perform the analysis
  .bayesianAttributesBoundFullAudit(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  # Create the summary table
  .bayesianAttributesBoundTableFullAudit(options, result, jaspResults, position = 3)

  # Interpretation for the evaluation phase
  if(options[["interpretation"]]){

    jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
    jaspResults[["evaluationHeader"]]$position <- 2

      jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>",result[["k"]], "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                            bined with the prior knowledge results in an <b>",options$confidence*100, "%</b> upper confidence bound of <b>",round(result[["bound"]]*100, 2),"%</b>. The cumulative knowledge states that there
                                                            is a <b>",options$confidence*100, "%</b> probability that the true error proportion in the population is lower than <b>",round(result[["bound"]]*100, 2),"%</b>."), "p")
      jaspResults[["resultParagraph"]]$position <- 3

  }

  # Create the prior and posterior plot ##
   if(options[['plotPriorAndPosterior']] && options[["correctID"]] != "")
   {
      if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
      {
      jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBoundFullAudit(options, result, jaspResults)
      jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "statistic", "plotPriorAndPosterior",
                                                                    "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                    "expected.errors", "kPercentageNumber", "kNumberNumber", "prior"))
      jaspResults[["priorAndPosteriorPlot"]] 		$position <- 4
      }
   }
  # Save the state
  state[["options"]] 					                  <- options
  return(state)
}

.readDataBayesianAttributesBound <- function(dataset, options){
  correctID <- options[['correctID']]
  if (is.null(dataset)) {
    if(correctID == ""){
      dataset   <- NULL
    } else {
      dataset   <- .readDataSetToEnd(columns.as.numeric = correctID)
    }
  }
  return(dataset)
}
