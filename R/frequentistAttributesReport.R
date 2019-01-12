frequentistAttributesReport <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							    <- list()

  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  options$show <- "percentage"
  options$statistic <- "bound"

  jaspResults$title 					<- "JASP Audit Report"

  jaspResults[["introParagraph"]] <- createJaspHtml("This report is automatically generated and serves the purpose of providing a statistically correct interpretation
                                                      for the planning and evaluation of a Bayesian attributes bound.", "p")
  jaspResults[["introParagraph"]]$position <- 1

  jaspResults[["objective"]] <- createJaspHtml("<u>Objective</u>", "h2")
  jaspResults[["objective"]]$position <- 2

  jaspResults[["objectiveParagraph"]] <- createJaspHtml(paste0("The objective of this attributes test is to determine with <b>", options$confidence*100,"%</b> confidence whether the proportion of errors in the population
                                                                lies below the materiality limit of <b>",options$materiality*100,"%</b>. An attributes test only considers the observations to be completely correct or
                                                                completely in error."), "p")
  jaspResults[["objectiveParagraph"]]$position <- 3

  jaspResults[["priorKnowledge"]] <- createJaspHtml("<u>Prior knowledge</u>", "h2")
  jaspResults[["priorKnowledge"]]$position <- 4

  .bayesianAttributesPlanning(options, jaspResults)
  result                      <- jaspResults[["result"]]$object

  jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("The inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined to be <b>", options$CR,"</b>. According
                                                                      to the Audit Risk Model, the required detection risk to maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be set to <b>",round(result[["alpha"]]*100, 2), "%</b>. From
                                                                      historical data, the most likely error in the data equals <b>", options$expected.k*100,"%</b>. The probability distribution that corresponds with this
                                                                      prior knowledge is the <b>Beta(",round(result[["priorA"]],2), ",", round(result[["priorB"]],2),")</b> distribution."), "p")
  jaspResults[["priorKnowledgeParagraph"]]$position <- 5

  if(options[["plotPrior"]]){
    if(is.null(jaspResults[["priorPlot"]]))
    {
    jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanning(options, result, jaspResults, plotWidth = )
    jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence",
                                                     "materiality", "expected.k", "limx", "plotPriorAndPosterior",
                                                     "plotPriorAndPosteriorAdditionalInfo", "show", "statistic"))
    jaspResults[["priorPlot"]]$position <- 6
    }
  }

  if(options[["n"]] != 0){

    if(options[["k"]] == 0){
      proportion <- 0
    } else {
      proportion <- options[["k"]] / options[["n"]] * 100
    }

    jaspResults[["resultTitle"]] <- createJaspHtml("<u>Result</u>", "h2")
    jaspResults[["resultTitle"]]$position <- 7

    .summaryBayesianAttributesBound(options, jaspResults)
    result                      <- jaspResults[["result"]]$object

    jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>",options$n, "</b> observations. Of these <b>",options$n, "</b> observations, <b>",options$k, "</b> were found to contain a complete error.
                                                              The proportion of errors in the data is <b>",proportion, "%</b> with an <b>",options$confidence*100, "%</b> upper confidence bound of <b>",round(result[["bound"]]*100, 2),"%</b>. This means that
                                                              there is a <b>",options$confidence*100, "%</b> true probability that the maximum error in the population lies below <b>",round(result[["bound"]]*100, 2),"%</b>."), "p")
    jaspResults[["resultParagraph"]]$position <- 8

    if(options[["plotPriorAndPosterior"]]){
      if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
      {
        jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBound(options, result, jaspResults, plotWidth = 400, plotHeight = 250)
        jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "n", "k", "limx", "statistic",
                                                                    "plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo",
                                                                    "materiality", "show", "expected.k"))
        jaspResults[["priorAndPosteriorPlot"]]$position <- 9
      }
    }

    jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
    jaspResults[["conclusionTitle"]]$position <- 10

    if(result[["bound"]] < options[["materiality"]]){
      above_below <- "below"
      approve <- "<b>contain no material misstatement</b>"
    } else {
      above_below <- "above"
      approve <- "<b>contain material misstatement</b>"
    }

    jaspResults[["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", options$confidence*100,"%</b> upper confidence bound on the population proportion of errors should lie below the set
                                                                    materiality. For the current data, the confidence bound lies ", above_below ," the materiality. The conclusion is that these data
                                                                    ",approve,"."), "p")
    jaspResults[["conclusionParagraph"]]$position <- 11

  }

  # Save the state
  state[["options"]] 					<- options
  return(state)

}
