classicalAttributesEvaluation <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							                     <- list()
  dataset                                      <- .readDataBayesianAttributesBound(dataset, options)
  # Set the title
  jaspResults$title 					                 <- "Attributes Bound"
  # Perform the analysis
  .attributesBound(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  .attributesBoundTable(options, result, jaspResults)
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

.attributesBound <- function(dataset, options, jaspResults){

  confidence              <- options[["confidence"]]
  correctID               <- options[["correctID"]]

  if(options[["IR"]] == "Low" && options[["CR"]] == "Low"){
      alpha               <- (1-confidence) / 0.30 / 0.30
  } else if (options[["IR"]] == "Low" && options[["CR"]] == "Medium"){
      alpha               <- (1-confidence) / 0.30 / 0.60
  } else if (options[["IR"]] == "Low" && options[["CR"]] == "High"){
      alpha               <- (1-confidence) / 0.30 / 1
  } else if (options[["IR"]] == "Medium" && options[["CR"]] == "High"){
      alpha               <- (1-confidence) / 0.60 / 1
  } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Medium"){
      alpha               <- (1-confidence) / 0.60 / 0.60
  } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Low"){
      alpha               <- (1-confidence) / 0.60 / 0.30
  } else if (options[["IR"]] == "High" && options[["CR"]] == "Low"){
      alpha               <- (1-confidence) / 1 / 0.30
  } else if (options[["IR"]] == "High" && options[["CR"]] == "Medium"){
      alpha               <- (1-confidence) / 0.60 / 1
  } else if (options[["IR"]] == "High" && options[["CR"]] == "High"){
      alpha               <- (1-confidence) / 1 / 1
  }

  if(is.null(dataset)){
    n                     <- 0
    k                     <- 0
  } else {
    n                     <- nrow(dataset)
    if(options[["correctID"]] != ""){
      k                     <- length(which(dataset[,.v(correctID)] == 1))
    } else {
      k                     <- "."
    }
  }


  if(n == 0){
    bound                 <- "."
    approve               <- "."
  } else {
    binomResult <- binom.test(x = k,
                              n = n,
                              p = options[["materiality"]],
                              alternative = "less",
                              conf.level = 1 - alpha)
    bound                 <- binomResult$conf.int[2]
    if(bound <= alpha){
      approve             <- "Yes"
    } else {
      approve             <- "No"
    }
  }

  resultList <- list()
  resultList[["n"]]           <- n
  resultList[["k"]]           <- k
  resultList[["IR"]]          <- options[["IR"]]
  resultList[["CR"]]          <- options[["CR"]]
  resultList[["confidence"]]  <- confidence
  resultList[["bound"]]       <- bound
  resultList[["approve"]]     <- approve
  resultList[["alpha"]]       <- alpha

  jaspResults[["result"]] <- createJaspState(resultList)
  jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "sampleFilter", "inference"))

}

.attributesBoundTable <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Classical Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID"))
  summaryTable$position <- position

  summaryTable$addColumnInfo(name = 'IR',     title = "Inherent risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'CR',     title = "Control risk",   type = 'string')
  summaryTable$addColumnInfo(name = 'SR',     title = "Detection risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'n',      title = "Sample size",    type = 'string')
  summaryTable$addColumnInfo(name = 'k',      title = "Errors",         type = 'string')

  summaryTable$addColumnInfo(name = 'bound',  title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
    if(result[["bound"]] == "."){
      boundTable          <- "."
    } else {
      boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(result[["bound"]] == "."){
      boundTable          <- "."
    } else {
      boundTable <- round(result[["bound"]],3)
    }
  }

  row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable)
  summaryTable$addRows(row)

}
