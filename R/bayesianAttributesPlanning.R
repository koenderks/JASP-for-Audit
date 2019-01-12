bayesianAttributesPlanning <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							    <- list()

  jaspResults$title 					<- "Bayesian Attributes Planning"
  .bayesianAttributesPlanning(options, jaspResults)
  result                      <- jaspResults[["result"]]$object
  .bayesianAttributesPlanningTable(options, result, jaspResults)
  if (options$implicitsample)
  {
    if(is.null(jaspResults[["sampletable"]]))
      .priorSampleTable(options, result, jaspResults)
  }

  if(options[['plotPriorAndPosterior']])
  {
     if(is.null(jaspResults[["priorPlot"]]))
     {
     jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanning(options, result, jaspResults)
     jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence",
                                                      "materiality", "expected.k", "limx", "plotPriorAndPosterior",
                                                      "plotPriorAndPosteriorAdditionalInfo", "show", "statistic"))
     jaspResults[["priorPlot"]] 		$position <- 4
     }
  }

  # Save the state
  state[["options"]] 					<- options
  return(state)

}

.bayesianAttributesPlanning <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence                <- options[["confidence"]]
    p                         <- options[["materiality"]]
    k                         <- options[["expected.k"]]

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

    n_noprior         <- .calculateBayesianSampleSize(k, p, 1 - confidence)
    n_withprior       <- .calculateBayesianSampleSize(k, p, alpha)

    pn                <- n_noprior - n_withprior
    pk                <- pn * k
    priorA           <- 1 + pk
    priorB           <- 1 + (pn - pk)

    resultList <- list()
    resultList[["n"]]           <- n_withprior
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["k"]]           <- k
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- confidence

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.k", "materiality"))

}

.bayesianAttributesPlanningTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Planning")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.k", "materiality", "show"))

  summaryTable$addColumnInfo(name = 'IR', title = "Inherent risk", type = 'string')
  summaryTable$addColumnInfo(name = 'CR', title = "Control risk", type = 'string')
  summaryTable$addColumnInfo(name = 'SR', title = "Sampling risk", type = 'string')
  summaryTable$addColumnInfo(name = 'k', title = "Expected errors", type = 'string')
  summaryTable$addColumnInfo(name = 'n', title = "Required sample size", type = 'string')

  summaryTable$position <- 1

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")
    ktable <- paste0(result[["k"]] * 100, "%")
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    ktable <- result[["k"]]
  }

  row <- list(IR = result[["IR"]],
              CR = result[["CR"]],
              SR = SRtable,
              k = ktable,
              n = result[["n"]])

  summaryTable$addRows(row)

}
