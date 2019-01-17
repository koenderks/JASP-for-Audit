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
     jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                        "plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo", "show",
                                                        "statistic", "kPercentageNumber", "kNumberNumber"))
     jaspResults[["priorPlot"]] 		$position <- 4
     }
  }

  # Save the state
  state[["options"]] 					<- options
  return(state)

}

.bayesianAttributesPlanning <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence              <- options[["confidence"]]
    p                       <- options[["materiality"]]

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

    n_noprior               <- .calculateBayesianSampleSize(options, 1 - confidence)
    n_withprior             <- .calculateBayesianSampleSize(options, alpha)

    pn                      <- n_noprior - n_withprior

    if(pn == 0){
        pk                  <- 0
        if(options[["expected.errors"]] == "kPercentage"){
            k                   <- options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            k                   <- options[["kNumberNumber"]]
        }
    } else {
        if(options[["expected.errors"]] == "kPercentage"){
            k                   <- options[["kPercentageNumber"]]
            pk                  <- pn * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            k                   <- options[["kNumberNumber"]]
            pk                  <- k
        }
    }
    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

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
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "kPercentageNumber",
                                              "kNumberNumber", "inference"))

}

.bayesianAttributesPlanningTable <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Attributes Planning Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "show",
                                  "kPercentageNumber", "kNumberNumber", "inference"))

  summaryTable$addColumnInfo(name = 'IR', title = "Inherent risk", type = 'string')
  summaryTable$addColumnInfo(name = 'CR', title = "Control risk", type = 'string')
  summaryTable$addColumnInfo(name = 'SR', title = "Detection risk", type = 'string')
  summaryTable$addColumnInfo(name = 'k', title = "Allowed errors", type = 'string')
  summaryTable$addColumnInfo(name = 'n', title = "Required sample size", type = 'string')

  summaryTable$position <- position

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- paste0(round(result[["k"]] * 100, 2), "%")
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- paste0(round(result[["k"]], 2), "%")
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  }

  row <- list(IR = result[["IR"]],
              CR = result[["CR"]],
              SR = SRtable,
              k = ktable,
              n = result[["n"]])

  summaryTable$addRows(row)

  message <- "The sample size is calculated using the <b>beta</b> distribution."
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

}
