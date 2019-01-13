bayesianAttributesBound <- function(jaspResults, dataset, options, state=NULL){
  # Set the state
  if(is.null(state))
      state 							                     <- list()
  # Read the data
  dataset                                      <- .readDataBayesianAttributesBound(dataset, options)
  # Set the title
  jaspResults$title 					                 <- "Bayesian Attributes Bound"
  # Perform the analysis
  .bayesianAttributesBound(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  # Create the summary table
  .bayesianAttributesBoundTable(options, result, jaspResults)
  # Create the implicit sample table
  if (options$implicitsample)
  {
    if(is.null(jaspResults[["sampletable"]]))
      .priorSampleTable(options, result, jaspResults)
  }
  # Create the prior and posterior plot ##
   if(options[['plotPriorAndPosterior']] && options[["correctID"]] != "")
   {
      if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
      {
      jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBound(options, result, jaspResults)
      jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx", "statistic", "plotPriorAndPosterior",
                                                                    "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                    "expected.errors", "kPercentageNumber", "kNumberNumber"))
      jaspResults[["priorAndPosteriorPlot"]] 		$position <- 4
      }
   }
   # Create the confidence bounds plot
   if(options[['plotBounds']] && options[["correctID"]] != "")
   {
      if(is.null(jaspResults[["confidenceBoundPlot"]]))
      {
      jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
      jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID",
                                                                 "show", "plotBounds", "materiality", "method",
                                                                 "expected.errors", "kPercentageNumber", "kNumberNumber"))
      jaspResults[["confidenceBoundPlot"]] 		$position <- 5
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

.bayesianAttributesBound <- function(dataset, options, jaspResults){

    confidence              <- options[["confidence"]]
    correctID               <- options[["correctID"]]
    materiality             <- options[["materiality"]]

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
      k                     <- length(which(dataset[,.v(correctID)] == 1))
    }

    n_noprior               <- .calculateBayesianSampleSize(options, 1 - confidence)
    n_withprior             <- .calculateBayesianSampleSize(options, alpha)

    pn                      <- n_noprior - n_withprior

    if(pn == 0){
        pk                  <- 0
        if(options[["expected.errors"]] == "kPercentage"){
            exp.k                   <- options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            exp.k                   <- options[["kNumberNumber"]]
        }
    } else {
        if(options[["expected.errors"]] == "kPercentage"){
            exp.k               <- options[["kPercentageNumber"]]
            pk                  <- pn * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            exp.k               <- options[["kNumberNumber"]]
            pk                  <- exp.k
        }
    }

    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    if(n == 0 || k > n){
      bound                 <- "."
      approve               <- "."
    } else {
        if(options[["statistic"]] == "bound"){
          bound             <- qbeta(p = confidence,
                                    shape1 = priorA + k,
                                    shape2 = priorB + (n - k),
                                    lower.tail = TRUE)
         if(bound < materiality){
           approve          <- "Yes"
         } else {
           approve          <- "No"
         }
       } else if(options[["statistic"]] =="interval"){
          bound             <- qbeta(p = c(  (1 - (1-(1-confidence)/2)) , (1 - ((1-confidence)/2)) ),
                                    shape1 = priorA + k,
                                    shape2 = priorB + (n - k),
                                    lower.tail = TRUE)
         if(bound[2] < materiality){
           approve          <- "Yes"
         } else {
           approve          <- "No"
         }
      }
  }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- k
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- confidence
    resultList[["bound"]]       <- bound
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["posteriorA"]]  <- priorA + k
    resultList[["posteriorB"]]  <- priorB + (n - k)
    resultList[["approve"]]     <- approve

    jaspResults[["result"]]     <- createJaspState(resultList)
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality",
                                                    "correctID", "expected.errors", "kPercentageNumber", "kNumberNumber"))

}

.bayesianAttributesBoundTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                  "expected.errors", "kPercentageNumber", "kNumberNumber"))
  summaryTable$position <- 1

  summaryTable$addColumnInfo(name = 'IR',   title = "Inherent risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'CR',   title = "Control risk",   type = 'string')
  summaryTable$addColumnInfo(name = 'SR',   title = "Sampling risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'n',    title = "Sample size",    type = 'string')
  summaryTable$addColumnInfo(name = 'k',    title = "Errors",         type = 'string')


  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
    if(result[["bound"]] == "."){
      if(options[["statistic"]] == "bound"){
        boundTable          <- "."
      } else if(options[["statistic"]] == "interval"){
        boundTable          <- c(".", ".")
      }
    } else {
      boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(result[["bound"]] == "."){
      if(options[["statistic"]] == "bound"){
        boundTable          <- "."
      } else if(options[["statistic"]] == "interval"){
        boundTable          <- c(".", ".")
      }
    } else {
      boundTable <- round(result[["bound"]],3)
    }
  }

  if(options[["statistic"]] == "bound"){
      summaryTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
      row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable)
      summaryTable$addRows(row)
  } else {
      summaryTable$addColumnInfo(name = 'ciLow', title = "Lower", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
      summaryTable$addColumnInfo(name = 'ciHigh', title = "Upper", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
      row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], ciLow = boundTable[1], ciHigh = boundTable[2])
      summaryTable$addRows(row)
  }

}
