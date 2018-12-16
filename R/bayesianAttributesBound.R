bayesianAttributesBound <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							                     <- list()
  dataset                                      <- .readDataBayesianAttributesBound(dataset, options)
  # Set the title
  jaspResults$title 					                 <- "Bayesian Audit Attributes Bound"
  # Perform the analysis
  .bayesianAttributesBound(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  .bayesianAttributesBoundTable(options, result, jaspResults)
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
                                                                    "expected.k"))
      jaspResults[["priorAndPosteriorPlot"]] 		$position <- 3
      }
   }

  # Save the state
  state[["options"]] 					                  <- options
  return(state)

}

.readDataBayesianAttributesBound <- function(dataset, options){
  correctID <- options[['correctID']]
  stratum <- options[["stratum"]]
  variables <- c(correctID, stratum)
  variables <- variables[variables != ""]
  if (is.null(dataset)) {
    if(length(variables) == 0){
      dataset   <- NULL
    } else {
      if(stratum == "" && correctID != ""){
        dataset   <- .readDataSetToEnd(columns.as.numeric = correctID)
      } else if(stratum != "" && correctID == ""){
        dataset   <- .readDataSetToEnd(columns.as.factor = stratum)
      } else if(stratum != "" && correctID != ""){
        dataset   <- .readDataSetToEnd(columns.as.numeric = correctID, columns.as.factor = stratum)
      }
    }
  }
  return(dataset)
}

.bayesianAttributesBound <- function(dataset, options, jaspResults){

    confidence              <- options[["confidence"]]
    correctID               <- options[["correctID"]]
    exp.k                   <- options[["expected.k"]]
    materiality             <- options[["materiality"]]

    if(is.null(dataset)){
      n                     <- 0
      k                     <- 0
    } else {
      n                     <- nrow(dataset)
      k                     <- length(which(dataset[,.v(correctID)] == 1))
    }

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

    n_noprior               <- .calculateBayesianSampleSize(exp.k, materiality, 1 - confidence)
    n_withprior             <- .calculateBayesianSampleSize(exp.k, materiality, alpha)

    pn                      <- n_noprior - n_withprior
    pk                      <- pn * exp.k
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
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "expected.k", "correctID"))

}

.bayesianAttributesBoundTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID", "expected.k"))
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
