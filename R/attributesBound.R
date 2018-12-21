attributesBound <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							                     <- list()
  dataset                                      <- .readDataBayesianAttributesBound(dataset, options)
  # Set the title
  jaspResults$title 					                 <- "Audit Attributes Bound"
  # Perform the analysis
  .attributesBound(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  .attributesBoundTable(options, result, jaspResults)
  if(options[["stratuminfo"]])
  {
    if(is.null(jaspResults[["stratuminfotable"]]))
      .stratumInfoTable(options, result, jaspResults, type = "frequentist")
  }
  if(options[['plotBounds']] && options[["correctID"]] != "")
  {
     if(is.null(jaspResults[["confidenceBoundPlot"]]))
     {
     jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
     jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID", "stratum",
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

  if(options[["stratum"]] == "" && options[["correctID"]] != ""){

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

  } else if(options[["stratum"]] != "" && options[["correctID"]] != ""){

  stratum <- options[["stratum"]]

  if(n == 0){
    bound                 <- "."
    approve               <- "."
  } else {

    strata <- dataset[, .v(stratum)]
    stratumbounds <- numeric()
    levels <- levels(strata)
    stratumN <- numeric()
    stratumK <- numeric()

    for(i in 1:length(levels)){
      level <- dataset[which(dataset[, .v(stratum)] == levels[i]), .v(correctID)]
      curN <- length(level)
      curK <- length(which(level == 1))
      binomResult <- binom.test(x = curK,
                           n = curN,
                           p = options[["materiality"]],
                           alternative = "less",
                           conf.level = 1 - alpha)
      levelbound           <- binomResult$conf.int[2]

      stratumbounds <- c(stratumbounds, levelbound)
      stratumN <- c(stratumN, curN)
      stratumK <- c(stratumK, curK)
    }

    if(options[["method"]] == "normal"){
      midpoints <- stratumbounds - (stratumbounds/2)
      boundVar <- sqrt(sum((stratumbounds-midpoints)^2))
      boundMean <- mean(midpoints)
      nsbound <- boundMean + boundVar
    } else if(options[["method"]] == "mean"){
      nsbound <- mean(stratumbounds)
    }

    binomResult <- binom.test(x = k,
                              n = n,
                              p = options[["materiality"]],
                              alternative = "less",
                              conf.level = 1 - alpha)
    bound                 <- binomResult$conf.int[2]

    if(nsbound < alpha){
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
  resultList[["nsbound"]]     <- nsbound
  resultList[["bound"]]       <- bound
  resultList[["approve"]]     <- approve
  resultList[["alpha"]]       <- alpha
  resultList[["stratumnames"]] <- levels
  resultList[["stratumbounds"]] <- stratumbounds
  resultList[["stratumsize"]] <- stratumN
  resultList[["stratumerrors"]] <- stratumK

  } else if((options[["stratum"]] != "" && options[["correctID"]] == "") || is.null(dataset)){

  resultList <- list()
  resultList[["n"]]           <- n
  resultList[["k"]]           <- k
  resultList[["IR"]]          <- options[["IR"]]
  resultList[["CR"]]          <- options[["CR"]]
  resultList[["confidence"]]  <- confidence
  resultList[["bound"]]       <- "."
  resultList[["approve"]]     <- "."
  resultList[["alpha"]]       <- alpha

  }

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "stratum", "method"))

}

.attributesBoundTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Classical Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID", "stratum", "method"))
  summaryTable$position <- 1

  summaryTable$addColumnInfo(name = 'IR',     title = "Inherent risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'CR',     title = "Control risk",   type = 'string')
  summaryTable$addColumnInfo(name = 'SR',     title = "Sampling risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'n',      title = "Sample size",    type = 'string')
  summaryTable$addColumnInfo(name = 'k',      title = "Errors",         type = 'string')

  if(options[["stratum"]] == ""){

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

  } else {

    summaryTable$addColumnInfo(name = 'boundns',  title = "Non-stratified", type = 'string', overtitle = paste0(result[["confidence"]]*100,"% Confidence bounds"))
    summaryTable$addColumnInfo(name = 'bounds',  title = "Stratified", type = 'string', overtitle = paste0(result[["confidence"]]*100,"% Confidence bounds"))

    if(options[["show"]] == "percentage"){
      SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
      if(result[["bound"]] == "."){
        boundTableNs          <- "."
        boundTableS          <- "."
      } else {
        boundTableNs <- paste0(round(result[["bound"]],3) * 100, "%")
        boundTableS <- paste0(round(result[["nsbound"]],3) * 100, "%")
      }
    } else if(options[["show"]] == "proportion"){
      SRtable <- round(result[["alpha"]], 3)
      if(result[["bound"]] == "."){
        boundTableNs          <- "."
        boundTableS          <- "."
      } else {
        boundTableNs <- round(result[["bound"]],3)
        boundTableS <- round(result[["nsbound"]],3)
      }
    }

    row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], boundns = boundTableNs, bounds = boundTableS)
    summaryTable$addRows(row)

  }

}
