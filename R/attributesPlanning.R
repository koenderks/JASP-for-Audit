attributesPlanning <- function(jaspResults, dataset, options, state=NULL){

  # Set the title
  jaspResults$title 					<- "Attributes Planning"
  # Perform the analysis
  .attributesPlanning(options, jaspResults)
  result                      <- jaspResults[["result"]]$object
  .attributesPlanningTable(options, result, jaspResults)

}

.calc.n.binomial <- function(options, alpha){
    for(n in 1:5000){
      if(options[["expected.errors"]] == "kPercentage"){
        impk <- ceiling(n * options[["kPercentageNumber"]])
      } else if(options[["expected.errors"]] == "kNumber"){
        impk <- options[["kNumberNumber"]]
      }
      if(impk >= n){ next }
      if(impk%%1 == 0){
          x <- choose(n, 0:impk) * options[["materiality"]]^(0:impk) * (1-options[["materiality"]])^(n - (0:impk))
          if(sum(x) < alpha)
              return(n)
      }
    }
}

.calc.n.hypergeometric <- function(options, alpha){
    for(n in 1:5000){
        if(options[["expected.errors"]] == "kPercentage"){
            k <- ceiling(n * options[["kPercentageNumber"]])
        } else if(options[["expected.errors"]] == "kNumber"){
            k <- options[["kNumberNumber"]]
        }
        K <- floor(options[["N"]] * options[["materiality"]])
        if(n <= k) { next }
        x <- choose(K, 0:k) * choose(options[["N"]]-K, n-(0:k)) / choose(options[["N"]], n)
        if(sum(x) < alpha)
            return(n)
    }
}

.attributesPlanning <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence <- options[["confidence"]]
    p <- options[["materiality"]]

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

    if(options[["distribution"]] == "binomial"){
      n <- .calc.n.binomial(options, alpha)
    } else if(options[["distribution"]] == "hypergeometric"){
      if(options[["N"]] == 0){
        # TODO: Error handling
      }
      n <- .calc.n.hypergeometric(options, alpha)
    }

    if(options[["expected.errors"]] == "kPercentage"){
      k <- options[["kPercentageNumber"]]
    } else if(options[["expected.errors"]] == "kNumber"){
      k <- options[["kNumberNumber"]] / n
    }

    resultList <- list()
    resultList[["n"]]             <- n
    resultList[["k"]]             <- k
    resultList[["IR"]]            <- options[["IR"]]
    resultList[["CR"]]            <- options[["CR"]]
    resultList[["alpha"]]         <- alpha
    resultList[["confidence"]]    <- confidence

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "distribution",
                                                "N", "kPercentageNumber", "kNumberNumber"))

}

.attributesPlanningTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                              <- createJaspTable("Classical Planning Table")
  jaspResults[["summaryTable"]]             <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "show", "distribution", "N",
                                  "expected.errors" , "kPercentageNumber", "kNumberNumber"))

  summaryTable$addColumnInfo(name = 'IR',   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'SR',   title = "Sampling risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'k',    title = "Expected errors",      type = 'string')
  summaryTable$addColumnInfo(name = 'n',    title = "Required sample size", type = 'string')

  summaryTable$position                     <- 1

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
      ktable <- round(result[["k"]], 2)
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

  if(options[["distribution"]] == "binomial"){
          message <- "The sample size is calculated using the <b>binomial</b> distribution."
          summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  } else if(options[["distribution"]] == "hypergeometric"){
          message <- paste0("The sample size is calculated using the <b>hypergeometric</b> distribution (N = ", options[["N"]] ,").")
          summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  }

}
