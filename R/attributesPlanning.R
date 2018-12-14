attributesPlanning <- function(jaspResults, dataset, options, state=NULL){

  # Set the title
  jaspResults$title 					<- "Audit Planning"
  # Perform the analysis
  .attributesPlanning(options, jaspResults)
  result                      <- jaspResults[["result"]]$object
  .attributesPlanningTable(options, result, jaspResults)

}

.calc.n.binomial <- function(k, materiality, alpha){
    for(n in 1:5000){
        impk <- n * k
        if(impk%%1 == 0){
            x <- choose(n, 0:impk) * materiality^(0:impk) * (1-materiality)^(n - (0:impk))
            if(sum(x) < alpha)
                return(n)
        }
    }
}

.attributesPlanning <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence <- options[["confidence"]]
    p <- options[["materiality"]]
    k <- options[["k"]]

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

    n <- .calc.n.binomial(k, p, alpha)

    resultList <- list()
    resultList[["n"]]             <- n
    resultList[["k"]]             <- k
    resultList[["IR"]]            <- options[["IR"]]
    resultList[["CR"]]            <- options[["CR"]]
    resultList[["alpha"]]         <- alpha
    resultList[["confidence"]]    <- confidence

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "k", "materiality"))

}

.attributesPlanningTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                              <- createJaspTable("Classical Planning Table")
  jaspResults[["summaryTable"]]             <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "k", "materiality", "show"))

  summaryTable$addColumnInfo(name = 'IR',   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'SR',   title = "Sampling risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'k',    title = "Expected errors",      type = 'string')
  summaryTable$addColumnInfo(name = 'n',    title = "Required sample size", type = 'string')

  summaryTable$position                     <- 1

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
