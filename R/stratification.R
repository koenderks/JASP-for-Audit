stratification <- function(jaspResults, dataset, options, state=NULL){

  if(is.null(state))
      state 							                     <- list()
  # Set the title
  jaspResults$title 					<- "Audit Stratification"
  dataset                     <- .readDataStratifiedAttributesPlanning(dataset, options)
  # Perform the analysis
  .stratifiedAttributesPlanning(dataset, options, jaspResults)
  result                      <- jaspResults[["result"]]$object
  .stratifiedAttributesPlanningTable(options, result, jaspResults)
  if (options[["plotDistribution"]])
  {
    if(is.null(jaspResults[["distributionPlot"]]))
      jaspResults[["distributionPlot"]] <- .stratifiedDistributionPlot(options, result)
      jaspResults[["distributionPlot"]]		  $dependOnOptions(c("stratum", "n", "plotDistribution"))
      jaspResults[["distributionPlot"]] 		$position <- 2
  }

  # Save the state
  state[["options"]] 					                  <- options
  return(state)

}

.readDataStratifiedAttributesPlanning <- function(dataset, options){
  stratum <- options[['stratum']]
  if (is.null(dataset)) {
    if(stratum == ""){
      dataset   <- NULL
    } else {
      dataset   <- .readDataSetToEnd(columns.as.factor = stratum)
    }
  }
  return(dataset)
}

.stratifiedAttributesPlanning <- function(dataset, options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    stratumid <- options[["stratum"]]

    if(is.null(dataset)){
      nlevels     <- 1
      ni          <- "."
      size        <- "."
      n           <- NULL
      levels      <- "."
      mes         <- FALSE
    } else {

      n                     <- options[["n"]]
      N                     <- length(dataset[, .v(stratumid)])
      levels                <- levels(dataset[, .v(stratumid)])
      nlevels               <- length(levels)

      if(n == 0){
        ni                  <- rep(".", nlevels)
        size                <- rep(".", nlevels)
        mes                 <- FALSE
      } else {

        ni                  <- numeric()
        size                <- numeric()
        for(i in 1:nlevels){
            cursize         <- length(dataset[which(dataset[, .v(stratumid)] == levels[i]), ])
            nicur           <- ceiling(cursize / N * options[["n"]])

            size <- c(size, cursize)
            ni <- c(ni, nicur)
        }
        if(N < n){
          mes <- TRUE
        } else {
          mes <- FALSE
        }
      }
  }

    resultList <- list()
    resultList[["n"]]               <- n
    resultList[["ni"]]              <- ni
    resultList[["stratumnames"]]    <- levels
    resultList[["nstratums"]]       <- nlevels
    resultList[["stratumsize"]]     <- size
    resultList[["message"]]         <- mes

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("stratum", "n"))

}

.stratifiedAttributesPlanningTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                              <- createJaspTable("Classical Stratification Table")
  jaspResults[["summaryTable"]]             <- summaryTable
  summaryTable$dependOnOptions(c("stratum", "n"))

  summaryTable$addColumnInfo(name = 'stratum',    title = "Stratum",        type = 'string')
  summaryTable$addColumnInfo(name = 'size',       title = "Size",           type = 'string')
  summaryTable$addColumnInfo(name = 'samplesize', title = "Sample size",    type = 'string')
  summaryTable$addColumnInfo(name = 'percentage', title = "% of stratum",   type = 'string')

  summaryTable$position                     <- 1

  for(i in 1:result[["nstratums"]]){
    if(result[["stratumsize"]] == "."){
      percentage = "."
    } else {
      percentage <- paste0(round(result[["ni"]][i] / result[["stratumsize"]][i] * 100, 2), "%")
    }
    row <- list(stratum = result[["stratumnames"]][i], size = result[["stratumsize"]][i], samplesize = result[["ni"]][i], percentage = percentage)
    summaryTable$addRows(row)
  }

  if(result[["stratumsize"]] == "."){
    row <- list(stratum = "Total:", size = ".", samplesize = ".", percentage = ".")
  } else {
    row <- list(stratum = "Total:", size = sum(result[["stratumsize"]]), samplesize = sum(result[["ni"]]), percentage = "")
  }
  summaryTable$addRows(row)

  if(result[["message"]]){
    message <- "The specified sample size is larger than the population size. Sampling with replacement is required."
    summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")
  }

}

.stratifiedDistributionPlot <- function(options, result){

  if(options[["n"]] == 0 || options[["stratum"]] == "")
    return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))

  p <- JASPgraphs::drawAxis(xName = "Stratum", xBreaks = 1:5, yBreaks = 1:5)
  p  <- ggplot2::ggplot(data = data.frame(x = result[["stratumnames"]], y = result[["ni"]]), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
        ggplot2::xlab("Stratum") +
        ggplot2::ylab("Counts")

  # JASP theme
  p <- JASPgraphs::themeJasp(p)

  return(createJaspPlot(plot = p, title = "Distribution Plot", width = 600, height = 450))

}
