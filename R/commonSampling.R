.SimpleRandomSamplingTable          <- function(dataset, options, jaspResults, type, sample, position = 1)
{

    if(!is.null(jaspResults[["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    variables                       <- unlist(options$variables)

    duplicates                      <- options$allowDuplicates
    sampleSize                      <- options$sampleSize
    manualSeed                      <- options$seedNumber

    if(options[["showSample"]]){
      table                           <- createJaspTable("Resulting sample")
      jaspResults[["table"]]          <- table
      table$position                  <- position

      table$dependOnOptions(c("variables", "allowDuplicates", "seed", "sampleSize", "seedNumber", "recordNumberVariable",
                              "samplingType", "auditType", "recordNumberVariableMUS", "monetaryVariableMUS", "variablesMUS",
                              "showSample", "samplingMethod"))

      table$addColumnInfo(name="number", title ="", type = "string")
      table$addColumnInfo(name="recordNumber", title ="Record Number", type = "string")
      for(i in variables){
          table$addColumnInfo(name=i,     type="string")
      }
    }

    if(type == "attributes"){
      if(is.null(recordVariable) || sampleSize == 0)
          return()
    } else {
      if(is.null(recordVariable) || is.null(monetaryVariable) || sampleSize == 0)
          return()
    }

    if(sampleSize > nrow(dataset) && duplicates == FALSE){
        message <- "The required sample size is larger than the number of records in the dataset. Please re-specify the sample size or allow duplicates."
        table$errorMessage <- message
        table$error <- "badData"
        return()
    }

    set.seed(manualSeed)

    if(is.null(jaspResults[["sample"]])){
      recordColumnIndex <- which(colnames(dataset) == .v(recordVariable))
      recordColumn <- dataset[, recordColumnIndex]

      if(type == "attributes"){
          recordColumn <- rank(dataset[, .v(recordVariable)])
          samp <- base::sample(x = recordColumn, size = sampleSize, replace = duplicates)
          sample <- as.data.frame(dataset[samp, ])
      } else {
          monetaryColumn <- dataset[, which(colnames(dataset) == .v(monetaryVariable))]
          monetaryColumn <- ceiling(monetaryColumn)
          sampleVector <- base::sample(recordColumn, size = sampleSize, replace = duplicates, prob = monetaryColumn)
          sample <- as.data.frame(dataset[sampleVector, ])
      }
      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOnOptions(c("variables", "allowDuplicates", "seed", "sampleSize", "seedNumber", "recordNumberVariable",
                              "samplingType", "recordNumberVariableMUS", "monetaryVariableMUS", "variablesMUS", "samplingMethod"))
    }

    if(options[["showSample"]]){
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row <- list()
          row[["number"]] <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          for(j in c(variables, monetaryVariable)){
              row[[j]] <- as.character(sample[i, .v(j)])
          }
          table$addRows(row)
      }
    }
}

.cellSamplingTable                    <- function(dataset, options, jaspResults, interval, type, sample, position = 2)
{

    if(!is.null(jaspResults[["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")        rankingVariable <- NULL
    variables                       <- unlist(options$variables)

    startingPoint                       <- options$startingPoint
    sampleSize                          <- options$sampleSize
    manualSeed                          <- options$seedNumber
    set.seed(manualSeed)

    if(options[["showSample"]]){
        table                               <- createJaspTable("Resulting sample")
        jaspResults[["table"]]              <- table
        table$position                      <- position
        table$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                                "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "showSample",
                              "samplingMethod"))

        table$addColumnInfo(name="number",          title ="", type = "string")
        table$addColumnInfo(name="recordNumber",    title ="Record Number", type = "string")
        for(i in c(variables, monetaryVariable)){
            table$addColumnInfo(name=i,             type="string")
        }
    }

    if(type == "attributes"){
        if(is.null(recordVariable) || sampleSize == 0)
            return()
    } else {
        if(is.null(recordVariable) || is.null(monetaryVariable) || sampleSize == 0)
            return()
    }

    if(sampleSize > nrow(dataset)){
        message <- "The required sample size is larger than the number of records in the dataset. Please respecify the sample size."
        table$errorMessage <- message
        table$error <- "badData"
        return()
    }

    if(startingPoint >= interval){
        message <- paste0("The starting point has to lie inside the interval of ", interval, ". Please respecify the starting point.")
        table$errorMessage <- message
        table$error <- "badData"
        return()
    }

    if(is.null(jaspResults[["sample"]])){
      if(!is.null(rankingVariable)){
          rankingColumn       <- dataset[, .v(rankingVariable)]
          dataset             <- dataset[order(rankingColumn), ]
      }

      if(type == "attributes"){
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          interval.mat        <- matrix(dataset[, .v(recordVariable)], ncol = interval, byrow = TRUE, nrow = sampleSize)

          sample.rows <- numeric()
          for(i in 1:sampleSize){
            sample.rows[i]    <- interval.mat[i, base::sample(1:ncol(interval.mat), size = 1)]
          }
      } else {
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          recordColumn        <- dataset[, .v(recordVariable)]
          monetaryColumn      <- dataset[, .v(monetaryVariable)]
          musList             <- rep(recordColumn, times = monetaryColumn)
          interval.mat        <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval)

          sample.rows <- numeric()
          for(i in 1:sampleSize){
              sample.rows[i]  <- interval.mat[i, base::sample(1:ncol(interval.mat), size = 1)]
              musList         <- musList[!musList %in% sample.rows[i]]
              interval.mat    <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval) # Redraw interval matrix
          }
      }

      sample                  <- as.data.frame(dataset[sample.rows, ])

      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                                                "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "samplingMethod"))
    }

    if(options[["showSample"]]){
        recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
        for(i in 1:nrow(sample)){
            row                 <- list()
            row[["number"]]     <- i
            row[["recordNumber"]] <- sample[i, recordColumnIndex]
            for(j in c(variables, monetaryVariable)){
                row[[j]]        <- as.character(sample[i, .v(j)])
            }
            table$addRows(row)
        }
    }
}

.SystematicSamplingTable            <- function(dataset, options, jaspResults, interval, type, sample, position = 2)
{

    if(!is.null(jaspResults[["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")        rankingVariable <- NULL
    variables                       <- unlist(options$variables)

    startingPoint                       <- options$startingPoint
    sampleSize                          <- options$sampleSize

    if(options[["showSample"]]){
      table                               <- createJaspTable("Resulting sample")
      jaspResults[["table"]]              <- table
      table$position                      <- position
      table$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                              "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "showSample",
                            "samplingMethod"))

      table$addColumnInfo(name="number",          title ="", type = "string")
      table$addColumnInfo(name="recordNumber",    title ="Record Number", type = "string")
      for(i in c(variables, monetaryVariable)){
          table$addColumnInfo(name=i,             type="string")
      }
    }

    if(type == "attributes"){
        if(is.null(recordVariable) || sampleSize == 0)
            return()
    } else {
        if(is.null(recordVariable) || is.null(monetaryVariable) || sampleSize == 0)
            return()
    }

    if(sampleSize > nrow(dataset)){
        message <- "The required sample size is larger than the number of records in the dataset. Please respecify the sample size."
        table$errorMessage <- message
        table$error <- "badData"
        return()
    }

    if(startingPoint >= interval){
        message <- paste0("The starting point has to lie inside the interval of ", interval, ". Please respecify the starting point.")
        table$errorMessage <- message
        table$error <- "badData"
        return()
    }

    if(is.null(jaspResults[["sample"]])){
      if(!is.null(rankingVariable)){
          rankingColumn       <- dataset[, .v(rankingVariable)]
          dataset             <- dataset[order(rankingColumn), ]
      }

      if(type == "attributes"){
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          interval.mat        <- matrix(dataset[, .v(recordVariable)], ncol = interval, byrow = TRUE, nrow = sampleSize)
          sample.rows         <- interval.mat[1:nrow(interval.mat), startingPoint]
      } else {
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          recordColumn        <- dataset[, .v(recordVariable)]
          monetaryColumn      <- dataset[, .v(monetaryVariable)]
          musList             <- rep(recordColumn, times = monetaryColumn)
          interval.mat        <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval)

          sample.rows <- numeric()
          for(i in 1:sampleSize){
              sample.rows[i]  <- interval.mat[i, startingPoint]
              musList         <- musList[!musList %in% sample.rows[i]]
              interval.mat    <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval) # Redraw interval matrix
          }
      }
      sample                  <- as.data.frame(dataset[sample.rows, ])
      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                              "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "samplingMethod"))
    }

    if(options[["showSample"]]){
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row                 <- list()
          row[["number"]]     <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          for(j in c(variables, monetaryVariable)){
              row[[j]]        <- as.character(sample[i, .v(j)])
          }
          table$addRows(row)
      }
    }
}

.samplingDescriptivesTable <- function(dataset, options, jaspResults, sample, position = 3)
{

    if(!is.null(jaspResults[["descriptives"]])) return()

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")       rankingVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    variables                       <- unlist(options$variables)


    all.variables                   <- c(rankingVariable, monetaryVariable, variables)
    descriptives                    <- createJaspTable("Sample Descriptives")
    jaspResults[["descriptives"]]   <- descriptives
    descriptives$transpose          <- TRUE
    descriptives$position           <- position

    descriptives$dependOnOptions(c("variables", "allowDuplicates", "seed", "sampleSize", "seedNumber",
                                    "showDescriptives", "mean", "sd", "var", "range", "min", "max", "median", "recordVariable", "rankingVariable",
                                   "variablesMUS", "recordNumberVariableMUS", "rankingVariableMUS", "monetaryVariableMUS"))

                                    descriptives$addColumnInfo(name="name",                        type="string", format="sf:4", title = "")
                                    descriptives$addColumnInfo(name="Valid cases",                 type="integer")
    if (options$mean)               descriptives$addColumnInfo(name="Mean",                        type="number", format="sf:4")
    if (options$median)             descriptives$addColumnInfo(name="Median",                      type="number", format="sf:4")
    if (options$sd)                 descriptives$addColumnInfo(name="Std. Deviation",              type="number", format="sf:4")
    if (options$var)                descriptives$addColumnInfo(name="Variance",                    type="number", format="sf:4")
    if (options$range)              descriptives$addColumnInfo(name="Range",                       type="number", format="sf:4")
    if (options$min)                descriptives$addColumnInfo(name="Minimum",                     type="number", format="sf:4")
    if (options$max)                descriptives$addColumnInfo(name="Maximum",                     type="number", format="sf:4")

    if(is.null(sample))
        return()

    for (variable in all.variables)
    {
      column    <- sample[[ .v(variable) ]]
      row <- list()

                                    row[["name"]]                   <- variable
                                    row[["Valid cases"]]            <- base::length(column)
      if(!is.factor(column))
      {
      if(options$mean)              row[["Mean"]]                   <- base::mean(column, na.rm = TRUE)
      if(options$sd)                row[["Std. Deviation"]]         <- stats::sd(column, na.rm = TRUE)
      if(options$var)               row[["Variance"]]               <- stats::var(column, na.rm = TRUE)
      if(options$median)            row[["Median"]]                 <- stats::median(column, na.rm = TRUE)
      if(options$range)             row[["Range"]]                  <- base::abs(base::range(column, na.rm = TRUE)[1] - base::range(column, na.rm = TRUE)[2])
      if(options$min)               row[["Minimum"]]                <- base::min(column, na.rm = TRUE)
      if(options$max)               row[["Maximum"]]                <- base::max(column, na.rm = TRUE)
      }

      descriptives$addRows(row)
    }

     descriptives$addFootnote(message="Not all statistics are available for <i>Nominal Text</i> variables", symbol="<i>Note.</i>")

}

.samplingInfoTable <- function(sample, total_data_value, options, jaspResults, position = 1, interval = NULL){

  if(!is.null(jaspResults[["simpleRandomSamplingInfoTable"]])) return()

  simpleRandomSamplingInfoTable                           <- createJaspTable("Sample Information")
  jaspResults[["simpleRandomSamplingInfoTable"]]          <- simpleRandomSamplingInfoTable
  simpleRandomSamplingInfoTable$position                  <- position
  simpleRandomSamplingInfoTable$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType", "samplingMethod",
                                                  "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "N", "seed", "seedNumber"))

  simpleRandomSamplingInfoTable$addColumnInfo(name="n", title ="Sample size", type = "string")
  simpleRandomSamplingInfoTable$addColumnInfo(name="V", title ="Sample value", type = "string")
  simpleRandomSamplingInfoTable$addColumnInfo(name="P", title ="% of total value", type = "string")
  if(options[["samplingType"]] != "simplerandomsampling")
    simpleRandomSamplingInfoTable$addColumnInfo(name="I", title ="Interval", type = "string")

  message <- paste0("The sample is drawn with <i>seed ", options[["seedNumber"]], "</i>.")
  simpleRandomSamplingInfoTable$addFootnote(message = message, symbol="<i>Note.</i>")

  sampleSize                              <- options$sampleSize
  sampleValue                             <- ceiling(sum(sample[, .v(options[["monetaryVariable"]])]))
  percOfTotal                             <- paste0(round(sampleValue / total_data_value * 100, 2), "%")

  row <- data.frame("n" = sampleSize, "V" = sampleValue, "P" = percOfTotal)
  if(options[["samplingType"]] != "simplerandomsampling")
    row <- cbind(row, I = interval)
  simpleRandomSamplingInfoTable$addRows(row)
}
