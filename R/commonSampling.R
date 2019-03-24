.randomSampling <- function(dataset, options, jaspResults, position = 1){

    if(!is.null(jaspResults[["selectionContainer"]][["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    variables                       <- unlist(options$variables)

    sampleSize                      <- jaspResults[["sampleSize"]]$object
    set.seed(options$seedNumber)

    if(is.null(jaspResults[["sample"]]$object)){
      
      recordColumnIndex <- which(colnames(dataset) == .v(recordVariable))
      recordColumn <- dataset[, recordColumnIndex]

      if(options[["samplingMethod"]] == "recordsampling"){
          samplingRegion <- 1:nrow(dataset)
          samp <- base::sample(x = samplingRegion, size = sampleSize, replace = TRUE)
          sample <- as.data.frame(dataset[samp, ])
          colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
      } else {
          monetaryColumn <- dataset[, .v(options[["monetaryVariable"]])]
          monetaryColumn <- ceiling(monetaryColumn)
          sampleVector <- base::sample(recordColumn, size = sampleSize, replace = TRUE, prob = monetaryColumn)
          sample <- as.data.frame(dataset[recordColumn %in% sampleVector, ])
          colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
      }
      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOnOptions(c("variables", "seedNumber", "sampleSize", "recordNumberVariable", "monetaryVariable", "samplingMethod", "samplingType", "auditType", "startingPoint",
                                                  "expected.errors", "kNumberNumber", "kPercentageNumber", "distribution", "IR", "CR"))
    }
    
    sample <- jaspResults[["sample"]]$object
    
    if(options[["showSample"]]){
      table                           <- createJaspTable("Selected observations")
      jaspResults[["selectionContainer"]][["table"]]          <- table
      table$position                  <- position

      table$dependOnOptions(c("variables", "sampleSize", "seedNumber", "recordNumberVariable", "monetaryVariable", "samplingType", "auditType",
                              "showSample", "samplingMethod", "expected.errors", "kNumberNumber", "kPercentageNumber"))

      table$addColumnInfo(name="number", title ="", type = "string")
      table$addColumnInfo(name="recordNumber", title ="Record Number", type = "string")
      if(options[["monetaryVariable"]] != "")
        table$addColumnInfo(name="bookValue", title ="Book value", type = "string")
      for(i in variables){
          table$addColumnInfo(name=i,     type="string")
      }
    
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row <- list()
          row[["number"]] <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          if(options[["monetaryVariable"]] != ""){
            row[["bookValue"]] <- sample[i, .v(options[["monetaryVariable"]])]
          }
          for(j in c(variables)){
              row[[j]] <- as.character(sample[i, .v(j)])
          }
          table$addRows(row)
      }
    }
}

.cellSampling <- function(dataset, options, jaspResults, position = 2){

    if(!is.null(jaspResults[["selectionContainer"]][["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")        rankingVariable <- NULL
    variables                       <- unlist(options$variables)

    startingPoint                       <- options[["startingPoint"]]
    sampleSize                          <- jaspResults[["sampleSize"]]$object
    set.seed(options[["seedNumber"]])

    if(is.null(jaspResults[["sample"]]$object)){
      
      if(!is.null(rankingVariable)){
          rankingColumn       <- dataset[, .v(rankingVariable)]
          dataset             <- dataset[order(rankingColumn), ]
      }

      if(options[["samplingMethod"]] == "recordsampling"){
        
        if(is.null(rankingVariable) && options[["monetaryVariable"]] != "")
          dataset             <- dataset[order(dataset[, .v(monetaryVariable)]), ]
        
          interval <- ceiling(nrow(dataset) / jaspResults[["sampleSize"]]$object)
        
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          recordColumn        <- dataset[, .v(recordVariable)]
          interval.mat        <- matrix(dataset[, .v(recordVariable)], ncol = interval, byrow = TRUE, nrow = sampleSize)
          
          sample.rows <- numeric(jaspResults[["sampleSize"]]$object)
          for(i in 1:nrow(interval.mat)){
              sample.rows[i] <- interval.mat[i, base::sample(1:ncol(interval.mat), size = 1)]
          }
          sample                  <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
          colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
          
      } else {
        
        if(is.null(rankingVariable) && options[["monetaryVariable"]] != "")
          dataset             <- dataset[order(dataset[, .v(monetaryVariable)]), ]
        
          interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / jaspResults[["sampleSize"]]$object)
        
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          recordColumn        <- dataset[, .v(recordVariable)]
          monetaryColumn      <- dataset[, .v(monetaryVariable)]
          musList             <- rep(recordColumn, times = monetaryColumn)
          interval.mat        <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval)

          sample.rows <- numeric(jaspResults[["sampleSize"]]$object)
          for(i in 1:nrow(interval.mat)){
              sample.rows[i] <- interval.mat[i, base::sample(1:ncol(interval.mat), size = 1)]
          }
          sample                  <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
          colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
      }

      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOnOptions(c("variables", "seedNumber", "sampleSize", "recordNumberVariable", "monetaryVariable", "samplingMethod", "samplingType", "auditType", "startingPoint",
                                                  "expected.errors", "kNumberNumber", "kPercentageNumber", "distribution", "IR", "CR"))
    }
    
    sample <- jaspResults[["sample"]]$object
    
    if(options[["showSample"]]){
      table                           <- createJaspTable("Selected observations")
      jaspResults[["selectionContainer"]][["table"]]          <- table
      table$position                  <- position

      table$dependOnOptions(c("variables", "sampleSize", "seedNumber", "recordNumberVariable", "monetaryVariable", "samplingType", "auditType",
                              "showSample", "samplingMethod", "expected.errors", "kNumberNumber", "kPercentageNumber"))

      table$addColumnInfo(name="number", title ="", type = "string")
      table$addColumnInfo(name="recordNumber", title ="Record Number", type = "string")
      if(options[["monetaryVariable"]] != "")
        table$addColumnInfo(name="bookValue", title ="Book value", type = "string")
      for(i in variables){
          table$addColumnInfo(name=i,     type="string")
      }
    
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row <- list()
          row[["number"]] <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          if(options[["monetaryVariable"]] != ""){
            row[["bookValue"]] <- sample[i, .v(options[["monetaryVariable"]])]
          }
          for(j in c(variables)){
              row[[j]] <- as.character(sample[i, .v(j)])
          }
          table$addRows(row)
      }
    }
}

.systematicSampling <- function(dataset, options, jaspResults, position = 2)
{

    if(!is.null(jaspResults[["selectionContainer"]][["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options$monetaryVariable)
    if(monetaryVariable == "")      monetaryVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")        rankingVariable <- NULL
    variables                       <- unlist(options$variables)

    startingPoint                       <- options[["startingPoint"]]
    sampleSize                          <- jaspResults[["sampleSize"]]$object

    if(is.null(jaspResults[["sample"]])){
      
      if(!is.null(rankingVariable)){
          rankingColumn       <- dataset[, .v(rankingVariable)]
          dataset             <- dataset[order(rankingColumn), ]
      }

      if(options[["samplingMethod"]] == "recordsampling"){
        
        if(is.null(rankingVariable) && options[["monetaryVariable"]] != "")
          dataset             <- dataset[order(dataset[, .v(monetaryVariable)]), ]
        
        interval <- ceiling(nrow(dataset) / jaspResults[["sampleSize"]]$object)
        
        recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
        recordColumn        <- dataset[, .v(recordVariable)]
        interval.mat        <- matrix(dataset[, .v(recordVariable)], ncol = interval, byrow = TRUE, nrow = sampleSize)  
        sample.rows       <- interval.mat[1:nrow(interval.mat), startingPoint]
        sample                  <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
        colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
          
      } else {
        
        if(is.null(rankingVariable) && options[["monetaryVariable"]] != "")
          dataset             <- dataset[order(dataset[, .v(monetaryVariable)]), ]
        
        interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / jaspResults[["sampleSize"]]$object)
        
        recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
        recordColumn        <- dataset[, .v(recordVariable)]
        monetaryColumn      <- dataset[, .v(monetaryVariable)]
        musList             <- rep(recordColumn, times = monetaryColumn)
        interval.mat        <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval)

        sample.rows         <- interval.mat[1:nrow(interval.mat), startingPoint]
        sample              <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
        colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
      }

      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOnOptions(c("variables", "seedNumber", "sampleSize", "recordNumberVariable", "monetaryVariable", "samplingMethod", "samplingType", "auditType", "startingPoint",
                                                  "expected.errors", "kNumberNumber", "kPercentageNumber", "distribution", "IR", "CR"))
    }
    
    sample <- jaspResults[["sample"]]$object
    
    if(options[["showSample"]]){
      table                           <- createJaspTable("Selected observations")
      jaspResults[["selectionContainer"]][["table"]]          <- table
      table$position                  <- position

      table$dependOnOptions(c("variables", "sampleSize", "seedNumber", "recordNumberVariable", "monetaryVariable", "samplingType", "auditType",
                              "showSample", "samplingMethod", "expected.errors", "kNumberNumber", "kPercentageNumber"))

      table$addColumnInfo(name="number", title ="", type = "string")
      table$addColumnInfo(name="recordNumber", title ="Record Number", type = "string")
      if(options[["monetaryVariable"]] != "")
        table$addColumnInfo(name="bookValue", title ="Book value", type = "string")
      for(i in variables){
          table$addColumnInfo(name=i,     type="string")
      }
    
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row <- list()
          row[["number"]] <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          if(options[["monetaryVariable"]] != ""){
            row[["bookValue"]] <- sample[i, .v(options[["monetaryVariable"]])]
          }
          for(j in c(variables)){
              row[[j]] <- as.character(sample[i, .v(j)])
          }
          table$addRows(row)
      }
    }
}

.sampleDescriptives <- function(dataset, options, jaspResults, position = 3){

    if(!is.null(jaspResults[["selectionContainer"]][["descriptives"]])) return()

    if(options[["showDescriptives"]]){
      
      sample <- jaspResults[["sample"]]$object

      recordVariable                  <- unlist(options$recordNumberVariable)
      if(recordVariable == "")        recordVariable <- NULL
      rankingVariable                 <- unlist(options$rankingVariable)
      if(rankingVariable == "")       rankingVariable <- NULL
      monetaryVariable                <- unlist(options$monetaryVariable)
      if(monetaryVariable == "")      monetaryVariable <- NULL
      variables                       <- unlist(options$variables)


      all.variables                   <- c(rankingVariable, monetaryVariable, variables)
      descriptives                    <- createJaspTable("Sample Descriptives")
      jaspResults[["selectionContainer"]][["descriptives"]]   <- descriptives
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
}

.samplingInfo <- function(dataset, options, jaspResults, position = 1){

  if(!is.null(jaspResults[["selectionContainer"]][["simpleRandomSamplingInfoTable"]])) return()

  simpleRandomSamplingInfoTable                           <- createJaspTable("Sample Information")
  jaspResults[["selectionContainer"]][["simpleRandomSamplingInfoTable"]]          <- simpleRandomSamplingInfoTable
  simpleRandomSamplingInfoTable$position                  <- position
  simpleRandomSamplingInfoTable$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType", "samplingMethod",
                                                  "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "N", "seed", "seedNumber"))

  simpleRandomSamplingInfoTable$addColumnInfo(name="n", title ="Sample size", type = "string")
  if(options[["auditType"]] == "mus"){
    simpleRandomSamplingInfoTable$addColumnInfo(name="V", title ="Sample value", type = "string")
    simpleRandomSamplingInfoTable$addColumnInfo(name="P", title ="% of total value", type = "string")  
  } else {
    simpleRandomSamplingInfoTable$addColumnInfo(name="P", title ="% of total observations", type = "string")  
  }
  if(options[["samplingType"]] != "simplerandomsampling")
    simpleRandomSamplingInfoTable$addColumnInfo(name="I", title ="Interval", type = "string")

  message <- paste0("The sample is drawn with <i>seed ", options[["seedNumber"]], "</i>.")
  simpleRandomSamplingInfoTable$addFootnote(message = message, symbol="<i>Note.</i>")
  
  sample <- jaspResults[["sample"]]$object
  total_data_value <- jaspResults[["total_data_value"]]$object 
  if(options[["samplingMethod"]] == "recordsampling"){
    interval <- ceiling(nrow(dataset) / jaspResults[["sampleSize"]]$object)
  } else {
    interval <- ceiling(sum(dataset[, .v(options[["monetaryVariable"]])]) / jaspResults[["sampleSize"]]$object)
  }

  sampleSize                              <- length(unique(sample[, .v(options[["recordNumberVariable"]])]))
  if(options[["auditType"]] == "mus"){
    sampleValue                             <- ceiling(sum(sample[, .v(options[["monetaryVariable"]])]))
    percOfTotal                             <- paste0(round(sampleValue / total_data_value * 100, 2), "%")
    row <- data.frame("n" = sampleSize, "V" = sampleValue, "P" = percOfTotal)
  } else {
    percOfTotal                             <- paste0(round(sampleSize / jaspResults[["N"]]$object * 100, 2), "%")
    row <- data.frame("n" = sampleSize, "P" = percOfTotal)
  }

  if(options[["samplingType"]] != "simplerandomsampling")
    row <- cbind(row, I = interval)
  simpleRandomSamplingInfoTable$addRows(row)
}
