.SimpleRandomSamplingTable          <- function(dataset, options, jaspResults, type, sample, position = 1)
{

    if(!is.null(jaspResults[["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    if(type == "attributes"){
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        variables                       <- unlist(options$variables)
        monetaryVariable                <- NULL
    } else {
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        monetaryVariable                <- unlist(options$monetaryVariable)
        if(monetaryVariable == "")      monetaryVariable <- NULL
        variables                       <- unlist(options$variables)
    }

    duplicates                      <- options$allowDuplicates
    sampleSize                      <- options$sampleSize
    seed                            <- options$seed
    manualSeed                      <- options$seedNumber
    if(options[["showSample"]]){
      table                           <- createJaspTable("Resulting sample")
      jaspResults[["table"]]          <- table
      table$position                  <- position

      table$dependOnOptions(c("variables", "allowDuplicates", "seed", "sampleSize", "seedNumber", "recordNumberVariable",
                              "samplingType", "auditType", "recordNumberVariableMUS", "monetaryVariableMUS", "variablesMUS", "showSample"))

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

    if(seed == "seedDefault")
        set.seed(1)
    if(seed == "seedManual")
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
                              "samplingType", "auditType", "recordNumberVariableMUS", "monetaryVariableMUS", "variablesMUS"))
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

    if(type == "attributes"){
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        rankingVariable                 <- unlist(options$rankingVariable)
        if(rankingVariable == "")        rankingVariable <- NULL
        variables                       <- unlist(options$variables)
        monetaryVariable                <- NULL
    } else {
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        monetaryVariable                <- unlist(options$monetaryVariable)
        if(monetaryVariable == "")      monetaryVariable <- NULL
        rankingVariable                 <- unlist(options$rankingVariable)
        if(rankingVariable == "")        rankingVariable <- NULL
        variables                       <- unlist(options$variables)
    }

    startingPoint                       <- options$startingPoint
    sampleSize                          <- options$sampleSize

    if(options[["showSample"]]){
        table                               <- createJaspTable("Resulting sample")
        jaspResults[["table"]]              <- table
        table$position                      <- position
        table$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                                "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "showSample"))

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
                                                "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS"))
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

    if(type == "attributes"){
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        rankingVariable                 <- unlist(options$rankingVariable)
        if(rankingVariable == "")        rankingVariable <- NULL
        variables                       <- unlist(options$variables)
        monetaryVariable                <- NULL
    } else {
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        monetaryVariable                <- unlist(options$monetaryVariable)
        if(monetaryVariable == "")      monetaryVariable <- NULL
        rankingVariable                 <- unlist(options$rankingVariable)
        if(rankingVariable == "")        rankingVariable <- NULL
        variables                       <- unlist(options$variables)
    }

    startingPoint                       <- options$startingPoint
    sampleSize                          <- options$sampleSize

    if(options[["showSample"]]){
      table                               <- createJaspTable("Resulting sample")
      jaspResults[["table"]]              <- table
      table$position                      <- position
      table$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                              "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "showSample"))

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
                              "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS"))
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

    if(options[["auditType"]] == "attributes"){
      recordVariable                  <- unlist(options$recordNumberVariable)
      if(recordVariable == "")        recordVariable <- NULL
      rankingVariable                 <- unlist(options$rankingVariable)
      if(rankingVariable == "")       rankingVariable <- NULL
      monetaryVariable                <- NULL
      variables                       <- unlist(options$variables)
    } else {
      recordVariable                  <- unlist(options$recordNumberVariableMUS)
      if(recordVariable == "")        recordVariable <- NULL
      monetaryVariable                <- unlist(options$monetaryVariableMUS)
      if(monetaryVariable == "")      monetaryVariable <- NULL
      rankingVariable                 <- unlist(options$rankingVariableMUS)
      if(rankingVariable == "")       rankingVariable <- NULL
      variables                       <- unlist(options$variablesMUS)
    }

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

.intervalTable <- function(dataset, options, jaspResults, interval, position = 1)
{

    if(!is.null(jaspResults[["intervalTable"]])) return()

    intervalTable                           <- createJaspTable("Sampling information")
    jaspResults[["intervalTable"]]          <- intervalTable
    intervalTable$position                  <- position
    intervalTable$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                            "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "N"))

    intervalTable$addColumnInfo(name="N", title ="Population size", type = "string")
    intervalTable$addColumnInfo(name="n", title ="Sample size", type = "string")
    if(options[["auditType"]] == "mus"){
      intervalTable$addColumnInfo(name="T", title ="Total value", type = "string")
    }
    intervalTable$addColumnInfo(name="I", title ="Interval width", type = "string")

    sampleSize                              <- options$sampleSize

    if(options[["auditType"]] == "mus"){
        row <- list("N" = nrow(dataset), "n" = sampleSize, "T" = round(sum(dataset[, .v(options[["monetaryVariableMUS"]])]), 2),"I" = round(interval, 0))
    } else {
        row <- list("N" = nrow(dataset), "n" = sampleSize, "I" = round(interval, 0))
    }
    intervalTable$addRows(row)

}

.plotSampleLocations <- function(options, rows, samples, xlab, jaspResults){

  df <- data.frame()
  p <- ggplot2::ggplot(df) +
      ggplot2::geom_point() +
      ggplot2::xlim(min(rows) - 1, max(rows) + 1) +
      ggplot2::ylim(-0.5, 0.5) +
      ggplot2::ylab(NULL) +
      ggplot2::xlab(xlab) +
      ggplot2::geom_segment(data = data.frame(x = min(rows) - 1, xend = max(rows) + 1, y = 0, yend = 0),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = min(rows) - 1, xend = min(rows) - 1, y = 0.3, yend = -0.3),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = max(rows) + 1, xend = max(rows) + 1, y = 0.3, yend = -0.3),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1) +
      ggplot2::geom_segment(data = data.frame(x = samples, xend = samples, y = rep(0.1, length(samples)), yend = rep(-0.1, length(samples))),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1, color = "red")

      if(options[["markSamples"]]){
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = samples, y = rep(0, length(samples)), label = samples), vjust = -1, hjust = -0.5)
      }

  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_blank())

  return(createJaspPlot(plot = p, title = "Sampling locations", width = 1000, height = 200))

}

.simpleRandomSamplingInfoTable <- function(dataset, options, jaspResults, position = 1){

  if(!is.null(jaspResults[["simpleRandomSamplingInfoTable"]])) return()

  simpleRandomSamplingInfoTable                           <- createJaspTable("Sampling information")
  jaspResults[["simpleRandomSamplingInfoTable"]]          <- simpleRandomSamplingInfoTable
  simpleRandomSamplingInfoTable$position                  <- position
  simpleRandomSamplingInfoTable$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable", "samplingType",
                                                  "variablesMUS", "rankingVariableMUS", "recordNumberVariableMUS", "monetaryVariableMUS", "N"))

  simpleRandomSamplingInfoTable$addColumnInfo(name="N", title ="Population size", type = "string")
  simpleRandomSamplingInfoTable$addColumnInfo(name="n", title ="Sample size", type = "string")
  if(options[["auditType"]] == "mus"){
    simpleRandomSamplingInfoTable$addColumnInfo(name="T", title ="Total value", type = "string")
  }

  sampleSize                              <- options$sampleSize

  if(options[["auditType"]] == "mus"){
      row <- list("N" = nrow(dataset), "n" = sampleSize, "T" = round(sum(dataset[, .v(options[["monetaryVariable"]])]), 2))
  } else {
      row <- list("N" = nrow(dataset), "n" = sampleSize)
  }
  simpleRandomSamplingInfoTable$addRows(row)

}
