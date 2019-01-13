#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


attributesSystematicSampling <- function(jaspResults, dataset, options, state=NULL)
{

    variables                       <- unlist(options$variables)
    sampleSize                      <- options$sampleSize
    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")       rankingVariable <- NULL
    variables.to.read               <- c(recordVariable, variables, rankingVariable)

    if(is.null(state))              state <- list()

    if (is.null(dataset))
        dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

    jaspResults$title               <- "Attributes Systematic Sampling"

    if(sampleSize == 0){
        interval                    <- "Not defined"
    } else {
        interval <- ceiling(nrow(dataset) / sampleSize)
    }

    .intervalTable(dataset, options, jaspResults, interval)

    .SystematicSamplingTable(dataset, options, jaspResults, interval)
    sample                          <- jaspResults[["sample"]]$object

    if(options$showDescriptives)    .samplingDescriptivesTable(dataset, options, jaspResults, sample)

    if(options[['sampleLocations']] && !is.null(recordVariable) && !is.null(sample))
    {
       if(is.null(jaspResults[["sampleLocationsPlot"]]))
       {
         if(ncol(sample) == 1){
           jaspResults[["sampleLocationsPlot"]] 		  <- .plotSampleLocations(options, 1:nrow(dataset), sample[, 1], "Index", jaspResults)
         } else {
            jaspResults[["sampleLocationsPlot"]] 		  <- .plotSampleLocations(options, 1:nrow(dataset), sample[, .v(recordVariable)], "Index", jaspResults)
         }
       jaspResults[["sampleLocationsPlot"]]		     $dependOnOptions(c("variables","allowDuplicates", "seed", "sampleSize", "seedNumber", "recordNumberVariable",
                                                                      "sampleLocations", "markSamples", "rankingVariable"))
       jaspResults[["sampleLocationsPlot"]] 		  $position <- 4
       }
    }

    state[["options"]]              <- options

    return(state)
}

.SystematicSamplingTable            <- function(dataset, options, jaspResults, interval)
{

    if(!is.null(jaspResults[["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")       rankingVariable <- NULL

    variables                       <- unlist(options$variables)
    startingPoint                   <- options$startingPoint
    sampleSize                      <- options$sampleSize
    table                           <- createJaspTable("Resulting sample")
    jaspResults[["table"]]          <- table
    table$position                  <- 2

    table$dependOnOptions(c("variables", "startingPoint", "sampleSize", "recordNumberVariable", "rankingVariable"))

    table$addColumnInfo(name="number", title ="", type = "string")
    table$addColumnInfo(name="recordNumber", title ="Record Number", type = "string")
    for(i in variables){
        table$addColumnInfo(name=i,     type="string")
    }

    if(is.null(recordVariable) || sampleSize == 0)
        return()

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

    recordColumnIndex <- which(colnames(dataset) == .v(recordVariable))

    if(!is.null(rankingVariable)){
      rankingColumn       <- dataset[, .v(rankingVariable)]
      dataset             <- dataset[order(rankingColumn), ]
    }

    interval.mat <- matrix(dataset[, .v(recordVariable)], ncol = interval, byrow = TRUE, nrow = sampleSize)
    sample.rows <- interval.mat[1:nrow(interval.mat), startingPoint]
    sample <- as.data.frame(dataset[sample.rows, ])

    for(i in 1:nrow(sample)){
        row <- list()
        row[["number"]] <- i
        row[["recordNumber"]] <- sample[i, recordColumnIndex]
        for(j in variables){
            row[[j]] <- as.character(sample[i, .v(j)])
        }
        table$addRows(row)
    }

    jaspResults[["sample"]] <- createJaspState(sample)
    jaspResults[["sample"]]$copyDependenciesFromJaspObject(table)

}
