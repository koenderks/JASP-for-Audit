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


attributesSimpleRandomSampling <- function(jaspResults, dataset, options, state=NULL)
{

    variables                       <- unlist(options$variables)
    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    variables.to.read               <- c(recordVariable, variables)

    if(is.null(state))              state <- list()

    if (is.null(dataset))
        dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

    jaspResults$title               <- "Attributes Simple Random Sampling"

    .SimpleRandomSamplingTable(dataset, options, jaspResults)
    sample                          <- jaspResults[["sample"]]$object

    if(options$showDescriptives)  .samplingDescriptivesTable(dataset, options, jaspResults, sample)

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
                                                                        "sampleLocations", "markSamples"))
         jaspResults[["sampleLocationsPlot"]] 		  $position <- 4
       }
    }

    state[["options"]]              <- options

    return(state)
}

.SimpleRandomSamplingTable          <- function(dataset, options, jaspResults, sample)
{

    if(!is.null(jaspResults[["table"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")
        recordVariable <- NULL

    variables                       <- unlist(options$variables)
    duplicates                      <- options$allowDuplicates
    sampleSize                      <- options$sampleSize
    seed                            <- options$seed
    manualSeed                      <- options$seedNumber
    table                           <- createJaspTable("Rsulting sample")
    jaspResults[["table"]]          <- table
    table$position                  <- 1

    table$dependOnOptions(c("variables","allowDuplicates", "seed", "sampleSize", "seedNumber", "recordNumberVariable"))

    table$addColumnInfo(name="number", title ="", type = "string")
    table$addColumnInfo(name="recordNumber", title ="Record Number", type = "string")
    for(i in variables){
            table$addColumnInfo(name=i,     type="string")
    }

    if(is.null(recordVariable) || sampleSize == 0)
        return()

    if(sampleSize > nrow(dataset) && duplicates == FALSE){
            message <- "The required sample size is larger than the number of records in the dataset. Please re-specify the sample size."
            table$errorMessage <- message
            table$error <- "badData"
            return()
    }

    if(seed == "seedDefault")
        set.seed(1)
    if(seed == "seedManual")
        set.seed(manualSeed)

    recordColumnIndex <- which(colnames(dataset) == .v(recordVariable))
    recordColumn <- rank(dataset[, .v(recordVariable)])

    samp <- base::sample(x = recordColumn, size = sampleSize, replace = duplicates)
    sample <- as.data.frame(dataset[samp, ])

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
