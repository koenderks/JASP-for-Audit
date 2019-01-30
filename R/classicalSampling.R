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

classicalSampling <- function(jaspResults, dataset, options, state=NULL)
{

  if(is.null(state))
      state 							    <- list()

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
  variables.to.read               <- c(recordVariable, variables, rankingVariable, monetaryVariable)

  if (is.null(dataset))
      dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  # Only runs when a record variable has been specified
  if(!is.null(recordVariable)){

    type <- options[["auditType"]]

    # Perform the sampling and draw the outcome tables
    if(options[["samplingType"]] == "simplerandomsampling"){
      if(type == "attributes"){
        .simpleRandomSamplingInfoTable(dataset, options, jaspResults, position = 1)
        .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "attributes", sample = jaspResults[["sample"]]$object, position = 2)
      } else {
        if(!is.null(monetaryVariable)){
          .simpleRandomSamplingInfoTable(dataset, options, jaspResults, position = 1)
          .SimpleRandomSamplingTable(dataset, options, jaspResults, type = "mus", sample = jaspResults[["sample"]]$object, position = 2)
        }
      }
    } else if(options[["samplingType"]] == "systematicsampling"){
      if(type == "attributes"){
        interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
        .intervalTable(dataset, options, jaspResults, interval, position = 1)
        .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 2)
      } else {
        if(!is.null(monetaryVariable)){
          interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
          .intervalTable(dataset, options, jaspResults, interval, position = 1)
          .SystematicSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 2)
        }
      }
    } else if(options[["samplingType"]] == "cellsampling"){
      if(type == "attributes"){
        interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
        .intervalTable(dataset, options, jaspResults, interval, position = 1)
        .cellSamplingTable(dataset, options, jaspResults, interval, type = "attributes", sample = jaspResults[["sample"]]$object, position = 2)
      } else {
        if(!is.null(monetaryVariable)){
          interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / options[["sampleSize"]])
          .intervalTable(dataset, options, jaspResults, interval, position = 1)
          .cellSamplingTable(dataset, options, jaspResults, interval, type = "mus", sample = jaspResults[["sample"]]$object, position = 2)
        }
      }
    }
    # Store the sample
    sample                          <- jaspResults[["sample"]]$object

    # Descriptives table
    if(options[["showDescriptives"]]){
      .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 3)
    }
  }

    state[["options"]]              <- options
    return(state)
}
