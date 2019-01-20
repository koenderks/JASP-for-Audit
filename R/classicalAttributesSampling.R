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


classicalAttributesSampling <- function(jaspResults, dataset, options, state=NULL)
{

  if(is.null(state))
      state 							    <- list()

    variables                       <- unlist(options$variables)
    recordVariable                  <- unlist(options$recordNumberVariable)
    if(recordVariable == "")        recordVariable <- NULL
    rankingVariable                 <- unlist(options$rankingVariable)
    if(rankingVariable == "")       rankingVariable <- NULL
    variables.to.read               <- c(recordVariable, variables, rankingVariable)

    if (is.null(dataset))
        dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

    if(options[["samplingType"]] == "simplerandomsampling"){
        .SimpleRandomSamplingTable(dataset, options, jaspResults, position = 1)
    } else if(options[["samplingType"]] == "systematicsampling"){

      if(options[["sampleSize"]] == 0){
          interval                    <- "Not defined"
      } else {
          interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
      }
        .intervalTable(dataset, options, jaspResults, interval, position = 1)
        .SystematicSamplingTable(dataset, options, jaspResults, interval, position = 2)

    } else if(options[["samplingType"]] == "cellsampling"){

      if(options[["sampleSize"]] == 0){
          interval                    <- "Not defined"
      } else {
          interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
      }
        .intervalTable(dataset, options, jaspResults, interval, position = 1)
        .cellSamplingTable(dataset, options, jaspResults, interval, position = 2)
    }
    sample                          <- jaspResults[["sample"]]$object

    if(options$showDescriptives)  .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 3)

    state[["options"]]              <- options
    return(state)
}
