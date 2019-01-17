audit <- function(jaspResults, dataset, options, state=NULL){

    options[["statistic"]] <- "bound" # Temporary

    if(is.null(state))
        state 							    <- list()

    jaspResults$title   <- "Audit"                       # Specify the title of the analysis

    # Planning phase

    .ARMformula(options, jaspResults, position = 5)   # Show the Audit Risk Model formula and quantify detection risk
    DR              <- jaspResults[["DR"]]$object

    if(options[["interpretation"]]){
        jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Procedure</u>", "h2")
        jaspResults[["procedureHeader"]]$position <- 1
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of an attributes bound procedure is to determine with a specified <b>", options[["confidence"]]*100,"%</b> confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality of <b>",options[["materiality"]]*100,"%</b>. An attributes bound procedure considers
                                                                  the observations in the population to be of one of two categories: 1) the observation is fully incorrect or 2) the observation
                                                                  is fully correct."), "p")
        jaspResults[["procedureParagraph"]]$position <- 2
        jaspResults[["AuditRiskModelHeader"]] <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
        jaspResults[["AuditRiskModelHeader"]]$position <- 3
        jaspResults[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined
                                                                        to be <b>", options$CR,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be <b>",round(DR*100, 2), "%</b>."), "p")
        jaspResults[["AuditRiskModelParagraph"]]$position <- 4
    }

    jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Planning</u>", "h2")
    jaspResults[["priorKnowledgeHeader"]]$position <- 6

    if(options[["inference"]] == "frequentist"){

        .attributesPlanning(options, jaspResults)
        result              <- jaspResults[["result"]]$object
        .attributesPlanningTable(options, result, jaspResults, position = 8)

        if(options[["interpretation"]]){

            if(options[["expected.errors"]] == "kPercentage"){
                expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
                max.errors <- ceiling(options[["kPercentageNumber"]] * result[["n"]])
            } else {
                expected.errors <- options[["kNumberNumber"]]
                max.errors <- options[["kNumberNumber"]]
            }
            jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The sample size that is required to prove an <b>",options$materiality*100,"%</b>
                                                                            upper confidence bound, assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. The sample size is calculated with the <b>", options[["distribution"]] , "</b>
                                                                            distribution. Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the population cannot be approved."), "p")
            jaspResults[["priorKnowledgeParagraph"]]$position <- 7

        }

    } else if(options[["inference"]] == "bayesian"){

        .bayesianAttributesPlanning(options, jaspResults)
        result              <- jaspResults[["result"]]$object
        .bayesianAttributesPlanningTable(options, result, jaspResults, position = 8)

        if(options[["interpretation"]]){

            if(options[["expected.errors"]] == "kPercentage"){
                expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
                max.errors <- ceiling(options[["kPercentageNumber"]] * result[["n"]])
            } else {
                expected.errors <- options[["kNumberNumber"]]
                max.errors <- options[["kNumberNumber"]]
            }
            jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The probability distribution that corresponds with
                                                                          this prior knowledge is the <b>Beta(",round(result[["priorA"]],2), ",", round(result[["priorB"]],2),")</b> distribution. This probability distribution states that there is a <b>",
                                                                              round(pbeta(options[["materiality"]], result[["priorA"]],result[["priorB"]])*100, 2) ,"%</b> prior probability that the
                                                                          population error is lower than materiality. The sample size that is required to prove an <b>",options$materiality*100,"%</b> upper confidence bound, assuming
                                                                          the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. Consequently, if <b>", max.errors ,"</b> or more full errors are found, the population cannot be approved."), "p")
            jaspResults[["priorKnowledgeParagraph"]]$position <- 7

        }

        if (options[["implicitsample"]])
        {
            if(is.null(jaspResults[["sampletable"]]))
                .priorSampleTable(options, result, jaspResults, position = 9)
        }

        if(options[['plotPrior']])
        {
            if(is.null(jaspResults[["priorPlot"]]))
            {
                jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanning(options, result, jaspResults)
                jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                                 "plotPrior", "plotPrior", "show",
                                                                 "statistic", "kPercentageNumber", "kNumberNumber", "inference"))
                jaspResults[["priorPlot"]] 		$position <- 10
            }
        }

    }

    # Sampling phase

    if(options[["recordNumberVariable"]] != ""){

        options[["sampleSize"]] <- result[["n"]]

        variables                       <- unlist(options$variables)
        recordVariable                  <- unlist(options$recordNumberVariable)
        if(recordVariable == "")        recordVariable <- NULL
        rankingVariable                 <- unlist(options$rankingVariable)
        if(rankingVariable == "")       rankingVariable <- NULL
        correctID                       <- unlist(options$correctID)
        if(correctID == "")             correctID <- NULL
        sampleFilter                    <- unlist(options$sampleFilter)
        if(sampleFilter == "")          sampleFilter <- NULL
        variables.to.read               <- c(recordVariable, variables, rankingVariable, correctID, sampleFilter)

        if (is.null(dataset))
            dataset                     <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

        jaspResults[["samplingHeader"]] <- createJaspHtml("<u>Sampling</u>", "h2")
        jaspResults[["samplingHeader"]]$position <- 11

        if(options[["interpretation"]]){

            if(options[["samplingType"]] == "simplerandomsampling"){
                technique <- "simple random sampling"
            } else if(options[["samplingType"]] == "systematicsampling"){
                technique <- "systematic sampling"
            } else if(options[["samplingType"]] == "cellsampling"){
                technique <- "cell sampling"
            }

            jaspResults[["samplingParagraph"]] <- createJaspHtml(paste0("From the population of ", options[["N"]], " observations, ", result[["n"]], " samples were drawn using a <b>", technique, "</b> method."), "p")
            jaspResults[["samplingParagraph"]]$position <- 12

        }

        if(options[["samplingType"]] == "simplerandomsampling"){
            .SimpleRandomSamplingTable(dataset, options, jaspResults, position = 13)
        } else if(options[["samplingType"]] == "systematicsampling"){

            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 13)
            .SystematicSamplingTable(dataset, options, jaspResults, interval, position = 14)

        } else if(options[["samplingType"]] == "cellsampling"){

            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 13)
            .cellSamplingTable(dataset, options, jaspResults, interval, position = 14)
        }
        sample                          <- jaspResults[["sample"]]$object

        if(options$showDescriptives)  .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 15)

    }

    # Evaluation phase

    if(options[["correctID"]] != ""){

        if(options[["sampleFilter"]] != ""){
            dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
        }

        jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
        jaspResults[["evaluationHeader"]]$position <- 16

        if(options[["inference"]] == "frequentist"){

            .attributesBound(dataset, options, jaspResults)
            result                                       <- jaspResults[["result"]]$object
            .attributesBoundTableFullAudit(options, result, jaspResults, position = 18)

            if(options[["interpretation"]]){

                jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>",options$n, "</b> observations, <b>", result[["k"]] , "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                                      bined with the prior knowledge results in an <b>",round((1 - result[["alpha"]]) * 100, 2), "%</b> upper confidence bound of <b>",round(result[["bound"]]*100, 2),"%</b>. The cumulative knowledge states that there
                                                                      is a <b>",options$confidence*100, "%</b> probability that, when one would repeaditly sample from this population, the maximum error is calculated to be lower
                                                                      than <b>",round(result[["bound"]]*100, 2),"%</b>."), "p")
                jaspResults[["resultParagraph"]]$position <- 17

            }

        } else if(options[["inference"]] == "bayesian"){

            options[["statistic"]] <- "bound"

            .bayesianAttributesBound(dataset, options, jaspResults)
            result                                       <- jaspResults[["result"]]$object
            .bayesianAttributesBoundTableFullAudit(options, result, jaspResults, position = 18)

            if(options[["interpretation"]]){

                jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>",options$n, "</b> observations, <b>",result[["k"]], "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                                      bined with the prior knowledge results in an <b>",options$confidence*100, "%</b> upper confidence bound of <b>",round(result[["bound"]]*100, 2),"%</b>. The cumulative knowledge states that there
                                                                      is a <b>",options$confidence*100, "%</b> probability that the true error proportion in the population is lower than <b>",round(result[["bound"]]*100, 2),"%</b>."), "p")
                jaspResults[["resultParagraph"]]$position <- 17

            }

            if(options[['plotPriorAndPosterior']] && options[["correctID"]] != "")
            {
                if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
                {
                    jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBound(options, result, jaspResults)
                    jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "statistic", "plotPriorAndPosterior",
                                                                               "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                               "expected.errors", "kPercentageNumber", "kNumberNumber", "inference", "sampleFilter"))
                    jaspResults[["priorAndPosteriorPlot"]] 		$position <- 19
                }
            }

        }

        if(options[["interpretation"]]){

            jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
            jaspResults[["conclusionTitle"]]$position <- 20

            if(result[["bound"]] < options[["materiality"]]){
                above_below <- "lower"
                approve <- "<b>no material misstatement</b>"
            } else if(result[["bound"]] >= options[["materiality"]]){
                above_below <- "higher"
                approve <- "<b>material misstatement, or more information has to be seen.</b>"
            }

            jaspResults[["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", options$confidence*100 ,"%</b> upper confidence bound on the population proportion of full errors should be determined to be
                                                                        lower than materiality, in this case <b>", options$materiality*100 ,"%</b>. For the current data, the confidence bound is ", above_below ," than materiality. The conclusion for
                                                                        these data is that the data contain ", approve ,"."), "p")
            jaspResults[["conclusionParagraph"]]$position <- 21

        }

    }

    # Save the state
    state[["options"]] 					<- options
    return(state)

}

.ARMformula <- function(options, jaspResults, position = 2){

    if(!is.null(jaspResults[["ARMformula"]])) return()

    AR <- 1 - options[["confidence"]]

    if(options[["IR"]] == "Low" && options[["CR"]] == "Low"){
        IR <- 0.30
        CR <- 0.30
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "Medium"){
        IR <- 0.30
        CR <- 0.60
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "High"){
        IR <- 0.30
        CR <- 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "High"){
        IR <- 0.60
        CR <- 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Medium"){
        IR <- 0.60
        CR <- 0.60
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Low"){
        IR <- 0.60
        CR <- 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Low"){
        IR <- 1
        CR <- 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Medium"){
        IR <- 1
        CR <- 0.60
    } else if (options[["IR"]] == "High" && options[["CR"]] == "High"){
        IR <- 1
        CR <- 1
    }
    # Audit Risk Model
    DR               <- AR / IR / CR

    if(options[["show"]] == "percentage"){
        text <- paste0("Audit risk (", round(AR * 100, 2),"%) = Inherent risk (", round(IR * 100, 2), "%) x Control risk (", round(CR * 100, 2), "%) x Detection risk (", round(DR * 100, 2), "%)")
    } else {
        text <- paste0("Audit risk (", round(AR, 2),") = Inherent risk (", round(IR, 2), ") x Control risk (", round(CR, 2), ") x Detection risk (", round(DR, 2), ")")
    }

    jaspResults[["ARMformula"]] <- createJaspHtml(text, "h3")
    jaspResults[["ARMformula"]]$position <- position
    jaspResults[["ARMformula"]]$dependOnOptions(c("IR", "CR", "confidence", "show"))

    jaspResults[["DR"]]     <- createJaspState(DR)
    jaspResults[["DR"]]     $dependOnOptions(c("IR", "CR", "confidence"))

}

.attributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Classical Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID", "sampleFilter", "inference"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'IR',     title = "Inherent risk",  type = 'string')
    evaluationTable$addColumnInfo(name = 'CR',     title = "Control risk",   type = 'string')
    evaluationTable$addColumnInfo(name = 'SR',     title = "Detection risk",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',      title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',      title = "Errors",         type = 'string')

    evaluationTable$addColumnInfo(name = 'bound',  title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')

    if(options[["show"]] == "percentage"){
        SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
        if(result[["bound"]] == "."){
            boundTable          <- "."
        } else {
            boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
        }
    } else if(options[["show"]] == "proportion"){
        SRtable <- round(result[["alpha"]], 3)
        if(result[["bound"]] == "."){
            boundTable          <- "."
        } else {
            boundTable <- round(result[["bound"]],3)
        }
    }

    row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable)
    evaluationTable$addRows(row)

}

.bayesianAttributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Bayesian Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "expected.errors", "kPercentageNumber", "kNumberNumber", "sampleFilter", "inference"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'IR',   title = "Inherent risk",  type = 'string')
    evaluationTable$addColumnInfo(name = 'CR',   title = "Control risk",   type = 'string')
    evaluationTable$addColumnInfo(name = 'SR',   title = "Detection risk",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',    title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',    title = "Errors",         type = 'string')


    if(options[["show"]] == "percentage"){
        SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
        if(result[["bound"]] == "."){
            if(options[["statistic"]] == "bound"){
                boundTable          <- "."
            } else if(options[["statistic"]] == "interval"){
                boundTable          <- c(".", ".")
            }
        } else {
            boundTable <- paste0(round(result[["bound"]],3) * 100, "%")
        }
    } else if(options[["show"]] == "proportion"){
        SRtable <- round(result[["alpha"]], 3)
        if(result[["bound"]] == "."){
            if(options[["statistic"]] == "bound"){
                boundTable          <- "."
            } else if(options[["statistic"]] == "interval"){
                boundTable          <- c(".", ".")
            }
        } else {
            boundTable <- round(result[["bound"]],3)
        }
    }

    if(options[["statistic"]] == "bound"){
        evaluationTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable)
        evaluationTable$addRows(row)
    } else {
        evaluationTable$addColumnInfo(name = 'ciLow', title = "Lower", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
        evaluationTable$addColumnInfo(name = 'ciHigh', title = "Upper", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], ciLow = boundTable[1], ciHigh = boundTable[2])
        evaluationTable$addRows(row)
    }

}
