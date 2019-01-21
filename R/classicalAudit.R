classicalAudit <- function(jaspResults, dataset, options, state=NULL){

    if(is.null(state))
        state 							    <- list()

    jaspResults$title   <- "Full Classical Audit"                       # Specify the title of the analysis

    # Interpretation for the Global Options phase
    if(options[["interpretation"]]){
      if(options[["auditType"]] == "attributes"){
        jaspResults[["procedureHeader"]] <- createJaspHtml("<u>Attributes Procedure</u>", "h2")
        jaspResults[["procedureHeader"]]$position <- 1
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of an attributes sampling procedure is to determine with a specified <b>", options[["confidence"]]*100,"%</b> confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality of <b>",options[["materiality"]]*100,"%</b>. An attributes bound procedure considers
                                                                  the observations in the population to be of one of two categories: 1) the observation is fully correct or 2) the observation is
                                                                  fully incorrect."), "p")
        jaspResults[["procedureParagraph"]]$position <- 2
      } else if(options[["auditType"]] == "mus"){
        jaspResults[["procedureHeader"]] <- createJaspHtml("<u>MUS Procedure</u>", "h2")
        jaspResults[["procedureHeader"]]$position <- 1
        jaspResults[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a monetary unit sampling (MUS) procedure is to determine with a specified <b>", options[["confidence"]]*100,"%</b> confidence whether the percentage
                                                                  of errors in the target population is lower than the specified materiality of <b>",options[["materiality"]]*100,"%</b>. A monetary unit sampling procedure considers
                                                                  the observations in the population to be proportional to the size of the observation."), "p")
        jaspResults[["procedureParagraph"]]$position <- 2
      }
    }

    # Planning phase
    if(options[["N"]] != 0){

      .ARMformula(options, jaspResults, position = 5)   # Show the Audit Risk Model formula and quantify detection risk
      DR              <- jaspResults[["DR"]]$object

      if(options[["interpretation"]]){
          jaspResults[["AuditRiskModelHeader"]] <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
          jaspResults[["AuditRiskModelHeader"]]$position <- 3
          jaspResults[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined
                                                                          to be <b>", options$CR,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be <b>",round(DR*100, 2), "%</b>."), "p")
          jaspResults[["AuditRiskModelParagraph"]]$position <- 4
      }

      jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]$position <- 6

      .attributesPlanningFullAudit(options, jaspResults)
      result              <- jaspResults[["result"]]$object
      .attributesPlanningTableFullAudit(options, result, jaspResults, position = 8)

      if(options[["expected.errors"]] == "kPercentage"){
          expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
          max.errors <- ceiling(options[["kPercentageNumber"]] * result[["n"]])
      } else {
          expected.errors <- options[["kNumberNumber"]]
          max.errors <- options[["kNumberNumber"]]
      }

      if(options[["interpretation"]]){

          jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The sample size that is required to prove an <b>",options$materiality*100,"%</b>
                                                                          upper confidence bound, assuming the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. The sample size is calculated with the <b>", options[["distribution"]] , "</b>
                                                                          distribution. Consequently, if <b>", max.errors ,"</b> or more full errors are found in the sample, the population cannot be approved."), "p")
          jaspResults[["priorKnowledgeParagraph"]]$position <- 7

      }

      # Decision plot
      if(options[['plotCriticalErrors']])
      {
          if(is.null(jaspResults[["criticalErrorPlot"]]))
          {
              allowed.errors <- 0:(max.errors-1)
              reject.errors <- max.errors : (max.errors + 2)
              jaspResults[["criticalErrorPlot"]] 		<- .plotCriticalErrorsPrior(allowed.errors, reject.errors, jaspResults)
              jaspResults[["criticalErrorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors",
                                                                          "show", "statistic", "kPercentageNumber", "kNumberNumber", "plotCriticalErrors"))
              jaspResults[["criticalErrorPlot"]] 		$position <- 9
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

            jaspResults[["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", options[["N"]], "</b> observations, <b>", result[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
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

        .attributesBoundFullAudit(dataset, options, jaspResults)
        result                                       <- jaspResults[["result"]]$object
        .attributesBoundTableFullAudit(options, result, jaspResults, position = 18)

        if(options[["interpretation"]]){

            jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>",options[["sampleSize"]], "</b> observations, <b>", result[["k"]] , "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                                  bined with the prior knowledge results in an <b>",round((1 - result[["alpha"]]) * 100, 2), "%</b> upper confidence bound of <b>",round(result[["bound"]]*100, 2),"%</b>. The cumulative knowledge states that there
                                                                  is a <b>",options$confidence*100, "%</b> probability that, when one would repeaditly sample from this population, the maximum error is calculated to be lower
                                                                  than <b>",round(result[["bound"]]*100, 2),"%</b>."), "p")
            jaspResults[["resultParagraph"]]$position <- 17

        }

        if(options[['plotBound']] && options[["correctID"]] != "")
        {
            if(is.null(jaspResults[["confidenceBoundPlot"]]))
            {
                jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
                jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID",
                                                                         "show", "plotBound", "materiality", "method", "inference"))
                jaspResults[["confidenceBoundPlot"]] 		$position <- 19
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
                                                                        lower than materiality, in this case <b>", options$materiality*100 ,"%</b>. For the current data, the confidence bound is <b>", above_below ,"</b> than materiality. The conclusion for
                                                                        these data is that the data contain ", approve ,"."), "p")
            jaspResults[["conclusionParagraph"]]$position <- 21

        }

    }

    # Save the state
    state[["options"]] 					<- options
    return(state)

}
