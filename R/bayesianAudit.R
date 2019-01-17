bayesianAudit <- function(jaspResults, dataset, options, state=NULL){

    options[["statistic"]]              <- "bound" # Temporary

    if(is.null(state))
        state 							            <- list()

    jaspResults$title                   <- "Full Bayesian Audit" # Specify the title of the analysis

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

    # Planning phase (starts when the population size is specified)
    if(options[["N"]] != 0){

      .ARMformula(options, jaspResults, position = 5)   # Show the Audit Risk Model formula and quantify detection risk
      DR              <- jaspResults[["DR"]]$object

      # Interpretation for the Planning phase
      if(options[["interpretation"]]){
          jaspResults[["AuditRiskModelHeader"]] <- createJaspHtml("<u>Audit Risk Model</u>", "h2")
          jaspResults[["AuditRiskModelHeader"]]$position <- 3
          jaspResults[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>",options$IR,"</b>. The internal control risk was determined
                                                                          to be <b>", options$CR,"</b>. According to the Audit Risk Model, the required detection risk to then maintain an audit risk of <b>", (1 - options$confidence) * 100, "%</b> should be <b>",round(DR*100, 2), "%</b>."), "p")
          jaspResults[["AuditRiskModelParagraph"]]$position <- 4
      }

      jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Planning</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]$position <- 6

      .bayesianAttributesPlanningFullAudit(options, jaspResults)
      result              <- jaspResults[["result"]]$object
      .bayesianAttributesPlanningTableFullAudit(options, result, jaspResults, position = 8)

      if(options[["expected.errors"]] == "kPercentage"){
          expected.errors <- paste0(round(options[["kPercentageNumber"]] * 100, 2), "%")
          max.errors <- floor(options[["kPercentageNumber"]] * result[["n"]]) + 1
      } else {
          expected.errors <- options[["kNumberNumber"]]
          max.errors <- options[["kNumberNumber"]] + 1
      }

      # Interpretation for the Planning phase
      if(options[["interpretation"]]){

          jaspResults[["priorKnowledgeParagraph"]] <- createJaspHtml(paste0("As prior knowledge, the most likely error in the data was specified to be <b>", expected.errors ,"</b>. The probability distribution that corresponds with
                                                                        this prior knowledge is the <b>Beta(",round(result[["priorA"]],2), ",", round(result[["priorB"]],2),")</b> distribution. This probability distribution states that there is a <b>",
                                                                            round(pbeta(options[["materiality"]], result[["priorA"]],result[["priorB"]])*100, 2) ,"%</b> prior probability that the
                                                                        population error is lower than materiality. The sample size that is required to prove an <b>",options$materiality*100,"%</b> upper confidence bound, assuming
                                                                        the sample contains <b>", expected.errors ,"</b> full errors, is <b>", result[["n"]] ,"</b>. Consequently, if <b>", max.errors ,"</b> or more full errors are found, the population cannot be approved."), "p")
          jaspResults[["priorKnowledgeParagraph"]]$position <- 7

      }

      # Implicit sample table
      if (options[["implicitsample"]])
      {
          if(is.null(jaspResults[["sampletable"]]))
              .priorSampleTable(options, result, jaspResults, position = 9)
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
              jaspResults[["criticalErrorPlot"]] 		$position <- 10
          }
      }

      # Prior plot
      if(options[['plotPrior']])
      {
          if(is.null(jaspResults[["priorPlot"]]))
          {
              jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanningFullAudit(options, result, jaspResults)
              jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                               "plotPrior", "plotPrior", "show", "prior",
                                                               "statistic", "kPercentageNumber", "kNumberNumber"))
              jaspResults[["priorPlot"]] 		$position <- 11
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
        jaspResults[["samplingHeader"]]$position <- 12

        # Interpretation for the sampling phase
        if(options[["interpretation"]]){

            if(options[["samplingType"]] == "simplerandomsampling"){
                technique <- "simple random sampling"
            } else if(options[["samplingType"]] == "systematicsampling"){
                technique <- "systematic sampling"
            } else if(options[["samplingType"]] == "cellsampling"){
                technique <- "cell sampling"
            }

            jaspResults[["samplingParagraph"]] <- createJaspHtml(paste0("From the population of <b>", options[["N"]], "</b> observations, <b>", result[["n"]], "</b> samples were drawn using a <b>", technique, "</b> method."), "p")
            jaspResults[["samplingParagraph"]]$position <- 13

        }

        if(options[["samplingType"]] == "simplerandomsampling"){
            .SimpleRandomSamplingTable(dataset, options, jaspResults, position = 14)
        } else if(options[["samplingType"]] == "systematicsampling"){

            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 14)
            .SystematicSamplingTable(dataset, options, jaspResults, interval, position = 15)

        } else if(options[["samplingType"]] == "cellsampling"){

            interval <- ceiling(nrow(dataset) / options[["sampleSize"]])
            .intervalTable(dataset, options, jaspResults, interval, position = 14)
            .cellSamplingTable(dataset, options, jaspResults, interval, position = 15)
        }
        sample                          <- jaspResults[["sample"]]$object

        if(options$showDescriptives)
            .samplingDescriptivesTable(dataset, options, jaspResults, sample, position = 16)

    }

    # Evaluation phase
    if(options[["correctID"]] != ""){

        if(options[["sampleFilter"]] != ""){
            dataset <- subset(dataset, dataset[, .v(sampleFilter)] == 1)
        }

        jaspResults[["evaluationHeader"]] <- createJaspHtml("<u>Evaluation</u>", "h2")
        jaspResults[["evaluationHeader"]]$position <- 17

        options[["statistic"]] <- "bound"

        .bayesianAttributesBoundFullAudit(dataset, options, jaspResults)
        result                                       <- jaspResults[["result"]]$object
        .bayesianAttributesBoundTableFullAudit(options, result, jaspResults, position = 19)

        # Interpretation for the evaluation phase
        if(options[["interpretation"]]){

            jaspResults[["resultParagraph"]] <- createJaspHtml(paste0("The sample consisted of <b>", nrow(dataset) , "</b> observations, <b>",result[["k"]], "</b> of which were found to contain a full error. The knowledge from these data, com-
                                                                  bined with the prior knowledge results in an <b>",options$confidence*100, "%</b> upper confidence bound of <b>",round(result[["bound"]]*100, 2),"%</b>. The cumulative knowledge states that there
                                                                  is a <b>",options$confidence*100, "%</b> probability that the true error proportion in the population is lower than <b>",round(result[["bound"]]*100, 2),"%</b>."), "p")
            jaspResults[["resultParagraph"]]$position <- 18

        }

        # Prior and Posterior plot
        if(options[['plotPriorAndPosterior']] && options[["correctID"]] != "")
        {
            if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
            {
                jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBoundFullAudit(options, result, jaspResults)
                jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx_backup", "statistic", "plotPriorAndPosterior",
                                                                           "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                           "expected.errors", "kPercentageNumber", "kNumberNumber", "inference", "sampleFilter"))
                jaspResults[["priorAndPosteriorPlot"]] 		$position <- 20
            }
        }

        # Interpretation for the evaluation phase
        if(options[["interpretation"]]){

            jaspResults[["conclusionTitle"]] <- createJaspHtml("<u>Conclusion</u>", "h2")
            jaspResults[["conclusionTitle"]]$position <- 21

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
            jaspResults[["conclusionParagraph"]]$position <- 22

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

.bayesianAttributesPlanningFullAudit <- function(options, jaspResults){

    if(!is.null(jaspResults[["result"]])) return()

    confidence              <- options[["confidence"]]
    p                       <- options[["materiality"]]

    if(options[["IR"]] == "Low" && options[["CR"]] == "Low"){
        alpha               <- (1-confidence) / 0.30 / 0.30
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "Medium"){
        alpha               <- (1-confidence) / 0.30 / 0.60
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "High"){
        alpha               <- (1-confidence) / 0.30 / 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "High"){
        alpha               <- (1-confidence) / 0.60 / 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Medium"){
        alpha               <- (1-confidence) / 0.60 / 0.60
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Low"){
        alpha               <- (1-confidence) / 0.60 / 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Low"){
        alpha               <- (1-confidence) / 1 / 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Medium"){
        alpha               <- (1-confidence) / 0.60 / 1
    } else if (options[["IR"]] == "High" && options[["CR"]] == "High"){
        alpha               <- (1-confidence) / 1 / 1
    }

    n_noprior               <- .calculateBayesianSampleSize(options, 1 - confidence)
    n_withprior             <- .calculateBayesianSampleSize(options, alpha)

    pn                      <- n_noprior - n_withprior

    if(pn == 0){
        pk                  <- 0
        if(options[["expected.errors"]] == "kPercentage"){
            k                   <- options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            k                   <- options[["kNumberNumber"]]
        }
    } else {
        if(options[["expected.errors"]] == "kPercentage"){
            k                   <- options[["kPercentageNumber"]]
            pk                  <- pn * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            k                   <- options[["kNumberNumber"]]
            pk                  <- k
        }
    }
    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    if(options[["prior"]] == "5050"){

      priorA <- 1
      priorB <- 1/((3/2) * options[["materiality"]]) - (1/3)

    }

    resultList <- list()
    resultList[["n"]]           <- n_withprior
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["k"]]           <- k
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- confidence

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "kPercentageNumber",
                                              "kNumberNumber", "inference", "prior"))

}

.bayesianAttributesPlanningTableFullAudit <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Attributes Planning Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "show",
                                  "kPercentageNumber", "kNumberNumber", "expectedBF"))

  summaryTable$addColumnInfo(name = 'IR', title = "Inherent risk", type = 'string')
  summaryTable$addColumnInfo(name = 'CR', title = "Control risk", type = 'string')
  summaryTable$addColumnInfo(name = 'SR', title = "Detection risk", type = 'string')
  summaryTable$addColumnInfo(name = 'k', title = "Allowed errors", type = 'string')
  summaryTable$addColumnInfo(name = 'n', title = "Required sample size", type = 'string')
  if(options[["expectedBF"]])
    summaryTable$addColumnInfo(name = 'expBF', title = "Expected Bayes factor", type = 'string')

  summaryTable$position <- position

  if(options[["show"]] == "percentage"){
    SRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- floor(result[["k"]] * result[["n"]])
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  } else if(options[["show"]] == "proportion"){
    SRtable <- round(result[["alpha"]], 3)
    if(options[["expected.errors"]] == "kPercentage"){
      ktable <- floor(result[["k"]] * result[["n"]])
    } else if(options[["expected.errors"]] == "kNumber"){
      ktable <- options[["kNumberNumber"]]
    }
  }

  if(options[["expectedBF"]]){
    row <- list(IR = result[["IR"]],
                CR = result[["CR"]],
                SR = SRtable,
                k = ktable,
                n = result[["n"]],
                expBF = .expectedBF(options, result, ktable))
  } else {
    row <- list(IR = result[["IR"]],
                CR = result[["CR"]],
                SR = SRtable,
                k = ktable,
                n = result[["n"]])
  }

  summaryTable$addRows(row)

  message <- "The sample size is calculated using the <b>beta</b> distribution."
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

}

.plotPriorBayesianAttributesPlanningFullAudit <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  mle <- floor(result[["k"]] * result[["n"]])

  xseq <- seq(0, options[["limx"]], 0.001)
  d <- data.frame(
      x = rep(xseq, 2),
      y = c(dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]]), dbeta(x = xseq, shape1 = result[["priorA"]] + mle, shape2 = result[["priorB"]] + (result[["n"]] - mle))),
      type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
  )

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  pointdata <- data.frame(x = options[["materiality"]], y = dbeta(options[["materiality"]], result[["priorA"]], result[["priorB"]]))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      #ggplot2::scale_linetype_manual(values=c("dotted", "dashed"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))
      ggplot2::scale_linetype_manual(values=c("dotted", "dashed"), guide = FALSE)

  if(options[["show"]] == "percentage"){
      p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))
  } else if(options[["show"]] == "proportion"){
      p <- p + ggplot2::scale_x_continuous(name = "Error proportion", breaks = xBreaks, limits = xLim)
  }

  if(options[["plotPriorAndPosteriorAdditionalInfo"]]){
      pdata <- data.frame(x = 0, y = 0, l = "1")
      p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
      p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 20, shape = 21, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))

      p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
                                      xlim = c(0, qbeta(options[["confidence"]], result[["priorA"]], result[["priorB"]])),
                                      geom = "area", fill = rgb(0, 1, 0.5, .7))
  }

  # xseq <- seq(0, options[["limx"]], 0.001)
  # d <- data.frame(
  #     x = xseq,
  #     y = dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]])
  # )
  #
  # xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
  # xLim <- range(xBreaks)
  # yBreaks <- c(0, 1.2*max(d$y))
  # yLim <- range(yBreaks)
  #
  # pointdata <- data.frame(x = options[["materiality"]], y = dbeta(options[["materiality"]], result[["priorA"]], result[["priorB"]]))
  #
  # p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
  #     ggplot2::geom_line(ggplot2::aes(x = x, y = y), lwd = 1, lty = 2)
  #
  # if(options[["show"]] == "percentage"){
  #   p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))
  # } else if(options[["show"]] == "proportion"){
  #   p <- p + ggplot2::scale_x_continuous(name = "Error proportion", breaks = xBreaks, limits = xLim)
  # }
  #
  # if(options[["plotPriorAndPosteriorAdditionalInfo"]]){
  #   pdata <- data.frame(x = 0, y = 0, l = "1")
  #   p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
  #   p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
  #   p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 20, shape = 21, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))
  #   if(options[["statistic"]] == "bound"){
  #     p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
  #                                     xlim = c(0, qbeta(options[["confidence"]], result[["priorA"]], result[["priorB"]])),
  #                                     geom = "area", fill = rgb(0, 1, 0.5, .7))
  #   } else if(options[["statistic"]] == "interval"){
  #     p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
  #                                     xlim = c(qbeta((1 - (1-(1-options[["confidence"]])/2)), result[["priorA"]], result[["priorB"]]), qbeta((1 - ((1-options[["confidence"]])/2)), result[["priorA"]], result[["priorB"]])),
  #                                     geom = "area", fill = rgb(0, 1, 0.5, .7))
  #   }
  # }

  p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

  thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p +
  	ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  return(createJaspPlot(plot = p, title = "Implied Prior and Posterior", width = plotWidth, height = plotHeight))

}

.bayesianAttributesBoundFullAudit <- function(dataset, options, jaspResults){

    confidence              <- options[["confidence"]]
    correctID               <- options[["correctID"]]
    materiality             <- options[["materiality"]]

    if(options[["IR"]] == "Low" && options[["CR"]] == "Low"){
        alpha               <- (1-confidence) / 0.30 / 0.30
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "Medium"){
        alpha               <- (1-confidence) / 0.30 / 0.60
    } else if (options[["IR"]] == "Low" && options[["CR"]] == "High"){
        alpha               <- (1-confidence) / 0.30 / 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "High"){
        alpha               <- (1-confidence) / 0.60 / 1
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Medium"){
        alpha               <- (1-confidence) / 0.60 / 0.60
    } else if (options[["IR"]] == "Medium" && options[["CR"]] == "Low"){
        alpha               <- (1-confidence) / 0.60 / 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Low"){
        alpha               <- (1-confidence) / 1 / 0.30
    } else if (options[["IR"]] == "High" && options[["CR"]] == "Medium"){
        alpha               <- (1-confidence) / 0.60 / 1
    } else if (options[["IR"]] == "High" && options[["CR"]] == "High"){
        alpha               <- (1-confidence) / 1 / 1
    }

    if(is.null(dataset)){
      n                     <- 0
      k                     <- 0
    } else {
      n                     <- nrow(dataset)
      k                     <- length(which(dataset[,.v(correctID)] == 1))
    }

    n_noprior               <- .calculateBayesianSampleSize(options, 1 - confidence)
    n_withprior             <- .calculateBayesianSampleSize(options, alpha)

    pn                      <- n_noprior - n_withprior

    if(pn == 0){
        pk                  <- 0
        if(options[["expected.errors"]] == "kPercentage"){
            exp.k                   <- options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            exp.k                   <- options[["kNumberNumber"]]
        }
    } else {
        if(options[["expected.errors"]] == "kPercentage"){
            exp.k               <- options[["kPercentageNumber"]]
            pk                  <- pn * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            exp.k               <- options[["kNumberNumber"]]
            pk                  <- exp.k
        }
    }

    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    if(options[["prior"]] == "5050"){

      priorA <- 1
      priorB <- 1/((3/2) * options[["materiality"]]) - (1/3)

    }

    if(n == 0 || k > n){
      bound                 <- "."
      approve               <- "."
    } else {
        if(options[["statistic"]] == "bound"){
          bound             <- qbeta(p = confidence,
                                    shape1 = priorA + k,
                                    shape2 = priorB + (n - k),
                                    lower.tail = TRUE)
         if(bound < materiality){
           approve          <- "Yes"
         } else {
           approve          <- "No"
         }
       } else if(options[["statistic"]] =="interval"){
          bound             <- qbeta(p = c(  (1 - (1-(1-confidence)/2)) , (1 - ((1-confidence)/2)) ),
                                    shape1 = priorA + k,
                                    shape2 = priorB + (n - k),
                                    lower.tail = TRUE)
         if(bound[2] < materiality){
           approve          <- "Yes"
         } else {
           approve          <- "No"
         }
      }
  }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- k
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- confidence
    resultList[["bound"]]       <- bound
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["posteriorA"]]  <- priorA + k
    resultList[["posteriorB"]]  <- priorB + (n - k)
    resultList[["approve"]]     <- approve

    jaspResults[["result"]]     <- createJaspState(resultList)
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality",
                                                    "correctID", "expected.errors", "kPercentageNumber", "kNumberNumber", "sampleFilter", "inference"))

}


.bayesianAttributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Bayesian Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "expected.errors", "kPercentageNumber", "kNumberNumber", "sampleFilter",
                                      "mostLikelyError", "bayesFactor", "N"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",  type = 'string')
    evaluationTable$addColumnInfo(name = 'n',    title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'k',    title = "Errors",         type = 'string')
    evaluationTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',    title = "Most Likely Error",         type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',     title = "Bayes factor",         type = 'string')

    mle <- floor(qbeta(p = 0.5, result[["posteriorA"]], result[["posteriorB"]]) * options[["N"]])


    if(options[["show"]] == "percentage"){
        materialityTable <- paste0(round(options[["materiality"]],2) * 100, "%")
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
        materialityTable <- round(options[["materiality"]], 2)
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
    if(options[["bayesFactor"]]){
      if(options[["mostLikelyError"]]){
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable, mle = mle, bf = .BF(options, result))
      } else {
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable, bf = .BF(options, result))
      }
    } else {
      if(options[["mostLikelyError"]]){
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable, mle = mle)
      } else {
        row <- list(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable)
      }
    }

    evaluationTable$addRows(row)

}

.plotPriorAndPosteriorBayesianAttributesBoundFullAudit <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  xseq <- seq(0, options[["limx_backup"]], 0.001)
  d <- data.frame(
      x = rep(xseq, 2),
      y = c(dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]]), dbeta(x = xseq, shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]])),
      type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
  )
  # Reorder factor levels to display in legend
  d$type = factor(d$type,levels(d$type)[c(2,1)])

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  pointdata <- data.frame(x = options[["materiality"]], y = dbeta(options[["materiality"]], result[["posteriorA"]], result[["posteriorB"]]))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      ggplot2::scale_linetype_manual(values=c("dashed", "solid"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))

  if(options[["show"]] == "percentage"){
    p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))
  } else if(options[["show"]] == "proportion"){
    p <- p + ggplot2::scale_x_continuous(name = "Error proportion", breaks = xBreaks, limits = xLim)
  }

  if(options[["plotPriorAndPosteriorAdditionalInfo"]]){
    pdata <- data.frame(x = 0, y = 0, l = "1")
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
    p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \nconfidence region"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 20, shape = 21, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)

    if(options[["statistic"]] == "bound"){
      p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]]), xlim = c(0, result[["bound"]]),
                                      geom = "area", fill = rgb(0, 0.25, 1, .5))
    } else if(options[["statistic"]] == "interval") {
      p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]]), xlim = c(result[["bound"]][1], result[["bound"]][2]),
                                      geom = "area", fill = rgb(0, 0.25, 1, .5))
    }
  }

  p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

  thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	       ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  return(createJaspPlot(plot = p, title = "Prior and Posterior", width = plotWidth, height = plotHeight))

}

.plotCriticalErrorsPrior <- function(allowed.errors, reject.errors, jaspResults){

    errorrange <- 0:max(reject.errors)
    errors <- c(allowed.errors, reject.errors)
    fill <- c(rep(rgb(0,1,0,.25), length(allowed.errors)),
              rep(rgb(1,0,0,.25), length(reject.errors)))

    rectdata <- data.frame(xmin = errors - 0.5, xmax = errors + 0.5, ymin = 0, ymax = 0.5,
                           fill = fill)

    df <- data.frame()
    p <- ggplot2::ggplot(df) +
        ggplot2::geom_point() +
        ggplot2::ylim(0, 1) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab("Sample errors")

    p <- p + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                data = rectdata, fill = rectdata$fill, color = "black")

    pdata <- data.frame(x = c(0,0), y = c(0,0), l = c("1","2"))
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0), rgb(1,0,0,0)))
    p <- p + ggplot2::scale_shape_manual(name = "", values = c(22,22), labels = c("Accept population", "Reject population"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 9, shape = 22, fill = c(rgb(0,1,0,.25),rgb(1,0,0,.25)), stroke = 2, color = "black")), order = 1)

    p <- p + ggplot2::scale_x_continuous(breaks = errorrange)


    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")
    p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_text(size = 17))

    return(createJaspPlot(plot = p, title = "Decision plot", width = 500, height = 300))

}

.expectedBF <- function(options, result, ktable){
  BF <- diff(pbeta(c(0, options[["materiality"]]), result[["priorA"]] + ktable, result[["priorB"]] + (result[["n"]] - ktable))) /
  diff(pbeta(c(options[["materiality"]], 1), result[["priorA"]] + ktable, result[["priorB"]] +(result[["n"]] - ktable)))
  return(round(BF,2))
}

.BF <- function(options, result){
  BF <- diff(pbeta(c(0, options[["materiality"]]), result[["posteriorA"]], result[["posteriorB"]])) /
  diff(pbeta(c(options[["materiality"]], 1), result[["posteriorA"]], result[["posteriorB"]]))
  return(round(BF, 2))
}
