audit <- function(jaspResults, dataset, options, state=NULL){

  options[["statistic"]] <- "bound" # Temporary

  if(is.null(state))
      state 							    <- list()

  jaspResults$title   <- "Planning"                       # Specify the title of the analysis
  DR <- .ARMformula(options, jaspResults, position = 5)   # Show the Audit Risk Model formula and quantify detection risk

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
        jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Prior knowledge</u>", "h2")
        jaspResults[["priorKnowledgeHeader"]]$position <- 6
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
      jaspResults[["priorKnowledgeHeader"]] <- createJaspHtml("<u>Prior knowledge</u>", "h2")
      jaspResults[["priorKnowledgeHeader"]]$position <- 6
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

    if(options[['plotPriorAndPosterior']])
    {
       if(is.null(jaspResults[["priorPlot"]]))
       {
       jaspResults[["priorPlot"]] 		<- .plotPriorBayesianAttributesPlanning(options, result, jaspResults)
       jaspResults[["priorPlot"]]		  $dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "limx",
                                                          "plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo", "show",
                                                          "statistic", "kPercentageNumber", "kNumberNumber", "inference"))
       jaspResults[["priorPlot"]] 		$position <- 10
       }
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

  return(DR)

}
