bayesianAttributesBound <- function(jaspResults, dataset, options, state=NULL){
  # Set the state
  if(is.null(state))
      state 							                     <- list()
  # Read the data
  dataset                                      <- .readDataBayesianAttributesBound(dataset, options)
  # Set the title
  jaspResults$title 					                 <- "Bayesian Audit Attributes Bound"
  # Perform the analysis
  .bayesianAttributesBound(dataset, options, jaspResults)
  result                                       <- jaspResults[["result"]]$object
  # Create the summary table
  .bayesianAttributesBoundTable(options, result, jaspResults)
  # Create the stratum information table
  if(options[["stratuminfo"]])
  {
    if(is.null(jaspResults[["stratuminfotable"]]))
      .stratumInfoTable(options, result, jaspResults, type = "bayesian")
  }
  # Create the implicit sample table
  if (options$implicitsample)
  {
    if(is.null(jaspResults[["sampletable"]]))
      .priorSampleTable(options, result, jaspResults)
  }
  # Create the prior and posterior plot ##
   if(options[['plotPriorAndPosterior']] && options[["correctID"]] != "")
   {
      if(is.null(jaspResults[["priorAndPosteriorPlot"]]))
      {
      jaspResults[["priorAndPosteriorPlot"]] 		<- .plotPriorAndPosteriorBayesianAttributesBound(options, result, jaspResults)
      jaspResults[["priorAndPosteriorPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "limx", "statistic", "plotPriorAndPosterior",
                                                                    "plotPriorAndPosteriorAdditionalInfo", "materiality", "show", "correctID",
                                                                    "expected.k", "stratum"))
      jaspResults[["priorAndPosteriorPlot"]] 		$position <- 3
      }
   }
   # Create the confidence bounds plot
   if(options[['plotBounds']] && options[["correctID"]] != "")
   {
      if(is.null(jaspResults[["confidenceBoundPlot"]]))
      {
      jaspResults[["confidenceBoundPlot"]] 		<- .plotConfidenceBounds(options, result, jaspResults)
      jaspResults[["confidenceBoundPlot"]]		$dependOnOptions(c("IR", "CR", "confidence", "correctID", "stratum",
                                                                 "show", "plotBounds", "materiality", "method"))
      jaspResults[["confidenceBoundPlot"]] 		$position <- 4
      }
   }
  # Save the state
  state[["options"]] 					                  <- options
  return(state)
}

.readDataBayesianAttributesBound <- function(dataset, options){
  correctID <- options[['correctID']]
  stratum <- options[["stratum"]]
  variables <- c(correctID, stratum)
  variables <- variables[variables != ""]
  if (is.null(dataset)) {
    if(length(variables) == 0){
      dataset   <- NULL
    } else {
      if(stratum == "" && correctID != ""){
        dataset   <- .readDataSetToEnd(columns.as.numeric = correctID)
      } else if(stratum != "" && correctID == ""){
        dataset   <- .readDataSetToEnd(columns.as.factor = stratum)
      } else if(stratum != "" && correctID != ""){
        dataset   <- .readDataSetToEnd(columns.as.numeric = correctID, columns.as.factor = stratum)
      }
    }
  }
  return(dataset)
}

.bayesianAttributesBound <- function(dataset, options, jaspResults){

    confidence              <- options[["confidence"]]
    correctID               <- options[["correctID"]]
    exp.k                   <- options[["expected.k"]]
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

    if(options[["correctID"]] == "" && options[["stratum"]] == ""){

      n                           <- 0
      k                           <- 0

      n_noprior                   <- .calculateBayesianSampleSize(exp.k, materiality, 1 - confidence)
      n_withprior                 <- .calculateBayesianSampleSize(exp.k, materiality, alpha)

      pn                          <- n_noprior - n_withprior
      pk                          <- pn * exp.k
      priorA                      <- 1 + pk
      priorB                      <- 1 + (pn - pk)

      resultList <- list()
      resultList[["n"]]           <- n
      resultList[["k"]]           <- k
      resultList[["implicitn"]]   <- pn
      resultList[["implicitk"]]   <- pk
      resultList[["IR"]]          <- options[["IR"]]
      resultList[["CR"]]          <- options[["CR"]]
      resultList[["alpha"]]       <- alpha
      resultList[["confidence"]]  <- confidence
      resultList[["bound"]]       <- "."
      resultList[["priorA"]]      <- priorA
      resultList[["priorB"]]      <- priorB
      resultList[["posteriorA"]]  <- priorA + k
      resultList[["posteriorB"]]  <- priorB + (n - k)
      resultList[["approve"]]     <- "."

    } else if(options[["correctID"]] != "" && options[["stratum"]] == ""){

      n                       <- nrow(dataset)
      k                       <- length(which(dataset[,.v(correctID)] == 1))
      n_noprior               <- .calculateBayesianSampleSize(exp.k, materiality, 1 - confidence)
      n_withprior             <- .calculateBayesianSampleSize(exp.k, materiality, alpha)
      pn                      <- n_noprior - n_withprior
      pk                      <- pn * exp.k
      priorA                  <- 1 + pk
      priorB                  <- 1 + (pn - pk)

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

  } else if(options[["correctID"]] == "" && options[["stratum"]] != ""){

    n                           <- nrow(dataset)
    k                           <- 0

    n_noprior                   <- .calculateBayesianSampleSize(exp.k, materiality, 1 - confidence)
    n_withprior                 <- .calculateBayesianSampleSize(exp.k, materiality, alpha)

    pn                          <- n_noprior - n_withprior
    pk                          <- pn * exp.k
    priorA                      <- 1 + pk
    priorB                      <- 1 + (pn - pk)

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- k
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- confidence
    resultList[["bound"]]       <- "."
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["posteriorA"]]  <- priorA + k
    resultList[["posteriorB"]]  <- priorB + (n - k)
    resultList[["approve"]]     <- "."

  } else if(options[["correctID"]] != "" && options[["stratum"]] != ""){

    stratum <- options[["stratum"]]

    stratumVar <- dataset[, .v(stratum)]
    errorVar <- dataset[, .v(correctID)]
    dat <- data.frame(error = errorVar, stratum = stratumVar)
    dat[["stratum"]] <- factor(dat[["stratum"]])

    n                       <- nrow(dat)
    k                       <- length(which(dat[["error"]] == 1))
    n_noprior               <- .calculateBayesianSampleSize(exp.k, materiality, 1 - confidence)
    n_withprior             <- .calculateBayesianSampleSize(exp.k, materiality, alpha)
    pn                      <- n_noprior - n_withprior
    pk                      <- pn * exp.k
    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    bound                   <- qbeta(p = confidence,
                                    shape1 = priorA + k,
                                    shape2 = priorB + (n - k),
                                    lower.tail = TRUE)

    strata                  <- dataset[, .v(stratum)]
    stratumbounds           <- numeric()
    levels                  <- levels(strata)
    stratumN                <- numeric()
    stratumK                <- numeric()

    for(i in 1:length(levels)){
      level                 <- dataset[which(dataset[, .v(stratum)] == levels[i]), .v(correctID)]
      curN                  <- length(level)
      curK                  <- length(which(level == 1))
      stratumN              <- c(stratumN, curN)
      stratumK              <- c(stratumK, curK)
    }

    # TODO: LOADING BRMS FAILS
    # prior <- c(brms::set_prior(paste0("beta(",priorA,",",priorB,")"), class = "Intercept"),
    #            brms::set_prior("cauchy(0, 1)", class = "sd", coef = "Intercept", group = "stratum"))
    # fit <- brms::brm(error ~ 1 + (1 | stratum), family = brms::bernoulli(link = "identity"),
    #            save_ranef = TRUE, data = dat, chains = 1, cores = 4, prior = prior,
    #            control = list(adapt_delta = 0.99999, max_treedepth = 15),
    #            warmup = 3000, iter = 10000, save_all_pars = TRUE, seed = 1)

    # samples <- brms::posterior_samples(fit)
    # posterior_samples <- samples[["b_Intercept"]]
    # denspost <- density(posterior_samples)
    # bound <- as.numeric(quantile(posterior_samples, confidence))
    # co <- coef(fit, summary = FALSE)$stratum[, , 1]
    # samphigh <- co[, "High"]
    # sampmedium <- co[, "Medium"]
    # samplow <- co[, "Low"]
    # denshigh <- density(samphigh)
    # denslow <- density(samplow)
    # densmedium <- density(sampmedium)
    # bound1 <- as.numeric(quantile(samphigh, probs = confidence))
    # bound2 <- as.numeric(quantile(samplow, probs = confidence))
    # bound3 <- as.numeric(quantile(sampmedium, probs = confidence))

    # TODO: HARDCODED!!!
    nsbound <- 0.05
    stratumbounds <- c(0.04, 0.06, 0.05)
    stratumnames <- c("High", "Low", "Medium")

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
    resultList[["approve"]]     <- "."
    resultList[["stratumnames"]] <- levels
    resultList[["stratumbounds"]] <- stratumbounds
    resultList[["stratumsize"]] <- stratumN
    resultList[["stratumerrors"]] <- stratumK
    resultList[["nsbound"]] <- nsbound

  }

    jaspResults[["result"]]     <- createJaspState(resultList)
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "expected.k", "correctID", "stratum"))

}

.bayesianAttributesBoundTable <- function(options, result, jaspResults){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                       <- createJaspTable("Bayesian Evaluation Table")
  jaspResults[["summaryTable"]]      <- summaryTable
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID", "expected.k", "stratum"))
  summaryTable$position <- 1

  summaryTable$addColumnInfo(name = 'IR',   title = "Inherent risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'CR',   title = "Control risk",   type = 'string')
  summaryTable$addColumnInfo(name = 'SR',   title = "Sampling risk",  type = 'string')
  summaryTable$addColumnInfo(name = 'n',    title = "Sample size",    type = 'string')
  summaryTable$addColumnInfo(name = 'k',    title = "Errors",         type = 'string')

  # Table for a non-stratified bound
  if(options[["stratum"]] == ""){

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
        summaryTable$addColumnInfo(name = 'bound', title = paste0(result[["confidence"]]*100,"% Confidence bound"), type = 'string')
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], bound = boundTable)
        summaryTable$addRows(row)
    } else {
        summaryTable$addColumnInfo(name = 'ciLow', title = "Lower", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
        summaryTable$addColumnInfo(name = 'ciHigh', title = "Upper", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Confidence interval"))
        row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], ciLow = boundTable[1], ciHigh = boundTable[2])
        summaryTable$addRows(row)
    }

  } else {
    # Table for a stratified bound
    summaryTable$addColumnInfo(name = 'boundns',  title = "Non-stratified", type = 'string', overtitle = paste0(result[["confidence"]]*100,"% Confidence bounds"))
    summaryTable$addColumnInfo(name = 'bounds',  title = "Stratified", type = 'string', overtitle = paste0(result[["confidence"]]*100,"% Confidence bounds"))

    if(options[["show"]] == "percentage"){
      SRtable <- paste0(round(result[["alpha"]],3) * 100, "%")
      if(result[["bound"]] == "."){
        boundTableNs          <- "."
        boundTableS          <- "."
      } else {
        boundTableNs <- paste0(round(result[["bound"]],3) * 100, "%")
        boundTableS <- paste0(round(result[["nsbound"]],3) * 100, "%")
      }
    } else if(options[["show"]] == "proportion"){
      SRtable <- round(result[["alpha"]], 3)
      if(result[["bound"]] == "."){
        boundTableNs          <- "."
        boundTableS          <- "."
      } else {
        boundTableNs <- round(result[["bound"]],3)
        boundTableS <- round(result[["nsbound"]],3)
      }
    }

    row <- list(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, n = result[["n"]], k = result[["k"]], boundns = boundTableNs, bounds = boundTableS)
    summaryTable$addRows(row)
  }
  
}
