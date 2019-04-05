.calc.n.beta <- function(options, alpha, jaspResults){
    for(n in 1:10000){
      impk <- base::switch(options[["expected.errors"]],
                            "kPercentage" = n * options[["kPercentageNumber"]],
                            "kNumber" = options[["kNumberNumber"]])
        if(impk >= n){ next }
        x                     <- qbeta(p = 1 - alpha, shape1 = 1 + impk, shape2 = 1 + (n - impk))
        if(x < jaspResults[["materiality"]]$object){
            return(n)
        }
    }
}

.dBetaBinom <- function (x, N, u, v, log = FALSE)
{
    logval <- lbeta(x + u, N - x + v) - lbeta(u, v) + lchoose(N, x)
    if (log) { ret <- logval } else { ret <- exp(logval) }
    return(ret)
}

.qBetaBinom <- function (p, N, u, v)
{
    pp <- cumsum(.dBetaBinom(0:N, N, u, v))
    return(sapply(p, function(x) sum(pp < x)))
}

.calc.n.betabinom <- function(options, alpha, jaspResults){
    N <- jaspResults[["N"]]$object
    for(n in 1:10000){
      impk <- base::switch(options[["expected.errors"]],
                            "kPercentage" = n * options[["kPercentageNumber"]],
                            "kNumber" = options[["kNumberNumber"]])
        if(impk >= n){ next }
        x                     <- .qBetaBinom(p = 1 - alpha, N = N, u = 1 + impk, v = 1 + (n - impk)) / N
        if(x < jaspResults[["materiality"]]$object){
            return(n)
        }
    }
}

.bayesianPlanningHelper <- function(options, jaspResults){

    if(!is.null(jaspResults[["planningResult"]]$object)) return()

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    if(jaspResults[["materiality"]]$object == 0){
      pk                    <- 0
      pn                    <- 0
      k                     <- 0
      priorA                <- 1
      priorB                <- 1
      n_withprior           <- 0
    } else {

      if(options[["distribution"]] == "beta"){
        n_noprior               <- .calc.n.beta(options, 1 - options[["confidence"]], jaspResults)
        n_withprior             <- .calc.n.beta(options, alpha, jaspResults)
      } else if(options[["distribution"]] == "beta-binomial"){
        n_noprior               <- .calc.n.betabinom(options, 1 - options[["confidence"]], jaspResults)
        n_withprior             <- .calc.n.betabinom(options, alpha, jaspResults)
      }

      pk                      <- 0
      pn                      <- n_noprior - n_withprior
      k                       <- base::switch(options[["expected.errors"]],
                                              "kPercentage" = options[["kPercentageNumber"]],
                                              "kNumber" = options[["kNumberNumber"]])
      if(pn != 0){
          if(options[["expected.errors"]] == "kPercentage"){
              k               <- options[["kPercentageNumber"]]
              pk              <- pn * options[["kPercentageNumber"]]
          } else if(options[["expected.errors"]] == "kNumber"){
              k               <- options[["kNumberNumber"]]
              pk              <- k
          }
      }

    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

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
    resultList[["confidence"]]  <- options[["confidence"]]

    jaspResults[["planningResult"]] <- createJaspState(resultList)
    jaspResults[["planningResult"]]$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "kPercentageNumber",
                                                      "kNumberNumber", "distribution", "N", "materialityValue", "recordNumberVariable",
                                                      "monetaryVariable"))

}

.bayesianPlanningTable <- function(dataset, options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["planningContainer"]][["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                                                <- createJaspTable("Planning Summary")
  jaspResults[["planningContainer"]][["summaryTable"]]        <- summaryTable
  summaryTable$position                                       <- position
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "show", "N", "kPercentageNumber", "kNumberNumber", "expectedBF", 
                                  "distribution", "materialityValue", "recordNumberVariable", "monetaryVariable", "populationValue", "auditType"))

  summaryTable$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')
  if(options[["expectedBF"]])
    summaryTable$addColumnInfo(name = 'expBF',              title = "Expected BF\u208B\u208A", type = 'string')

  message <- base::switch(options[["distribution"]],
                            "beta" = "The sample size is based on the <b>beta</b> distribution.",
                            "beta-binomial" = paste0("The sample size is based on the <b>beta-binomial</b> distribution (N = ", jaspResults[["N"]]$object ,")."))
                              
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  ktable <- base::switch(options[["expected.errors"]],
                          "kPercentage" = round(result[["k"]] * result[["n"]], 2),
                          "kNumber" = options[["kNumberNumber"]])
  DRtable <- paste0(round(result[["alpha"]], 3) * 100, "%")

  if(jaspResults[["materiality"]]$object == 0){
    row <- data.frame(materiality = ".", IR = result[["IR"]], CR = result[["CR"]], DR = DRtable, k = 0, n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    summaryTable$addRows(row)
    return()
  }

  materialityTitle <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue <- base::switch(options[["auditType"]],
                                    "attributes" = ceiling(jaspResults[["materiality"]]$object * sum(dataset[, .v(options[["monetaryVariable"]])])),
                                    "mus" = options[["materialityValue"]])

  materiality <- base::switch(options[["auditType"]],
                                "attributes" = materialityTitle,
                                "mus" = materialityValue)

  if(!jaspResults[["ready"]]$object){
    row <- data.frame(materiality = materiality, IR = result[["IR"]],CR = result[["CR"]], DR = DRtable, k = ktable, n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    summaryTable$addRows(row)
    return()
  }

  row <- data.frame(materiality = materiality, IR  = result[["IR"]], CR = result[["CR"]], DR = DRtable, k = ktable, n = result[["n"]])
  if(options[["expectedBF"]])
    row <- cbind(row, expBF = .expectedBF(options, result, ktable, jaspResults))
  summaryTable$addRows(row)
}

.priorSampleTable <- function(options, result, jaspResults, position = 3){

  if(!is.null(jaspResults[["planningContainer"]][["sampletable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  if(options[["implicitsample"]]){

  sampletable                       <- createJaspTable("Implicit Sample")
  jaspResults[["planningContainer"]][["sampletable"]]      <- sampletable
  sampletable$position              <- position
  sampletable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "implicitsample", "statistic",
                                "show", "kPercentageNumber", "kNumberNumber", "distribution", "prior", "N", "materialityValue"))

  sampletable$addColumnInfo(name = 'implicitn', title = "Prior sample size", type = 'string')
  sampletable$addColumnInfo(name = 'implicitk', title = "Prior errors", type = 'string')
  sampletable$addColumnInfo(name = 'priorbound', title = paste0(result[["confidence"]]*100,"% Prior confidence bound"), type = 'string')

  message <- paste0("Sample sizes shown are implicit sample sizes derived from the ARM risk assessments: IR = <b>", options[["IR"]], "</b> and CR = <b>", options[["CR"]], "</b>.")
  sampletable$addFootnote(message = message, symbol="<i>Note.</i>")
  
  if(!jaspResults[["ready"]]$object)
    return()

  implicitn <- round(result[["implicitn"]], 2)
  implicitk <- round(result[["implicitk"]], 2)
  
  if(options[["distribution"]] == "beta")
    priorBound <- round(qbeta(p = options[["confidence"]], shape1 = result[["priorA"]], shape2 = result[["priorB"]]), 3)
  if(options[["distribution"]] == "beta-binomial")
    priorBound <- round(.qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object, u = result[["priorA"]], v = result[["priorB"]]) / jaspResults[["N"]]$object, 3)

  priorBound <- paste0(priorBound * 100, "%")
  row <- data.frame(implicitn = implicitn, implicitk = implicitk, priorbound = priorBound)
  sampletable$addRows(row)
  }
}

.plotPrior <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){
  
  if(options[["distribution"]] == "beta"){

    mle <- floor(result[["k"]] * result[["n"]])
    
    if(!options[["expectedPosterior"]]){
      
      xseq <- seq(0, options[["limx"]], 0.001)
      d <- data.frame(
          x = rep(xseq, 2),
          y = dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
          type = c(rep("Prior", length(xseq)))
      )
      
    } else {
      
      k   <- ifelse(options[["expected.errors"]] == "kPercentage", yes = round(options[["kPercentageNumber"]] * result[["n"]], 2), no = options[["kNumberNumber"]])
      
      xseq <- seq(0, options[["limx"]], 0.001)
      d <- data.frame(
          x = rep(xseq, 2),
          y = c(dbeta(x = xseq, shape1 = result[["priorA"]], shape2 = result[["priorB"]]), dbeta(x = xseq, shape1 = result[["priorA"]] + k, shape2 = result[["priorB"]] + (result[["n"]] - k))),
          type = c(rep("Prior", length(xseq)), rep("Expected posterior", length(xseq)))
      )
      # Reorder factor levels to display in legend
      d$type = factor(d$type,levels(d$type)[c(2,1)])
    }

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    xLim <- range(xBreaks)
    yBreaks <- c(0, 1.2*max(d$y))
    yLim <- range(yBreaks)

    pointdata <- data.frame(x = jaspResults[["materiality"]]$object, y = dbeta(jaspResults[["materiality"]]$object, result[["priorA"]], result[["priorB"]]))
    
    if(!options[["expectedPosterior"]]){
      scaleValues <- c("dashed")
      guide <- FALSE
    } else {
      scaleValues <- c("dashed", "dotted")
      guide <- ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1)
    }

    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
        ggplot2::scale_linetype_manual(values=scaleValues, guide = guide)

    p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))


    if(options[["plotPriorAdditionalInfo"]]){
        pdata <- data.frame(x = 0, y = 0, l = "1")
        p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
        if(options[["expectedPosterior"]]){
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior \nconfidence region"))
        } else {
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
        }
        p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))

        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
                                        xlim = c(0, qbeta(options[["confidence"]], result[["priorA"]], result[["priorB"]])),
                                        geom = "area", fill = rgb(0, 1, 0.5, .7))
    }

    p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

    thm <- ggplot2::theme(
  		axis.ticks.y = ggplot2::element_blank(),
  		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
  	)
    p <- p +
    	ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
    	ggplot2::theme()

    p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm
  
  } else {
    
    if(!options[["expectedPosterior"]]){
      
      xseq <- seq(0, jaspResults[["N"]]$object, 1)[1:ceiling(options[["limx"]] * jaspResults[["N"]]$object)]
      d <- data.frame(
          x = xseq,
          y = .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = result[["priorA"]], v = result[["priorB"]])[1:ceiling(options[["limx"]] * jaspResults[["N"]]$object)], 
          type = c(rep("Prior", length(xseq)))
      )
      
    } else {
        
        k   <- ifelse(options[["expected.errors"]] == "kPercentage", yes = round(options[["kPercentageNumber"]] * result[["n"]], 2), no = options[["kNumberNumber"]])
        
        xseq <- seq(0, jaspResults[["N"]]$object, 1)[1:ceiling(options[["limx"]] * jaspResults[["N"]]$object)]
        d <- data.frame(
            x = rep(xseq, 2),
            y = c(.dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = result[["priorA"]], v = result[["priorB"]])[1:ceiling(options[["limx"]] * jaspResults[["N"]]$object)] ,
                  .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = result[["priorA"]] + k, v = result[["priorB"]] + (result[["n"]] - k))[1:ceiling(options[["limx"]] * jaspResults[["N"]]$object)]),
            type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
        )
        # Reorder factor levels to display in legend
        d$type = factor(d$type,levels(d$type)[c(2,1)])
    }
    
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    xLim <- range(xBreaks)
    yBreaks <- c(0, 1.2*max(d$y))
    yLim <- range(yBreaks)

    pointdata <- data.frame(x = jaspResults[["materiality"]]$object * jaspResults[["N"]]$object, y = .dBetaBinom(ceiling(jaspResults[["materiality"]]$object * jaspResults[["N"]]$object), 
                          N = jaspResults[["N"]]$object, result[["priorA"]], result[["priorB"]]))
                          
    if(!options[["expectedPosterior"]]){
      scaleValues <- c("dashed")
      guide <- FALSE
    } else {
      scaleValues <- c("dashed", "dotted")
      guide <- ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1)
    }
                          
    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
        ggplot2::scale_linetype_manual(values = scaleValues, guide = guide)

    p <- p + ggplot2::scale_x_continuous(name = "Population errors", breaks = xBreaks, limits = xLim, labels = xBreaks)
    
    if(options[["plotPriorAdditionalInfo"]]){
        pdata <- data.frame(x = 0, y = 0, l = "1")
        p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
        p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
        p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))
        
        df <- data.frame(x = 0:jaspResults[["N"]]$object, y = .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = result[["priorA"]], v = result[["priorB"]]))
        lim <- .qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object, u = result[["priorA"]], v = result[["priorB"]])    
        df <- df[1:lim, ]
        p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 1, 0.5, .7))
    }

    
    p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

    thm <- ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
    )
    p <- p +
      ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
      ggplot2::theme()

    p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm
    
  }

  return(createJaspPlot(plot = p, title = "Implied Prior from Risk Assessments", width = plotWidth, height = plotHeight))

}

.bayesianAttributesBound <- function(dataset, options, jaspResults){

    ar                        <- 1 - options[["confidence"]]
    ir                        <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                        <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                     <- ar / ir / cr

    if(options[["distribution"]] == "beta"){
      n_noprior               <- .calc.n.beta(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.beta(options, alpha, jaspResults)
    } else if(options[["distribution"]] == "beta-binomial"){
      n_noprior               <- .calc.n.betabinom(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.betabinom(options, alpha, jaspResults)
    }

    pk                        <- 0
    pn                        <- n_noprior - n_withprior
    exp.k                     <- base::switch(options[["expected.errors"]],
                                              "kPercentage" = options[["kPercentageNumber"]],
                                              "kNumber" = options[["kNumberNumber"]])
    if(pn != 0){
        if(options[["expected.errors"]] == "kPercentage"){
            exp.k             <- options[["kPercentageNumber"]]
            pk                <- pn * options[["kPercentageNumber"]]
        } else if(options[["expected.errors"]] == "kNumber"){
            exp.k             <- options[["kNumberNumber"]]
            pk                <- exp.k
        }
    }

    n                         <- 0
    k                         <- 0
    if(options[["correctID"]] != ""){
      n                       <- nrow(dataset)
      k                       <- length(which(dataset[,.v(options[["correctID"]])] == 1))
    }

    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    bound <- "."
    interval <- c(".", ".")
    if(n != 0 && k <= n){
      if(options[["boundMethod"]] == "betaBound")
        bound             <- qbeta(p = options[["confidence"]], shape1 = priorA + k, shape2 = priorB + (n - k), lower.tail = TRUE)
        interval          <- qbeta(p = c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2), shape1 = priorA + k, shape2 = priorB + (n - k), lower.tail = TRUE)
      if(options[["boundMethod"]] == "betabinomialBound")
        bound             <- .qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object, u = priorA + k, v = priorB + (n - k)) / jaspResults[["N"]]$object
        interval          <- .qBetaBinom(p = c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2), N = jaspResults[["N"]]$object, u = priorA + k, v = priorB + (n - k)) / jaspResults[["N"]]$object
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- k
    resultList[["implicitn"]]   <- pn
    resultList[["implicitk"]]   <- pk
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["alpha"]]       <- alpha
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["interval"]]    <- interval
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["posteriorA"]]  <- priorA + k
    resultList[["posteriorB"]]  <- priorB + (n - k)

    jaspResults[["result"]]     <- createJaspState(resultList)
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality",
                                                    "correctID", "expected.errors", "kPercentageNumber",
                                                    "kNumberNumber", "sampleFilter", "prior", "k", "n",
                                                    "distribution", "N", "materialityValue", "populationValue", "variableType"))
}

.bayesianAttributesBoundTable <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID", "expected.errors", "kPercentageNumber", "kNumberNumber", 
                                      "sampleFilter", "mostLikelyError", "bayesFactor", "distribution", "materialityValue", "variableType", "boundMethod", "displayCredibleInterval"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",        type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",        type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Full errors",        type = 'string')
    if(!options[["displayCredibleInterval"]]){
      evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
      if(options[["monetaryVariable"]] != "")
          evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    } else {
      intervalTitles <- paste0(round(c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2) * 100, 3), "%")
      evaluationTable$addColumnInfo(name = 'cilow',          title = intervalTitles[1], type = 'string', overtitle = paste0(result[["confidence"]] * 100,"% Credible interval"))
      evaluationTable$addColumnInfo(name = 'cihigh',         title = intervalTitles[2], type = 'string', overtitle = paste0(result[["confidence"]] * 100,"% Credible interval"))
      if(options[["monetaryVariable"]] != ""){
        evaluationTable$addColumnInfo(name = 'projectedlow',         title = "Lower",           type = 'string', overtitle = "Projected misstatement")
        evaluationTable$addColumnInfo(name = 'projectedhigh',         title = "Upper",           type = 'string', overtitle = "Projected misstatement")
      }
    }
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A",     type = 'string')
      
    message <- base::switch(options[["boundMethod"]],
                              "betaBound" = "The confidence bound is calculated according to the <b>beta</b> distribution.",
                              "betabinomialBound" = "The confidence bound is calculated according to the <b>beta-binomial</b> distribution.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    mle <- ceiling( (result[["posteriorA"]] - 1) / (result[["posteriorA"]] + result[["posteriorB"]] - 2) * jaspResults[["N"]]$object )

    if(options[["correctID"]] == ""){
      row                   <- data.frame(materiality = ".", n = ".", k = ".", bound = ".")
      if(!options[["displayCredibleInterval"]]){
        row <- cbind(row, bound = ".")
        if(options[["monetaryVariable"]] != "")
          row <- cbind(row, projm = ".")
      } else {
        row <- cbind(row, cilow = ".", cihigh = ".")
        if(options[["monetaryVariable"]] != "")
          row <- cbind(row, projectedlow = ".", projectedhigh = ".")
      }
      if(options[["mostLikelyError"]])
        row                 <- cbind(row, mle = ".")
      if(options[["bayesFactor"]])
        row                 <- cbind(row, bf = ".")
      evaluationTable$addRows(row)
      return()
    }

    if(options[["auditType"]] == "attributes"){
      materialityTable <- paste0(round(jaspResults[["materiality"]]$object, 2) * 100, "%")
    } else {
      materialityTable <- options[["materialityValue"]]
    }
    
    if(!options[["displayCredibleInterval"]]){
      boundTable <- result[["bound"]]
      projectedMisstatement <- "."
      if(!"." %in% boundTable){
        boundTable            <- round(result[["bound"]], 4)
        projectedMisstatement <- ceiling(result[["bound"]] * jaspResults[["total_data_value"]]$object)
        boundTable            <- paste0(boundTable * 100, "%")
      }  
      row <- data.frame(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable)
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projm = projectedMisstatement)
    } else {
      intervalTable <- result[["interval"]]
      projectedMisstatement <- "."
      if(!"." %in% intervalTable){
        intervalTable             <- round(result[["interval"]], 4)
        projectedMisstatement     <- ceiling(result[["interval"]] * jaspResults[["total_data_value"]]$object)
        intervalTable             <- paste0(intervalTable * 100, "%")
      }
      row <- data.frame(materiality = materialityTable, n = result[["n"]], k = result[["k"]], cilow = intervalTable[1], cihigh = intervalTable[2])
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projectedlow = projectedMisstatement[1], projectedhigh = projectedMisstatement[2])
    }
    if(options[["mostLikelyError"]])
      row                   <- cbind(row, mle = mle)
    if(options[["bayesFactor"]])
      row                   <- cbind(row, bf = .BF(options, result, jaspResults))
    evaluationTable$addRows(row)
}

.priorAndPosteriorBayesianAttributes <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){
  
  if(options[["boundMethod"]] == "betaBound"){

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

    pointdata <- data.frame(x = jaspResults[["materiality"]]$object, y = dbeta(jaspResults[["materiality"]]$object, result[["posteriorA"]], result[["posteriorB"]]))

    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
        ggplot2::scale_linetype_manual(values=c("dashed", "solid"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))

    p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))

    if(options[["plotPriorAndPosteriorAdditionalInfo"]]){
      pdata <- data.frame(x = 0, y = 0, l = "1")
      p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
      p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \nconfidence region"))
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)
      
      if(!options[["displayCredibleInterval"]]){
        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]]), xlim = c(0, result[["bound"]]),
                                        geom = "area", fill = rgb(0, 0.25, 1, .5))  
      } else {
        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["posteriorA"]], shape2 = result[["posteriorB"]]), xlim = result[["interval"]],
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
    
  } else if(options[["boundMethod"]] == "betabinomialBound"){
      
      xseq <- seq(0, jaspResults[["N"]]$object, 1)[1:ceiling(options[["limx_backup"]] * jaspResults[["N"]]$object)]
      d <- data.frame(
          x = rep(xseq, 2),
          y = c(.dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = result[["priorA"]], v = result[["priorB"]])[1:ceiling(options[["limx_backup"]] * jaspResults[["N"]]$object)], 
                .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = result[["posteriorA"]], v = result[["posteriorB"]])[1:ceiling(options[["limx_backup"]] * jaspResults[["N"]]$object)]),
          type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
      )
      
      xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
      xLim <- range(xBreaks)
      yBreaks <- c(0, 1.2*max(d$y))
      yLim <- range(yBreaks)
  
      pointdata <- data.frame(x = jaspResults[["materiality"]]$object * jaspResults[["N"]]$object, y = .dBetaBinom(ceiling(jaspResults[["materiality"]]$object * jaspResults[["N"]]$object), 
                            N = jaspResults[["N"]]$object, result[["posteriorA"]], result[["posteriorB"]]))
                            
      p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
          ggplot2::scale_linetype_manual(values=c("solid", "dashed"), guide = FALSE)
  
      p <- p + ggplot2::scale_x_continuous(name = "Population errors", breaks = xBreaks, limits = xLim, labels = xBreaks)
      
      if(options[["plotPriorAdditionalInfo"]]){
          pdata <- data.frame(x = 0, y = 0, l = "1")
          p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior confidence region"))
          p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")))
          
          df <- data.frame(x = 0:jaspResults[["N"]]$object, y = .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = result[["posteriorA"]], v = result[["posteriorB"]]))
          if(!options[["displayCredibleInterval"]]){
            lim <- .qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object, u = result[["posteriorA"]], v = result[["posteriorB"]])    
            df <- df[1:lim, ]
            p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 0.25, 1, .5))
          } else {
            lim <- .qBetaBinom(p = c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2), N = jaspResults[["N"]]$object, u = result[["posteriorA"]], v = result[["posteriorB"]])    
            df <- df[lim[1]:lim[2], ]
            p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 0.25, 1, .5))
          }
      }
        
      p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")
  
      thm <- ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
      )
      p <- p +
        ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
        ggplot2::theme()
  
      p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm
      
    }

  return(createJaspPlot(plot = p, title = "Prior and Posterior Plot", width = plotWidth, height = plotHeight))

}

.expectedBF <- function(options, result, ktable, jaspResults){
    priorOdds       <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), result[["priorA"]], result[["priorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), result[["priorA"]], result[["priorB"]]))
    posteriorOdds   <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), result[["priorA"]] + ktable, result[["priorB"]] + (result[["n"]] + ktable))) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), result[["priorA"]] + ktable, result[["priorB"]] + (result[["n"]] + ktable)))
    BF              <- round(posteriorOdds / priorOdds, 2)
    return(BF)
}

.BF <- function(options, result, jaspResults){
  priorOdds         <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), result[["priorA"]], result[["priorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), result[["priorA"]], result[["priorB"]]))
  posteriorOdds     <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), result[["posteriorA"]], result[["posteriorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), result[["posteriorA"]], result[["posteriorB"]]))
  BF                <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

.BFsamples <- function(options, result, jaspResults){
  densprior         <- density(result[["prior"]])
  priorCDF          <- approxfun(densprior$x, densprior$y, yleft=0, yright=0)
  priorLeft         <- integrate(priorCDF, lower = 0, upper = jaspResults[["materiality"]]$object)$value
  priorRight        <- integrate(priorCDF, lower = jaspResults[["materiality"]]$object, upper = 1)$value
  priorOdds         <- priorLeft / priorRight
  densposterior     <- density(result[["posterior"]])
  posteriorCDF      <- approxfun(densposterior$x, densposterior$y, yleft=0, yright=0)
  posteriorLeft     <- integrate(posteriorCDF, lower = 0, upper = jaspResults[["materiality"]]$object)$value
  posteriorRight    <- integrate(posteriorCDF, lower = jaspResults[["materiality"]]$object, upper = 1)$value
  posteriorOdds     <- posteriorLeft / posteriorRight
  BF                <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

.coxAndSnellBound <- function(dataset, options, jaspResults, priorPi = 0.50, priorMu = 0.50, priorA = 1, priorB = 6){
    # Based on the paper:
    # Cox, D. R., & Snell, E. J. (1979). On sampling and the estimation of rare errors. Biometrika, 66(1), 125-132.
    # Default prior options (pi = 0.10, mu = 0.40) are recommendations from the paper
    n                         <- 0
    M                         <- 0
    z                         <- 0
    bound                     <- 0
    interval                  <- c(0, 0)
    prior                     <- NULL
    posterior                 <- NULL
    alpha                     <- 1 - options[["confidence"]]
    
    if(jaspResults[["runEvaluation"]]$object){
      
      sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["correctID"]]))]
      n                       <- nrow(sample)
      t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["correctID"]])]
      z                       <- t / sample[, .v(options[["monetaryVariable"]])]
      z                       <- subset(z, z > 0)
      M                       <- length(z)
      z_bar                   <- mean(z)
      if(M == 0)
          z_bar               <- 0

      prior_part_1            <- (0 + priorA) / (0 + priorB)
      prior_part_2            <- ((priorMu * (priorB - 1)) + (0 * 0)) / (0 + (priorA / priorPi))
      prior                   <- prior_part_1 * prior_part_2 * stats::rf(n = 1e6, df1 = (2 * (0 + priorA)), df2 = ( 2 *(0 + priorB)))

      posterior_part_1        <- (M + priorA) / (M + priorB)
      posterior_part_2        <- ((priorMu * (priorB - 1)) + (M * z_bar)) / (n + (priorA / priorPi))
      posterior               <- posterior_part_1 * posterior_part_2 * stats::rf(n = 1e6, df1 = (2 * (M + priorA)), df2 = ( 2 *(M + priorB)))

      bound                   <- as.numeric(quantile(posterior, probs = options[["confidence"]], na.rm = TRUE))
      interval                <- as.numeric(quantile(posterior, probs = c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2), na.rm = TRUE))
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["interval"]]    <- interval
    resultList[["alpha"]]       <- alpha
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["priorPi"]]     <- priorPi
    resultList[["priorMu"]]     <- priorMu
    resultList[["posteriorA"]]  <- priorA + sum(z)
    resultList[["posteriorB"]]  <- priorB + (n - (sum(z)))
    resultList[["prior"]]       <- prior
    resultList[["posterior"]]   <- posterior

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctID", "sampleFilter", "auditType", "boundMethod", "monetaryVariable"))
}

.bayesianMusBoundTable <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "correctID", "sampleFilter", "distribution", "mostLikelyError", "correctMUS", "sampleFilterMUS",
                                      "boundMethod", "bayesFactor", "materialityValue", "populationValue", "variableType", "displayCredibleInterval"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",            type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",            type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",                 type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Total tainting",         type = 'string')
    
    if(!options[["displayCredibleInterval"]]){
      evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
      if(options[["monetaryVariable"]] != "")
          evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    } else {
      intervalTitles <- paste0(round(c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2) * 100, 3), "%")
      evaluationTable$addColumnInfo(name = 'cilow',          title = intervalTitles[1], type = 'string', overtitle = paste0(result[["confidence"]] * 100,"% Credible interval"))
      evaluationTable$addColumnInfo(name = 'cihigh',         title = intervalTitles[2], type = 'string', overtitle = paste0(result[["confidence"]] * 100,"% Credible interval"))
      if(options[["monetaryVariable"]] != ""){
        evaluationTable$addColumnInfo(name = 'projectedlow',         title = "Lower",           type = 'string', overtitle = "Projected misstatement")
        evaluationTable$addColumnInfo(name = 'projectedhigh',         title = "Upper",           type = 'string', overtitle = "Projected misstatement")
      }
    }
    
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                    type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A",         type = 'string')

    message <- base::switch(options[["boundMethod"]],
                                      "coxAndSnellBound" = "The confidence bound is calculated according to the <b>Cox and Snell</b> method.",
                                      "regressionBound" = "The confidence bound is calculated according to the <b>Regression</b> method.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")
    
    if(options[["auditType"]] == "mus"){
        materialityTable <- ceiling(options[["materialityValue"]])
    } else {
      materialityTable <- paste0(round(options[["materiality"]] * 100, 2) , "%")
    }

    # Return empty table with materiality
    if(!jaspResults[["runEvaluation"]]$object){
      row <- data.frame(materiality = materialityTable, n = ".", fk = ".", k = ".")
      if(!options[["displayCredibleInterval"]]){
        row <- cbind(row, bound = ".")
        if(options[["monetaryVariable"]] != "")
          row <- cbind(row, projm = ".")
      } else {
        row <- cbind(row, cilow = ".", cihigh = ".")
        if(options[["monetaryVariable"]] != "")
          row <- cbind(row, projectedlow = ".", projectedhigh = ".")
      }
      evaluationTable$addRows(row)
      return()
    }
    
    total_data_value <- jaspResults[["total_data_value"]]$object

    errors                  <- round(sum(result[["z"]]), 2)
    mle                     <- 0
    
    if(options[["boundMethod"]] == "coxAndSnellBound"){
        mle <- ceiling( sum(result[["z"]]) / result[["n"]] * total_data_value )
    } else if(options[["boundMethod"]] == "regressionBound"){
        mle <- round(result[["mle"]], 2)
    }
    
    if(!options[["displayCredibleInterval"]]){
      boundTable <- result[["bound"]]
      projectedMisstatement <- "."
      if(!"." %in% boundTable){
        boundTable            <- round(result[["bound"]], 4)
        projectedMisstatement <- ceiling(result[["bound"]] * total_data_value)
        boundTable            <- paste0(boundTable * 100, "%")
      }  
      row <- data.frame(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, bound = boundTable)
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projm = projectedMisstatement)
    } else {
      intervalTable <- result[["interval"]]
      projectedMisstatement <- "."
      if(!"." %in% intervalTable){
        intervalTable             <- round(result[["interval"]], 4)
        projectedMisstatement     <- ceiling(result[["interval"]] * total_data_value)
        intervalTable             <- paste0(intervalTable * 100, "%")
      }
      row <- data.frame(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, cilow = intervalTable[1], cihigh = intervalTable[2])
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projectedlow = projectedMisstatement[1], projectedhigh = projectedMisstatement[2])
    }
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    if(options[["bayesFactor"]])
      row <- cbind(row, bf = .BFsamples(options, result, jaspResults))
    evaluationTable$addRows(row)
}

.priorAndPosteriorBayesianMUS <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  prior <- density(result[["prior"]], from = 0, to = options[["limx_backup"]], n = 2^10)
  posterior <- density(result[["posterior"]], from = 0, to = options[["limx_backup"]], n = 2^10)

  d <- data.frame(
      x = c(prior$x, posterior$x),
      y = c(prior$y, posterior$y),
      type = c(rep("Prior", length(prior$x)), rep("Posterior", length(posterior$x)))
  )
  # Reorder factor levels to display in legend
  d$type = factor(d$type,levels(d$type)[c(2,1)])

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(posterior$x, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  posteriorPointData <- subset(d, d$type == "Posterior")
  posteriorPointDataY <- posteriorPointData$y[which.min(abs(posteriorPointData$x - jaspResults[["materiality"]]$object))]

  pointdata <- data.frame(x = jaspResults[["materiality"]]$object, y = posteriorPointDataY)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      ggplot2::scale_linetype_manual(values=c("dashed", "solid"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))

  p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))

  if(options[["plotPriorAndPosteriorAdditionalInfo"]]){
    pdata <- data.frame(x = 0, y = 0, l = "1")
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
    p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \nconfidence region"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)
    if(!options[["displayCredibleInterval"]]){
        p <- p + ggplot2::geom_area(mapping = ggplot2::aes(x = x, y = y), data = subset(subset(d, d$type == "Posterior"), subset(d, d$type == "Posterior")$x <= result[["bound"]]), fill = rgb(0, 0.25, 1, .5))
    } else {
      subset1 <- subset(subset(d, d$type == "Posterior"), subset(d, d$type == "Posterior")$x <= result[["interval"]][2])
      subset2 <- subset(subset1, subset1$x >= result[["interval"]][1])
      p <- p + ggplot2::geom_area(mapping = ggplot2::aes(x = x, y = y), data = subset2, fill = rgb(0, 0.25, 1, .5))
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

  return(createJaspPlot(plot = p, title = "Prior and Posterior Plot", width = plotWidth, height = plotHeight))

}

.regressionEstimatorBayesian <- function(dataset, options, total_data_value, jaspResults){

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."

    if(options[["correctMUS"]] != "" && options[["sampleFilter"]] != "" && options[["monetaryVariable"]] != ""){
        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["correctMUS"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["correctMUS"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        zplus                   <- sort(subset(z, z > 0), decreasing = TRUE)
        M                       <- length(which(t != 0))

        B                       <- total_data_value
        N                       <- options[["N"]]
        b                       <- sample[, .v(options[["monetaryVariable"]])]
        w                       <- sample[, .v(options[["correctMUS"]])]
        #b1                      <- as.numeric(coef(lm(w ~ b))[2])
        set.seed(1)
        formula                 <- lm(w ~ b)
        bayesFit                <- .bayesfit(formula, 100000)
        b1                      <- median(bayesFit[, 2])

        meanb                   <- mean(b)
        meanw                   <- mean(w)

        mleregression           <- (N * meanw + b1 * (B - N * meanb)) - B
        upperValue              <- mleregression + qt(p = options[["confidence"]], df = n - 1) * sqrt(1 - cor(b, w)^2) * sd(w) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) )
        bound                   <- (upperValue + B) / B
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctMUS", "sampleFilterMUS", "auditType", "boundMethod", "monetaryVariable", "materialityValue", 
                                              "variableType"))
}

.bayesfit <- function(lmfit, N){
    QR<-lmfit$qr
    df.residual<-lmfit$df.residual
    R<-qr.R(QR) ## R component
    coef<-lmfit$coef
    Vb<-chol2inv(R) ## variance(unscaled)
    s2<-(t(lmfit$residuals)%*%lmfit$residuals)
    s2<-s2[1,1]/df.residual

    ## now to sample residual variance
    sigma<-df.residual*s2/rchisq(N,df.residual)
    coef.sim<-sapply(sigma,function(x) MASS::mvrnorm(1,coef,Vb*x))
    ret<-data.frame(t(coef.sim))
    names(ret)<-names(lmfit$coef)
    ret$sigma<-sqrt(sigma)
    ret
}
