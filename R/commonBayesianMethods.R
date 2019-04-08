.calc.n.beta <- function(options, alpha, jaspResults){
    for(n in 1:5000){
      impk <- base::switch(options[["expectedErrors"]], "expectedRelative" = n * options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]])
        if(impk >= n){ next }
        x <- qbeta(p = 1 - alpha, shape1 = 1 + impk, shape2 = 1 + (n - impk))
        if(x < jaspResults[["materiality"]]$object) return(n)
    }
}

.dBetaBinom <- function (x, N, u, v, log = FALSE){
    logval <- lbeta(x + u, N - x + v) - lbeta(u, v) + lchoose(N, x)
    if (log) { ret <- logval } else { ret <- exp(logval) }
    return(ret)
}

.qBetaBinom <- function (p, N, u, v){
    pp <- cumsum(.dBetaBinom(0:N, N, u, v))
    return(sapply(p, function(x) sum(pp < x)))
}

.calc.n.betabinom <- function(options, alpha, jaspResults){
    N <- jaspResults[["N"]]$object
    for(n in 1:5000){
      impk <- base::switch(options[["expectedErrors"]], "expectedRelative" = n * options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]])
      if(impk >= n){ next }
      x <- .qBetaBinom(p = 1 - alpha, N = N, u = 1 + impk, v = 1 + (n - impk)) / N
      if(x < jaspResults[["materiality"]]$object) return(n)
    }
}

.bayesianPlanningHelper <- function(options, jaspResults){

    if(!is.null(jaspResults[["planningResult"]]$object)) 
      return(jaspResults[["planningResult"]]$object)

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

      if(options[["planningModel"]] == "beta"){
        n_noprior               <- .calc.n.beta(options, 1 - options[["confidence"]], jaspResults)
        n_withprior             <- .calc.n.beta(options, alpha, jaspResults)
      } else if(options[["planningModel"]] == "beta-binomial"){
        n_noprior               <- .calc.n.betabinom(options, 1 - options[["confidence"]], jaspResults)
        n_withprior             <- .calc.n.betabinom(options, alpha, jaspResults)
      }

      pk                      <- 0
      pn                      <- n_noprior - n_withprior
      k                       <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]])
      if(pn != 0){
          if(options[["expectedErrors"]] == "expectedRelative"){
              k               <- options[["expectedPercentage"]]
              pk              <- pn * k
          } else if(options[["expected.errors"]] == "expectedAbsolute"){
              k               <- options[["expectedNumber"]]
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
    jaspResults[["planningResult"]]$dependOnOptions(c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "expectedPercentage",
                                                      "expectedNumber", "planningModel", "materialityValue", "recordNumberVariable", "materiality", "monetaryVariable"))
    return(jaspResults[["planningResult"]]$object)
}

.bayesianPlanningTable <- function(dataset, options, planningResult, jaspResults, position = 1){

  if(!is.null(jaspResults[["planningContainer"]][["planningSummary"]])) return() #The options for this table didn't change so we don't need to rebuild it

  planningSummary                                             <- createJaspTable("Planning Summary")
  jaspResults[["planningContainer"]][["planningSummary"]]        <- planningSummary
  planningSummary$position                                       <- position
  planningSummary$dependOnOptions(c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "expectedPercentage", "expectedNumber", "expectedBF", 
                                    "planningModel", "materialityValue", "recordNumberVariable", "monetaryVariable", "materiality"))

  planningSummary$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  planningSummary$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  planningSummary$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  planningSummary$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  planningSummary$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  planningSummary$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')
  if(options[["expectedBF"]])
    planningSummary$addColumnInfo(name = 'expBF',              title = "Expected BF\u208B\u208A", type = 'string')

  message <- base::switch(options[["planningModel"]],
                            "beta" = "The sample size is based on the <b>beta</b> distribution.",
                            "beta-binomial" = paste0("The sample size is based on the <b>beta-binomial</b> distribution (N = ", jaspResults[["N"]]$object ,")."))                          
  planningSummary$addFootnote(message = message, symbol="<i>Note.</i>")

  ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(planningResult[["k"]] * planningResult[["n"]], 2), "expectedAbsolute" = options[["expectedNumber"]])
  DRtable <- paste0(round(planningResult[["alpha"]], 3) * 100, "%")

  if(jaspResults[["materiality"]]$object == 0){
    row <- data.frame(materiality = ".", IR = planningResult[["IR"]], CR = planningResult[["CR"]], DR = DRtable, k = 0, n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    planningSummary$addRows(row)
    return()
  }

  materialityTitle  <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue  <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * sum(dataset[, .v(options[["monetaryVariable"]])])), "materialityAbsolute" = options[["materialityValue"]])
  materiality       <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = materialityValue)

  if(!jaspResults[["ready"]]$object){
    row <- data.frame(materiality = materiality, IR = planningResult[["IR"]],CR = planningResult[["CR"]], DR = DRtable, k = ktable, n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    planningSummary$addRows(row)
    return()
  }

  row <- data.frame(materiality = materiality, IR  = planningResult[["IR"]], CR = planningResult[["CR"]], DR = DRtable, k = ktable, n = planningResult[["n"]])
  if(options[["expectedBF"]])
    row <- cbind(row, expBF = .expectedBF(options, planningResult, ktable, jaspResults))
  planningSummary$addRows(row)
}

.implicitSampleTable <- function(options, result, jaspResults, position = 3){

  if(!is.null(jaspResults[["planningContainer"]][["sampletable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  if(options[["implicitSampleTable"]]){

  sampletable                       <- createJaspTable("Implicit Sample")
  jaspResults[["planningContainer"]][["sampletable"]]      <- sampletable
  sampletable$position              <- position
  sampletable$dependOnOptions(c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "implicitSampleTable", "expectedPercentage", "expectedNumber", 
                                  "planningModel", "materialityValue"))

  sampletable$addColumnInfo(name = 'implicitn', title = "Implicit sample size", type = 'string')
  sampletable$addColumnInfo(name = 'implicitk', title = "Implicit errors", type = 'string')
  sampletable$addColumnInfo(name = 'priorbound', title = paste0(options[["confidence"]]*100,"% Prior confidence bound"), type = 'string')

  message <- paste0("Sample sizes shown are implicit sample sizes derived from the ARM risk assessments: IR = <b>", options[["IR"]], "</b> and CR = <b>", options[["CR"]], "</b>.")
  sampletable$addFootnote(message = message, symbol="<i>Note.</i>")
  
  if(!jaspResults[["ready"]]$object)
    return()

  implicitn <- round(result[["implicitn"]], 2)
  implicitk <- round(result[["implicitk"]], 2)
  
  if(options[["planningModel"]] == "beta")
    priorBound <- round(qbeta(p = options[["confidence"]], shape1 = result[["priorA"]], shape2 = result[["priorB"]]), 3)
  if(options[["planningModel"]] == "beta-binomial")
    priorBound <- round(.qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object, u = result[["priorA"]], v = result[["priorB"]]) / jaspResults[["N"]]$object, 3)

  priorBound <- paste0(priorBound * 100, "%")
  row <- data.frame(implicitn = implicitn, implicitk = implicitk, priorbound = priorBound)
  sampletable$addRows(row)
  }
}

.plotPrior <- function(options, planningResult, jaspResults, plotWidth = 600, plotHeight = 450){
  
  if(options[["planningModel"]] == "beta"){
    mle <- floor(planningResult[["k"]] * planningResult[["n"]])
  
    if(!options[["priorPlotExpectedPosterior"]]){  
      xseq <- seq(0, options[["priorPlotLimit"]], 0.001)
      d <- data.frame(
          x = rep(xseq, 2),
          y = dbeta(x = xseq, shape1 = planningResult[["priorA"]], shape2 = planningResult[["priorB"]]),
          type = c(rep("Prior", length(xseq)))
      )  
    } else {  
      k   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = round(options[["expectedPercentage"]] * planningResult[["n"]], 2), no = options[["expectedNumber"]])
      xseq <- seq(0, options[["priorPlotLimit"]], 0.001)
      d <- data.frame(
          x = rep(xseq, 2),
          y = c(dbeta(x = xseq, shape1 = planningResult[["priorA"]], shape2 = planningResult[["priorB"]]), dbeta(x = xseq, shape1 = planningResult[["priorA"]] + k, shape2 = planningResult[["priorB"]] + (planningResult[["n"]] - k))),
          type = c(rep("Prior", length(xseq)), rep("Expected posterior", length(xseq)))
      )
      # Reorder factor levels to display in legend
      d$type = factor(d$type,levels(d$type)[c(2,1)])
    }

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    xLim <- range(xBreaks)
    yBreaks <- c(0, 1.2*max(d$y))
    yLim <- range(yBreaks)

    pointdata <- data.frame(x = jaspResults[["materiality"]]$object, y = dbeta(jaspResults[["materiality"]]$object, planningResult[["priorA"]], planningResult[["priorB"]]))
    
    if(!options[["priorPlotExpectedPosterior"]]){
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


    if(options[["priorPlotAdditionalInfo"]]){
        pdata <- data.frame(x = 0, y = 0, l = "1")
        p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
        if(options[["priorPlotExpectedPosterior"]]){
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior \nconfidence region"))
        } else {
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
        }
        p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))

        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = planningResult[["priorA"]], shape2 = planningResult[["priorB"]]),
                                        xlim = c(0, qbeta(options[["confidence"]], planningResult[["priorA"]], planningResult[["priorB"]])),
                                        geom = "area", fill = rgb(0, 1, 0.5, .7))
    }

    p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

    thm <- ggplot2::theme(
  		axis.ticks.y = ggplot2::element_blank(),
  		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
  	)
    p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
    	       ggplot2::theme()
    p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm
  
  } else {
    
    if(!options[["priorPlotExpectedPosterior"]]){    
      xseq <- seq(0, jaspResults[["N"]]$object, 1)[1:ceiling(options[["priorPlotLimit"]] * jaspResults[["N"]]$object)]
      d <- data.frame(
          x = xseq,
          y = .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = planningResult[["priorA"]], v = planningResult[["priorB"]])[1:ceiling(options[["priorPlotLimit"]] * jaspResults[["N"]]$object)], 
          type = c(rep("Prior", length(xseq)))
      )    
    } else {    
        k   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = round(options[["expectedPercentage"]] * planningResult[["n"]], 2), no = options[["expectedNumber"]])
        
        xseq <- seq(0, jaspResults[["N"]]$object, 1)[1:ceiling(options[["priorPlotLimit"]] * jaspResults[["N"]]$object)]
        d <- data.frame(
            x = rep(xseq, 2),
            y = c(.dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = planningResult[["priorA"]], v = planningResult[["priorB"]])[1:ceiling(options[["priorPlotLimit"]] * jaspResults[["N"]]$object)] ,
                  .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = planningResult[["priorA"]] + k, v = planningResult[["priorB"]] + (planningResult[["n"]] - k))[1:ceiling(options[["priorPlotLimit"]] * jaspResults[["N"]]$object)]),
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
                          N = jaspResults[["N"]]$object, planningResult[["priorA"]], planningResult[["priorB"]]))
                          
    if(!options[["priorPlotExpectedPosterior"]]){
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
    
    if(options[["priorPlotAdditionalInfo"]]){
        pdata <- data.frame(x = 0, y = 0, l = "1")
        p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
        if(options[["priorPlotExpectedPosterior"]]){
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior \nconfidence region"))
        } else {
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
        }
        p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))
        
        df <- data.frame(x = 0:jaspResults[["N"]]$object, y = .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = planningResult[["priorA"]], v = planningResult[["priorB"]]))
        lim <- .qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object, u = planningResult[["priorA"]], v = planningResult[["priorB"]])    
        df <- df[1:lim, ]
        p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 1, 0.5, .7))
    }
  
    p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")
    thm <- ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
    )
    p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
              ggplot2::theme()
    p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm  
  }
  return(createJaspPlot(plot = p, title = "Implied Prior from Risk Assessments", width = plotWidth, height = plotHeight))
}

.bayesianAttributesBound <- function(dataset, options, jaspResults){
  
  if(!is.null(jaspResults[["evaluationResult"]]$object)) 
    return(jaspResults[["evaluationResult"]]$object)

    ar                        <- 1 - options[["confidence"]]
    ir                        <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                        <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                     <- ar / ir / cr

    if(options[["planningModel"]] == "beta"){
      n_noprior               <- .calc.n.beta(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.beta(options, alpha, jaspResults)
    } else if(options[["planningModel"]] == "beta-binomial"){
      n_noprior               <- .calc.n.betabinom(options, 1 - options[["confidence"]], jaspResults)
      n_withprior             <- .calc.n.betabinom(options, alpha, jaspResults)
    }

    pk                        <- 0
    pn                        <- n_noprior - n_withprior
    exp.k                     <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]])
    if(pn != 0){
        if(options[["expectedErrors"]] == "expectedRelative"){
            exp.k             <- options[["expectedPercentage"]]
            pk                <- pn * exp.k
        } else {
            exp.k             <- options[["expectedNumber"]]
            pk                <- exp.k
        }
    }

    n                         <- 0
    k                         <- 0
    if(options[["auditResult"]] != ""){
      n                       <- nrow(dataset)
      k                       <- length(which(dataset[,.v(options[["auditResult"]])] == 1))
    }

    priorA                  <- 1 + pk
    priorB                  <- 1 + (pn - pk)

    bound <- "."
    interval <- c(".", ".")
    if(n != 0 && k <= n){
      if(options[["estimator"]] == "betaBound")
        bound             <- qbeta(p = options[["confidence"]], shape1 = priorA + k, shape2 = priorB + (n - k), lower.tail = TRUE)
        interval          <- qbeta(p = c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2), shape1 = priorA + k, shape2 = priorB + (n - k), lower.tail = TRUE)
      if(options[["estimator"]] == "betabinomialBound")
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

    jaspResults[["evaluationResult"]]     <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]     $dependOnOptions(c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "expectedErrors", "expectedPercentage", "expectedNumber", "sampleFilter",
                                                    "planningModel", "materialityValue", "variableType", "materiality"))
    return(jaspResults[["evaluationResult"]]$object)
}

.bayesianAttributesBoundTable <- function(options, evaluationResult, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "expectedErrors", "expectedPercentage", "expectedNumber", 
                                      "sampleFilter", "mostLikelyError", "bayesFactor", "planningModel", "materialityValue", "variableType", "estimator", "displayCredibleInterval"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",        type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",        type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Full errors",        type = 'string')
    if(!options[["displayCredibleInterval"]]){
      evaluationTable$addColumnInfo(name = 'bound',         title = paste0(options[["confidence"]] * 100,"% Confidence bound"), type = 'string')
      if(options[["monetaryVariable"]] != "")
          evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    } else {
      intervalTitles <- paste0(round(c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2) * 100, 3), "%")
      evaluationTable$addColumnInfo(name = 'cilow',          title = intervalTitles[1], type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      evaluationTable$addColumnInfo(name = 'cihigh',         title = intervalTitles[2], type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      if(options[["monetaryVariable"]] != ""){
        evaluationTable$addColumnInfo(name = 'projectedlow',         title = "Lower",           type = 'string', overtitle = "Projected misstatement")
        evaluationTable$addColumnInfo(name = 'projectedhigh',         title = "Upper",           type = 'string', overtitle = "Projected misstatement")
      }
    }
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A",     type = 'string')
      
    message <- base::switch(options[["estimator"]],
                              "betaBound" = "The confidence bound is calculated according to the <b>beta</b> distribution.",
                              "betabinomialBound" = "The confidence bound is calculated according to the <b>beta-binomial</b> distribution.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    mle <- ceiling( (evaluationResult[["posteriorA"]] - 1) / (evaluationResult[["posteriorA"]] + evaluationResult[["posteriorB"]] - 2) * jaspResults[["N"]]$object )

    if(options[["auditResult"]] == ""){
      row                   <- data.frame(materiality = ".", n = ".", k = ".")
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
    
    materialityTable <- ifelse(options[["materiality"]] == "materialityRelative", yes = paste0(round(jaspResults[["materiality"]]$object, 2) * 100, "%"), no = options[["materialityValue"]])
  
    if(!options[["displayCredibleInterval"]]){
      boundTable <- evaluationResult[["bound"]]
      projectedMisstatement <- "."
      if(!"." %in% boundTable){
        boundTable            <- round(evaluationResult[["bound"]], 4)
        projectedMisstatement <- ceiling(evaluationResult[["bound"]] * jaspResults[["total_data_value"]]$object)
        boundTable            <- paste0(boundTable * 100, "%")
      }  
      row <- data.frame(materiality = materialityTable, n = evaluationResult[["n"]], k = evaluationResult[["k"]], bound = boundTable)
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projm = projectedMisstatement)
    } else {
      intervalTable <- evaluationResult[["interval"]]
      projectedMisstatement <- "."
      if(!"." %in% intervalTable){
        intervalTable             <- round(evaluationResult[["interval"]], 4)
        projectedMisstatement     <- ceiling(evaluationResult[["interval"]] * jaspResults[["total_data_value"]]$object)
        intervalTable             <- paste0(intervalTable * 100, "%")
      }
      row <- data.frame(materiality = materialityTable, n = evaluationResult[["n"]], k = evaluationResult[["k"]], cilow = intervalTable[1], cihigh = intervalTable[2])
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projectedlow = projectedMisstatement[1], projectedhigh = projectedMisstatement[2])
    }
    if(options[["mostLikelyError"]])
      row                   <- cbind(row, mle = mle)
    if(options[["bayesFactor"]])
      row                   <- cbind(row, bf = .BF(options, evaluationResult, jaspResults))
    evaluationTable$addRows(row)
}

.priorAndPosteriorBayesianAttributes <- function(options, evaluationResult, jaspResults, plotWidth = 600, plotHeight = 450){
  
  if(options[["estimator"]] == "betaBound"){

    xseq <- seq(0, options[["priorAndPosteriorPlotLimit"]], 0.001)
    d <- data.frame(
        x = rep(xseq, 2),
        y = c(dbeta(x = xseq, shape1 = evaluationResult[["priorA"]], shape2 = evaluationResult[["priorB"]]), dbeta(x = xseq, shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]])),
        type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
    )
    # Reorder factor levels to display in legend
    d$type = factor(d$type,levels(d$type)[c(2,1)])

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    xLim <- range(xBreaks)
    yBreaks <- c(0, 1.2*max(d$y))
    yLim <- range(yBreaks)

    pointdata <- data.frame(x = jaspResults[["materiality"]]$object, y = dbeta(jaspResults[["materiality"]]$object, evaluationResult[["posteriorA"]], evaluationResult[["posteriorB"]]))

    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
        ggplot2::scale_linetype_manual(values=c("dashed", "solid"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))

    p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))

    if(options[["priorAndPosteriorPlotAdditionalInfo"]]){
      pdata <- data.frame(x = 0, y = 0, l = "1")
      p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
      p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \nconfidence region"))
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)
      
      if(!options[["displayCredibleInterval"]]){
        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]]), xlim = c(0, evaluationResult[["bound"]]),
                                        geom = "area", fill = rgb(0, 0.25, 1, .5))  
      } else {
        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]]), xlim = evaluationResult[["interval"]],
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
    
  } else if(options[["estimator"]] == "betabinomialBound"){
      
      xseq <- seq(0, jaspResults[["N"]]$object, 1)[1:ceiling(options[["priorAndPosteriorPlotLimit"]] * jaspResults[["N"]]$object)]
      d <- data.frame(
          x = rep(xseq, 2),
          y = c(.dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = evaluationResult[["priorA"]], v = evaluationResult[["priorB"]])[1:ceiling(options[["priorAndPosteriorPlotLimit"]] * jaspResults[["N"]]$object)], 
                .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = evaluationResult[["posteriorA"]], v = evaluationResult[["posteriorB"]])[1:ceiling(options[["priorAndPosteriorPlotLimit"]] * jaspResults[["N"]]$object)]),
          type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
      )
      
      xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
      xLim <- range(xBreaks)
      yBreaks <- c(0, 1.2*max(d$y))
      yLim <- range(yBreaks)
  
      pointdata <- data.frame(x = jaspResults[["materiality"]]$object * jaspResults[["N"]]$object, y = .dBetaBinom(ceiling(jaspResults[["materiality"]]$object * jaspResults[["N"]]$object), 
                            N = jaspResults[["N"]]$object, evaluationResult[["posteriorA"]], evaluationResult[["posteriorB"]]))
                            
      p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
          ggplot2::scale_linetype_manual(values=c("solid", "dashed"), guide = FALSE)
  
      p <- p + ggplot2::scale_x_continuous(name = "Population errors", breaks = xBreaks, limits = xLim, labels = xBreaks)
      
      if(options[["priorAndPosteriorPlotAdditionalInfo"]]){
          pdata <- data.frame(x = 0, y = 0, l = "1")
          p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior confidence region"))
          p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")))
          
          df <- data.frame(x = 0:jaspResults[["N"]]$object, y = .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object, u = evaluationResult[["posteriorA"]], v = evaluationResult[["posteriorB"]]))
          if(!options[["displayCredibleInterval"]]){
            lim <- .qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object, u = evaluationResult[["posteriorA"]], v = evaluationResult[["posteriorB"]])    
            df <- df[1:lim, ]
            p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 0.25, 1, .5))
          } else {
            lim <- .qBetaBinom(p = c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2), N = jaspResults[["N"]]$object, u = evaluationResult[["posteriorA"]], v = evaluationResult[["posteriorB"]])    
            df <- df[lim[1]:lim[2], ]
            p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 0.25, 1, .5))
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
    }
  return(createJaspPlot(plot = p, title = "Prior and Posterior Plot", width = plotWidth, height = plotHeight))
}

.expectedBF <- function(options, planningResult, ktable, jaspResults){
    priorOdds       <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), planningResult[["priorA"]], planningResult[["priorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), planningResult[["priorA"]], planningResult[["priorB"]]))
    posteriorOdds   <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), planningResult[["priorA"]] + ktable, planningResult[["priorB"]] + (planningResult[["n"]] + ktable))) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), planningResult[["priorA"]] + ktable, planningResult[["priorB"]] + (planningResult[["n"]] + ktable)))
    BF              <- round(posteriorOdds / priorOdds, 2)
    return(BF)
}

.BF <- function(options, planningResult, jaspResults){
  priorOdds         <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), planningResult[["priorA"]], planningResult[["priorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), planningResult[["priorA"]], planningResult[["priorB"]]))
  posteriorOdds     <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), planningResult[["posteriorA"]], planningResult[["posteriorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), planningResult[["posteriorA"]], planningResult[["posteriorB"]]))
  BF                <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

.BFsamples <- function(options, evaluationResult, jaspResults){
  densprior         <- density(evaluationResult[["prior"]])
  priorCDF          <- approxfun(densprior$x, densprior$y, yleft=0, yright=0)
  priorLeft         <- integrate(priorCDF, lower = 0, upper = jaspResults[["materiality"]]$object)$value
  priorRight        <- integrate(priorCDF, lower = jaspResults[["materiality"]]$object, upper = 1)$value
  priorOdds         <- priorLeft / priorRight
  densposterior     <- density(evaluationResult[["posterior"]])
  posteriorCDF      <- approxfun(densposterior$x, densposterior$y, yleft=0, yright=0)
  posteriorLeft     <- integrate(posteriorCDF, lower = 0, upper = jaspResults[["materiality"]]$object)$value
  posteriorRight    <- integrate(posteriorCDF, lower = jaspResults[["materiality"]]$object, upper = 1)$value
  posteriorOdds     <- posteriorLeft / posteriorRight
  BF                <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

.coxAndSnellBound <- function(dataset, options, jaspResults, priorPi = 0.50, priorMu = 0.50, priorA = 1, priorB = 6){
  if(!is.null(jaspResults[["evaluationResult"]]$object)) 
    return(jaspResults[["evaluationResult"]]$object)
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
      
      sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
      n                       <- nrow(sample)
      t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
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

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOnOptions(c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materiality", "estimator", "monetaryVariable"))
    return(jaspResults[["evaluationResult"]]$object)
}

.bayesianAuditValueBoundTable <- function(options, evaluationResult, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "sampleFilter", "planningModel", "mostLikelyError", "estimator", "bayesFactor", 
                                        "materialityValue", "variableType", "displayCredibleInterval"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",            type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",            type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",                 type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Total tainting",         type = 'string')
    
    if(!options[["displayCredibleInterval"]]){
      evaluationTable$addColumnInfo(name = 'bound',         title = paste0(options[["confidence"]] * 100,"% Confidence bound"), type = 'string')
      if(options[["monetaryVariable"]] != "")
          evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    } else {
      intervalTitles <- paste0(round(c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2) * 100, 3), "%")
      evaluationTable$addColumnInfo(name = 'cilow',          title = intervalTitles[1], type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      evaluationTable$addColumnInfo(name = 'cihigh',         title = intervalTitles[2], type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      if(options[["monetaryVariable"]] != ""){
        evaluationTable$addColumnInfo(name = 'projectedlow',         title = "Lower",           type = 'string', overtitle = "Projected misstatement")
        evaluationTable$addColumnInfo(name = 'projectedhigh',         title = "Upper",           type = 'string', overtitle = "Projected misstatement")
      }
    }
    
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                    type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A",         type = 'string')

    message <- base::switch(options[["estimator"]],
                                      "coxAndSnellBound" = "The confidence bound is calculated according to the <b>Cox and Snell</b> method.",
                                      "regressionBound" = "The confidence bound is calculated according to the <b>Regression</b> method.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")
    
    materialityTable <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]], no = paste0(round(options[["materialityPercentage"]] * 100, 2) , "%"))

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

    errors                  <- round(sum(evaluationResult[["z"]]), 2)
    mle                     <- 0
    
    if(options[["estimator"]] == "coxAndSnellBound"){
        mle <- ceiling( sum(evaluationResult[["z"]]) / evaluationResult[["n"]] * total_data_value )
    } else if(options[["estimator"]] == "regressionBound"){
        mle <- round(evaluationResult[["mle"]], 2)
    }
    
    if(!options[["displayCredibleInterval"]]){
      boundTable <- evaluationResult[["bound"]]
      projectedMisstatement <- "."
      if(!"." %in% boundTable){
        boundTable            <- round(evaluationResult[["bound"]], 4)
        projectedMisstatement <- ceiling(evaluationResult[["bound"]] * total_data_value)
        boundTable            <- paste0(boundTable * 100, "%")
      }  
      row <- data.frame(materiality = materialityTable, n = evaluationResult[["n"]], fk = evaluationResult[["k"]], k = errors, bound = boundTable)
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projm = projectedMisstatement)
    } else {
      intervalTable <- evaluationResult[["interval"]]
      projectedMisstatement <- "."
      if(!"." %in% intervalTable){
        intervalTable             <- round(evaluationResult[["interval"]], 4)
        projectedMisstatement     <- ceiling(evaluationResult[["interval"]] * total_data_value)
        intervalTable             <- paste0(intervalTable * 100, "%")
      }
      row <- data.frame(materiality = materialityTable, n = evaluationResult[["n"]], fk = evaluationResult[["k"]], k = errors, cilow = intervalTable[1], cihigh = intervalTable[2])
      if(options[["monetaryVariable"]] != "")
        row <- cbind(row, projectedlow = projectedMisstatement[1], projectedhigh = projectedMisstatement[2])
    }
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    if(options[["bayesFactor"]])
      row <- cbind(row, bf = .BFsamples(options, evaluationResult, jaspResults))
    evaluationTable$addRows(row)
}

.priorAndPosteriorFromSamples <- function(options, evaluationResult, jaspResults, plotWidth = 600, plotHeight = 450){

  prior <- density(evaluationResult[["prior"]], from = 0, to = options[["priorAndPosteriorPlotLimit"]], n = 2^10)
  posterior <- density(evaluationResult[["posterior"]], from = 0, to = options[["priorAndPosteriorPlotLimit"]], n = 2^10)

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

  if(options[["priorAndPosteriorPlotAdditionalInfo"]]){
    pdata <- data.frame(x = 0, y = 0, l = "1")
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
    p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \nconfidence region"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)
    if(!options[["displayCredibleInterval"]]){
        p <- p + ggplot2::geom_area(mapping = ggplot2::aes(x = x, y = y), data = subset(subset(d, d$type == "Posterior"), subset(d, d$type == "Posterior")$x <= evaluationResult[["bound"]]), fill = rgb(0, 0.25, 1, .5))
    } else {
      subset1 <- subset(subset(d, d$type == "Posterior"), subset(d, d$type == "Posterior")$x <= evaluationResult[["interval"]][2])
      subset2 <- subset(subset1, subset1$x >= evaluationResult[["interval"]][1])
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

.regressionBoundBayesian <- function(dataset, options, total_data_value, jaspResults){
  if(!is.null(jaspResults[["evaluationResult"]]$object)) 
    return(jaspResults[["evaluationResult"]]$object)

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."
    mle                     <- 0

    if(options[["auditResult"]] != "" && options[["sampleFilter"]] != "" && options[["monetaryVariable"]] != ""){
        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        zplus                   <- sort(subset(z, z > 0), decreasing = TRUE)
        M                       <- length(which(t != 0))

        B                       <- total_data_value
        N                       <- jaspResults[["N"]]$object
        b                       <- sample[, .v(options[["monetaryVariable"]])]
        w                       <- sample[, .v(options[["auditResult"]])]
        
        colnames(sample)        <- c("bookValue", "auditValue")
        formula                 <- auditValue ~ bookValue
        basResult               <- BAS::bas.lm(formula = formula, data = sample)
        b1                      <- coef(basResult)$postmean[2]

        meanb                   <- mean(b)
        meanw                   <- mean(w)

        mle                     <- N * meanw + b1 * (B - N * meanb)
        stand.dev               <- sd(w) * sqrt(1 - cor(b, w)^2) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) )
        upperValue              <- mle + qt(p = 1 - alpha, df = n - 1) * stand.dev
        if(upperValue == 0){
          bound                 <- 0
        } else {
          bound                 <- (upperValue - B) / B
        }
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
    resultList[["mle"]]         <- mle

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOnOptions(c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materiality", "estimator", "monetaryVariable"))
    return(jaspResults[["evaluationResult"]]$object)
}
