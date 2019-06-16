.calc.n.beta <- function(options, alpha, jaspResults){
    jaspResults$startProgressbar(5000)
    for(n in 1:5000){
      jaspResults$progressbarTick()
      impk <- base::switch(options[["expectedErrors"]], "expectedRelative" = n * options[["expectedPercentage"]], "expectedAbsolute" = (options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n))
      if(impk >= n){ next }
      x <- qbeta(p = 1 - alpha, shape1 = 1 + impk, shape2 = 1 + (n - impk))
      if(x < jaspResults[["materiality"]]$object){
        return(n)
      }
    }
    jaspResults[["errorInSampler"]] <- createJaspState(TRUE)
    jaspResults[["errorInSampler"]]$dependOn(options = c("materiality", "materialityPercentage", "materialityValue", "expectedNumber", "expectedErrors", "populationValue", "populationSize"))
    return(1)
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
    jaspResults$startProgressbar(5000)
    N <- jaspResults[["N"]]$object
    for(n in 1:5000){
      jaspResults$progressbarTick()
      impk <- base::switch(options[["expectedErrors"]], "expectedRelative" = n * options[["expectedPercentage"]], "expectedAbsolute" = (options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n))
      if(impk >= n){ next }
      x <- .qBetaBinom(p = 1 - alpha, N = N, u = 1 + impk, v = 1 + (n - impk)) / N
      if(x < jaspResults[["materiality"]]$object){
        return(n)
      }
    }
    jaspResults[["errorInSampler"]] <- createJaspState(TRUE)
    jaspResults[["errorInSampler"]]$dependOn(options = c("materiality", "materialityPercentage", "materialityValue", "expectedNumber", "expectedErrors", "populationValue", "populationSize"))
    return(1)
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
      k                       <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object)
      if(pn != 0){
          if(options[["expectedErrors"]] == "expectedRelative"){
              k               <- options[["expectedPercentage"]]
              pk              <- pn * k
          } else if(options[["expectedErrors"]] == "expectedAbsolute"){
              k               <- options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object
              pk              <- pn * k
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
    jaspResults[["planningResult"]]$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "expectedPercentage",
                                                      "expectedNumber", "planningModel", "materialityValue", "recordNumberVariable", "materiality", "monetaryVariable"))
    return(jaspResults[["planningResult"]]$object)
}

.bayesianPlanningTable <- function(dataset, options, planningResult, jaspResults, position = 1){

  if(!is.null(jaspResults[["planningContainer"]][["planningSummary"]])) return() #The options for this table didn't change so we don't need to rebuild it

  planningSummary                                             <- createJaspTable("Planning summary")
  jaspResults[["planningContainer"]][["planningSummary"]]        <- planningSummary
  planningSummary$position                                       <- position
  planningSummary$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "expectedPercentage", "expectedNumber", "expectedBF",
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
                            "beta" = paste0("The required sample size is based on the <b>beta</b> distribution <i>(\u03B1 = ", round(planningResult[["priorA"]], 2) ,", \u03B2 = ", round(planningResult[["priorB"]], 2), ")</i>."),
                            "beta-binomial" = paste0("The required sample size is based on the <b>beta-binomial</b> distribution <i>(N = ", jaspResults[["N"]]$object ,", \u03B1 = ", round(planningResult[["priorA"]], 2) , ", \u03B2 = ", round(planningResult[["priorB"]], 2), ")</i>."))
  planningSummary$addFootnote(message = message, symbol="<i>Note.</i>")

  if(!is.null(jaspResults[["errorInSampler"]])){
    planningSummary$setError("There is no sample size (< 5000) large enough to prove the current materiality. Please try other values.")
    return()
  }

  ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(planningResult[["k"]] * planningResult[["n"]], 2), "expectedAbsolute" = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * planningResult[["n"]], 2))
  DRtable <- paste0(round(planningResult[["alpha"]], 3) * 100, "%")

  if(jaspResults[["materiality"]]$object == 0){
    row <- data.frame(materiality = ".", IR = planningResult[["IR"]], CR = planningResult[["CR"]], DR = DRtable, k = 0, n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    planningSummary$addRows(row)
    return()
  }

  materialityTitle  <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue  <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * sum(dataset[, .v(options[["monetaryVariable"]])])), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, options[["materialityValue"]]))
  materiality       <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = materialityValue)
  kTitle            <- base::switch(options[["expectedErrors"]], "expectedRelative" = ktable, "expectedAbsolute" = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))

  if(!jaspResults[["ready"]]$object){
    row <- data.frame(materiality = materiality, IR = planningResult[["IR"]],CR = planningResult[["CR"]], DR = DRtable, k = kTitle, n = ".")
    if(options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    planningSummary$addRows(row)
    return()
  }

  row <- data.frame(materiality = materiality, IR  = planningResult[["IR"]], CR = planningResult[["CR"]], DR = DRtable, k = kTitle, n = planningResult[["n"]])
  if(options[["expectedBF"]])
    row <- cbind(row, expBF = .expectedBF(options, planningResult, ktable, jaspResults))
  planningSummary$addRows(row)
}

.implicitSampleTable <- function(options, result, jaspResults, position = 3){

  if(!is.null(jaspResults[["planningContainer"]][["sampletable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if(options[["implicitSampleTable"]]){

  sampletable                       <- createJaspTable("Implicit sample")
  jaspResults[["planningContainer"]][["sampletable"]]      <- sampletable
  sampletable$position              <- position
  sampletable$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "implicitSampleTable", "expectedPercentage", "expectedNumber",
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
      k   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = round(options[["expectedPercentage"]] * planningResult[["n"]], 2), no = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * planningResult[["n"]], 2))
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
        k   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = round(options[["expectedPercentage"]] * planningResult[["n"]], 2), no = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * planningResult[["n"]], 2))

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
  return(createJaspPlot(plot = p, title = "Implied prior from risk assessments", width = plotWidth, height = plotHeight))
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
    exp.k                     <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object)
    if(pn != 0){
        if(options[["expectedErrors"]] == "expectedRelative"){
            exp.k             <- options[["expectedPercentage"]]
            pk                <- pn * exp.k
        } else {
            exp.k             <- options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object
            pk                <- exp.k
        }
    }

    n                         <- 0
    k                         <- 0
    if(options[["auditResult"]] != ""){
      n                     <- jaspResults[["sampleSize"]]$object
      kIndex                <- which(dataset[,.v(options[["auditResult"]])] == 1)
      kNumber               <- dataset[kIndex ,.v(options[["sampleFilter"]])]
      k                     <- length(rep(kIndex, times = kNumber))
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
    jaspResults[["evaluationResult"]]     $dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "expectedErrors", "expectedPercentage", "expectedNumber", "sampleFilter",
                                                    "planningModel", "materialityValue", "variableType", "materiality"))
    return(jaspResults[["evaluationResult"]]$object)
}

.bayesianAttributesBoundTable <- function(options, evaluationResult, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "expectedErrors", "expectedPercentage", "expectedNumber",
                                      "sampleFilter", "mostLikelyError", "bayesFactor", "planningModel", "materialityValue", "variableType", "estimator", "areaUnderPosterior", "valuta"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",        type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",        type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Full errors",        type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                type = 'string')
    if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
      evaluationTable$addColumnInfo(name = 'bound',         title = paste0(options[["confidence"]] * 100,"% Confidence bound"), type = 'string')
      if(options[["monetaryVariable"]] != "")
          evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    } else {
      evaluationTable$addColumnInfo(name = 'cilow',          title = "Lower", type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      evaluationTable$addColumnInfo(name = 'cihigh',         title = "Upper", type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      if(options[["monetaryVariable"]] != ""){
        evaluationTable$addColumnInfo(name = 'projectedlow',         title = "Lower",           type = 'string', overtitle = "Projected misstatement")
        evaluationTable$addColumnInfo(name = 'projectedhigh',         title = "Upper",           type = 'string', overtitle = "Projected misstatement")
      }
    }
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A",     type = 'string')

    message <- base::switch(options[["estimator"]],
                              "betaBound" = "The confidence bound is calculated according to the <b>beta</b> distribution.",
                              "betabinomialBound" = "The confidence bound is calculated according to the <b>beta-binomial</b> distribution.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    mle <- 0
    if(jaspResults[["N"]]$object != 0)
      mle <- paste0(round((evaluationResult[["posteriorA"]] - 1) / (evaluationResult[["posteriorA"]] + evaluationResult[["posteriorB"]] - 2), 4) * 100, "%")

    if(options[["auditResult"]] == ""){
      row                   <- data.frame(materiality = ".", n = ".", k = ".")
      if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
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

    materialityTable <- ifelse(options[["materiality"]] == "materialityRelative", yes = paste0(round(jaspResults[["materiality"]]$object, 2) * 100, "%"), no = paste(jaspResults[["valutaTitle"]], options[["materialityValue"]]))

    if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
      boundTable <- evaluationResult[["bound"]]
      projectedMisstatement <- "."
      if(!"." %in% boundTable){
        boundTable            <- round(evaluationResult[["bound"]], 4)
        projectedMisstatement <- paste(jaspResults[["valutaTitle"]]$object, ceiling(evaluationResult[["bound"]] * jaspResults[["total_data_value"]]$object))
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
        projectedMisstatement     <- paste(jaspResults[["valutaTitle"]]$object, ceiling(evaluationResult[["interval"]] * jaspResults[["total_data_value"]]$object))
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

      if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
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
          if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
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
  return(createJaspPlot(plot = p, title = "Prior and posterior plot", width = plotWidth, height = plotHeight))
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

.coxAndSnellBound <- function(dataset, options, jaspResults, priorA = 0, priorB = 0){
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
    priorPi                   <- priorA / (priorA + priorB)
    a                         <- 1
    b                         <- 3
    priorMu                   <- 1 # prior mean taint is set to 1 when planning conservatively
    

    if(jaspResults[["runEvaluation"]]$object){

      sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
      t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
      z                       <- t / sample[, .v(options[["monetaryVariable"]])]
      z                       <- rep(z, times = dataset[ ,.v(options[["sampleFilter"]])])
      n                       <- length(z)
      z                       <- subset(z, z > 0)
      M                       <- length(z)
      z_bar                   <- mean(z)
      if(M == 0)
          z_bar               <- 0

      set.seed(options[["seed"]])
      prior                   <- rbeta(n = 1e6, shape1 = priorA, shape2 = priorB) 

      posterior_part_1        <- (M + a) / (M + b)
      posterior_part_2        <- ((priorMu * (b - 1)) + (M * z_bar)) / (n + (a / priorPi))
      posterior               <- posterior_part_1 * posterior_part_2 * stats::rf(n = 1e6, df1 = (2 * (M + a)), df2 = ( 2 *(M + b)))

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
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materiality", "estimator", "monetaryVariable"))
    return(jaspResults[["evaluationResult"]]$object)
}

.bayesianAuditValueBoundTable <- function(options, evaluationResult, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationContainer"]][["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation summary")
    jaspResults[["evaluationContainer"]][["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "sampleFilter", "planningModel", "mostLikelyError", "estimator", "bayesFactor",
                                        "materialityValue", "variableType", "areaUnderPosterior", "valuta"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",            type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",            type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",                 type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Total tainting",         type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                    type = 'string')

    if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
      evaluationTable$addColumnInfo(name = 'bound',         title = paste0(options[["confidence"]] * 100,"% Credible bound"), type = 'string')
      if(options[["monetaryVariable"]] != "")
          evaluationTable$addColumnInfo(name = 'projm',         title = "Projected Misstatement",           type = 'string')
    } else {
      evaluationTable$addColumnInfo(name = 'cilow',          title = "Lower", type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      evaluationTable$addColumnInfo(name = 'cihigh',         title = "Upper", type = 'string', overtitle = paste0(options[["confidence"]] * 100,"% Credible interval"))
      if(options[["monetaryVariable"]] != ""){
        evaluationTable$addColumnInfo(name = 'projectedlow',         title = "Lower",           type = 'string', overtitle = "Projected misstatement")
        evaluationTable$addColumnInfo(name = 'projectedhigh',         title = "Upper",           type = 'string', overtitle = "Projected misstatement")
      }
    }

    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A",         type = 'string')

    area <- ifelse(options[["areaUnderPosterior"]]=="displayCredibleBound", yes = "bound", no = "interval")
    message <- base::switch(options[["estimator"]],
                                      "coxAndSnellBound" = paste0("The credible ", area ," is calculated according to the <b>Cox and Snell</b> method."),
                                      "regressionBound" = paste0("The credible ", area ," is calculated according to the <b>Regression</b> method."))
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    materialityTable <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = paste(jaspResults[["valutaTitle"]]$object, options[["materialityValue"]]), no = paste0(round(options[["materialityPercentage"]] * 100, 2) , "%"))

    # Return empty table with materiality
    if(!jaspResults[["runEvaluation"]]$object){
      row <- data.frame(materiality = materialityTable, n = ".", fk = ".", k = ".")
      if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
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

    total_data_value        <- jaspResults[["total_data_value"]]$object

    errors                  <- round(sum(evaluationResult[["z"]]), 2)
    mle                     <- 0

    if(options[["estimator"]] == "coxAndSnellBound"){
        mle <- paste0(round(sum(evaluationResult[["z"]]) / evaluationResult[["n"]], 4) * 100, "%")
    } else if(options[["estimator"]] == "regressionBound"){
        mle <- round(evaluationResult[["mle"]], 2) # CHANGE
    }

    if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
      boundTable <- evaluationResult[["bound"]]
      projectedMisstatement <- "."
      if(!"." %in% boundTable){
        boundTable            <- round(evaluationResult[["bound"]], 4)
        projectedMisstatement <- paste(jaspResults[["valutaTitle"]]$object, ceiling(evaluationResult[["bound"]] * total_data_value))
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
        projectedMisstatement     <- paste(jaspResults[["valutaTitle"]]$object, ceiling(evaluationResult[["interval"]] * total_data_value))
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
    if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
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
  return(createJaspPlot(plot = p, title = "Prior and posterior plot", width = plotWidth, height = plotHeight))
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
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materiality", "estimator", "monetaryVariable"))
    return(jaspResults[["evaluationResult"]]$object)
}

.decisionAnalysisBayesian <- function(options, jaspResults){

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr
  
  n <- c(.calc.n.beta(options, alpha, jaspResults), .calc.n.betabinom(options, alpha, jaspResults))
  k <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(options[["expectedPercentage"]] * n, 2), "expectedAbsolute" = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n, 2))
  
  d <- data.frame(y = c(n, k), 
                  dist = rep(c("Beta", "Beta-binomial"), 2),
                  nature = rep(c("Expected error-free", "Expected errors"), each = 2))
  d$dist = factor(d$dist,levels(d$dist)[c(2,1)])
  d$nature = factor(d$nature,levels(d$nature)[c(1,2)])
  
  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = dist, y = y, fill = nature)) +
      ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::ylab("Sample size") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb")) +
      ggplot2::labs(fill = "") +
      ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30))) +
      ggplot2::annotate("text", y = k, x = c(2, 1), label = k, size = 6, vjust = 0.5, hjust = -0.3) + 
      ggplot2::annotate("text", y = n, x = c(2, 1), label = n, size = 6, vjust = 0.5, hjust = -0.5) + 
      ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1*max(n))), min.n = 4), limits = c(0, ceiling(1.1*max(n)))) +
      ggplot2::ylim(0, ceiling(1.2*max(n)))
  p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")

  optN <- base::switch(which.min(n), "1" = "beta", "2" = "beta-binomial")
  jaspResults[["mostEfficientPlanningDistribution"]] <- createJaspState(optN)
  jaspResults[["mostEfficientPlanningDistribution"]]$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", 
                                                                            "decisionPlot", "materialityValue"))
  
  return(createJaspPlot(plot = p, title = "Decision analysis", width = 600, height = 300))
}
