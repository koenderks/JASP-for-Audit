.calculateBayesianSampleSize <- function(options, alpha){
    for(n in 1:5000){
      impk <- base::switch(options[["expected.errors"]],
                            "kPercentage" = n * options[["kPercentageNumber"]],
                            "kNumber" = options[["kNumberNumber"]])
        if(impk >= n){ next }
        x                     <- qbeta(p = 1 - alpha, shape1 = 1 + impk, shape2 = 1 + (n - impk))
        if(x < options[["materiality"]]){
            return(n)
        }
    }
}

.dBetaBinom <- function (x, N, u, v, log = FALSE)
{
    logval <- lbeta(x + u, N - x + v) - lbeta(u, v) + lchoose(N, x)
    if (log) {
        ret <- logval
    }
    else {
        ret <- exp(logval)
    }
    return(ret)
}

.qBetaBinom <- function (p, N, u, v)
{
    pp <- cumsum(.dBetaBinom(0:N, N, u, v))
    return(sapply(p, function(x) sum(pp < x)))
}

.calculateBayesianSampleSizeBetaBinom <- function(options, alpha, N){
    for(n in 1:5000){
      impk <- base::switch(options[["expected.errors"]],
                            "kPercentage" = n * options[["kPercentageNumber"]],
                            "kNumber" = options[["kNumberNumber"]])
        if(impk >= n){ next }
        x                     <- .qBetaBinom(p = 1 - alpha, N = N, u = 1 + impk, v = 1 + (n - impk))
        if((x / N) < options[["materiality"]]){
            return(n)
        }
    }
}

.bayesianAttributesPlanningFullAudit <- function(options, jaspResults){

    if(!is.null(jaspResults[["planningResult"]])) return()

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.30, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.30, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    if(options[["distribution"]] == "binomial"){
      n_noprior               <- .calculateBayesianSampleSize(options, 1 - options[["confidence"]])
      n_withprior             <- .calculateBayesianSampleSize(options, alpha)
    } else if(options[["distribution"]] == "hypergeometric"){
      n_noprior               <- .calculateBayesianSampleSizeBetaBinom(options, 1 - options[["confidence"]], options[["N"]])
      n_withprior             <- .calculateBayesianSampleSizeBetaBinom(options, alpha, options[["N"]])
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

    if(options[["prior"]] == "ARM"){
      priorA                  <- 1 + pk
      priorB                  <- 1 + (pn - pk)
    } else if(options[["prior"]] == "5050"){
      priorA                <- 1
      priorB                <- 1/((3/2) * options[["materiality"]]) - (1/3)
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
                                                      "kNumberNumber", "prior", "distribution", "N"))

}

.bayesianAttributesPlanningTableFullAudit <- function(options, result, jaspResults, position = 1){

  if(!is.null(jaspResults[["summaryTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  summaryTable                        <- createJaspTable("Bayesian Attributes Planning Table")
  jaspResults[["summaryTable"]]       <- summaryTable
  summaryTable$position               <- position
  summaryTable$dependOnOptions(c("IR", "CR", "confidence", "expected.errors", "materiality", "show", "N",
                                  "kPercentageNumber", "kNumberNumber", "expectedBF", "prior", "distribution"))

  summaryTable$addColumnInfo(name = 'IR',     title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',     title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'SR',     title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',      title = "Allowed errors",        type = 'string')
  summaryTable$addColumnInfo(name = 'n',      title = "Required sample size",  type = 'string')
  if(options[["expectedBF"]])
    summaryTable$addColumnInfo(name = 'expBF', title = "Expected BF\u208B\u208A", type = 'string')

  message <- base::switch(options[["distribution"]],
                            "binomial" = "The sample size is calculated using the <b>beta</b> distribution.",
                            "hypergeometric" = paste0("The sample size is calculated using the <b>beta-binomial</b> distribution (N = ", options[["N"]] ,")."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  ktable <- base::switch(options[["expected.errors"]],
                          "kPercentage" = floor(result[["k"]] * result[["n"]]),
                          "kNumber" = options[["kNumberNumber"]])
  SRtable <- base::switch(options[["show"]],
                            "percentage" = paste0(round(result[["alpha"]], 3) * 100, "%"),
                            "proportion" = round(result[["alpha"]], 3))

  row <- data.frame(IR = result[["IR"]], CR = result[["CR"]], SR = SRtable, k = ktable, n = result[["n"]])
  if(options[["expectedBF"]])
    row <- cbind(row, expBF = .expectedBF(options, result, ktable))
  summaryTable$addRows(row)
}

.priorSampleTable <- function(options, result, jaspResults, position = 3){

  if(!is.null(jaspResults[["sampletable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  sampletable                       <- createJaspTable("Prior Sample Table")
  jaspResults[["sampletable"]]      <- sampletable
  sampletable$position              <- position
  sampletable$dependOnOptions(c("IR", "CR", "confidence", "materiality", "expected.errors", "implicitsample", "statistic",
                                "show", "kPercentageNumber", "kNumberNumber", "distribution", "prior", "N"))

  sampletable$addColumnInfo(name = 'implicitn', title = "Prior sample size", type = 'string')
  sampletable$addColumnInfo(name = 'implicitk', title = "Prior errors", type = 'string')
  if(options[["statistic"]] == "bound"){
    sampletable$addColumnInfo(name = 'priorbound', title = paste0(result[["confidence"]]*100,"% Prior confidence bound"), type = 'string')
  } else {
    sampletable$addColumnInfo(name = 'ciLow', title = "Lower", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Prior confidence interval"))
    sampletable$addColumnInfo(name = 'ciHigh', title = "Upper", type = "string", overtitle = paste0(result[["confidence"]]*100,"% Prior confidence interval"))
  }

  message <- paste0("Sample sizes shown are implicit sample sizes derived from the ARM risk assessments: IR = <b>", options[["IR"]], "</b> and CR = <b>", options[["CR"]], "</b>.")
  sampletable$addFootnote(message = message, symbol="<i>Note.</i>")

  implicitn <- round(result[["implicitn"]], 2)
  implicitk <- round(result[["implicitk"]], 2)

  priorBound <- base::switch(options[["statistic"]],
                              "bound" = round(qbeta(p = options[["confidence"]], shape1 = result[["priorA"]], shape2 = result[["priorB"]]), 3),
                              "interval" = round(qbeta(p = c(  (1 - (1-(1-options[["confidence"]])/2)) , (1 - ((1-options[["confidence"]])/2)) ), shape1 = result[["priorA"]], shape2 = result[["priorB"]]), 3))
  if(options[["show"]] == "percentage")
    priorBound <- paste0(priorBound * 100, "%")
  if(options[["statistic"]] == "bound"){
      row <- data.frame(implicitn = implicitn, implicitk = implicitk, priorbound = priorBound)
  } else {
    row <- data.frame(implicitn = implicitn, implicitk = implicitk, ciLow = priorBound[1], ciHigh = priorBound[2])
  }
  sampletable$addRows(row)
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
      ggplot2::scale_linetype_manual(values=c("dotted", "dashed"), guide = FALSE)

  if(options[["show"]] == "percentage"){
      p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))
  } else if(options[["show"]] == "proportion"){
      p <- p + ggplot2::scale_x_continuous(name = "Error proportion", breaks = xBreaks, limits = xLim)
  }

  if(options[["plotPriorAdditionalInfo"]]){
      pdata <- data.frame(x = 0, y = 0, l = "1")
      p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
      p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Prior confidence region"))
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 20, shape = 21, fill = rgb(0, 1, 0.5, .7), stroke = 2, color = "black")))

        if(options[["statistic"]] == "bound"){
          p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
                                          xlim = c(0, qbeta(options[["confidence"]], result[["priorA"]], result[["priorB"]])),
                                          geom = "area", fill = rgb(0, 1, 0.5, .7))
        } else if(options[["statistic"]] == "interval"){
          p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = result[["priorA"]], shape2 = result[["priorB"]]),
                                          xlim = c(qbeta((1 - (1-(1-options[["confidence"]])/2)), result[["priorA"]], result[["priorB"]]), qbeta((1 - ((1-options[["confidence"]])/2)), result[["priorA"]], result[["priorB"]])),
                                          geom = "area", fill = rgb(0, 1, 0.5, .7))
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

  return(createJaspPlot(plot = p, title = "Implied Prior and Posterior", width = plotWidth, height = plotHeight))

}

.bayesianAttributesBoundFullAudit <- function(dataset, options, jaspResults){

    ar                        <- 1 - options[["confidence"]]
    ir                        <- base::switch(options[["IR"]], "Low" = 0.30, "Medium" = 0.60, "High" = 1)
    cr                        <- base::switch(options[["CR"]], "Low" = 0.30, "Medium" = 0.60, "High" = 1)
    alpha                     <- ar / ir / cr

    if(options[["distribution"]] == "binomial"){
      n_noprior               <- .calculateBayesianSampleSize(options, 1 - options[["confidence"]])
      n_withprior             <- .calculateBayesianSampleSize(options, alpha)
    } else if(options[["distribution"]] == "hypergeometric"){
      n_noprior               <- .calculateBayesianSampleSizeBetaBinom(options, 1 - options[["confidence"]], options[["N"]])
      n_withprior             <- .calculateBayesianSampleSizeBetaBinom(options, alpha, options[["N"]])
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

    if(options[["prior"]] == "ARM"){
      priorA                  <- 1 + pk
      priorB                  <- 1 + (pn - pk)
    } else if(options[["prior"]] == "5050"){
      priorA                  <- 1
      priorB                  <- 1/((3/2) * options[["materiality"]]) - (1/3)
    }

    bound <- "."
    if(n != 0 && k <= n){
      if(options[["statistic"]] == "bound"){
          bound             <- qbeta(p = options[["confidence"]], shape1 = priorA + k, shape2 = priorB + (n - k), lower.tail = TRUE)
       } else if(options[["statistic"]] =="interval"){
          bound             <- qbeta(p = c(  (1 - (1-(1-options[["confidence"]])/2)) , (1 - ((1-options[["confidence"]])/2)) ),
                                    shape1 = priorA + k, shape2 = priorB + (n - k), lower.tail = TRUE)
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
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["posteriorA"]]  <- priorA + k
    resultList[["posteriorB"]]  <- priorB + (n - k)

    jaspResults[["result"]]     <- createJaspState(resultList)
    jaspResults[["result"]]     $dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality",
                                                    "correctID", "expected.errors", "kPercentageNumber",
                                                    "kNumberNumber", "sampleFilter", "prior", "k", "n",
                                                    "distribution", "N"))
}

.bayesianAttributesBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Bayesian Attributes Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$position              <- position
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "expected.errors", "kPercentageNumber", "kNumberNumber", "sampleFilter",
                                      "mostLikelyError", "bayesFactor", "N", "n", "k", "prior", "distribution", "prior"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",        type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",        type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Full errors",        type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A",     type = 'string')

    mle <- ceiling( (result[["posteriorA"]] - 1) / (result[["posteriorA"]] + result[["posteriorB"]] - 2) * options[["N"]] )

    if(options[["correctID"]] == ""){
      row                   <- data.frame(materiality = ".", n = ".", k = ".", bound = ".")
      if(options[["mostLikelyError"]])
        row                 <- cbind(row, mle = ".")
      if(options[["bayesFactor"]])
        row                 <- cbind(row, bf = ".")
      evaluationTable$addRows(row)
      return()
    }

    materialityTable        <- round(options[["materiality"]], 2)
    if(options[["show"]] == "percentage")
      materialityTable      <- paste0(materialityTable * 100, "%")

    boundTable <- result[["bound"]]
    if(!"." %in% boundTable){
      boundTable            <- round(result[["bound"]],3)
      if(options[["show"]] == "percentage")
        boundTable          <- paste0(boundTable * 100, "%")
    }

    row                     <- data.frame(materiality = materialityTable, n = result[["n"]], k = result[["k"]], bound = boundTable)
    if(options[["mostLikelyError"]])
      row                   <- cbind(row, mle = mle)
    if(options[["bayesFactor"]])
      row                   <- cbind(row, bf = .BF(options, result))
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

  return(createJaspPlot(plot = p, title = "Prior and Posterior Plot", width = plotWidth, height = plotHeight))

}

.expectedBF <- function(options, result, ktable){
    priorOdds       <- diff(pbeta(c(0, options[["materiality"]]), result[["priorA"]], result[["priorB"]])) / diff(pbeta(c(options[["materiality"]], 1), result[["priorA"]], result[["priorB"]]))
    posteriorOdds   <- diff(pbeta(c(0, options[["materiality"]]), result[["priorA"]] + ktable, result[["priorB"]] + (result[["n"]] + ktable))) / diff(pbeta(c(options[["materiality"]], 1), result[["priorA"]] + ktable, result[["priorB"]] + (result[["n"]] + ktable)))
    BF              <- round(posteriorOdds / priorOdds, 2)
    return(BF)
}

.BF <- function(options, result){
  priorOdds         <- diff(pbeta(c(0, options[["materiality"]]), result[["priorA"]], result[["priorB"]])) / diff(pbeta(c(options[["materiality"]], 1), result[["priorA"]], result[["priorB"]]))
  posteriorOdds     <- diff(pbeta(c(0, options[["materiality"]]), result[["posteriorA"]], result[["posteriorB"]])) / diff(pbeta(c(options[["materiality"]], 1), result[["posteriorA"]], result[["posteriorB"]]))
  BF                <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

.BFsamples <- function(options, result){
  densprior         <- density(result[["prior"]])
  priorCDF          <- approxfun(densprior$x, densprior$y, yleft=0, yright=0)
  priorLeft         <- integrate(priorCDF, lower = 0, upper = options[["materiality"]])$value
  priorRight        <- integrate(priorCDF, lower = options[["materiality"]], upper = 1)$value
  priorOdds         <- priorLeft / priorRight
  densposterior     <- density(result[["posterior"]])
  posteriorCDF      <- approxfun(densposterior$x, densposterior$y, yleft=0, yright=0)
  posteriorLeft     <- integrate(posteriorCDF, lower = 0, upper = options[["materiality"]])$value
  posteriorRight    <- integrate(posteriorCDF, lower = options[["materiality"]], upper = 1)$value
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
    bound                     <- NULL
    prior                     <- NULL
    posterior                 <- NULL
    alpha                     <- 1 - options[["confidence"]]
    if(options[["correctMUS"]] != "" && options[["monetaryVariableMUS"]] != ""){
      sample                  <- dataset[, c(.v(options[["monetaryVariableMUS"]]), .v(options[["correctMUS"]]))]
      n                       <- nrow(sample)
      t                       <- sample[, .v(options[["monetaryVariableMUS"]])] - sample[, .v(options[["correctMUS"]])]
      z                       <- t / sample[, .v(options[["monetaryVariableMUS"]])]
      z                       <- subset(z, z > 0)
      M                       <- length(z)
      z_bar                   <- mean(z)
      if(M == 0)
          z_bar               <- 0

      prior_part_1            <- (0 + priorA) / (0 + priorB)
      prior_part_2            <- ((priorMu * (priorB - 1)) + (0 * 0)) / (0 + (priorA / priorPi))
      prior                   <- prior_part_1 * prior_part_2 * stats::rf(n = 1e5, df1 = (2 * (0 + priorA)), df2 = ( 2 *(0 + priorB)))

      posterior_part_1        <- (M + priorA) / (M + priorB)
      posterior_part_2        <- ((priorMu * (priorB - 1)) + (M * z_bar)) / (n + (priorA / priorPi))
      posterior               <- posterior_part_1 * posterior_part_2 * stats::rf(n = 1e5, df1 = (2 * (M + priorA)), df2 = ( 2 *(M + priorB)))

      bound <- base::switch(options[["statistic"]],
                            "bound" = as.numeric(quantile(posterior, probs = (1 - alpha), na.rm = TRUE)),
                            "interval" = as.numeric(quantile(posterior, probs = c(  (1 - (1-(1-alpha)/2)) , (1 - ((1-alpha)/2)) ), na.rm = TRUE)))
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
    resultList[["priorA"]]      <- priorA
    resultList[["priorB"]]      <- priorB
    resultList[["priorPi"]]     <- priorPi
    resultList[["priorMu"]]     <- priorMu
    resultList[["posteriorA"]]  <- priorA + sum(z)
    resultList[["posteriorB"]]  <- priorB + (n - (sum(z)))
    resultList[["prior"]]       <- prior
    resultList[["posterior"]]   <- posterior

    jaspResults[["result"]] <- createJaspState(resultList)
    jaspResults[["result"]]$dependOnOptions(c("IR", "CR", "confidence", "correctMUS", "sampleFilterMUS",
                                              "auditType", "boundMethodMUS", "monetaryVariableMUS"))
}

.bayesianMusBoundTableFullAudit <- function(options, result, jaspResults, position = 1){

    if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Bayesian MUS Evaluation Table")
    jaspResults[["evaluationTable"]]      <- evaluationTable
    evaluationTable$dependOnOptions(c("IR", "CR", "confidence", "statistic", "materiality", "show", "correctID",
                                      "sampleFilter", "distribution", "mostLikelyError", "N", "correctMUS", "sampleFilterMUS",
                                      "boundMethodMUS", "bayesFactor"))
    evaluationTable$position <- position

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",    type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",    type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",         type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Sum of fractional errors",         type = 'string')

    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(result[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",            type = 'string')
    if(options[["bayesFactor"]])
      evaluationTable$addColumnInfo(name = 'bf',          title = "BF\u208B\u208A", type = 'string')

    message <- base::switch(options[["boundMethodMUS"]],
                                      "coxAndSnellBound" = "The confidence bound is calculated according to the <b>Cox and Snell</b> method.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    if(options[["correctMUS"]] == "" || options[["sampleFilterMUS"]] == ""){
      row                   <- data.frame(materiality = ".", n = ".", k = ".", bound = ".")
      if(options[["mostLikelyError"]])
        row                 <- cbind(row, mle = ".")
      if(options[["bayesFactor"]])
        row                 <- cbind(row, bf = ".")
      evaluationTable$addRows(row)
      return()
    }

    errors                  <- round(sum(result[["z"]]), 2)
    mle                     <- 0
    if(options[["N"]] != 0)
      mle <- floor( sum(result[["z"]]) / result[["n"]] * options[["N"]] )

    materialityTable        <- round(options[["materiality"]], 2)
    if(options[["show"]] == "percentage")
      materialityTable      <- paste0(materialityTable * 100, "%")

    boundTable <- result[["bound"]]
    if(!"." %in% boundTable){
      boundTable            <- round(result[["bound"]],3)
      if(options[["show"]] == "percentage")
        boundTable          <- paste0(boundTable * 100, "%")
    }

    row <- data.frame(materiality = materialityTable, n = result[["n"]], fk = result[["k"]], k = errors, bound = boundTable)
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    if(options[["bayesFactor"]])
      row <- cbind(row, bf = .BFsamples(options, result))
    evaluationTable$addRows(row)
}

.plotPriorAndPosteriorBayesianMUSBoundFullAudit <- function(options, result, jaspResults, plotWidth = 600, plotHeight = 450){

  prior <- density(result[["prior"]], from = 0, to = options[["limx_backup"]])
  posterior <- density(result[["posterior"]], from = 0, to = options[["limx_backup"]])

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
  posteriorPointDataY <- posteriorPointData$y[which.min(abs(posteriorPointData$x - options[["materiality"]]))]

  pointdata <- data.frame(x = options[["materiality"]], y = posteriorPointDataY)

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
      p <- p + ggplot2::geom_area(mapping = ggplot2::aes(x = x, y = y), data = subset(subset(d, d$type == "Posterior"), subset(d, d$type == "Posterior")$x <= result[["bound"]]), fill = rgb(0, 0.25, 1, .5))
    } else if(options[["statistic"]] == "interval") {
      p <- p + ggplot2::geom_area(mapping = ggplot2::aes(x = x, y = y), data = subset(subset(d, d$type == "Posterior"), subset(d, d$type == "Posterior")$x >= result[["bound"]][1] && subset(d, d$type == "Posterior")$x <= result[["bound"]][2]), fill = rgb(0, 0.25, 1, .5))
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
