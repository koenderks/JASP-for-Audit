.auditRiskModel <- function(options, jaspResults){

  if(!is.null(jaspResults[["ARMcontainer"]]))
    return(jaspResults[["ARMcontainer"]]$object)

  jaspResults[["ARMcontainer"]] <- createJaspContainer(title= "<u>Audit Risk Model</u>")
  jaspResults[["ARMcontainer"]]$position <- 2
  jaspResults[["ARMcontainer"]]$dependOn(options = c("confidence", "IR", "CR", "materialityPercentage", "materialityValue", "materiality", "explanatoryText", "valuta"))

  #  Audit Risk Model formula
  .ARMformula(options, jaspResults, position = 2)
  DR                          <- jaspResults[["DR"]]$object
  
  if(!is.null(jaspResults[["ARMcontainer"]][["AuditRiskModelParagraph"]])){
    return()
  } else {
    if(options[["explanatoryText"]]){
      materialityLevelLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]], 10) * 100, "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
      auditRiskLabel        <- paste0(round((1 - options[["confidence"]]) * 100, 2), "%")
      dectectionRiskLabel   <- paste0(round(DR * 100, 2), "%")
  
      jaspResults[["ARMcontainer"]][["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>", options[["IR"]] ,"</b>. The internal control risk was determined
                                                                      to be <b>", options[["CR"]] ,"</b>. According to the Audit Risk Model, the required detection risk to maintain an audit risk of <b>", auditRiskLabel, "</b> for a materiality
                                                                      of <b>", materialityLevelLabel ,"</b> should be <b>", dectectionRiskLabel , "</b>. The translation of High, Medium and Low to probabilities is done according to <b>IODAD (2007)</b>."), "p")
      jaspResults[["ARMcontainer"]][["AuditRiskModelParagraph"]]$position <- 1
      jaspResults[["ARMcontainer"]][["AuditRiskModelParagraph"]]$dependOn(options = c("confidence", "IR", "CR", "materialityPercentage", "materialityValue", "valuta"))
    }
  }
}

.ARMformula <- function(options, jaspResults, position = 2){

    if(!is.null(jaspResults[["ARMcontainer"]][["ARMformula"]])) return()

    AR                      <- 1 - options[["confidence"]]
    IR                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    CR                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    DR                      <- AR / IR / CR

    jaspResults[["DR"]]     <- createJaspState(DR)
    jaspResults[["DR"]]     $dependOn(options = c("IR", "CR", "confidence"))

    text <- paste0("Audit risk (", round(AR * 100, 2),"%) = Inherent risk (", round(IR * 100, 2), "%) x Control risk (", round(CR * 100, 2), "%) x Detection risk (", round(DR * 100, 2), "%)")

    jaspResults[["ARMcontainer"]][["ARMformula"]] <- createJaspHtml(text, "h3")
    jaspResults[["ARMcontainer"]][["ARMformula"]]$position <- position
    jaspResults[["ARMcontainer"]][["ARMformula"]]$dependOn(options = c("IR", "CR", "confidence"))
}

.bookValueDescriptives <- function(dataset, options, jaspResults, position){

  if(!is.null(jaspResults[["procedureContainer"]][["bookValueDescriptives"]])) return() #The options for this table didn't change so we don't need to rebuild it

  dataTable                                                 <- createJaspTable("Book value descriptives")
  jaspResults[["procedureContainer"]][["bookValueDescriptives"]]        <- dataTable
  dataTable$position                                        <- position
  dataTable$dependOn(options = c("monetaryVariable", "recordNumberVariable", "bookValueDescriptives"))

  dataTable$addColumnInfo(name = 'popSize',     title = "Population size",        type = 'string')
  dataTable$addColumnInfo(name = 'value',       title = "Total value",            type = 'string')
  dataTable$addColumnInfo(name = 'mean',        title = "Mean",                   type = 'string')
  dataTable$addColumnInfo(name = 'sd',          title = "Std. deviation",         type = 'string')
  dataTable$addColumnInfo(name = 'p1',          title = "25%",                    type = 'string', overtitle = "Percentile")
  dataTable$addColumnInfo(name = 'p2',          title = "50%",                    type = 'string', overtitle = "Percentile")
  dataTable$addColumnInfo(name = 'p3',          title = "75%",                    type = 'string', overtitle = "Percentile")

  if(!jaspResults[["ready"]]$object){
    return()
  }

  popSize                           <- jaspResults[["N"]]$object
  values                            <- dataset[, .v(options[["monetaryVariable"]])]
  total.value                       <- paste(jaspResults[["valutaTitle"]]$object, round(sum(abs(values)), 2))
  mean.value                        <- paste(jaspResults[["valutaTitle"]]$object, round(mean(values), 2))
  sd.value                          <- paste(jaspResults[["valutaTitle"]]$object, round(sd(values), 2))
  Q                                 <- paste(jaspResults[["valutaTitle"]]$object, round(as.numeric(quantile(values, c(0.25, 0.50, 0.75))), 2))

  row <- data.frame(popSize = popSize, value = total.value, mean = mean.value, sd = sd.value, p1 = Q[1], p2 = Q[2], p3 = Q[3])
  dataTable$addRows(row)
}

.bookValueDistribution <- function(dataset, options, jaspResults){

    values <- dataset[, .v(options[["monetaryVariable"]])]

    meanx <- mean(values)
    sdx <- sd(values)
    q <- as.numeric(quantile(values, c(0.25, 0.5, 0.75)))
    minx <- min(q[1], meanx - sdx)
    maxx <- max(q[3], meanx + sdx)

    p <- .plotMarginalJfA(values, options[["monetaryVariable"]], jaspResults)

    p <- p + ggplot2::geom_point(ggplot2::aes(x = q[1], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = q[2], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = q[3], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx, y = 0), shape = 21, fill = "red", stroke = 2, size = 5)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx + sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx - sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)

    pdata <- data.frame(x = c(0,0,0), y = c(0,0,0), l = c("1","2","3"))
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0))) +
    ggplot2::scale_shape_manual(name = "", values = c(21,21,21), labels = c("Mean", "Mean \u00B1 sd", "Quartile")) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = c(5, 4, 3), shape = 21, fill = c("red","dodgerblue1", "orange"), stroke = 2, color = "black")), order = 1) +
    ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -10, r = 50))) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"))
    
    p <- JASPgraphs::themeJasp(p, legend.position = "top")

    return(createJaspPlot(plot = p, title = "Book value distribution", width = 600, height = 300))
}

.plotMarginalJfA <- function(column, variableName, jaspResults, rugs = FALSE, displayDensity = FALSE) {
  column <- as.numeric(column)
  variable <- na.omit(column)

  if(length(variable) == 0)
    return(NULL)

  h <- hist(variable, plot = FALSE)

  if (!displayDensity)
    yhigh <- max(h$counts)
  else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }

  ylow <- 0

  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)

  if (!displayDensity)
    p <-
      JASPgraphs::drawAxis(
        xName = paste0("Book values (", jaspResults[["valutaTitle"]]$object, ")"), yName = "Counts", xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  else
    p <-
      JASPgraphs::drawAxis(
        xName = variableName, yName = "Density", xBreaks = xticks,
        yBreaks = c(0,  1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )

  if (displayDensity)
    p <- p +
      ggplot2::geom_histogram(
        data = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill = "grey",
        col = "black",
        size = .7,
        center = ((h$breaks[2] - h$breaks[1])/2)
      ) +
      ggplot2::geom_line(
        data = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd = 1,
        col = "black"
      )
  else
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill     = "grey",
        col      = "black",
        size     = .7,
        center    = ((h$breaks[2] - h$breaks[1])/2)
      )

  # JASP theme
  p <- JASPgraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  # TODO: Fix jaspgraphs axis width X vs Y. See @vandenman.

  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.evaluationInformation <- function(options, evaluationResult, jaspResults){

  materiality       <- jaspResults[["materiality"]]$object
  bound             <- evaluationResult[["bound"]]
  proj.misstatement <- bound * jaspResults[["total_data_value"]]$object
  mle               <- ifelse(options[["variableType"]] == "variableTypeCorrect", yes = evaluationResult[["k"]] / evaluationResult[["n"]], no = sum(evaluationResult[["z"]]) / evaluationResult[["n"]])
  
  label             <- rev(c("Materiality", "Maximum error", "Most likely error"))
  values            <- rev(c(materiality, bound, mle))
  
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute")
    values          <- values * jaspResults[["total_data_value"]]$object
  
  boundColor        <- ifelse(bound < materiality, yes = rgb(0,1,.7,1), no = rgb(1,0,0,1))
  fillUp            <- rev(c("#1380A1", boundColor, "#1380A1"))
  yBreaks           <- as.numeric(JASPgraphs::getPrettyAxisBreaks(c(0, values), min.n = 4))
  
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute"){
    x.labels        <- format(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4), scientific = FALSE)
    values.labels   <- paste(jaspResults[["valutaTitle"]]$object, ceiling(values))
    x.title         <- ""
  } else {
    x.labels        <- paste0(round(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4) * 100, 4), "%")
    values.labels   <- paste0(round(values * 100, 2), "%")
    x.title         <- ""
  }

  tb                <- data.frame(x = label, values = values)
  tb$x              <- factor(tb$x, levels = tb$x)
  p                 <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
                        ggplot2::geom_bar(stat = "identity", col = "black", size = 1, fill = fillUp) +
                        ggplot2::coord_flip() +
                        ggplot2::xlab(NULL) +
                        ggplot2::ylab(x.title) +
                        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
                        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"))+
                        ggplot2::annotate("text", y = values, x = c(1, 2, 3), label = values.labels, size = 6, vjust = 0.5, hjust = -0.3) + 
                        ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), 0.01), min.n = 4), limits = c(0, 1.1*max(values)), labels = x.labels)
  p                 <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)
  return(createJaspPlot(plot = p, title = "Evaluation information", width = 600, height = 300))
}

.correlationPlot <- function(dataset, options, jaspResults) {

    d <- data.frame(xx= dataset[,.v(options[["monetaryVariable"]])], yy= dataset[,.v(options[["auditResult"]])])
    co <- cor(d$xx, d$yy)
    d <- na.omit(d)
    d <- ceiling(d)
    xVar <- d$xx
    yVar <- d$yy

    fit <- vector("list", 1)# vector("list", 4)
    fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), data = d)

    bestModel <- 1 # which.min(Bic)

    # format x labels
    xlow <- min(pretty(xVar))
    xhigh <- max(pretty(xVar))
    xticks <- pretty(c(xlow, xhigh))
    xLabs <- vector("character", length(xticks))
    xLabs <- format(xticks, digits= 3, scientific = FALSE)

    # Format y labels
    yticks <- xticks
    yLabs <- vector("character", length(yticks))
    yLabs <- format(yticks, digits= 3, scientific = FALSE)

    co <- round(co, 3)

    cols <- rep("gray", nrow(d))
    cols[which(d$xx != d$yy)] <- "red"

    p <- JASPgraphs::drawAxis(xName = paste0("Book values (", jaspResults[["valutaTitle"]]$object, ")"), yName = paste0("Audit values (", jaspResults[["valutaTitle"]]$object, ")"), xBreaks = xticks, yBreaks = yticks, yLabels = yLabs, xLabels = xLabs, force = TRUE)
    p <- JASPgraphs::drawPoints(p, dat = d, size = 3, fill = cols)
    p <- .poly.predJfA(fit[[bestModel]], plot = p, line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd = 1)
    p <- p + ggplot2::annotate("text", x = xticks[1], y = (yticks[length(yticks)] - ((yticks[length(yticks)] - yticks[length(yticks) - 1]) / 2)),
                                label = paste0("italic(r) == ", co), size = 8, parse = TRUE, hjust = -0.5, vjust = 0.5)
    p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"), panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"))
    
    p <- JASPgraphs::themeJasp(p)
    return(createJaspPlot(plot = p, title = "Correlation plot", width = 500, height = 400))
}

.poly.predJfA <- function(fit, plot = NULL, line=FALSE, xMin, xMax, lwd) {
  # create function formula
  f <- vector("character", 0)

  for (i in seq_along(coef(fit))) {
    if (i == 1) {
      temp <- paste(coef(fit)[[i]])
      f <- paste(f, temp, sep="")
    }

    if (i > 1) {
      temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
      f <- paste(f, temp, sep="+")
    }
  }

  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text=f))

  if (line == FALSE) {
    return(predY)
  }

  if (line) {
    plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),mapping = ggplot2::aes(x = x, y = predY), size=lwd, lty = 1)
    return(plot)
  }
}

.readDataProcedure <- function(options, jaspResults){
  
  recordNumberVariable                    <- options[["recordNumberVariable"]]
  if(recordNumberVariable == "")          recordNumberVariable <- NULL 
  monetaryVariable                        <- options[["monetaryVariable"]]
  if(monetaryVariable == "")              monetaryVariable <- NULL 
  
  if(!is.null(recordNumberVariable)){
    variables                             <- recordNumberVariable
    if(!is.null(monetaryVariable)){
      variables <- c(variables, monetaryVariable)
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)
      jaspResults[["N"]]                  <- createJaspState(nrow(dataset))
      jaspResults[["total_data_value"]]   <- createJaspState( ceiling(sum(dataset[, .v(monetaryVariable)])))
      jaspResults[["ready"]]              <- createJaspState(TRUE) # Ready for analysis
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)
      jaspResults[["N"]]                  <- createJaspState(nrow(dataset))
      jaspResults[["total_data_value"]]   <- createJaspState(0.01)
      if(options[["materiality"]] == "materialityRelative"){
        jaspResults[["ready"]]            <- createJaspState(TRUE) # Ready for analysis
      } else {
        jaspResults[["ready"]]            <- createJaspState(FALSE) # Ready for analysis
      }
    }
  } else {
      dataset                             <- NULL
      jaspResults[["N"]]                  <- createJaspState(0)
      jaspResults[["total_data_value"]]   <- createJaspState(0.01)
      jaspResults[["ready"]]              <- createJaspState(FALSE)
  }
  materialityReady <- ifelse(options[["materiality"]] == "materialityRelative", yes = options[["materialityPercentage"]], no = options[["materialityValue"]])
  if(materialityReady == 0)
    jaspResults[["ready"]]              <- createJaspState(FALSE)

  jaspResults[["N"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["total_data_value"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["ready"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable", "materiality"))
  return(dataset)
}

.readDataSelection <- function(options){
  recordVariable                  <- unlist(options[["recordNumberVariable"]])
  if(recordVariable == "")        recordVariable <- NULL
  rankingVariable                 <- unlist(options[["rankingVariable"]])
  if(rankingVariable == "")       rankingVariable <- NULL
  monetaryVariable                <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")      monetaryVariable <- NULL
  variables                       <- unlist(options[["additionalVariables"]])
  variables.to.read               <- c(recordVariable, variables, rankingVariable, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)
  return(dataset)
}

.execution <- function(options, jaspResults){
  if(options[["pasteVariables"]] && is.null(jaspResults[["pastingDone"]]$object)){  
    dataset                       <- .readDataSetToEnd(columns.as.numeric = options[["recordNumberVariable"]])
    sampleFilter                  <- rep(0, jaspResults[["N"]]$object)
    rowNumber                     <- which(dataset[, .v(options[["recordNumberVariable"]])] %in% jaspResults[["sample"]]$object[, .v(options[["recordNumberVariable"]])])
    noOfTimesInSample             <- table(jaspResults[["sampleVector"]]$object)
    sampleFilter[rowNumber]       <- 1 * noOfTimesInSample
    sampleFilter                  <- as.integer(sampleFilter)
    emptyVariable                 <- rep(NA, jaspResults[["N"]]$object)
    .setColumnDataAsNominal(options[["sampleFilter"]], sampleFilter)
    #base::switch(options[["variableType"]], "variableTypeCorrect" = .setColumnDataAsNominal(options[["variableName"]], emptyVariable), "variableTypeAuditValues" = .setColumnDataAsScale(options[["variableName"]], emptyVariable))
    jaspResults[["pastingDone"]]  <- createJaspState(TRUE)                
  }
}

.readDataEvaluation <- function(options, jaspResults){
  recordVariable                  <- unlist(options[["recordNumberVariable"]])
  if(recordVariable == "")        recordVariable <- NULL
  monetaryVariable                <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")      monetaryVariable <- NULL
  sampleFilter                    <- unlist(options[["sampleFilter"]])
  if(sampleFilter == "")          sampleFilter <- NULL
  auditResult                     <- unlist(options[["auditResult"]])
  if(auditResult == "")           auditResult <- NULL
  variables.to.read               <- c(recordVariable, auditResult, sampleFilter, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  jaspResults[["runEvaluation"]] <- createJaspState( (!is.null(auditResult) && !is.null(sampleFilter)) )
  jaspResults[["runEvaluation"]]$dependOn(options = c("auditResult", "sampleFilter"))
  return(dataset)
}

.errorHandlingProcedure <- function(options, dataset){
  variables <- NULL
  if(options[["recordNumberVariable"]] != "")
    variables <- c(variables, options[["recordNumberVariable"]])
  if(options[["monetaryVariable"]] != "")
    variables <- c(variables, options[["monetaryVariable"]])
  n <- nrow(dataset)

    .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
            all.target = variables, message="short", observations.amount= paste0("< ", n),
            exitAnalysisIfErrors = TRUE)
}