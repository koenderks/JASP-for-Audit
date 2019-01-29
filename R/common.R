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

.plotCriticalErrorsPrior <- function(allowed.errors, reject.errors, jaspResults){

    errorrange <- 0:max(reject.errors)
    errors <- c(allowed.errors, reject.errors)
    fill <- c(rep(rgb(0,1,.5,.7), length(allowed.errors)),
              rep(rgb(1,0,0,.7), length(reject.errors)))

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
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 9, shape = 22, fill = c(rgb(0,1,.5,.7),rgb(1,0,0,.7)), stroke = 2, color = "black")), order = 1)

    p <- p + ggplot2::scale_x_continuous(breaks = errorrange)


    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")
    p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_text(size = 17))

    return(createJaspPlot(plot = p, title = "Decision Plot", width = 500, height = 300))

}
