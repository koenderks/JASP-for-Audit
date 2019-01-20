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
