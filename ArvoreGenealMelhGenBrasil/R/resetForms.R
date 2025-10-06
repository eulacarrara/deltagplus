resetForms <- function(session) {
    # 2.1) Reset geral do contêiner (funciona para inputs base do Shiny)
    # shinyjs::reset("form_inputs")
    
    updateTextInput(session, "aluno", value = "")
    shinyWidgets::updateVirtualSelect(session, "gradOrient", selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "gradUni",    selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "gradCurso",  selected = NULL)
    
    shinyWidgets::updateVirtualSelect(session, "mestOrient", selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "mestUni",    selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "mestCurso",  selected = NULL)
    
    shinyWidgets::updateVirtualSelect(session, "docOrient",  selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "docUni",     selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "docCurso",   selected = NULL)
    
    shinyWidgets::updateVirtualSelect(session, "sandOrient", selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "sandUni",    selected = NULL)
    
    shinyWidgets::updateVirtualSelect(session, "company",    selected = NULL)
    shinyWidgets::updateVirtualSelect(session, "cargo",      selected = NULL)
    
    # Datepicker: limpa datas
    shinyWidgets::updateAirDateInput(session, "gradDateInput", value = NULL)
    shinyWidgets::updateAirDateInput(session, "mestDateInput", value = NULL)
    shinyWidgets::updateAirDateInput(session, "docDateInput",  value = NULL)
    
    # Checkbox Sandwich: volta para FALSE (esconde painel condicional)
    shinyWidgets::updatePrettyCheckbox(session, "docSand", value = FALSE)
    
    
}