modalFilters <- function() {
    
    
    
    
    # as.data.table(contaminationTbl)
    
    
    modalDialog(
        id      = "modalFilters",
        title   = "Applying Filters",
        # status  = "info",
        size    = "l",
        trigger = "filter",
        easyClose = FALSE,
        virtualSelectInput(
            inputId   = "docOri",
            label     = "Filtre por Orientador de Doutorado",
            choices   = unique(dadosMelhoramento$Doutorado),
            selected  = NULL,
            multiple  = TRUE,
            search = TRUE,
            markSearchResults = TRUE,
            options   = list(
                maxOptions       = 1,
                noneSelectedText = "Selecione…",
                liveSearch       = TRUE
            )
        ),
        virtualSelectInput(
            inputId  = "mestOri",
            label    = "Filtre por Orientador de Mestrado",
            choices   = unique(dadosMelhoramento$Mestrado),
            selected  = NULL,
            multiple  = TRUE,
            search = TRUE,
            markSearchResults = TRUE,
            options   = list(
                maxOptions       = 1,
                noneSelectedText = "Selecione…",
                liveSearch       = TRUE
            )
        ),
        virtualSelectInput(
            inputId  = "gradOri",
            label    = "Filtre por Orientador de Graduação",
            choices   = unique(dadosMelhoramento$Graduacao),
            selected  = NULL,
            multiple  = TRUE,
            search = TRUE,
            markSearchResults = TRUE,
            options   = list(
                maxOptions       = 1,
                noneSelectedText = "Selecione…",
                liveSearch       = TRUE
            )
        ),
        virtualSelectInput(
            inputId  = "nome",
            label    = "Filtre por seu Nome",
            choices   = unique(dadosMelhoramento$Nome),
            selected  = NULL,
            multiple  = TRUE,
            search = TRUE,
            markSearchResults = TRUE,
            options   = list(
                maxOptions       = 1,
                noneSelectedText = "Selecione…",
                liveSearch       = TRUE
            )
        ),
        footer = tagList(
            modalButton(label = "Cancel"),
            actionButton(
                inputId = "okFilter", 
                label = "Apply Filter", 
                icon = icon("filter"),
                class = "btn btn-success",
                style = "color: #FFFFFF; font-weight: bold"
            )
        )
    )
    
}