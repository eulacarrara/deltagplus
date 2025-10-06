shinyServer(function(input, output, session) {
    
    
    
    melhorRel <- reactiveValues(
        dados = NULL,
        novoDado = NULL,
        sankeyGraph = NULL,
        visGraph = NULL
    )
    
    
    
    observeEvent(input$addDado, {
        
        print(input$aluno)
        if (input$aluno == "") {
            shinyWidgets::sendSweetAlert(
                session,
                title = "Erro ao enviar dados!!!",
                text = "Nome não pode ficar em branco",
                type = "error"
            )
        }
        
        req(input$aluno != "")
        
        
        
        aluno <- tbl(con, dbplyr::in_schema("public", "melhoramento_ufv_brasil_all")) %>%
            filter(Nome == input$aluno) %>% 
            collect()
        
        if (nrow(aluno) > 0) {
            shinyWidgets::sendSweetAlert(
                session,
                title = "Nome já existe!!!",
                text = glue("{input$aluno} já existe no banco de dados"),
                type = "warning"
            )
        }
        
        req(nrow(aluno) == 0)
        
        
        melhorRel$novoDado <- createNewEntry(
            aluno = input$aluno,
            gradOrient = input$gradOrient, 
            gradUni = input$gradUni,
            gradCurso = input$gradCurso, 
            gradDateInput = input$gradDateInput,
            mestOrient = input$mestOrient, 
            mestUni = input$mestUni, 
            mestCurso = input$mestCurso, 
            mestDateInput = input$mestDateInput,
            docOrient = input$docOrient,  
            docUni = input$docUni,  
            docCurso = input$docCurso,  
            docDateInput = input$docDateInput,
            docSand = input$docSand,    
            sandOrient = input$sandOrient, 
            sandUni = input$sandUni,
            company = input$company, 
            cargo = input$cargo
        )
        
        
        html_sumario <- build_confirm_html(melhorRel$novoDado, font_px = 16)  # ajuste 11–13 se quiser
        
        confirmSweetAlert(
            session,
            inputId    = "confirmaNovoDado",
            title      = "Confirmar dados?",
            text       = HTML(html_sumario),
            type       = "question",
            html       = TRUE,
            btn_labels = c("Corrigir", "Confirmar"),
            btn_colors = c("#6c757d", "#28a745"),
            closeOnClickOutside = FALSE
        )
        
    })
    
    
    
    
    observeEvent(input$confirmaNovoDado, ignoreInit = TRUE, {
        
        if (input$confirmaNovoDado) {
            
            
            DBI::dbAppendTable(
                con,
                name  = DBI::Id(schema = "public", table = "melhoramento_ufv_brasil_all"),
                value = melhorRel$novoDado
            )
            
            melhorRel$dados <- tbl(con, dbplyr::in_schema("public", "melhoramento_ufv_brasil_all")) %>%
                collect()
            
            
            
            novoAluno <- melhorRel$dados %>% 
                filter(Nome == input$aluno)
            
            if (nrow(novoAluno) > 0) {
                shinyWidgets::sendSweetAlert(
                    session,
                    title = "Sucesso!!!",
                    text = glue("{input$aluno} adicionado com sucesso"),
                    type = "success"
                )
                
                bs4Dash::updateBox("box_formulario", action = "toggle")
                
                updateTextInput(session, "aluno", value = "")
                
                melhorRel$sankeyGraph  <- makeSankey_echarts(melhorRel$dados)
                melhorRel$visGraph = buildVisNetwork(melhorRel$dados)
                
            }
            
            
        }
        
    })
    
    # observeEvent(input$after_confirm, {
    #     shinyjs::runjs("toggleSidebar();")
    #     print("tudo certo")
    #     # shinyjs::
    #     
    #     # DBI::dbCreateTable(
    #     #     con,
    #     #     name  = DBI::Id(schema = "public", table = "melhoramento_ufv_brasil_new"),
    #     #     fields = melhorRel$novoDado
    #     # )
    #     # 
    #     # dadosMelhoramento <- tbl(con, dbplyr::in_schema("public", "melhoramento_ufv_brasil")) %>%
    #     #     select(-id) %>%
    #     #     collect()
    #     
    #     
    # })
    
    
    
    observe({
        melhorRel$dados <- dadosMelhoramento
    })
    
    
    
    observe({



        updateVirtualSelect(
            session = session,
            inputId = "docOri",
            label = "Filtre por Orientador de Doutorado",
            choices = orientadoresDoc
        )



        updateVirtualSelect(
            session = session,
            inputId = "mestOri",
            label = "Filtre por Orientador de Mestrado",
            choices = orientadoresMest
        )



        updateVirtualSelect(
            session = session,
            inputId = "gradOri",
            label = "Filtre por Orientador de Graduação",
            choices = orientadoresGrad
        )




        virtualSelectInput(
            inputId = "nome",
            label = "Filtre por seu Nome",
            choices = nomes
        )

    })
    
    
    
    output$graphics <- renderUI({
        
        if (input$graphType == "visNet") {
            visNetworkOutput("net", height = "80vh")
            
        } else if (input$graphType == "sankey") {
            # echarts4r::echarts4rOutput(
            #     outputId = "arvoreSankey", 
            #     height = "720px", 
            #     width = "100%"
            #     )
            output$arvoreSankeyUI <- renderUI({
                req(!is.null(melhorRel$dados))
                n_nodes <- length(unique(c(
                    na.omit(dadosMelhoramento$Nome),
                    na.omit(dadosMelhoramento$Orientador_Graduacao),
                    na.omit(dadosMelhoramento$Orientador_Mestrado),
                    na.omit(dadosMelhoramento$Orientador_Doutorado)
                )))
                h <- max(420L, min(26L * n_nodes, 12000L))
                echarts4r::echarts4rOutput("arvoreSankey", height = paste0(h, "px"), width = "100%")
            })
            
            
        }
        
        
        
        
        
        
    })
    
    
    
    observe({
        melhorRel$sankeyGraph  <- makeSankey_echarts(melhorRel$dados)
        melhorRel$visGraph = buildVisNetwork(melhorRel$dados)
        
    })
    
    

    output$arvoreSankey <- echarts4r::renderEcharts4r({
        # makeSankey_echarts(melhorRel$dados)
        melhorRel$sankeyGraph
    })
    
    
    
    
    output$net <- renderVisNetwork({
        # buildVisNetwork(melhorRel$dados)
        melhorRel$visGraph
        
        
    })
    
    
    
    session$onSessionEnded(function() stopApp())
})
