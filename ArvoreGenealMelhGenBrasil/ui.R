shinyUI(
  dashboardPage(
    scrollToTop = TRUE,
    title = dashboardBrand(
      title = "Genética e Melhoramento Animal"
    ),
    dark = FALSE,
    skin = "light",
    header = dashboardHeader(
      title = dashboardBrand(
        title = tags$span("Genética e Melhoramento Animal",
                          class = "brand-text",
                          style = "font-size:16px; line-height:1.1;"),
        color = "lightblue",
        opacity = 0.8
      )
    ),
    sidebar = dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem(
          text = "Árvore 'genealógica'",
          tabName = "arvGen",
          icon = icon("tree")
        )
      ),
    ),
    body = dashboardBody(
      tags$head(tags$style(HTML("
        .main-header { 
          position: sticky; 
          top: 0; 
          z-index: 1040;
        }
      "))),
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "arvGen",
          fluidRow(
            box(
              width = 12,
              id = "box_formulario",
              title = "Formulário",
              status = "secondary",
              icon = icon("list-alt"),
              solidHeader = TRUE,
              div(
                id = "form_inputs",
                textInput(
                  inputId = "aluno", 
                  label = "Nome do Aluno:"
                ),
                hr(), 
                h3("Graduação"),
                fluidRow(
                  column(
                    width = 4,
                    virtualSelectInput(
                      inputId = "gradOrient", 
                      label = "Selecione o orientador",
                      choices = professores,
                      allowNewOption = TRUE
                    ),
                  ),
                  column(
                    width = 3,
                    virtualSelectInput(
                      inputId = "gradUni", 
                      label = "Selecione a Universidade",
                      choices = unique(
                        c(
                          Universities, 
                          "Universidade Federal de Viçosa",
                          "Universidade Federal da Bahia", 
                          "Universidade de São Paulo"
                        )
                      ),
                      allowNewOption = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    virtualSelectInput(
                      inputId = "gradCurso", 
                      label = "Selecione o curso",
                      choices = unique(
                        c(
                          graduacao,
                          "Medicina Veterinária", 
                          "Zootecnia", 
                          "Biologia", 
                          "Biotecnologia"
                        )
                      ),
                      allowNewOption = TRUE
                    )
                  ),
                  column(
                    width = 2,
                    airDatepickerInput(
                      inputId = "gradDateInput",
                      label = "Data da defesa",
                      autoClose = TRUE,
                      value = Sys.Date()
                    ),
                  ),
                ),
                hr(), 
                h3("Mestrado"),
                fluidRow(
                  column(
                    width = 4,
                    virtualSelectInput(
                      inputId = "mestOrient", 
                      label = "Selecione o orientador",
                      choices = professores,
                      allowNewOption = TRUE
                    ),
                  ),
                  column(
                    width = 3,
                    virtualSelectInput(
                      inputId = "mestUni", 
                      label = "Selecione a Universidade",
                      choices = unique(
                        c(
                          Universities, 
                          "Universidade Federal de Viçosa",
                          "Universidade Federal da Bahia", 
                          "Universidade de São Paulo"
                        )
                      ),
                      allowNewOption = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    virtualSelectInput(
                      inputId = "mestCurso", 
                      label = "Selecione o curso",
                      choices = unique(
                        c(
                          masterDoctCourses,
                          "Melhoramento Animal", 
                          "Genética e Melhoramento", 
                          "Genética", 
                          "Biotecnologia"
                        )
                      ),
                      allowNewOption = TRUE
                    )
                  ),
                  column(
                    width = 2,
                    airDatepickerInput(
                      inputId = "mestDateInput",
                      label = "Data da defesa",
                      autoClose = TRUE,
                      value = Sys.Date()
                    ),
                  ),
                ),
                hr(),
                h3("Doutorado"),
                fluidRow(
                  column(
                    width = 4,
                    virtualSelectInput(
                      inputId = "docOrient", 
                      label = "Selecione o orientador",
                      choices = professores,
                      allowNewOption = TRUE
                    ),
                  ),
                  column(
                    width = 3,
                    virtualSelectInput(
                      inputId = "docUni", 
                      label = "Selecione a Universidade",
                      choices = unique(
                        c(
                          Universities, 
                          "Universidade Federal de Viçosa",
                          "Universidade Federal da Bahia", 
                          "Universidade de São Paulo"
                        )
                      ),
                      allowNewOption = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    virtualSelectInput(
                      inputId = "docCurso", 
                      label = "Selecione o curso",
                      choices =unique(
                        c(
                          masterDoctCourses,
                          "Melhoramento Animal", 
                          "Genética e Melhoramento", 
                          "Genética", 
                          "Biotecnologia"
                        )
                      ),
                      allowNewOption = TRUE
                    )
                  ),
                  column(
                    width = 2,
                    airDatepickerInput(
                      inputId = "docDateInput",
                      label = "Data da defesa",
                      autoClose = TRUE,
                      value = Sys.Date()
                    ),
                  ),
                ),
                prettyCheckbox(
                  inputId = "docSand",
                  label = "Fiz doutorado Sandwich", 
                  value = FALSE,
                  status = "success",
                  icon = icon("square-check"), 
                  outline = TRUE,
                  animation = "jelly"
                ),
                conditionalPanel(
                  condition = "input.docSand",
                  h3("Doutorado Sandwich"),
                  fluidRow(
                    column(
                      width = 6,
                      virtualSelectInput(
                        inputId = "sandOrient", 
                        label = "Selecione o orientador",
                        choices = professores,
                        allowNewOption = TRUE
                      ),
                    ),
                    column(
                      width = 6,
                      virtualSelectInput(
                        inputId = "sandUni", 
                        label = "Selecione a Universidade",
                        choices = unique(
                          c(
                            Universities, 
                            "Universidade Federal de Viçosa",
                            "Universidade Federal da Bahia", 
                            "Universidade de São Paulo"
                          )
                        ),
                        allowNewOption = TRUE
                      )
                    )
                  ),
                ),
                hr(),
                actionButton(   
                  inputId = "addDado",
                  label =  "Adicionar",
                  icon = icon("plus"),
                  title = "Adicionar"
                )
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Árvore genealógica do melhoramento animal no Brasil",
              status = "success",
              icon = icon("project-diagram"),
              solidHeader = TRUE,
              selectInput(
                "graphType",
                label = "Escolha o gráfico",
                choices = list("Sankey Network" = "sankey", "Vis Network" = "visNet")
              ),
              uiOutput("graphics")
            )
          )
        )
      )
    )
  )
)