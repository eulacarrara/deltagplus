
createNewEntry <- function(
        aluno,
        gradOrient, gradUni, gradCurso, gradDateInput,
        mestOrient, mestUni, mestCurso, mestDateInput,
        docOrient,  docUni,  docCurso,  docDateInput,
        docSand = FALSE, sandOrient = NULL, sandUni = NULL,
        company = NULL, cargo = NULL
) {
    
    to_chr <- \(x) x %>% as.character() %>% stringr::str_squish() %>% na_if("")
    to_date <- \(x) if (inherits(x, "Date")) x else suppressWarnings(as.Date(x))
    
    newEntry <- tibble(
        Nome = aluno %>% to_chr(),
        Orientador_Graduacao = gradOrient %>% to_chr(),
        Universidade_Graduacao = gradUni %>% to_chr(),
        Curso_Graduacao = gradCurso %>% to_chr(),
        DataDefesa_Graduacao = gradDateInput %>% to_date(),
        
        Orientador_Mestrado = mestOrient %>% to_chr(),
        Universidade_Mestrado = mestUni %>% to_chr(),
        Curso_Mestrado = mestCurso %>% to_chr(),
        DataDefesa_Mestrado = mestDateInput %>% to_date(),
        
        Orientador_Doutorado = docOrient %>% to_chr(),
        Universidade_Doutorado = docUni %>% to_chr(),
        Curso_Doutorado = docCurso %>% to_chr(),
        DataDefesa_Doutorado = docDateInput %>% to_date(),
        
        Dout_Sandwich = isTRUE(docSand),
        Orientador_Sandwich = if (isTRUE(docSand)) sandOrient %>% to_chr() else NA_character_,
        Universidade_Sandwich = if (isTRUE(docSand)) sandUni %>% to_chr() else NA_character_,
        
        Empresa = company %>% to_chr(),
        Cargo = cargo %>% to_chr()
    )
    
    # print(newEntry)
    
    
    return(newEntry)
}

# # 
# aluno = "Haniel"
# gradOrient = "Adriana Luiza Somavilla"
# gradUni = "Universidade Federal de Viçosa"
# gradCurso = "Medicina Veterinária"
# gradDateInput = "2025-09-01"
# mestOrient = "Adriana Luiza Somavilla"
# mestUni = "Universidade Federal de Viçosa"
# mestCurso = "Melhoramento Animal"
# mestDateInput = "2025-09-01"
# docOrient = "Adriana Luiza Somavilla"
# docUni = "Universidade Federal de Viçosa"
# docCurso = "Melhoramento Animal"
# docDateInput = "2025-09-01"
# docSand = FALSE
# sandOrient = "Adriana Luiza Somavilla"
# sandUni = "Universidade Federal de Viçosa"
# company = "Empresa A"
# cargo = "Analista"
# 
# # 
# d <- createNewEntry(
#     aluno,
#     gradOrient ,
#     gradUni,
#     gradCurso ,
#     gradDateInput,
#     mestOrient ,
#     mestUni ,
#     mestCurso ,
#     mestDateInput,
#     docOrient ,
#     docUni ,
#     docCurso ,
#     docDateInput,
#     docSand    ,
#     sandOrient ,
#     sandUni,
#     company ,
#     cargo
# )
