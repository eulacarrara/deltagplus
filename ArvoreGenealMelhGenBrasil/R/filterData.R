
filterData <- function(df,
                       Orientador_Graduacao = NULL,
                       Orientador_Mestrado = NULL,
                       Orientador_Doutorado = NULL,
                       Nome = NULL) {
    
    # Nome <- "Adriana Luiza Somavilla"
    # 1) put all filters in a named list
    filters <- list(
        Orientador_Graduacao = Orientador_Graduacao,
        Orientador_Mestrado =  Orientador_Mestrado,
        Orientador_Doutorado = Orientador_Doutorado,
        Nome = Nome
    )
    
    # 2) coerce any length-1 "" into NULL, then drop NULLs or zero-length
    filters <- filters %>%
        map(function(x) {
            if (is.character(x) && length(x) == 1 && x == "") {
                NULL
            } else {
                x
            }
        }) %>%
        keep(~ !is.null(.) && length(.) > 0)
    
    # 3) fold into successive filter() calls
    finalTable <- reduce2(
        .x    = names(filters),  # column names
        .y    = filters,         # their selected values
        .init = df,
        .f    = function(data, col, vals) {
            data %>% filter(.data[[col]] %in% vals)
        }
    ) %>% 
        collect()
    
    return(finalTable)
}
