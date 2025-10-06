pacman::p_load(
    shiny,
    httr,
    jsonlite,
    shinyWidgets,
    data.table,
    dplyr,
    tibble,
    bs4Dash,
    fresh,
    shinyjs,
    DT,
    firebase,
    rlist,
    googleAuthR,
    curl,
    stringr,
    glue,
    cli,
    parsedate,
    tidyr,
    plotly,
    openxlsx, 
    lubridate,
    httr2,
    RPostgres,
    DBI,
    stringr,
    networkD3,
    webshot2,
    htmlwidgets,
    echarts4r,
    readr,
    dbplyr,
    odbc,
    forcats,
    purrr,
    shinybusy,
    shinyBS,
    scales,
    rlang,
    htmltools,
    visNetwork,
    igraph,
    stringi
)


pooler_host <- "aws-0-sa-east-1.pooler.supabase.com"  # COLE EXATO do painel
user <- "postgres.jsxoiiwvayxrddttpfgh"
project_ref <- "jsxoiiwvayxrddttpfgh"


# pegar IPv4 do pooler (descarta IPv6)
ipv4_only <- function(host) {
    ips <- tryCatch(curl::nslookup(host), error = function(e) character(0))
    ipv4 <- ips[!str_detect(ips, ":")]
    if (length(ipv4)) ipv4[[1]] else NA_character_
}
ip_pooler <- ipv4_only(pooler_host)

# tenta com hostaddr=IPv4 (bypass DNS) mantendo host p/ SNI/SSL
con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host      = pooler_host,
    hostaddr  = if (!is.na(ip_pooler)) ip_pooler else NULL,
    port      = 5432,
    dbname    = "postgres",
    user      = paste0("postgres.", project_ref),,
    password  = "RdKyynJpx4rai8W",
    sslmode   = "require"
)



dadosMelhoramento <- tbl(con, dbplyr::in_schema("public", "melhoramento_ufv_brasil_all")) %>% 
    # select(-id) %>% 
    collect()


dadosMelhoramento %>% 
    filter(Nome == "Haniel Cedraz de Oliveira")
# as.data.table(dadosMelhoramento)
# 
# 
# df <- dadosMelhoramento %>%
#     filter(Orientador_Doutorado == "Paulo Sávio Lopes")
# 
# 
# df <- filterData(
#     df = dadosMelhoramento, 
#     Orientador_Graduacao = NULL,
#     Orientador_Mestrado = NULL,
#     Orientador_Doutorado = "Paulo Sávio Lopes",
#     Nome = NULL
# ) # 
# 
# makeSankey_echarts(df)

orientadoresDoc <- dadosMelhoramento %>% 
    filter(!Orientador_Doutorado %in% c("NA", "")) %>% 
    pull(Orientador_Doutorado) %>% unique() %>% sort()



orientadoresMest <- dadosMelhoramento %>% 
    filter(!Orientador_Mestrado %in% c("NA", "")) %>% 
    pull(Orientador_Mestrado) %>% unique() %>% sort()

orientadoresGrad <- dadosMelhoramento %>%
    filter(!Orientador_Graduacao %in% c("NA", "")) %>%
    pull(Orientador_Graduacao) %>% unique() %>% sort()


nomes <- dadosMelhoramento %>%
    filter(!Nome %in% c("NA", "")) %>%
    pull(Nome) %>% unique() %>% sort()



professores <- dadosMelhoramento %>% 
    select(
        Nome, 
        Orientador_Graduacao, 
        Orientador_Mestrado, 
        Orientador_Doutorado
    ) %>%
    pivot_longer(everything(), names_to = "source", values_to = "Professors") %>%
    filter(!is.na(Professors)) %>% 
    filter(Professors != "") %>% 
    pull(Professors) %>% unique()


# professores <- unique(
#     c(
#         dadosMelhoramento$Nome, 
#         dadosMelhoramento$Orientador_Graduacao, 
#         dadosMelhoramento$Orientador_Mestrado, 
#         dadosMelhoramento$Orientador_Doutorado
#     )
# ) %>% sort()
# 
# 
# professores <- trimws(professores[nzchar(professores)])



Universities <- dadosMelhoramento %>% 
    select(
        Universidade_Graduacao,
        Universidade_Mestrado,
        Universidade_Doutorado,
        Universidade_Sandwich
    ) %>%
    pivot_longer(everything(), names_to = "source", values_to = "Universities") %>%
    filter(!is.na(Universities)) %>% 
    pull(Universities) %>% unique()


graduacao <- dadosMelhoramento %>% 
    select(Curso_Graduacao) %>%
    filter(!is.na(Curso_Graduacao)) %>% 
    pull(Curso_Graduacao) %>% unique()


masterDoctCourses <- dadosMelhoramento %>% 
    select(
        Curso_Mestrado,
        Curso_Doutorado
    ) %>%
    pivot_longer(everything(), names_to = "source", values_to = "Cursos") %>%
    filter(!is.na(Cursos)) %>% 
    pull(Cursos) %>% unique()
